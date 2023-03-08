------------------------------------------------------------------------------
--                            GNAT LSIF Indexer                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;
with Interfaces;

with VSS.Strings.Conversions;
with VSS.String_Vectors;

with GNATCOLL.VFS;
with Libadalang.Analysis;
with Libadalang.Common;

with GNATdoc.Comments.Helpers;

with LSIF.Command_Line;
with LSIF.Configuration;
with LSIF.Projects;
with LSIF.Serializer;

procedure LSIF.Driver is

   use type VSS.Strings.Virtual_String;

   type Range_Information is record
      Id         : Interfaces.Integer_64;
      Sloc       : Libadalang.Slocs.Source_Location_Range;
      Definition : Libadalang.Analysis.Defining_Name;
   end record;

   package Range_Vectors is
      new Ada.Containers.Vectors (Positive, Range_Information);

   type Document_Information is record
      Id     : Interfaces.Integer_64;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Ranges : Range_Vectors.Vector;
   end record;

   type Document_Information_Access is access all Document_Information;

   package Document_Maps is
     new Ada.Containers.Hashed_Maps
       (GNATCOLL.VFS.Virtual_File,
        Document_Information_Access,
        GNATCOLL.VFS.Full_Name_Hash,
        GNATCOLL.VFS."=");

   type Defining_Name_Information is record
      Result_Set_Id       : Interfaces.Integer_64;
      Reference_Result_Id : Interfaces.Integer_64;
   end record;

   function Hash
     (Node : Libadalang.Analysis.Defining_Name)
      return Ada.Containers.Hash_Type is (Node.As_Ada_Node.Hash);

   package Defining_Name_Maps is
     new Ada.Containers.Hashed_Maps
       (Libadalang.Analysis.Defining_Name,
        Defining_Name_Information,
        Hash,
        Libadalang.Analysis."=",
        "=");

   Documents : Document_Maps.Map;
   Defs      : Defining_Name_Maps.Map;

   procedure Pass_1
     (Document : in out Document_Information;
      Node     : Libadalang.Analysis.Ada_Node'Class);

   procedure Pass_2 (Document : in out Document_Information);

   procedure Analyze_Range
     (Document : in out Document_Information;
      First    : Libadalang.Common.Token_Reference;
      Last     : Libadalang.Common.Token_Reference);

   -------------------
   -- Analyze_Range --
   -------------------

   procedure Analyze_Range
     (Document : in out Document_Information;
      First    : Libadalang.Common.Token_Reference;
      Last     : Libadalang.Common.Token_Reference)
   is
      First_Location : Libadalang.Slocs.Source_Location_Range :=
        Libadalang.Common.Sloc_Range (Libadalang.Common.Data (First));
      Last_Location  : Libadalang.Slocs.Source_Location_Range :=
        Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Last));
      Id_Node        : Libadalang.Analysis.Ada_Node :=
        Document.Unit.Root.Lookup
          ((Last_Location.Start_Line, Last_Location.Start_Column));
      Canonical      : Libadalang.Analysis.Defining_Name;
      Is_Canonical   : Boolean;

   begin
      if Id_Node.Kind not in Libadalang.Common.Ada_Name then
         --  Node is not a name, nothing to do.

         return;
      end if;

      --  Lookup for defining names: current, when name if defining, and
      --  canonical.

      begin
         declare
            use type Libadalang.Analysis.Defining_Name;

            Name       : constant Libadalang.Analysis.Name          :=
              Id_Node.As_Name;
            Referenced : constant Libadalang.Analysis.Defining_Name :=
              Name.P_Referenced_Defining_Name;
            Current    : constant Libadalang.Analysis.Defining_Name :=
              Name.P_Enclosing_Defining_Name;

         begin
            if Referenced.Is_Null then
               --  P_Referenced_Defining_Name returns null for the canonical
               --  definition of the entity.

               if not Current.Is_Null then
                  Canonical := Current.P_Canonical_Part;
               end if;

            else
               Canonical := Referenced.P_Canonical_Part;
            end if;

            Is_Canonical := Canonical = Current;
         end;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "LAL name resolution failure at "
               & Libadalang.Analysis.Image (Id_Node)
               & ": "
               & Ada.Exceptions.Exception_Information (E));

            return;
      end;

      --  Return when there is no declaration found

      if Canonical.Is_Null then
         return;
      end if;

      --  Return when declaration doesn't belongs to project's files.

      if not Documents.Contains
               (GNATCOLL.VFS.Create_From_UTF8 (Canonical.Unit.Get_Filename))
      then
         return;
      end if;

      --  Create "range" and generate information

      declare
         Vertex          : Range_Information :=
           (Id         => LSIF.Serializer.Allocate_Identifier,
            Sloc       =>
              (Start_Line   => First_Location.Start_Line,
               Start_Column => First_Location.Start_Column,
               End_Line     => Last_Location.End_Line,
               End_Column   => Last_Location.End_Column),
            Definition => Canonical);
         Hover_Result_Id : Interfaces.Integer_64 := 0;
         Info            : Defining_Name_Information;
         Code_Snippet    : VSS.String_Vectors.Virtual_String_Vector;
         Documentation   : VSS.String_Vectors.Virtual_String_Vector;

      begin
         Document.Ranges.Append (Vertex);

         --  Generate "range" vertex

         LSIF.Serializer.Write_Range_Vertex
           (Vertex.Id,
            "",
            Vertex.Sloc.Start_Line,
            Vertex.Sloc.Start_Column,
            "",
            Vertex.Sloc.End_Line,
            Vertex.Sloc.End_Column);

         if Is_Canonical then
            --  "resultSet"

            Info.Result_Set_Id := LSIF.Serializer.Allocate_Identifier;

            LSIF.Serializer.Write_Result_Set_Vertex (Info.Result_Set_Id);
            LSIF.Serializer.Write_Next_Edge (Vertex.Id, Info.Result_Set_Id);

            --  "textDocument/hover"

            Hover_Result_Id := LSIF.Serializer.Allocate_Identifier;

            begin
               GNATdoc.Comments.Helpers.Get_Plain_Text_Documentation
                 (Canonical, (others => <>), Code_Snippet, Documentation);

            exception
               when E : others =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "GNATdoc exception at "
                     & Libadalang.Analysis.Image (Id_Node)
                     & ": "
                     & Ada.Exceptions.Exception_Information (E));
            end;

            LSIF.Serializer.Write_Hover_Result_Vertex
              (Hover_Result_Id,
               Code_Snippet.Join_Lines (VSS.Strings.LF, False),
               Documentation.Join_Lines (VSS.Strings.LF, False));
            LSIF.Serializer.Write_Text_Document_Hover_Edge
              (Info.Result_Set_Id, Hover_Result_Id);

            --  "textDocument/references": "definitions"

            Info.Reference_Result_Id := LSIF.Serializer.Allocate_Identifier;

            LSIF.Serializer.Write_Reference_Result_Vertex
              (Info.Reference_Result_Id);
            LSIF.Serializer.Write_Text_Document_References_Edge
              (Info.Result_Set_Id, Info.Reference_Result_Id);
            LSIF.Serializer.Write_Item_Definitions_Edge
              (Info.Reference_Result_Id, (1 => Vertex.Id), Document.Id);

            Defs.Insert (Vertex.Definition, Info);
         end if;
      end;
   end Analyze_Range;

   ------------
   -- Pass_1 --
   ------------

   procedure Pass_1
     (Document : in out Document_Information;
      Node     : Libadalang.Analysis.Ada_Node'Class)
   is
      use type Libadalang.Common.Token_Reference;
      use all type Libadalang.Common.Token_Kind;

      Token : Libadalang.Common.Token_Reference := Node.Token_Start;
      First : Libadalang.Common.Token_Reference;
      Last  : Libadalang.Common.Token_Reference;

   begin
      --  Go through all tokens of the file and select interesing one. This
      --  algoriphm take in sense behavior of the GitLab's LSIF importer,
      --  instead of follow normal behavior of the token processing. For
      --  instance, sequence of "A.B.C" is processed as single range pointing
      --  to C, while for IDEs it ie expected to have three ranges for A, B,
      --  and C.

      loop
         exit when Token = Libadalang.Common.No_Token;

         --  Ada.Text_IO.Put_Line (Libadalang.Common.Image (Token));

         case Libadalang.Common.Kind (Libadalang.Common.Data (Token)) is
            when Ada_Identifier =>
               First := Token;
               Last  := Token;

               --  GitLab: Lookup for prefixed name
               --
               --  XXX prefixed name can continue on new line!

               loop
                  Token := Libadalang.Common.Next (Token);

                  case Libadalang.Common.Kind
                    (Libadalang.Common.Data (Token))
                  is
                     when Ada_Dot =>
                        null;

                     when Ada_Identifier =>
                        Last := Token;

                     when Ada_String =>
                        --  Operator name.

                        Last := Token;

                     when Ada_Whitespace | Ada_Comma | Ada_Par_Close
                        | Ada_Semicolon | Ada_Tick | Ada_All | Ada_Label_End
                        =>
                        exit;

                     when others =>
                        Ada.Text_IO.Put_Line
                          (Libadalang.Common.Image (Token)
                           & " at "
                           & Libadalang.Slocs.Image (Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Token)))
                          );
                        raise Program_Error;
                  end case;
               end loop;

               Analyze_Range (Document, First, Last);

            when Ada_Dot =>
               First := Token;
               Last  := Token;

               --  GitLab: continuation of the prefixed name
               --
               --  XXX Doesn't check when it starts suffix after close
               --  parenthesis.

               loop
                  Token := Libadalang.Common.Next (Token);

                  case Libadalang.Common.Kind
                    (Libadalang.Common.Data (Token))
                  is
                     when Ada_Dot =>
                        null;

                     when Ada_Identifier =>
                        Last := Token;

                     --  when Ada_String =>
                     --     --  Operator name.
                     --
                     --     Last := Token;
                     --
                     when Ada_Whitespace | Ada_Comma | Ada_Par_Close
                        | Ada_Semicolon | Ada_Tick | Ada_All
                        =>
                        exit;

                     when others =>
                        Ada.Text_IO.Put_Line
                          (Libadalang.Common.Image (Token)
                           & " at "
                           & Libadalang.Slocs.Image (Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Token)))
                          );
                        raise Program_Error;
                  end case;
               end loop;

               if First /= Last then
                  Analyze_Range (Document, First, Last);
               end if;

            when Ada_Equal | Ada_Notequal | Ada_Amp | Ada_Not | Ada_And
               | Ada_Minus | Ada_Plus | Ada_Or | Ada_Gt | Ada_Divide | Ada_Mod
               | Ada_Lte | Ada_Mult | Ada_Lt | Ada_Power | Ada_Xor | Ada_Gte
               | Ada_String | Ada_Char
            =>
               --  - operator
               --  - string literal when used ad operator name
               --  - character literal

               Analyze_Range (Document, Token, Token);

            when Ada_With | Ada_Whitespace | Ada_Semicolon | Ada_Comment
               | Ada_Procedure | Ada_Is | Ada_Use | Ada_Type | Ada_Record
               | Ada_Colon | Ada_End | Ada_Package | Ada_New | Ada_Par_Open
               | Ada_Comma | Ada_Par_Close | Ada_Access | Ada_All | Ada_Assign
               | Ada_Integer | Ada_Tick | Ada_Begin | Ada_Loop | Ada_Exit
               | Ada_When | Ada_Case | Ada_Arrow | Ada_If | Ada_Then
               | Ada_Raise | Ada_Else | Ada_Declare | Ada_Pipe | Ada_In
               | Ada_Elsif | Ada_Others | Ada_Exception | Ada_For | Ada_Of
               | Ada_Constant | Ada_Doubledot | Ada_Diamond | Ada_Termination
               | Ada_Pragma | Ada_Null | Ada_Return | Ada_Out | Ada_Array
               | Ada_Range | Ada_Function | Ada_Body | Ada_Do | Ada_While
               | Ada_Private | Ada_Renames | Ada_Subtype | Ada_At
               | Ada_Limited | Ada_Separate | Ada_Goto | Ada_Label_Start
            =>
               null;

            when others =>
               Ada.Text_IO.Put_Line (Libadalang.Common.Image (Token));
               raise Program_Error;
         end case;

         Token := Libadalang.Common.Next (Token);
      end loop;
   end Pass_1;

   ------------
   -- Pass_2 --
   ------------

   procedure Pass_2 (Document : in out Document_Information) is

      package Definitions_Maps is
        new Ada.Containers.Hashed_Maps
          (Libadalang.Analysis.Defining_Name,
           LSIF.Serializer.Identifier_Vectors.Vector,
           Hash,
           Libadalang.Analysis."=",
           LSIF.Serializer.Identifier_Vectors."=");

      References : Definitions_Maps.Map;

   begin
      for R of Document.Ranges loop
         if Defs.Contains (R.Definition) then
            if References.Contains (R.Definition) then
               declare
                  Aux : LSIF.Serializer.Identifier_Vectors.Vector :=
                    References (R.Definition);

               begin
                  Aux.Append (R.Id);
                  References.Replace (R.Definition, Aux);
               end;

            else
               declare
                  Aux : LSIF.Serializer.Identifier_Vectors.Vector;

               begin
                  Aux.Append (R.Id);
                  References.Insert (R.Definition, Aux);
               end;
            end if;

            LSIF.Serializer.Write_Next_Edge
              (R.Id, Defs (R.Definition).Result_Set_Id);
            --  XXX Is this edge necessary for GitLab?

         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "error: unregistered defining name: "
               & Libadalang.Analysis.Image (R.Definition));
         end if;
      end loop;

      for Cursor in References.Iterate loop
         LSIF.Serializer.Write_Item_References_Edge
           (Defs (Definitions_Maps.Key (Cursor)).Reference_Result_Id,
            Definitions_Maps.Element (Cursor),
            Document.Id);
      end loop;
   end Pass_2;

begin
   LSIF.Configuration.Initialize;
   LSIF.Command_Line.Initialize;
   LSIF.Projects.Initialize;
   LSIF.Command_Line.Apply_Options;
   LSIF.Projects.Build_Set_Of_Files;
   LSIF.Serializer.Initialize;

   --  Generate meta information

   declare
      Path : constant String :=
        LSIF.Configuration.Workspace_Root.Display_Full_Name;

   begin
      LSIF.Serializer.Write_Meta_Data_Vertex
        ("file://"
         & VSS.Strings.Conversions.To_Virtual_String
           (Path (Path'First .. Path'Last - 1)));
   end;

   --  Generate list of the documents

   for Source of LSIF.Configuration.Sources loop
      declare
         Document : not null Document_Information_Access :=
           new Document_Information'
             (Id     => LSIF.Serializer.Allocate_Identifier,
              Unit   =>
                LSIF.Configuration.LAL_Context.Get_From_File
                  (String (Source.Path_Name.Name)),
              others => <>);

      begin
         Documents.Insert (Source.Path_Name.Virtual_File, Document);

         LSIF.Serializer.Write_Document_Vertex
           (Document.Id,
            "file://"
            & VSS.Strings.Conversions.To_Virtual_String
              (Source.Path_Name.Virtual_File.Display_Full_Name));
      end;
   end loop;

   --  Pass 1: populate range information

   for Document of Documents loop
      Pass_1 (Document.all, Document.Unit.Root);

      declare
         Range_Vertices :
           LSIF.Serializer.Identifier_Array
             (Document.Ranges.First_Index .. Document.Ranges.Last_Index);

      begin
         for Index in Range_Vertices'Range loop
            Range_Vertices (Index) := Document.Ranges (Index).Id;
         end loop;

         LSIF.Serializer.Write_Contains_Edge (Document.Id, Range_Vertices);
      end;
   end loop;

   --  Pass 2: populate cross references information

   for Document of Documents loop
      Pass_2 (Document.all);
   end loop;
end LSIF.Driver;
