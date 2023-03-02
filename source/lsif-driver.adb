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
with Ada.Text_IO;
with Interfaces;

with GNATCOLL.VFS;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Project_Provider;

with VSS.Strings.Conversions;

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

   type File_Information is record
      Id     : Interfaces.Integer_64;
      Unit   : Libadalang.Analysis.Analysis_Unit;
      Ranges : Range_Vectors.Vector;
   end record;

   type File_Information_Access is access all File_Information;

   package File_Maps is
     new Ada.Containers.Hashed_Maps
       (GNATCOLL.VFS.Virtual_File,
        File_Information_Access,
        GNATCOLL.VFS.Full_Name_Hash,
        GNATCOLL.VFS."=");
       --  (GPR2.Path_Name.Object,
        --  File_Information_Access,
        --  GPR2.Path_Name.Hash,
        --  GPR2.Path_Name."=");
   --  package File_Id_Maps is
   --    new Ada.Containers.Hashed_Maps
   --      (GPR2.Path_Name.Object,
   --       Interfaces.Integer_64,
   --       GPR2.Path_Name.Hash,
   --       GPR2.Path_Name."=");

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

   Project_Tree    : GPR2.Project.Tree.Object;
   Project_Context : GPR2.Context.Object;
   LAL_Context     : Libadalang.Analysis.Analysis_Context;

   --  File_Id         : File_Id_Maps.Map;
   Files : File_Maps.Map;
   Defs  : Defining_Name_Maps.Map;

   procedure Pass_1
     (File : in out File_Information;
      Node : Libadalang.Analysis.Ada_Node'Class);

   procedure Pass_2 (File : in out File_Information);

   procedure Analyze_Range
     (File  : in out File_Information;
      First : Libadalang.Common.Token_Reference;
      Last  : Libadalang.Common.Token_Reference);

   -------------------
   -- Analyze_Range --
   -------------------

   procedure Analyze_Range
     (File  : in out File_Information;
      First : Libadalang.Common.Token_Reference;
      Last  : Libadalang.Common.Token_Reference)
   is
      use all type Libadalang.Common.Ada_Node_Kind_Type;

      First_Location : Libadalang.Slocs.Source_Location_Range :=
        Libadalang.Common.Sloc_Range (Libadalang.Common.Data (First));
      Last_Location  : Libadalang.Slocs.Source_Location_Range :=
        Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Last));
      Id_Node        : Libadalang.Analysis.Ada_Node :=
        File.Unit.Root.Lookup
          ((Last_Location.Start_Line, Last_Location.Start_Column));
      Parent_Node    : Libadalang.Analysis.Ada_Node :=
        Id_Node.Parent;
      Ref_Decl       : Libadalang.Analysis.Defining_Name;
      Self_Decl      : Libadalang.Analysis.Defining_Name;

   begin
      if Id_Node.Kind = Ada_Aliased_Present
        or else Id_Node.Kind = Ada_Not_Null_Present
        or else Parent_Node.Kind = Ada_Attribute_Ref
        or else Parent_Node.Kind = Ada_Pragma_Node
      then
         --  Nothing to do:
         --   - reserved word "aliased"
         --   - "not" in "not null"
         --   - attribute reference "'Attribute"
         --   - pragma

         return;
      end if;

         --  Ada.Text_IO.Put_Line
         --    (Libadalang.Slocs.Image (First_Location)
         --     & " "
         --     & Libadalang.Slocs.Image (Last_Location));

      --  Lookup for declaration

      begin
         Ref_Decl := Id_Node.P_Gnat_Xref;

      exception
         when Libadalang.Common.Property_Error =>
            Ref_Decl := Libadalang.Analysis.No_Defining_Name;

            Ada.Text_IO.Put
              (Libadalang.Analysis.Image (Id_Node)
               & " "
               & Libadalang.Analysis.Image (Parent_Node));
            Ada.Text_IO.New_Line;

            raise;
      end;

      if Parent_Node.Kind = Ada_Defining_Name then
         Self_Decl := Parent_Node.As_Defining_Name;

      elsif Parent_Node.Kind = Ada_Dotted_Name
              and then Parent_Node.Parent.Kind = Ada_Defining_Name
      then
         Self_Decl := Parent_Node.Parent.As_Defining_Name;
      end if;

      if Ref_Decl.Is_Null then
         --  In some cases Ref_Decl is null for "primary" declaration of the
         --  entity.

         Ref_Decl := Self_Decl;
      end if;

      --  Return when there is no declaration found

      if Ref_Decl.Is_Null then
         --  if not Self_Decl.Is_Null then
         --     Ada.Text_IO.Put_Line
         --       (Ada.Text_IO.Standard_Error,
         --        "error: unresolved declaration: "
         --        & Libadalang.Analysis.Image (Id_Node)
         --        & " "
         --        & Libadalang.Analysis.Image (Parent_Node));
         --
         --     --  raise Program_Error;
         --     Ref_Decl := Self_Decl;
         --
         --  else
         --     return;
         --  end if;

            --  Ada.Text_IO.Put_Line
            --    (Ada.Text_IO.Standard_Error,
            --     "error: unresolved declaration: "
            --     & Libadalang.Analysis.Image (Id_Node)
            --     & " "
            --     & Libadalang.Analysis.Image (Parent_Node));

            --  raise Program_Error;
         return;
      end if;

      --  Return when declaration doesn't belongs to project's files.

      if not Files.Contains
               (GNATCOLL.VFS.Create_From_UTF8 (Ref_Decl.Unit.Get_Filename))
      then
            --  Ada.Text_IO.Put_Line
            --    (Ada.Text_IO.Standard_Error,
            --     "error: external reference: "
            --     & Libadalang.Analysis.Image (Id_Node)
            --     & " "
            --     & Libadalang.Analysis.Image (Parent_Node)
            --     & "  =>  "
            --     & Libadalang.Analysis.Image (Decl)
            --    );
         return;
      end if;

      declare
         use type Libadalang.Analysis.Defining_Name;

         Vertex          : Range_Information :=
           (Id         => LSIF.Serializer.Allocate_Identifier,
            Sloc       =>
              (Start_Line   => First_Location.Start_Line,
               Start_Column => First_Location.Start_Column,
               End_Line     => Last_Location.End_Line,
               End_Column   => Last_Location.End_Column),
            Definition => Ref_Decl);
         Hover_Result_Id : Interfaces.Integer_64 := 0;
         Info            : Defining_Name_Information;

      begin
         File.Ranges.Append (Vertex);

         --  Generate "range" vertex

         LSIF.Serializer.Write_Range_Vertex
           (Vertex.Id,
            "",
            Vertex.Sloc.Start_Line,
            Vertex.Sloc.Start_Column,
            "",
            Vertex.Sloc.End_Line,
            Vertex.Sloc.End_Column);

         if Ref_Decl = Self_Decl then
            Info.Result_Set_Id := LSIF.Serializer.Allocate_Identifier;

            LSIF.Serializer.Write_Result_Set_Vertex (Info.Result_Set_Id);
            LSIF.Serializer.Write_Next_Edge (Vertex.Id, Info.Result_Set_Id);

            Hover_Result_Id := LSIF.Serializer.Allocate_Identifier;

            LSIF.Serializer.Write_Hover_Result_Vertex (Hover_Result_Id);
            LSIF.Serializer.Write_Text_Document_Hover_Edge
              (Info.Result_Set_Id, Hover_Result_Id);

            Info.Reference_Result_Id := LSIF.Serializer.Allocate_Identifier;

            LSIF.Serializer.Write_Reference_Result_Vertex
              (Info.Reference_Result_Id);
            LSIF.Serializer.Write_Text_Document_References_Edge
              (Info.Result_Set_Id, Info.Reference_Result_Id);
            LSIF.Serializer.Write_Item_Definitions_Edge
              (Info.Reference_Result_Id, (1 => Vertex.Id), File.Id);

            Defs.Insert (Vertex.Definition, Info);
         end if;
      end;

         --  Ada.Text_IO.Put_Line
         --    (Libadalang.Slocs.Image (First_Location)
         --     & " "
         --     & Libadalang.Slocs.Image (Last_Location));
         --
         --  Ada.Text_IO.Put
         --    (Libadalang.Analysis.Image (Id_Node)
         --     & " "
         --     & Libadalang.Analysis.Image (Parent_Node));
         --
         --  if Parent_Node.Kind = Ada_Dotted_Name then
         --     Ada.Text_IO.Put (" " & Libadalang.Analysis.Image (Parent_Node.Parent));
         --  end if;
         --
         --  Ada.Text_IO.Put ("  =>  " & Libadalang.Analysis.Image (Decl));
         --  Ada.Text_IO.Put ("  " & Decl.Unit.Get_Filename);
         --  Ada.Text_IO.New_Line;
      --  end if;
   end Analyze_Range;

   ------------
   -- Pass_1 --
   ------------

   procedure Pass_1
     (File : in out File_Information;
      Node : Libadalang.Analysis.Ada_Node'Class)
   is

      use type Libadalang.Common.Token_Reference;
      use all type Libadalang.Common.Token_Kind;

      --  type States is (None, Identifier);

      Token : Libadalang.Common.Token_Reference := Node.Token_Start;
      --  State : States := None;
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
         --  exit when Token = Node.Token_End;

         --  Ada.Text_IO.Put_Line (Libadalang.Common.Image (Token));

         case Libadalang.Common.Kind (Libadalang.Common.Data (Token)) is

            --  when Ada_With | Ada_Whitespace | Ada_Procedure
            --     =>
            --     null;
            --
            --  when Ada_Semicolon | Ada_Comment =>

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

               Analyze_Range (File, First, Last);
               --  if State = None then
               --     State := Identifier;
               --     First := Token;
               --  end if;
               --
               --  Last  := Token;

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
                     when
                          --  Ada_Whitespace
                          Ada_Comma
                        | Ada_Par_Close
                        | Ada_Semicolon
                        --  | Ada_Tick | Ada_All
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
               --  if State = None then
                  Analyze_Range (File, First, Last);

                  --  raise Program_Error;
--  --                    null;
--                    State := Identifier;
--                    First := Token;

               else
                  raise Program_Error;
               end if;

            when Ada_String =>
               --  Operator's names use string syntax for declarations and
               --  may use string syntax for references.

               --  if State = None then
                  declare
                     use all type Libadalang.Common.Ada_Node_Kind_Type;

                     Last_Sloc  : Libadalang.Slocs.Source_Location_Range :=
                       Libadalang.Common.Sloc_Range
                         (Libadalang.Common.Data (Token));
                     Id_Node    : Libadalang.Analysis.Ada_Node :=
                       Node.Lookup
                         ((Last_Sloc.Start_Line, Last_Sloc.Start_Column));
                     Parent_Node : Libadalang.Analysis.Ada_Node :=
                    Id_Node.Parent;
                  begin
                     if Parent_Node.Kind
                          not in Ada_Concat_Op | Ada_Concat_Operand
                                   | Ada_Param_Assoc | Ada_Paren_Expr
                     then
                     Ada.Text_IO.Put_Line ("String:");

                        Libadalang.Analysis.Print (Id_Node);
                        Libadalang.Analysis.Print (Parent_Node);
                        raise Program_Error;
                  end if;
                  Analyze_Range (File, Token, Token);
                  end;

               --  else
               --     Last := Token;
               --     --  raise Program_Error;
               --  end if;

            when Ada_Equal | Ada_Notequal | Ada_Amp | Ada_Not | Ada_And
               | Ada_Minus | Ada_Plus | Ada_Or
               =>
                  declare
                     use all type Libadalang.Common.Ada_Node_Kind_Type;

                     --  Last_Sloc  : Libadalang.Slocs.Source_Location_Range :=
                     --    Libadalang.Common.Sloc_Range
                     --      (Libadalang.Common.Data (Token));
                     --  Id_Node    : Libadalang.Analysis.Ada_Node :=
                     --    Node.Lookup
                     --      ((Last_Sloc.Start_Line, Last_Sloc.Start_Column));
                     --  Parent_Node : Libadalang.Analysis.Ada_Node :=
                    --  Id_Node.Parent;
               begin
                  --  Ada.Text_IO.Put_Line
                  --    ("Operator: "
                  --     & Libadalang.Common.Image (Token)
                  --     & Libadalang.Analysis.Image (Id_Node));

                  --  if Id_Node.Kind = Ada_Op_Concat then
                  --     Ada.Text_IO.Put_Line (" concatenation");
                  --
                  --  elsif Id_Node.Kind = Ada_Op_Minus then
                  --     Ada.Text_IO.Put_Line (" minus");
                  --
                  --  elsif Id_Node.Kind = Ada_Op_Plus then
                  --     Ada.Text_IO.Put_Line (" plus");
                  --
                  --  elsif Id_Node.Kind = Ada_Op_Eq then
                  --     Ada.Text_IO.Put_Line (" equal");
                  --
                  --  elsif Id_Node.Kind = Ada_Op_Neq then
                  --     Ada.Text_IO.Put_Line (" equal");
                  --
                  --  elsif Id_Node.Kind = Ada_Op_And_Then then
                  --     Ada.Text_IO.Put_Line (" ans then");
                  --
                  --  elsif Id_Node.Kind = Ada_Op_Not_In then
                  --     --  Nothing to do
                  --     Ada.Text_IO.Put_Line (" membership");
                  --
                  --  else
                  --     Ada.Text_IO.New_Line;
                  --     Libadalang.Analysis.Print (Id_Node);
                  --     Libadalang.Analysis.Print (Parent_Node);
                  --  end if;

                  Analyze_Range (File, Token, Token);
               end;
               --  Ada.Text_IO
               --  raise Program_Error;

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
               | Ada_Range | Ada_Function | Ada_Body | Ada_Do
               =>
               null;
               --  if State = Identifier then
               --     raise Program_Error;
               --     State := None;
               --
               --     declare
               --        use all type Libadalang.Common.Ada_Node_Kind_Type;
               --
               --        First_Sloc : Libadalang.Slocs.Source_Location_Range :=
               --          Libadalang.Common.Sloc_Range
               --            (Libadalang.Common.Data (First));
               --        Last_Sloc  : Libadalang.Slocs.Source_Location_Range :=
               --          Libadalang.Common.Sloc_Range
               --            (Libadalang.Common.Data (Last));
               --        Id_Node    : Libadalang.Analysis.Ada_Node :=
               --          Node.Lookup
               --            ((Last_Sloc.Start_Line, Last_Sloc.Start_Column));
               --        Parent_Node : Libadalang.Analysis.Ada_Node :=
               --          Id_Node.Parent;
               --
               --     begin
               --     Ada.Text_IO.Put_Line
               --       ("Identifier found at"
               --        & Libadalang.Slocs.Line_Number'Image (First_Sloc.Start_Line)
               --        & Libadalang.Slocs.Column_Number'Image (First_Sloc.Start_Column)
               --        & Libadalang.Slocs.Line_Number'Image (Last_Sloc.End_Line)
               --        & Libadalang.Slocs.Column_Number'Image (Last_Sloc.End_Column)
               --        & " "
               --        & Libadalang.Common.Image (First)
               --        & " "
               --        & Libadalang.Common.Image (Last));
               --
               --        --  Ada.Text_IO.Put_Line
               --        --    & Libadalang.Slocs.Line_Number'Image (First_Sloc.Start_Line)
               --        --     & Libadalang.Slocs.Column_Number'Image (First_Sloc.Start_Column)
               --        --     & Libadalang.Slocs.Line_Number'Image (Last_Sloc.End_Line)
               --        --     & Libadalang.Slocs.Column_Number'Image (Last_Sloc.End_Column));
               --
               --        if Parent_Node.Kind = Ada_Dotted_Name then
               --           --  Resolve dotted name to parent node
               --
               --           Parent_Node := Parent_Node.Parent;
               --        end if;
               --
               --        --  if Parent_Node.Kind = Ada_Defining_Name then
               --        if Id_Node.Kind = Ada_Aliased_Present then
               --           Ada.Text_IO.Put_Line
               --             ("   Not an identifier (""aliased"" keyword)");
               --
               --        elsif Id_Node.Kind /= Ada_Identifier then
               --           Libadalang.Analysis.Print (Id_Node);
               --          --  (Node.Lookup
               --          --     ((Last_Sloc.Start_Line, Last_Sloc.Start_Column)));
               --           Ada.Text_IO.Put_Line
               --             (Libadalang.Analysis.Image (Id_Node));
               --          --     (Node.Lookup
               --          --          ((Last_Sloc.Start_Line, Last_Sloc.Start_Column))));
               --           Libadalang.Analysis.Print (Parent_Node);
               --
               --        elsif Id_Node.Kind = Ada_Identifier
               --          and then Id_Node.Parent.Kind = Ada_Attribute_Ref
               --        then
               --           Ada.Text_IO.Put_Line
               --             ("   Not an identifier (attribute reference)");
               --        --  else
               --           --  Libadalang.Analysis.Print (Id_Node);
               --           --  Libadalang.Analysis.Print (Id_Node.Parent);
               --        end if;
               --     end;
               --  end if;

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

   procedure Pass_2 (File : in out File_Information) is

      package Definitions_Maps is
        new Ada.Containers.Hashed_Maps
          (Libadalang.Analysis.Defining_Name,
           LSIF.Serializer.Identifier_Vectors.Vector,
           Hash,
           Libadalang.Analysis."=",
           LSIF.Serializer.Identifier_Vectors."=");

      References : Definitions_Maps.Map;

   begin
      for R of File.Ranges loop
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
            File.Id);
      end loop;
   end Pass_2;

begin
   --  Load project file

      begin
         Project_Tree.Load_Autoconf
           (GPR2.Path_Name.Create_File
              (GPR2.Filename_Type'("gnat/lsif.gpr")),
                   --  (VSS.Strings.Conversions.To_UTF_8_String
                   --       (GNATdoc.Command_Line.Project_File))),
            Project_Context);

         Project_Tree.Update_Sources (With_Runtime => True);

      exception
         when GPR2.Project_Error =>
            for Message of Project_Tree.Log_Messages.all loop
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error, Message.Format);
            end loop;

            raise;
      end;

      --  Create Libadalang context and unit provider

      --  Event_Handler :=
      --    Libadalang.Analysis.Create_Event_Handler_Reference
      --      (Missing_File_Event_Handler'(null record));

      LAL_Context :=
        Libadalang.Analysis.Create_Context
          (Unit_Provider =>
             Libadalang.Project_Provider.Create_Project_Unit_Provider
               (Project_Tree));
           --  Event_Handler => Event_Handler);
      --  LAL_Context.Discard_Errors_In_Populate_Lexical_Env (False);

   LSIF.Serializer.Initialize;

   declare
      Path : constant String :=
        Project_Tree.Root_Project.Path_Name.Virtual_File.Get_Parent
          .Get_Parent.Display_Dir_Name;

   begin
      LSIF.Serializer.Write_Meta_Data_Vertex
        ("file://"
         & VSS.Strings.Conversions.To_Virtual_String
           (Path (Path'First .. Path'Last - 1)));
   end;

   for Source of Project_Tree.Root_Project.Sources loop
      declare
         File : not null File_Information_Access :=
           new File_Information'
             (Id     => LSIF.Serializer.Allocate_Identifier,
              Unit   =>
                LAL_Context.Get_From_File (String (Source.Path_Name.Name)),
              others => <>);

      begin
         Files.Insert (Source.Path_Name.Virtual_File, File);

         LSIF.Serializer.Write_Document_Vertex
           (File.Id,
            "file://"
            & VSS.Strings.Conversions.To_Virtual_String
              (Source.Path_Name.Virtual_File.Display_Full_Name));
      end;
   end loop;

   --  Pass 1: populate range information

   for File of Files loop
      Pass_1 (File.all, File.Unit.Root);

      declare
         Range_Vertices :
           LSIF.Serializer.Identifier_Array
             (File.Ranges.First_Index .. File.Ranges.Last_Index);

      begin
         for Index in Range_Vertices'Range loop
            Range_Vertices (Index) := File.Ranges (Index).Id;
         end loop;

         LSIF.Serializer.Write_Contains_Edge (File.Id, Range_Vertices);
      end;
   end loop;

   --  Pass 2: populate cross references information

   for File of Files loop
      Pass_2 (File.all);
   end loop;
end LSIF.Driver;
