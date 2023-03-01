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

with Ada.Text_IO;
with Interfaces;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

with VSS.Strings.Conversions;
with VSS.Text_Streams.Standadrs;
with VSS.JSON.Push_Writers;

procedure LSIF.Driver is

   use type Interfaces.Integer_64;
   use type VSS.Strings.Virtual_String;

   Counter         : Interfaces.Integer_64 := 1;

   Project_Tree    : GPR2.Project.Tree.Object;
   Project_Context : GPR2.Context.Object;
   LAL_Context     : Libadalang.Analysis.Analysis_Context;
   Output          : aliased VSS.Text_Streams.Output_Text_Stream'Class :=
     VSS.Text_Streams.Standadrs.Standard_Output;
   Writer          : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
   Success         : Boolean := True;

   procedure Pass_1 (Node : Libadalang.Analysis.Ada_Node'Class);

   procedure Pass_1 (Node : Libadalang.Analysis.Ada_Node'Class) is
   begin
      null;
   end Pass_1;

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

   Writer.Set_Stream (Output'Unchecked_Access);

   declare
      Path : constant String :=
        Project_Tree.Root_Project.Path_Name.Virtual_File.Get_Parent
          .Get_Parent.Display_Dir_Name;

   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Counter, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("vertex", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("metaData", Success);
      Writer.Key_Name ("version", Success);
      Writer.String_Value ("0.4.0", Success);
      Writer.Key_Name ("positionEncoding", Success);
      Writer.String_Value ("utf-16", Success);
      Writer.Key_Name ("projectRoot", Success);
      Writer.String_Value
        ("file://"
         & VSS.Strings.Conversions.To_Virtual_String
           (Path (Path'First .. Path'Last - 1)),
         Success);
      Writer.Key_Name ("toolInfo", Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("name", Success);
      Writer.String_Value ("lsif-ada", Success);
      Writer.End_Object (Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
      Counter := Counter + 1;
   end;

   for Source of Project_Tree.Root_Project.Sources loop
      --  Ada.Text_IO.Put_Line ("f");

      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Counter, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("vertex", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("document", Success);
      Writer.Key_Name ("uri", Success);
      Writer.String_Value
        ("file://"
         & VSS.Strings.Conversions.To_Virtual_String
           (Source.Path_Name.Virtual_File.Display_Full_Name),
         Success);
      Writer.Key_Name ("languageId", Success);
      Writer.String_Value ("ada", Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
      Counter := Counter + 1;

      declare
         Unit     : constant Libadalang.Analysis.Analysis_Unit :=
           LAL_Context.Get_From_File
             (String (Source.Path_Name.Name));

      begin
         Pass_1 (Unit.Root);
         null;
      end;
   end loop;
end LSIF.Driver;
