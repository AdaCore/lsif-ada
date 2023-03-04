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

with VSS.Command_Line;
with VSS.String_Vectors;
with VSS.Strings.Conversions;

with GNATCOLL.VFS;
with GPR2;

with LSIF.Configuration;

package body LSIF.Command_Line is

   Project_Option            : constant VSS.Command_Line.Value_Option :=
     (Short_Name  => "P",
      Long_Name   => "project",
      Value_Name  => "project_file",
      Description => "Project file to process");
   Scenario_Option           : constant VSS.Command_Line.Name_Value_Option :=
     (Short_Name  => "X",
      Long_Name   => <>,
      Name_Name   => "variable",
      Value_Name  => "value",
      Description => "Set scenario variable");
   Workspace_Root_Option     : constant VSS.Command_Line.Value_Option :=
     (Short_Name  => <>,
      Long_Name   => "workspace-root",
      Value_Name  => "path",
      Description => "Root directory of the workspace");

   Positional_Project_Option : constant VSS.Command_Line.Positional_Option :=
     (Name        => "project_file",
      Description => "Project file to process");

   -------------------
   -- Apply_Options --
   -------------------

   procedure Apply_Options is
   begin
      if VSS.Command_Line.Is_Specified (Workspace_Root_Option) then
         LSIF.Configuration.Workspace_Root :=
           GNATCOLL.VFS.Create_From_Base
             (GNATCOLL.VFS.Filesystem_String
                (VSS.Strings.Conversions.To_UTF_8_String
                   (VSS.Command_Line.Value (Workspace_Root_Option))),
              GNATCOLL.VFS.Get_Current_Dir.Full_Name);
         LSIF.Configuration.Workspace_Root.Normalize_Path;
      end if;
   end Apply_Options;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Positional : VSS.String_Vectors.Virtual_String_Vector;

   begin
      VSS.Command_Line.Add_Option (Project_Option);
      VSS.Command_Line.Add_Option (Scenario_Option);
      VSS.Command_Line.Add_Option (Workspace_Root_Option);
      VSS.Command_Line.Add_Option (Positional_Project_Option);

      VSS.Command_Line.Process;

      Positional := VSS.Command_Line.Positional_Arguments;

      --  Extract name of the project file from the option or positional
      --  argument.

      if VSS.Command_Line.Is_Specified (Project_Option) then
         LSIF.Configuration.Project_File :=
           VSS.Command_Line.Value (Project_Option);
      end if;

      if LSIF.Configuration.Project_File.Is_Empty then
         if Positional.Is_Empty then
            VSS.Command_Line.Report_Error ("no project file specified");
         end if;

         LSIF.Configuration.Project_File := Positional.Element (1);
         Positional.Delete_First;
      end if;

      if not Positional.Is_Empty then
         VSS.Command_Line.Report_Error
           ("more than one project files specified");
      end if;

      --  Create context to process project file

      for NV of VSS.Command_Line.Values (Scenario_Option) loop
         if NV.Name.Is_Empty then
            VSS.Command_Line.Report_Error
              ("scenario name can't be empty");
         end if;

         LSIF.Configuration.Project_Context.Insert
           (GPR2.Name_Type (VSS.Strings.Conversions.To_UTF_8_String (NV.Name)),
            VSS.Strings.Conversions.To_UTF_8_String (NV.Value));
      end loop;
   end Initialize;

end LSIF.Command_Line;
