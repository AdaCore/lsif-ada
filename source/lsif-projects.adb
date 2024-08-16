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

with VSS.Application;
with VSS.Command_Line;
with VSS.Strings.Conversions;

with GNATCOLL.VFS;
with GPR2.Options;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Build.Source.Sets;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

with LSIF.Configuration;

package body LSIF.Projects is

   use type GNATCOLL.VFS.Virtual_File;

   procedure Register_Attributes;
   --  Register project file's attributes.

   LSIF_Package                     : constant GPR2.Package_Id :=
     GPR2."+" ("lsif");

   Excluded_Project_Files_Attribute : constant GPR2.Attribute_Id :=
     GPR2."+" ("excluded_project_files");
   Workspace_Root_Attribute         : constant GPR2.Attribute_Id :=
     GPR2."+" ("workspace_root");

   LSIF_Workspace_Root              : constant GPR2.Q_Attribute_Id :=
     (LSIF_Package, Workspace_Root_Attribute);
   LSIF_Excluded_Project_Files      : constant GPR2.Q_Attribute_Id :=
     (LSIF_Package, Excluded_Project_Files_Attribute);

   Project_Tree           : GPR2.Project.Tree.Object;
   Excluded_Project_Files : GPR2.Path_Name.Set.Object;

   ------------------------
   -- Build_Set_Of_Files --
   ------------------------

   procedure Build_Set_Of_Files is
      use LSIF.Configuration.Source_Vector;
      use type GPR2.Language_Id;
   begin
      --  Prepare list of source files

      for View of Project_Tree loop
         if not View.Is_Externally_Built
           and then not Excluded_Project_Files.Contains (View.Path_Name)
         then
            for Source of View.Sources loop
               if Source.Language = GPR2.Ada_Language
                 and then
                   GNATCOLL.VFS.Greatest_Common_Path
                     ((1 => LSIF.Configuration.Workspace_Root,
                       2 => Source.Path_Name.Virtual_File))
                 = LSIF.Configuration.Workspace_Root
               then
                  LSIF.Configuration.Sources.Append (Source);
               end if;
            end loop;
         end if;
      end loop;
   end Build_Set_Of_Files;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Export GPR_TOOL scenario variable when necessary

      if not LSIF.Configuration.Project_Context.Contains ("GPR_TOOL")
        and not VSS.Application.System_Environment.Contains ("GPR_TOOL")
      then
         LSIF.Configuration.Project_Context.Insert ("GPR_TOOL", "lsif-ada");
      end if;

      --  Register attributes

      Register_Attributes;

      --  Load project file

      declare
         Opts    : GPR2.Options.Object;
         Success : Boolean;
      begin
         Opts.Add_Switch
           (GPR2.Options.P,
            VSS.Strings.Conversions.To_UTF_8_String
              (LSIF.Configuration.Project_File));
         Success := Project_Tree.Load
           (Opts, With_Runtime => True);

         if not Success then
            VSS.Command_Line.Report_Error
              ("unable to load project file");
         else
            Success := Project_Tree.Set_Context (LSIF.Configuration.Project_Context);

            if not Success then
               VSS.Command_Line.Report_Error
                 ("unable to load the context");
            end if;
         end if;

         Project_Tree.Update_Sources;

      exception
         when GPR2.Project_Error =>
            for Message of Project_Tree.Log_Messages.all loop
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error, Message.Format);
            end loop;

            raise;
      end;

      --  Create Libadalang context and unit provider

      LSIF.Configuration.LAL_Context :=
        Libadalang.Analysis.Create_Context
          (Unit_Provider =>
             Libadalang.Project_Provider.Create_Project_Unit_Provider
               (Project_Tree));

      --  Set root directory of the project

      if Project_Tree.Root_Project.Has_Attribute (LSIF_Workspace_Root) then
         declare
            Attribute : constant GPR2.Project.Attribute.Object :=
              Project_Tree.Root_Project.Attribute (LSIF_Workspace_Root);

         begin
            LSIF.Configuration.Workspace_Root :=
              GNATCOLL.VFS.Create_From_Base
                (GNATCOLL.VFS.Filesystem_String (Attribute.Value.Text),
                 Project_Tree.Root_Project.Dir_Name.Virtual_File
                 .Full_Name.all);
            LSIF.Configuration.Workspace_Root.Normalize_Path;
         end;
      end if;

      --  Setup list of excluded project files

      if Project_Tree.Root_Project.Has_Attribute
        (LSIF_Excluded_Project_Files)
      then
         declare
            Attribute : constant GPR2.Project.Attribute.Object :=
              Project_Tree.Root_Project.Attribute (LSIF_Excluded_Project_Files);

         begin
            for Item of Attribute.Values loop
               if Item.Text'Length = 0 then
                  VSS.Command_Line.Report_Error
                    ("empty name of the project file");
               end if;

               if Project_Tree.Root_Project.Has_Source
                 (GPR2.Filename_Type (Item.Text))
               then
                  VSS.Command_Line.Report_Error
                    ("unable to resolve project file path");
               end if;

               declare
                  Path : constant GPR2.Path_Name.Object
                     := Project_Tree.Root_Project.Source_Path
                          (GPR2.Filename_Type (Item.Text), True, True);
               begin
                 Excluded_Project_Files.Append (Path);
               end;
            end loop;
         end;
      end if;
   end Initialize;

   -------------------------
   -- Register_Attributes --
   -------------------------

   procedure Register_Attributes is
   begin
      GPR2.Project.Registry.Pack.Add
        (LSIF_Package, GPR2.Project.Registry.Pack.Everywhere);

      GPR2.Project.Registry.Attribute.Add
        (Name                 => LSIF_Excluded_Project_Files,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => GPR2.Project.Registry.Attribute.List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
      GPR2.Project.Registry.Attribute.Add
        (Name                 => LSIF_Workspace_Root,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
   end Register_Attributes;

end LSIF.Projects;
