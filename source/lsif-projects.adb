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
with VSS.Strings.Conversions;

with GNATCOLL.VFS;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

with LSIF.Configuration;

package body LSIF.Projects is

   procedure Register_Attributes;
   --  Register project file's attributes.

   LSIF_Package             : constant GPR2.Package_Id :=
     GPR2."+" ("lsif");

   Workspace_Root_Attribute : constant GPR2.Attribute_Id :=
     GPR2."+" ("workspace_root");

   LSIF_Workspace_Root      : constant GPR2.Q_Attribute_Id :=
     (LSIF_Package, Workspace_Root_Attribute);

   Project_Tree : GPR2.Project.Tree.Object;

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

      begin
         Project_Tree.Load_Autoconf
           (GPR2.Path_Name.Create_File
              (GPR2.Filename_Type
                 (VSS.Strings.Conversions.To_UTF_8_String
                    (LSIF.Configuration.Project_File))),
            LSIF.Configuration.Project_Context);

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

      LSIF.Configuration.LAL_Context :=
        Libadalang.Analysis.Create_Context
          (Unit_Provider =>
             Libadalang.Project_Provider.Create_Project_Unit_Provider
               (Project_Tree));

      --  Set root directory of the project

      LSIF.Configuration.Workspace_Root :=
        Project_Tree.Root_Project.Dir_Name.Virtual_File;

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
         end;
      end if;

      LSIF.Configuration.Workspace_Root.Normalize_Path;

      --  Prepare list of source files

      for Source of Project_Tree.Root_Project.Sources loop
         if Source.Is_Ada then
            LSIF.Configuration.Sources.Insert (Source);
         end if;
      end loop;
   end Initialize;

   -------------------------
   -- Register_Attributes --
   -------------------------

   procedure Register_Attributes is
   begin
      GPR2.Project.Registry.Pack.Add
        (LSIF_Package, GPR2.Project.Registry.Pack.Everywhere);

      GPR2.Project.Registry.Attribute.Add
        (Name                 => LSIF_Workspace_Root,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => GPR2.Project.Registry.Attribute.Single,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);
   end Register_Attributes;

end LSIF.Projects;
