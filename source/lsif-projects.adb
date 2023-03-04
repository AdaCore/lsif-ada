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

with VSS.Strings.Conversions;

with GPR2.Path_Name;
with GPR2.Project.Tree;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

with LSIF.Configuration;

package body LSIF.Projects is

   Project_Tree : GPR2.Project.Tree.Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
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

      LSIF.Configuration.Project_Root :=
        Project_Tree.Root_Project.Path_Name.Virtual_File.Get_Parent
          .Get_Parent;

      --  Prepare list of source files

      for Source of Project_Tree.Root_Project.Sources loop
         if Source.Is_Ada then
            LSIF.Configuration.Sources.Insert (Source);
         end if;
      end loop;
   end Initialize;

end LSIF.Projects;
