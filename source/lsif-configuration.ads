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

--  Tool's configuration

with Ada.Containers.Vectors;

with VSS.Strings;

with GNATCOLL.VFS;
with GPR2.Build.Source;   use GPR2.Build.Source;
with GPR2.Context;
with Libadalang.Analysis;

package LSIF.Configuration is

   package Source_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => GPR2.Build.Source.Object);

   Output_File     : VSS.Strings.Virtual_String;
   --  File to output generated data

   Project_File    : VSS.Strings.Virtual_String;
   --  Project file to process

   Project_Context : GPR2.Context.Object;
   --  Set of scenario variables defined in the command line.

   Workspace_Root  : GNATCOLL.VFS.Virtual_File;
   --  Root directory of the workspace.

   Sources         : Source_Vector.Vector;
   --  List of source files to process

   LAL_Context     : Libadalang.Analysis.Analysis_Context;
   --  Libadalang context

   procedure Initialize;
   --  Initialize configuration to default values.

end LSIF.Configuration;
