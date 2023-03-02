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

with Interfaces;

with Libadalang;

with VSS.Strings;

package LSIF.Serializer is

   procedure Initialize;

   type Identifier_Array is array (Positive range <>) of Interfaces.Integer_64;

   function Allocate_Identifier return Interfaces.Integer_64;

   procedure Write_Contains_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertices : Identifier_Array);

   procedure Write_Document_Vertex
     (Identifier : Interfaces.Integer_64;
      URI        : VSS.Strings.Virtual_String);

   procedure Write_Item_Definitions_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertices : Identifier_Array;
      Document    : Interfaces.Integer_64);

   procedure Write_Meta_Data_Vertex
     (Project_Root : VSS.Strings.Virtual_String);

   procedure Write_Next_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertex   : Interfaces.Integer_64);

   procedure Write_Range_Vertex
     (Identifier   : Interfaces.Integer_64;
      Start_Text   : Libadalang.Text.Text_Type;
      Start_Line   : Libadalang.Slocs.Line_Number;
      Start_Column : Libadalang.Slocs.Column_Number;
      End_Text     : Libadalang.Text.Text_Type;
      End_Line     : Libadalang.Slocs.Line_Number;
      End_Column   : Libadalang.Slocs.Column_Number);

   procedure Write_Reference_Result_Vertex
     (Identifier : Interfaces.Integer_64);

   procedure Write_Result_Set_Vertex
     (Identifier : Interfaces.Integer_64);

   procedure Write_Text_Document_References_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertex   : Interfaces.Integer_64);

end LSIF.Serializer;
