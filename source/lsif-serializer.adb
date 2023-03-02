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

with VSS.JSON.Push_Writers;
with VSS.Text_Streams.Standadrs;

package body LSIF.Serializer is

   Counter : Interfaces.Integer_64 := 1;

   Output  : aliased VSS.Text_Streams.Output_Text_Stream'Class :=
     VSS.Text_Streams.Standadrs.Standard_Output;
   Writer  : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
   Success : Boolean := True;

   -------------------------
   -- Allocate_Identifier --
   -------------------------

   function Allocate_Identifier return Interfaces.Integer_64 is
      use type Interfaces.Integer_64;

   begin
      return Result : Interfaces.Integer_64 := Counter do
         Counter := Counter + 1;
      end return;
   end Allocate_Identifier;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Writer.Set_Stream (Output'Access);
   end Initialize;

   -------------------------
   -- Write_Contains_Edge --
   -------------------------

   procedure Write_Contains_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertices : Identifier_Array) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Allocate_Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("edge", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("contains", Success);
      Writer.Key_Name ("outV", Success);
      Writer.Integer_Value (Out_Vertex, Success);
      Writer.Key_Name ("inVs", Success);
      Writer.Start_Array (Success);

      for Identifier of In_Vertices loop
         Writer.Integer_Value (Identifier);
      end loop;

      Writer.End_Array (Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Contains_Edge;

   ---------------------------
   -- Write_Document_Vertex --
   ---------------------------

   procedure Write_Document_Vertex
     (Identifier : Interfaces.Integer_64;
      URI        : VSS.Strings.Virtual_String) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("vertex", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("document", Success);
      Writer.Key_Name ("uri", Success);
      Writer.String_Value (URI, Success);
      Writer.Key_Name ("languageId", Success);
      Writer.String_Value ("ada", Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Document_Vertex;

   ---------------------------------
   -- Write_Item_Definitions_Edge --
   ---------------------------------

   procedure Write_Item_Definitions_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertices : Identifier_Array;
      Document    : Interfaces.Integer_64) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Allocate_Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("edge", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("item", Success);
      Writer.Key_Name ("outV", Success);
      Writer.Integer_Value (Out_Vertex, Success);
      Writer.Key_Name ("inVs", Success);
      Writer.Start_Array (Success);

      for Identifier of In_Vertices loop
         Writer.Integer_Value (Identifier);
      end loop;

      Writer.End_Array (Success);

      Writer.Key_Name ("document", Success);
      Writer.Integer_Value (Document, Success);
      Writer.Key_Name ("property", Success);
      Writer.String_Value ("definitions", Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Item_Definitions_Edge;

   --------------------------------
   -- Write_Item_References_Edge --
   --------------------------------

   procedure Write_Item_References_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertices : Identifier_Array;
      Document    : Interfaces.Integer_64) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Allocate_Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("edge", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("item", Success);
      Writer.Key_Name ("outV", Success);
      Writer.Integer_Value (Out_Vertex, Success);
      Writer.Key_Name ("inVs", Success);
      Writer.Start_Array (Success);

      for Identifier of In_Vertices loop
         Writer.Integer_Value (Identifier);
      end loop;

      Writer.End_Array (Success);

      Writer.Key_Name ("document", Success);
      Writer.Integer_Value (Document, Success);
      Writer.Key_Name ("property", Success);
      Writer.String_Value ("references", Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Item_References_Edge;

   --------------------------------
   -- Write_Item_References_Edge --
   --------------------------------

   procedure Write_Item_References_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertices : Identifier_Vectors.Vector;
      Document    : Interfaces.Integer_64) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Allocate_Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("edge", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("item", Success);
      Writer.Key_Name ("outV", Success);
      Writer.Integer_Value (Out_Vertex, Success);
      Writer.Key_Name ("inVs", Success);
      Writer.Start_Array (Success);

      for Identifier of In_Vertices loop
         Writer.Integer_Value (Identifier);
      end loop;

      Writer.End_Array (Success);

      Writer.Key_Name ("document", Success);
      Writer.Integer_Value (Document, Success);
      Writer.Key_Name ("property", Success);
      Writer.String_Value ("references", Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Item_References_Edge;

   -----------------------------------
   -- Write_Reference_Result_Vertex --
   -----------------------------------

   procedure Write_Reference_Result_Vertex
     (Identifier : Interfaces.Integer_64) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("vertex", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("referenceResult", Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Reference_Result_Vertex;

   -----------------------------
   -- Write_Result_Set_Vertex --
   -----------------------------

   procedure Write_Result_Set_Vertex
     (Identifier : Interfaces.Integer_64) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("vertex", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("resultSet", Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Result_Set_Vertex;

   ----------------------------
   -- Write_Meta_Data_Vertex --
   ----------------------------

   procedure Write_Meta_Data_Vertex
     (Project_Root : VSS.Strings.Virtual_String) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Allocate_Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("vertex", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("metaData", Success);
      Writer.Key_Name ("version", Success);
      Writer.String_Value ("0.4.0", Success);
      Writer.Key_Name ("positionEncoding", Success);
      Writer.String_Value ("utf-16", Success);
      Writer.Key_Name ("projectRoot", Success);
      Writer.String_Value (Project_Root, Success);
      Writer.Key_Name ("toolInfo", Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("name", Success);
      Writer.String_Value ("lsif-ada", Success);
      Writer.End_Object (Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Meta_Data_Vertex;

   ---------------------
   -- Write_Next_Edge --
   ---------------------

   procedure Write_Next_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertex   : Interfaces.Integer_64) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Allocate_Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("edge", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("next", Success);
      Writer.Key_Name ("outV", Success);
      Writer.Integer_Value (Out_Vertex, Success);
      Writer.Key_Name ("inV", Success);
      Writer.Integer_Value (In_Vertex);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Next_Edge;

   ------------------------
   -- Write_Range_Vertex --
   ------------------------

   procedure Write_Range_Vertex
     (Identifier   : Interfaces.Integer_64;
      Start_Text   : Libadalang.Text.Text_Type;
      Start_Line   : Libadalang.Slocs.Line_Number;
      Start_Column : Libadalang.Slocs.Column_Number;
      End_Text     : Libadalang.Text.Text_Type;
      End_Line     : Libadalang.Slocs.Line_Number;
      End_Column   : Libadalang.Slocs.Column_Number)
   is
      use type Interfaces.Integer_64;

      --  XXX Conversion of the character implemented incorrectly, it should
      --  be UTF-16 offsets.

   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("vertex", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("range", Success);
      Writer.Key_Name ("start", Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("line", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (Start_Line) - 1, Success);
      Writer.Key_Name ("character", Success);
      Writer.Integer_Value
        (Interfaces.Integer_64 (Start_Column) - 1, Success);
      Writer.End_Object (Success);
      Writer.Key_Name ("end", Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("line", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (End_Line) - 1, Success);
      Writer.Key_Name ("character", Success);
      Writer.Integer_Value (Interfaces.Integer_64 (End_Column), Success);
      Writer.End_Object (Success);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Range_Vertex;

   -----------------------------------------
   -- Write_Text_Document_References_Edge --
   -----------------------------------------

   procedure Write_Text_Document_References_Edge
     (Out_Vertex  : Interfaces.Integer_64;
      In_Vertex   : Interfaces.Integer_64) is
   begin
      Writer.Start_Document (Success);
      Writer.Start_Object (Success);
      Writer.Key_Name ("id", Success);
      Writer.Integer_Value (Allocate_Identifier, Success);
      Writer.Key_Name ("type", Success);
      Writer.String_Value ("edge", Success);
      Writer.Key_Name ("label", Success);
      Writer.String_Value ("textDocument/references", Success);
      Writer.Key_Name ("outV", Success);
      Writer.Integer_Value (Out_Vertex, Success);
      Writer.Key_Name ("inV", Success);
      Writer.Integer_Value (In_Vertex);
      Writer.End_Object (Success);
      Writer.End_Document (Success);
      Output.New_Line (Success);
   end Write_Text_Document_References_Edge;

end LSIF.Serializer;
