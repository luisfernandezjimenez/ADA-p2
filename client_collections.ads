-- Work carried out by Luis Fernández Jiménez

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Lower_Layer_UDP;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package Client_Collections is
   
   package ASU renames Ada.Strings.Unbounded;
   package LLU renames Lower_Layer_UDP;

   type Collection_Type is limited private;

   Client_Collection_Error: exception;

   procedure Add_Client (Collection: in out Collection_Type;
                         EP: in LLU.End_Point_Type;
                         Nick: in ASU.Unbounded_String;
                         Unique: in Boolean);

   procedure Delete_Client (Collection: in out Collection_Type;
                            Nick: in ASU.Unbounded_String);

   function Search_Client (Collection: in Collection_Type;
                           EP: in LLU.End_Point_Type)
                           return ASU.Unbounded_String;
      
      use type LLU.End_Point_Type;

   procedure Send_To_All (Collection: in Collection_Type;
                          P_Buffer: access LLU.Buffer_Type);

   function Collection_Image (Collection: in Collection_Type)
                              return String;
      
      use ASU;

private

   type Cell;
   type Cell_A is access Cell;
   type Cell is record
      Client_EP: LLU.End_Point_Type;
      Nick: ASU.Unbounded_String;
      Next: Cell_A;
   end record;

   type Collection_Type is record
      P_First: Cell_A;
      Total: Natural := 0;
   end record;

end Client_Collections;
