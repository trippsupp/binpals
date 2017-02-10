-- Author: Vance Field
-- Date: January 29, 2017
-- Course: ITEC 320

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;



procedure Binpals is

   -- A type for Unsigned 32 bit integers
   type UNS32 is mod 2**32;

   -- Must create an I/O package for UNS32
   package UNS32_IO is new Ada.Text_IO.Modular_IO (Num => UNS32);
   use UNS32_IO;

   -- procedure / function declarations
   procedure Print(Original : Uns32);

   -- new procedures / functions here
   procedure Print(Original : Uns32) is
   begin
      Put("Original:           ");
      Put("1073741824");
      Put("  ");
      Put("00000000000000000000000000000010");
      Put(" ");
      Put("Different"); New_Line;
      Put("Rev     :           "); New_Line;
      Put("Pruned  :           "); New_Line;
      Put("Rev     :           "); New_Line;
   end Print;


   -- Main variables
   Data_Set_Size : Uns32;
   Original : Uns32;
   --Temp : Uns32;
   --X : Uns32;


begin
   Get(Data_set_Size);

   for I in 1 .. Data_Set_Size loop
      Get(Original);

      Print(Original);

   end loop;




   -- loop for each data set
  -- for I in 1 .. 32 loop

  --    Temp := X mod 2;
  --    Put(Temp, 1);
  --    X := X / 2;

  -- end loop;



end;
