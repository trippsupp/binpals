-- Author: Vance Field
-- Date: January 29, 2017
-- Course: ITEC 320

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;



procedure Binpals is
   type Unsigned_N is mod 2**5; -- mod 32 ???

   function ConvertToBinary(OriginalTemp : Integer) return Integer;
   procedure print(Original : in Integer);

   -- new procedures / functions here




   -- Converts and returns the 32 bit binary form of the given decimal number
   function ConvertToBinary(OriginalTemp : Integer) return Integer is
      Original : Integer := OriginalTemp;
      Binary : Integer := 0;
      Remainder : Integer;
      Binary_Temp : Integer := 1;
   begin
      while Original >= 1 loop
         Remainder := Original mod 2;
         Original := Original / 2;
         if Remainder = 1 then
            Binary := Binary + Binary_Temp;
         end if;
         Binary_Temp := Binary_Temp * 10;
      end loop;
      return Binary;
   end ConvertToBinary;


   -- prints the resulting output for each data set
   procedure Print(Original : in Integer) is
   begin
      Put("Original:");
   end Print;


   Data_Set_Size : Integer; -- size of the data set
   Original : Integer; -- original decimal number scanned
   Bin_Original : Integer; -- original decimal number in 32 bit binary form


begin
   Get(Original); -- get the size of the data set

   Bin_Original := ConvertToBinary(Original);
   Put("binary of input is: ");
   Put(Bin_Original); New_Line;

  -- for I in 1 .. Data_set_size loop
  --    Get(Original);

      -- bin_Original := ConvertToBinary(original);
  --    Put("findBinary(5) = "); New_Line;
  --    Put(FindBinary(5)); New_Line;

      -- bin_Reversed := findReversed(bin_Original);

      --Print(Original);

  -- end loop;

end;
