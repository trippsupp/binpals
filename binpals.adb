-- Author: Vance Field
-- Date: January 29, 2017
-- Course: ITEC 320

-- Purpose: This program reads a set of numbers and for each number checks
--   whether the 32 bit representation of that number and a pruned version of
--   the number are binary palindromes.
-- Input is a sequence of values (a dataset). The first value is the size of
--   the dataset.
-- Input is one or more positive integers, separated by white space.
-- Sample input:
--  1
--  2
-- Sample output:
-- Original:          2  00000000000000000000000000000010 Different
--    Rev  : 1073741824  01000000000000000000000000000000
-- Pruned  :          1  1                                Same
--    Rev  :          1  1

-- Help received: Dr. Okie, class notes

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Binpals is

   -- A type for Unsigned 32 bit integers
   type UNS32 is mod 2**32;

   -- Must create an I/O package for UNS32
   package UNS32_IO is new Ada.Text_IO.Modular_IO (UNS32);
   use UNS32_IO;

   -- procedure / function declarations
   procedure PrintOriginal (Original : UNS32);
   procedure PrintBinary (Original : UNS32);
   function GetReversed (Original : UNS32) return UNS32;
   procedure CheckPalindrome (Original : UNS32);
   procedure PrintReversed (Reversed : UNS32);
   procedure PrintReversedBinary (Original, Reversed : UNS32);

   ----------------------------------------------
   -- Purpose: Prints the original value
   -- Parameters: Original: value to print
   ----------------------------------------------
   procedure PrintOriginal (Original : UNS32) is
   begin
      -- print original value
      Put("Original:");
      Put(Original);
      Put(" ");
   end PrintOriginal;

   -----------------------------------------------------
   -- Purpose: Prints and compares the binary value
   -- Parameters: Original: value to find the binary of
   -----------------------------------------------------
   procedure PrintBinary (Original : UNS32) is
      Binary : UNS32 := Original;
      Remainder : Uns32 := 0;
      Binary_Bin : Uns32 := 0;
   begin
      -- loop to print binary value
      for I in 1 .. 32 loop
         Binary_Bin := (2 ** (abs(I-32)));
         if Binary >= Binary_Bin then
            Put("1");
            Binary := Binary - Binary_Bin;
         else
            Put("0");
         end if;
      end loop;

   end PrintBinary;

   ---------------------------------------------------------------------
   -- Purpose: Compares whether the value is the same as it is reversed
   -- Parameters: Original: value to compare
   ---------------------------------------------------------------------
   procedure CheckPalindrome (Original : UNS32) is
   begin
      if Original = GetReversed(Original) then
         Put(" Same");
      else
         Put(" Different");
      end if;
   end CheckPalindrome;

   ---------------------------------------------------------
   -- Purpose: Reverses the original value
   -- Parameters: Original: value to reverse
   -- Postcondition: Returns the reversed value of original
   ---------------------------------------------------------
   function GetReversed (Original : UNS32) return UNS32 is
      Binary_Bin : UNS32 := Original;
      Remainder : UNS32 := 0;
      Reversed : UNS32 := 0;
   begin
      -- loop to reverse value
      for I in 1 .. 32 loop
         Remainder := Binary_Bin mod 2;
         Binary_Bin := Binary_Bin / 2;
         if Remainder = 1 then
            Reversed := Reversed + (2 ** (abs(I-32)));
         end if;
      end loop;
      return Reversed;
   end GetReversed;

   ---------------------------------------------
   -- Purpose: Prints the reversed value
   -- Parameters: Reversed: value to print
   ---------------------------------------------
   procedure PrintReversed (Reversed : UNS32) is
   begin
      Put("Reversed:");
      Put(Reversed);
      Put(" ");
   end PrintReversed;

   ------------------------------------------------------------------
   -- Purpose: Prints the reversed binary value
   -- Parameters: Original: original value, Reversed: value to print
   ------------------------------------------------------------------
   procedure PrintReversedBinary (Original, Reversed : UNS32) is
      Binary_Bin : UNS32 := Original; --
      Remainder : UNS32;
   begin
      -- loop to print
      for I in 1 .. 32 loop
         Remainder := Binary_Bin mod 2;
         Binary_Bin := Binary_Bin / 2;
         Put(Remainder,1); -- width 1
      end loop;
   end PrintReversedBinary;

   -- Main procedure variables
   Data_Set_Size : Uns32; -- the size of the data set
   Original : Uns32; -- the original value

begin
   -- get the size of the data set
   Get(Data_set_Size);

   -- loop through the data set
   for I in 1 .. Data_Set_Size loop
      -- get the original value
      Get(Original);
      -- print the original value
      PrintOriginal(Original);
      -- print the corresponding binary value
      PrintBinary(Original);
      -- checks to see if the binary values are palindromes (the same)
      CheckPalindrome(Original);  New_Line;
      -- print the reversed value
      PrintReversed(GetReversed(Original));
      -- print the reversed binary value
      PrintReversedBinary(Original, GetReversed(Original)); New_Line;

   end loop;




end;
