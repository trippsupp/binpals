-- Author: Vance Field
-- Date: January 29, 2017
-- Course: ITEC 320

-- Purpose: This program reads a set of numbers and for each number checks
--   whether the 32 bit representation of that number and a pruned version of
--   the number are binary palindromes.
-- Input is a sequence of one or more positive integers, separated by white
-- space. The first value is the size of the dataset.
-- Sample input:
--  2
--  2
-- 13
--  Sample output:
-- Original:          2  00000000000000000000000000000010 Different
-- Reversed: 1073741824  01000000000000000000000000000000
-- Pruned  :          1  1                                Same
-- Reversed:          1  1
-- Original:         13  00000000000000000000000000001101 Different
-- Reversed: 2952790016  10110000000000000000000000000000
-- Pruned  :         13  1101                             Different
-- Reversed:         11  1011

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
   procedure CheckIfSame (Val1, Val2 : UNS32);
   procedure PrintReversed (Reversed : UNS32);
   procedure PrintReversedBinary (Original, Reversed : UNS32);
   function GetPrunedDecimal(Original : UNS32) return UNS32;
   procedure PrintPruned (Pruned : UNS32);
   procedure PrintPrunedBinary (Original : UNS32);
   function GetPrunedReversedDecimal(Original : UNS32) return UNS32;
   procedure PrintPrunedReversed (PrunedReversed : UNS32);
   procedure PrintPrunedreversedBinary (Original : UNS32);

   ----------------------------------------------
   -- Purpose: Prints the original value
   -- Parameters: Original: value to print
   ----------------------------------------------
   procedure PrintOriginal (Original : UNS32) is
   begin
      -- print original value
      Put("Original:");
      Put(Original);
      Put("  "); -- whitespace
   end PrintOriginal;

   -----------------------------------------------------------------
   -- Purpose: Prints the binary value
   -- Parameters: Original: the decimal value to find the binary of
   -----------------------------------------------------------------
   procedure PrintBinary (Original : UNS32) is
      Binary : UNS32 := Original;
      Binary_Bin : Uns32 := 0;
   begin
      -- loop to print binary value
      for I in 1 .. 32 loop
         Binary_Bin := (2**(abs(I-32)));
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
   -- Parameters: Val1, Val2: values to compare
   ---------------------------------------------------------------------
   procedure CheckIfSame (Val1, Val2 : UNS32) is
   begin
      if Val1 = Val2 then -- if the original and reversed values are the same
         Put(" Same");
      else
         Put(" Different");
      end if;
   end CheckIfSame;

   ---------------------------------------------------------
   -- Purpose: Reverses the original value
   -- Parameters: Original: value to reverse
   -- Postcondition: Returns the reversed value of original
   ---------------------------------------------------------
   function GetReversed (Original : UNS32) return UNS32 is
      Binary_Bin : UNS32 := Original;
      Bit : UNS32 := 0; -- represents a bit
      Reversed : UNS32 := 0; -- reversed decimal value
   begin
      -- loop to reverse value
      for I in 1 .. 32 loop
         Bit := Binary_Bin mod 2;
         Binary_Bin := Binary_Bin / 2;
         if Bit = 1 then
            Reversed := Reversed + (2**(abs(I-32)));
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
      Put("  ");
   end PrintReversed;

   ------------------------------------------------------------------
   -- Purpose: Prints the reversed binary value
   -- Parameters: Original: original value, Reversed: value to print
   ------------------------------------------------------------------
   procedure PrintReversedBinary (Original, Reversed : UNS32) is
      Binary : UNS32 := Original; -- binary value
      Bit : UNS32; -- running remainder
   begin
      -- loop to print
      for I in 1 .. 32 loop
         Bit := Binary mod 2; -- mod by 2 to get the remainder
         Binary := Binary / 2; -- divide by 2 to continue
         Put(Bit,1); -- width 1
      end loop;
   end PrintReversedBinary;

   --------------------------------------------------------------------------
   -- Purpose: Returns the decimal value of the pruned original binary value
   -- Parameters: Original: decimal value to prune in binary
   -- Postcondition: Returns the decimal value of the pruned binary value
   --------------------------------------------------------------------------
   function GetPrunedDecimal(Original : UNS32) return UNS32 is
      Binary : UNS32 := Original; -- serves as binary value
      Pruned : UNS32 := 0; -- the decimal value of the pruned binary value
      Bit : UNS32 := 0; -- represents a bit
      NumOfZeros : UNS32 := 0; -- running count of zeros
      CheckZeros : UNS32 := 0; -- a check to continue loops
      Pwr : Natural := 0; -- power must be type natural?
                          -- type UNS32 will not work for ** operation
   begin
      -- loop through entire binary value
      for I in 1 .. 32 loop
         Bit := Binary mod 2; -- gives you the remainder
         -- if the bit (remainder) is 1
         if Bit = 1 then
            if CheckZeros = 0 then
               NumOfZeros := 0; -- count of zeros is set to 0
               Pwr := 0; -- power is set to 0
            end if;
            Pruned := Pruned + (2**Pwr); -- sum pruned decimal value
            NumOfZeros := 0; -- reset the number of zeros
            CheckZeros := 1;
         else
            NumOfZeros := NumOfZeros+1;
         end if;
         Pwr := Pwr + 1;-- increment power
         Binary := Binary / 2; -- divide by 2 for the next loop
      end loop;
      return Pruned;
   end GetPrunedDecimal;

   ----------------------------------------------
   -- Purpose: Prints the pruned decimal value
   -- Parameters: Pruned: decimal value to print
   ----------------------------------------------
   procedure PrintPruned (Pruned : UNS32) is
   begin
      Put("Pruned  :");
      Put(Pruned);
      Put("  ");
   end PrintPruned;

   ----------------------------------------------------------------
   -- Purpose: Prints the pruned binary value of the given decimal
   -- Parameters: Original: decimal value to print the binary of
   ----------------------------------------------------------------
   procedure PrintPrunedBinary (Original : UNS32) is
      Binary : UNS32 := Original;
      Binary_Bin : UNS32 := 0;
      NumOfZeros : UNS32 := 0;
      CheckZeros : UNS32 := 0;
   begin
      for I in 1 .. 32 loop
         Binary_Bin := (2**(abs(I-32)));
         if Binary >= Binary_Bin then
            if CheckZeros = 0 then
               NumOfZeros := 0;
            end if;
            for Ii in 1 .. NumOfZeros loop
               Put("0");
            end loop;
            Put("1");
            NumOfZeros := 0;
            CheckZeros := 1;
            Binary := Binary - Binary_Bin;
         else
            NumOfZeros := NumOfZeros+ 1;
         end if;
      end loop;
   end PrintPrunedBinary;

   ----------------------------------------------------------------------------
   -- Purpose: Returns the decimal value after it has been pruned and reversed
   -- Parameters: Original: value to prune, reverse, and return
   -- Postcondition: Returns the pruned reversed decimal value of original
   ----------------------------------------------------------------------------
   function GetPrunedReversedDecimal(Original : UNS32) return UNS32 is
      PrunedReversed : UNS32 := 0; -- pruned reversed value
      Binary : UNS32 := Original; -- binary value
      Count : UNS32 := 0; -- count
      Binary_Bin : UNS32 := 0; -- binary bin to hold remainder
      Pwr : Natural := 0; -- needed for ** operation
   begin
      -- loop entire 32 bit integer
      for I in 1 .. 32 loop
         Binary_Bin := (2**(abs(I-32)));
         if Binary >= Binary_Bin then
            Binary := Binary - Binary_Bin;
            if Count = 0 then
               Pwr := 0; -- set power to 0
            end if;
            PrunedReversed := PrunedReversed + (2**Pwr);
            Count := 1; -- count is 1
         end if;
         Pwr := Pwr + 1; -- increment power
      end loop;
      return PrunedReversed;
   end GetPrunedReversedDecimal;

   ---------------------------------------------------------
   -- Purpose: Prints the pruned reversed decimal value
   -- Parameters: Pruned: decimal value to print
   ---------------------------------------------------------
   procedure PrintPrunedReversed(PrunedReversed : UNS32) is
   begin
      Put("Reversed:");
      Put(PrunedReversed);
      Put("  ");
   end PrintPrunedReversed;

     -------------------------------------------------------------------------
   -- Purpose: Prints the pruned binary value of the given decimal in reverse
   -- Parameters: Original: decimal value to print the binary of
   ---------------------------------------------------------------------------
   procedure PrintPrunedreversedBinary (Original : UNS32) is
      Bit : UNS32 := 0;
      Binary : UNS32 := Original;
      NumOfZeros : UNS32 := 0;
      CheckZeros : UNS32 := 0;
      Pwr : Natural := 0; -- need natural for ** operation
   begin
      for I in 1 .. 32 loop
         Bit := Binary mod 2;
         if Bit = 1 then
            if CheckZeros = 0 then
               NumOfZeros := 0;
               Pwr := 0;
            end if;
            for Ii in 1 .. NumOfZeros loop
               Put("0");
            end loop;
            Put("1");
            NumOfZeros := 0;
            CheckZeros := 1;
         else
            NumOfZeros := NumOfZeros +1;
         end if;
         Pwr := Pwr + 1;
         Binary := Binary / 2;
      end loop;
   end Printprunedreversedbinary;

   -- Main procedure variables
   Data_Set_Size : Uns32; -- the size of the data set
   Original : Uns32; -- represents the original decimal value

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
      -- checks to see if the values are palindromes (the same)
      CheckIfSame(Original, GetReversed(Original));  New_Line;
      -- print the reversed value
      PrintReversed(GetReversed(Original));
      -- print the reversed binary
      PrintReversedBinary(Original, GetReversed(Original)); New_Line;
      -- Print the Pruned decimal
      PrintPruned(GetPrunedDecimal(Original));
      -- printPrunedBinary
      PrintPrunedBinary(GetPrunedDecimal(Original));--, GetPrunedDecimal(Original));
      -- set column for comparison text
      Set_Col(To => 55);
      -- checks to see if the values are the same
      CheckIfSame(GetPrunedDecimal(Original),
                  GetPrunedReversedDecimal(Original)); New_Line;
      -- print pruned dreversed decimal
      PrintPrunedReversed(GetPrunedReversedDecimal(Original));
      -- print pruned reversed binary
      PrintPrunedreversedBinary(Original); New_Line(2);
   end loop;

end;
