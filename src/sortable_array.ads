-------------------------------------------------------------------------------
--
-- Sortable_Array
--
-- Description:
--
--   The all-mighty Sortable_Array generic type. It's a fixed length array of
--   Naturals. It's the thing we sort. There are a few constants to experiment
--   with - minimum/maximum array values, the length of the array. Here we
--   define the array's value, range and the array itself.
--
-------------------------------------------------------------------------------

generic

  -- Minimum and Maximum array values
  Min  : Natural := 0;
  Max  : Natural := 9;

  -- Number of values in the array
  Size : Natural := 100;

package Sortable_Array is

  -- Array values are Naturals within the given range
  subtype Value is Natural range Min .. Max;

  -- The array it self
  type Sortable_Array is array (Integer range <>) of Value;

  -- Generate the random Sortable_Array to be sorted.
  function Generate_Random_Array (Array_Size : Natural := Size) return Sortable_Array;

  -- Swap two elements at the given indices
  procedure Swap (Object : in out Sortable_Array;
                  Left   : in Integer;
                  Right  : in Integer);

  -- Return a boolean indicating whether the array is sorted (ascending)
  function Is_Sorted (Object : in Sortable_Array) return Boolean;

  -- Printing formats: line-by-line, condensed rows
  type Print_Format_Type is (Line_By_Line,
                             Condensed);

  -- Print the array in one of the provided formats
  procedure Print (Object : in Sortable_Array;
                   Format : in Print_Format_Type := Condensed);

end Sortable_Array;
