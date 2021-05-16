
--  @summary
--  Sortable Array
--
--  @description
--
--  The all-mighty Sortable_Array generic type. It's a fixed length array of
--  Naturals. It's the thing we sort. There are a few constants to experiment
--  with - minimum/maximum array values, the length of the array. Here we
--  define the array's value, range and the array itself.
--
generic

  -- Minimum and Maximum array values
  Min  : Natural := 0;
  Max  : Natural := 9;

  -- Number of values in the array
  Size : Natural := 100;

package Sortable_Array is

  -- Array values are Naturals within the given range
  subtype Value is Natural range Min .. Max;

  -- The array itself
  type Object is array (Integer range <>) of Value;

  --  Generate the random Sortable_Array to be sorted.
  --
  --  @param Array_Size  the size of the array to generate
  --
  --  @return  the random of array
  --
  function Generate
    (Array_Size : Natural := Size)
     return Object;

  --  Swap two elements at the given indices
  --
  --  @param This   the sortable array
  --  @param Left   the left element to swap
  --  @param Right  the right element to swap
  --
  procedure Swap
    (This  : in out Object;
     Left  : in Integer;
     Right : in Integer);

  --  Return a boolean indicating whether the array is sorted (ascending)
  --
  --  @param This  the sortable array
  --
  --  @return  a boolean indicating whether or not the given array is sorted
  --
  function Is_Sorted
    (This : in Object)
     return Boolean;

  -- Printing formats: line-by-line, condensed rows
  type Print_Format_Type is (Line_By_Line,
                             Condensed);

  --  Print the array in one of the provided formats
  --
  --  @param This    the sortable array
  --  @param Format  the format to print the array in
  --
  procedure Print
    (This   : in Object;
     Format : in Print_Format_Type := Condensed);

end Sortable_Array;
