package body Insertion_Sort is

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Insertion Sort.
  --
  -- Notes:
  --   A simple sorting algorithm that takes elements from the list one by
  --   one and inserts them into their correct position in the sorted array.
  --   Insertion tends to be expensive because it requires shifting all
  --   following elements over by one.
  --
  --
  -- Performance:
  --   Worst-case: O(n^2)
  --   Best-case:  O(n)
  --   Average:    O(n^2)
  --
  overriding
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object is

    Arr : Random_Array.Object := Sort_Array;

    J   : Integer := 0;
    Key : Integer := 0;

    Size : constant Natural := Arr'Size/Arr(0)'Size;

  begin

    for Index in 1 .. Size - 1 loop

      Key := Arr (Index);
      J   := Index - 1;

      -- Move elements 0 to i-1 that are greater than the key to one
      -- position ahead of their current position.

      while J >= 0 and then Arr (J) > Key loop
        Arr (J+1) := Arr (J);
        J         := J - 1;
      end loop;

      Arr (J+1) := Key;

    end loop;

    return Arr;

  end Sort;

end Insertion_Sort;
