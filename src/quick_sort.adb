package body Quick_Sort is

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Perform a quicksort in-place on the given Sortable_Aray.
  --
  -- Notes:
  --   Quicksort is a recursive, divide and conquer algorithm. It uses three
  --   basic steps to sort the array:
  --
  --     1. Choose an element of the array to be the pivot.
  --
  --     2. Reorder the array so that elements less than the pivot come before
  --        it and elements greater than the pivot come after it.
  --
  --     3. Recursively apply the above to sub-array of elements less than the
  --        pivot and to the sub-array of elements greater than the pivot.
  --
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object is

    Arr : Random_Array.Object := Sort_Array;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Moves the pivot to its position in the sorted array, places all
    --   smaller elements to the left of the pivot, and all greater elements to
    --   the right of the pivot.
    --
    function Partition (P_Arr : in out Random_Array.Object;
                        Low   : in Integer;
                        High  : in Integer)
                          return Natural is

      I  : Integer := Low - 1;

      --
      -- The pivot is important to the performance of the quicksort
      -- algorithm. Here we choose the last element in the partition as the
      -- pivot. This tends to result in poor performance on arrays that are
      -- already sorted (these crop up a lot towards the end of the sort).
      --

      Pivot : constant Random_Array.Functions.Value := P_Arr (High);

    begin

      for Index in Natural range Low .. High - 1 loop

        if P_Arr (Index) <= Pivot then

          I := I + 1;
          Random_Array.Functions.Swap (P_Arr, I, Index);

        end if;

      end loop;

      Random_Array.Functions.Swap (P_Arr, I+1, High);

      return I + 1;

    end Partition;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --
    --
    procedure Recursive_Sort
      (S_Arr : in out Random_Array.Object;
       Low   : in Integer;
       High  : in Integer) is
      Split : Integer;
    begin

      if Low < High then

        Split := Partition (S_Arr, Low, High);

        Recursive_Sort (S_Arr, Low, Split - 1);
        Recursive_Sort (S_Arr, Split + 1, High);

      end if;

    end Recursive_Sort;

  begin

    Recursive_Sort (Arr, Arr'First, Arr'Last);
    return Arr;

  end Sort;

end Quick_Sort;
