package body Merge_Sort is

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Merge Sort.
  --
  -- Notes:
  --   Merge Sort is a divide and conquer algorithm. It uses two basic steps
  --   to sort the array:
  --
  --     1. Divide the unsorted array into n sub-arrays, each containing one
  --        element (an array of of one element is considered sorted).
  --
  --     2. Merge sub-arrays by adding the lower current element of either array
  --        to the sorted array. Repeat until only one sorted array remains.

  --
  -- Performance:
  --   Worst-case: O(n*log(n))
  --   Best-case:  O(n*log(n))
  --   Average:    O(n*log(n))
  --
  --
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object is

    Merge_Array : Random_Array.Object := Sort_Array;

    ----------------------------------------------------------------------------
    --
    function Merge (Left  : in Random_Array.Object;
                    Right : in Random_Array.Object) return Random_Array.Object is

      L : Integer := Left'First;
      R : Integer := Right'First;
      M : Integer := Left'First;

    begin

      -- Loop while both arrays still have elements to merge
      while L <= Left'Last and R <= Right'Last loop

        if Left (L) <= Right (R) then
          Merge_Array (M) := Left (L);
          L := L + 1;
        else
          Merge_Array (M) := Right (R);
          R := R + 1;
        end if;

        M := M + 1;

      end loop;

      -- Add any remaining elements of Left to the merged array
      while L <= Left'Last loop
        Merge_Array (M) := Left (L);
        L := L + 1;
        M := M + 1;
      end loop;

      -- Add any remaining elements of Right to the merged array
      while R <= Right'Last loop
        Merge_Array (M) := Right (R);
        R := R + 1;
        M := M + 1;
      end loop;

      return Merge_Array;

    end Merge;

  begin

    if Sort_Array'First < Sort_Array'Last then

      declare

        Mid : constant Integer := Sort_Array'First + (Sort_Array'Last - Sort_Array'First)/2;

        Left_Sub    : Random_Array.Object (Sort_Array'First .. Mid)  := (others => 0);
        Right_Sub   : Random_Array.Object (Mid+1 .. Sort_Array'Last) := (others => 0);

      begin

        Left_Sub  := This.Sort (Sort_Array (Sort_Array'First .. Mid));
        Right_Sub := This.Sort (Sort_Array (Mid+1 .. Sort_Array'Last));

        Merge_Array := Merge (Left_Sub, Right_Sub);

      end;

    end if;

    return Merge_Array;

  end Sort;

end Merge_Sort;
