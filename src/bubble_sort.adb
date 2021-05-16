package body Bubble_Sort is

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Bubble Sort.
  --
  -- Notes:
  --   Bubble sort repeatedly steps through the array, compares each pair
  --   of adjacent values, and swaps them if they're in the wrong order.
  --
  -- Performance:
  --   Worst-case: O(n^2)
  --   Best-case:  O(n)
  --   Average:    O(n^2)
  --
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object is

    use type Random_Array.Functions.Value;

    Arr     : Random_Array.Object := Sort_Array;
    Swapped : Boolean        := True;

  begin

    -- Continue until there are no more elements to swap
    while Swapped loop

      Swapped := False;

      for X in Natural range 1 .. Arr'Length - 1 loop

        if Arr(X-1) > Arr(X) then

          Random_Array.Functions.Swap (Arr, X-1, X);
          Swapped := True;

        end if;

      end loop;

    end loop;

    return Arr;

  end Sort;

end Bubble_Sort;
