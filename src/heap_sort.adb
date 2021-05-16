
package body Heap_Sort is

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Heap Sort.
  --
  -- Notes:
  --
  --
  -- Performance:
  --   Worst-Case:
  --   Best-Case:
  --   Average:
  --
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object is

    Arr : Random_Array.Object := Sort_Array;


    ----------------------------------------------------------------------------
    --
    procedure Heapify (Sort_Array : in out Random_Array.Object;
                       N          : in Integer;
                       I          : in Integer) is

      High : Integer := I;
      L    : Integer := 2*I + 1;
      R    : Integer := 2*i + 2;

    begin

      if L < N and then Sort_Array (L) > Sort_Array (High) then
        High := L;
      end if;

      if R < N and then Sort_Array (R) > Sort_Array (High) then
        High := R;
      end if;

      if High /= I then
        Random_Array.Functions.Swap (Sort_Array, I, High);
        Heapify (Sort_Array, N, High);
      end if;

    end Heapify;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --
    --
    procedure Perform_Sort
      (S_Arr : in out Random_Array.Object;
       Size  : in Integer) is
    begin

      for Index in reverse 0 .. Size/2 - 1 loop
        Heapify (S_Arr, Size, Index);
      end loop;

      for Index in reverse 0 .. Size - 1 loop
        Random_Array.Functions.Swap (S_Arr, 0, Index);
        Heapify (S_Arr, Index, 0);
      end loop;

    end Perform_Sort;

    -- Size of the heap = size of the array / size of one element
    Heap_Size : constant Natural := Sort_Array'Size / Sort_Array (0)'Size;

  begin

    Perform_Sort (Arr, Heap_Size);
    return Arr;

  end Sort;

end Heap_Sort;
