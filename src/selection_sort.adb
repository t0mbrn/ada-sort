package body Selection_Sort is

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Selection Sort.
  --
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object is

    Arr : Random_Array.Object := Sort_Array;
    Min : Integer := 0;

  begin

    for Index in Arr'First .. Arr'Last - 1 loop

      Min := Index;

      for I in Index+1 .. Arr'Last loop

        if Arr(I) < Arr(Min) then
          Min := I;
        end if;

      end loop;

      if Min /= Index then
        Random_Array.Functions.Swap (Arr, Index, Min);
      end if;

    end loop;

    return Arr;

  end Sort;

end Selection_Sort;
