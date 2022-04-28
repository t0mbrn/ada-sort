with Sort_Interface;
with Random_Array;

package Quick_Sort is

  type Object is new Sort_Interface.Object with null record;

  overriding
  function Name (This : in Object) return String is ("Quick Sort");

--- ```
--- declare
---     Name : constant String := "Quick Sort";
--- begin
---     AUnit.Assertions.Assert
---      (Actual   => Algorithm.Name,
---       Expected => Name,
---       Message  => "Name did not match expected. Expected: " & Name);
--- end;
--- ```

    overriding function Sort
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object;

--- ```
--- declare
---     Input  : Random_Array.Object := Random_Array.Functions.Generate;
---     Output : Random_Array.Object (Input'Range);
--- begin
---
---     Output := Algorithm.Sort (Input);
---
---     AUnit.Assertions.Assert
---      (Condition => Random_Array.Functions.Is_Sorted (Output),
---       Message   => "The array was not sorted correctly.");
--- end;
--- ```

end Quick_Sort;
