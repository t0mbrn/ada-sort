with Sort_Interface;
with Random_Array;

package Bubble_Sort is

  type Object is new Sort_Interface.Object with null record;


--- ```
--- -- Testing for the correct Name
--- declare
---     Algorithm : Bubble_Sort.Object;
---     Name : constant String := "Bubble Sort";
--- begin
---     Ahven.Assert
---      (Condition   => Algorithm.Name = Name,
---       Message  => "Name did not match expected. Expected: " & Name);
--- end;
--- ```
  overriding
  function Name (This : in Object) return String is ("Bubble Sort");


--- ```
--- declare
---     Algorithm : Bubble_Sort.Object;
---     Input  : Random_Array.Object := Random_Array.Functions.Generate;
---     Output : Random_Array.Object (Input'Range);
--- begin
---
---     Output := Algorithm.Sort (Input);
---
---     Ahven.Assert
---      (Condition => Random_Array.Functions.Is_Sorted (Output),
---       Message   => "The array was not sorted correctly.");
--- end;
--- ```
  overriding
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object;


end Bubble_Sort;
