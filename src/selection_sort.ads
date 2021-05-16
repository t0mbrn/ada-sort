with Sort_Interface;
with Random_Array;

package Selection_Sort is

  type Object is new Sort_Interface.Object with null record;

  overriding
  function Name (This : in Object) return String is ("Selection Sort");

  overriding
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object;

end Selection_Sort;
