with Sort_Interface;
with Random_Array;

package Merge_Sort is

  type Object is new Sort_Interface.Object with null record;

  type Object_Access is access Object'class;

  overriding
  function Name (This : in Object) return String is ("Merge Sort");

  overriding
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object;

end Merge_Sort;
