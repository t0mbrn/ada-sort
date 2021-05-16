with Sort_Interface;
with Random_Array;

package Heap_Sort is

  type Object is new Sort_Interface.Object with null record;

  overriding
  function Name (This : in Object) return String is ("Heap Sort");

  overriding
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object;

end Heap_Sort;
