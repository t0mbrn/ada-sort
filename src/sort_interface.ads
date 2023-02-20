with Random_Array;

--  @summary
--  Interface for Sorting Algorithms
--
--  @description
--  This package defines the interface for sorting algorithms to implement. The
--  main routine Sort defines the algorithm itself.
--
package Sort_Interface is

  type Object is interface;

  --  Return the name of this sorting algorithm.
  --
  --  @return  the name of the sorting algorithm.
  --
  function Name (This : in Object) return String is abstract;

  --  Sort the input array.
  --
  --  @param This        the sort interface implementation
  --  @param Sort_Array  the array to be sorted.
  --
  --  @return  the sorted array
  --
  function Sort
    (This       : in Object;
     Sort_Array : in Random_Array.Object)
     return Random_Array.Object is abstract;

end Sort_Interface;
