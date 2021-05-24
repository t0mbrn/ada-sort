with Bubble_Sort;
with Heap_Sort;
with Insertion_Sort;
with Merge_Sort;
with Selection_Sort;
with Sort_Interface;
with Quick_Sort;

--  @summary
--  Container for Sorting Algorithms
--
--  @description
--  This package provides a container for storing the full list of sorting
--  algorithms.
--
package Algorithms is

  Invalid_Index : Exception;

  subtype Algorithm_Range is Integer range 1 .. 6;

  type Algorithm_List is array (Algorithm_Range) of access Sort_Interface.Object'Class;


  --  Return the list of sorting algorithms.
  --
  --  @return  the list of sorting algorithms
  --
  function Get_Algorithms return Algorithm_List;


  --  Return the sorting algorithms implementation at the given index.
  --
  --  @return  the sorting algorithms implementation.
  --
  function Get_Algorithm
    (Index : in Integer)
     return access Sort_Interface.Object'Class;


private

  Sorting_Algorithms : constant Algorithm_List :=
    (new Insertion_Sort.Object,
     new Selection_Sort.Object,
     new Bubble_Sort.Object,
     new Quick_Sort.Object,
     new Merge_Sort.Object,
     new Heap_Sort.Object);

end Algorithms;
