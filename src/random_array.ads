with Sortable_Array;

package Random_Array is

  -- Instantiation of the Sortable_Array generic
  package Functions is new Sortable_Array
    (Min  => 0,
     Max  => 99,
     Size => 500);
  
  subtype Object is Functions.Object;

end Random_Array;
