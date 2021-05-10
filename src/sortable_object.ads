with Sortable_Value;

generic

  type Data_Type is Sortable_Value.Value;

  -- Number of values in the array
  Size : Natural := 100;

package Sortable_Object is

  type Sortable_Object is interface;

  type Array_Type is array (Index_Type) of Data_Type;

  function Compare (This  : in Sortable_Object;
                    Left  : in Index_Type;
                    Right : in Index_Type)
                    return Boolean;

  procedure Swap (This  : in Sortable_Object;
                  Left  : in Index_Type;
                  Right : in Index_Type);

  function Is_Sorted (This : in Sortable_Object) return Boolean;

  procedure Print (This : in Sortable_Object);

end Sortable_Array;
