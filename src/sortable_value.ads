package Sortable is

  type Value is interface with null record;

  ------------------------------------------------------------------------------
  --
  function "<" (L : in Value;
                R : in Value)
                return Boolean is abstract;

  ------------------------------------------------------------------------------
  --
  function "=" (L : in Value;
                R : in Value)
                return Boolean is abstract;

  ------------------------------------------------------------------------------
  --
  function Image_Of (Self : in Value)
                     return String is abstract;


end Sortable;
