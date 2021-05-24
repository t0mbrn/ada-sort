package body Algorithms is

  function Get_Algorithms return Algorithm_List is (Sorting_Algorithms);

  function Get_Algorithm
    (Index : in Integer)
     return access Sort_Interface.Object'Class is

  begin

    return Sorting_Algorithms (Algorithm_Range (Index));

  exception

    when Constraint_Error =>

      raise Invalid_Index;

  end Get_Algorithm;


end Algorithms;
