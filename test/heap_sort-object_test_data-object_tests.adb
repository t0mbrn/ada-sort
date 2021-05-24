--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Heap_Sort.Object_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Heap_Sort.Object_Test_Data.Object_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

  Algorithm : Heap_Sort.Object;

--  begin read only
--  end read only

--  begin read only
   procedure Test_Name (Gnattest_T : in out Test_Object);
   procedure Test_Name_80c71b (Gnattest_T : in out Test_Object) renames Test_Name;
--  id:2.2/80c71b2a4a7912fe/Name/1/0/
   procedure Test_Name (Gnattest_T : in out Test_Object) is
   --  heap_sort.ads:8:3:Name
--  end read only

    pragma Unreferenced (Gnattest_T);

    Name : constant String := "Heap Sort";

  begin

    AUnit.Assertions.Assert
      (Actual   => Algorithm.Name,
       Expected => Name,
       Message  => "Name did not match expected. Expected: " & Name);

--  begin read only
   end Test_Name;
--  end read only


--  begin read only
   procedure Test_Sort (Gnattest_T : in out Test_Object);
   procedure Test_Sort_e56c88 (Gnattest_T : in out Test_Object) renames Test_Sort;
--  id:2.2/e56c885d1de4b874/Sort/1/0/
   procedure Test_Sort (Gnattest_T : in out Test_Object) is
   --  heap_sort.ads:11:3:Sort
--  end read only

    pragma Unreferenced (Gnattest_T);

    Input  : Random_Array.Object := Random_Array.Functions.Generate;
    Output : Random_Array.Object (Input'Range);

  begin

    Output := Algorithm.Sort (Input);

    AUnit.Assertions.Assert
      (Condition => Random_Array.Functions.Is_Sorted (Output),
       Message   => "The array was not sorted correctly.");

--  begin read only
   end Test_Sort;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
  null;
--  begin read only
--  end read only
end Heap_Sort.Object_Test_Data.Object_Tests;
