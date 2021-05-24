--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sortable_Array.Test_Data.

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
package body Sortable_Array.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Generate (Gnattest_T : in out Test);
   procedure Test_Generate_1e25f0 (Gnattest_T : in out Test) renames Test_Generate;
--  id:2.2/1e25f05dd3fb2567/Generate/1/0/
   procedure Test_Generate (Gnattest_T : in out Test) is
   --  sortable_array.ads:35:3:Generate
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (True,
         "Test not implemented.");

--  begin read only
   end Test_Generate;
--  end read only


--  begin read only
   procedure Test_Swap (Gnattest_T : in out Test);
   procedure Test_Swap_48d5fe (Gnattest_T : in out Test) renames Test_Swap;
--  id:2.2/48d5fe4a6b9368cb/Swap/1/0/
   procedure Test_Swap (Gnattest_T : in out Test) is
   --  sortable_array.ads:45:3:Swap
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (True,
         "Test not implemented.");

--  begin read only
   end Test_Swap;
--  end read only


--  begin read only
   procedure Test_Is_Sorted (Gnattest_T : in out Test);
   procedure Test_Is_Sorted_87493b (Gnattest_T : in out Test) renames Test_Is_Sorted;
--  id:2.2/87493b74e62aefbc/Is_Sorted/1/0/
   procedure Test_Is_Sorted (Gnattest_T : in out Test) is
   --  sortable_array.ads:56:3:Is_Sorted
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (True,
         "Test not implemented.");

--  begin read only
   end Test_Is_Sorted;
--  end read only


--  begin read only
   procedure Test_Print (Gnattest_T : in out Test);
   procedure Test_Print_46e9a1 (Gnattest_T : in out Test) renames Test_Print;
--  id:2.2/46e9a19d34a69035/Print/1/0/
   procedure Test_Print (Gnattest_T : in out Test) is
   --  sortable_array.ads:69:3:Print
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (True,
         "Test not implemented.");

--  begin read only
   end Test_Print;
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
end Sortable_Array.Test_Data.Tests;
