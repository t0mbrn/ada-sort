--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Algorithms.Test_Data.

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
package body Algorithms.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Algorithms (Gnattest_T : in out Test);
   procedure Test_Get_Algorithms_109b78 (Gnattest_T : in out Test) renames Test_Get_Algorithms;
--  id:2.2/109b784f9a334590/Get_Algorithms/1/0/
   procedure Test_Get_Algorithms (Gnattest_T : in out Test) is
   --  algorithms.ads:29:3:Get_Algorithms
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Algorithms;
--  end read only


--  begin read only
   procedure Test_Get_Algorithm (Gnattest_T : in out Test);
   procedure Test_Get_Algorithm_38bb9b (Gnattest_T : in out Test) renames Test_Get_Algorithm;
--  id:2.2/38bb9bc554f01123/Get_Algorithm/1/0/
   procedure Test_Get_Algorithm (Gnattest_T : in out Test) is
   --  algorithms.ads:36:3:Get_Algorithm
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Algorithm;
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
end Algorithms.Test_Data.Tests;
