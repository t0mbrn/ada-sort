--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Bubble_Sort.Object_Test_Data is

   Local_Object : aliased GNATtest_Generated.GNATtest_Standard.Bubble_Sort.Object;
   procedure Set_Up (Gnattest_T : in out Test_Object) is
   begin
      Gnattest_T.Fixture := Local_Object'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Object) is
   begin
      null;
   end Tear_Down;

end Bubble_Sort.Object_Test_Data;
