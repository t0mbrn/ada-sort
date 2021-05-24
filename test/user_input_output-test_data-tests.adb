--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into User_Input_Output.Test_Data.

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
package body User_Input_Output.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Prompt_User (Gnattest_T : in out Test);
   procedure Test_Prompt_User_016ddf (Gnattest_T : in out Test) renames Test_Prompt_User;
--  id:2.2/016ddf28b1716c15/Prompt_User/1/0/
   procedure Test_Prompt_User (Gnattest_T : in out Test) is
   --  user_input_output.ads:14:3:Prompt_User
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

   --  Ada.Text_IO.Stub_Data.Set_Stub_Put_Line_a61d17_7da60c( );

      AUnit.Assertions.Assert
        (True,
         "Test not implemented.");

--  begin read only
   end Test_Prompt_User;
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
end User_Input_Output.Test_Data.Tests;
