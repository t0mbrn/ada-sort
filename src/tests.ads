package Tests is

  type Output_Format_Type is (Full,
                              Time_Summary,
                              Result_Summary,
                              Sort_Check);

  Test_Output : constant Output_Format_Type := Full;


  -------------------------------------------------------------------------------
  -- TEST RUNS
  -------------------------------------------------------------------------------
  procedure Sort_Test_One (Output_Format : in Output_Format_Type := Test_Output);


end Tests;
