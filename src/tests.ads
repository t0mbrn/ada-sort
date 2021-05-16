package Tests is

  type Output_Format_Type is
    (Full,
     Time_Summary,
     Result_Summary,
     Sort_Check);

  Default_Format : constant Output_Format_Type := Full;

  -- Test Cases
  procedure Test_One (Format : in Output_Format_Type := Default_Format);

end Tests;
