package Tests is

  type Output_Format is
    (Full,
     Time_Summary,
     Result_Summary,
     Sort_Check);

  Default_Format : constant Output_Format := Full;

  -- Test Cases
  procedure Test_One;

end Tests;
