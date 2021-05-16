with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Bubble_Sort;
with Heap_Sort;
with Insertion_Sort;
with Merge_Sort;
with Quick_Sort;
with Random_Array;
with Selection_Sort;
with Sort_Interface;

use Ada.Strings.Unbounded;

package body Tests is

  -- Array of Test Results
  Test_Count : constant Natural := 6;

  -- The random array to sort
  Original : constant Random_Array.Object := Random_Array.Functions.Generate;

  type Test_Result is record
    Test_Name : Unbounded_String;
    Run_Time  : Duration := 0.0;
    Output    : Random_Array.Object (Original'Range);
  end record;

  type Test_Results is array (1 .. Test_Count) of Test_Result;

  procedure Print
    (Results  : in Test_Results;
     Format   : in Output_Format_Type := Default_Format) is

    ----------------------------------------------------------------------------
    --
    procedure Separator is
    begin
      Ada.Text_IO.Put_Line ("------------------------");
    end Separator;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --  Print the original random array prior to sorting.
    --
    procedure Print_Original is
    begin

      Separator;
      Ada.Text_IO.Put_Line ("Random Array:");
      Separator;
      Random_Array.Functions.Print (Original);
      Separator;

    end Print_Original;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Print sorted arrays from the Test Results array.
    --
    procedure Print_Results is
    begin

      Ada.Text_IO.Put_Line ("Sort Results:");
      Separator;

      -- Print sorted arrays
      for Index in Results'Range loop
        Ada.Text_IO.Put_Line (To_String (Results (Index).Test_Name));
        Random_Array.Functions.Print (Results (Index).Output);
      end loop;

    end Print_Results;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Print execution times from the Test Results array.
    --
    procedure Print_Times is
    begin

      Ada.Text_IO.Put_Line ("Times: ");
      Separator;

      for Index in Results'Range loop
        Ada.Text_IO.Put_Line (To_String (Results (Index).Test_Name) & ": " & Results (Index).Run_Time'Img);
      end loop;

    end Print_Times;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Print the results of a sort check on the sorted arrays.
    --
    procedure Print_Sort_Check is
    begin

      Ada.Text_IO.Put_Line ("Sort Check: ");
      Separator;

      for Index in Results'Range loop
        Ada.Text_IO.Put_Line (To_String (Results (Index).Test_Name) & ": " & Random_Array.Functions.Is_Sorted (Results (Index).Output)'Img);
      end loop;

    end Print_Sort_Check;

  begin

    -- Print the original array before sorting
    Print_Original;

    -- Print test output according to the given format choice
    case Format is

      when Full =>

        Print_Results;
        Separator;
        Print_Times;
        Separator;
        Print_Sort_Check;
        Separator;

      when Time_Summary =>
        Print_Times;

      when Result_Summary =>
        Print_Results;

      when Sort_Check =>
        Print_Sort_Check;

    end case;

  end Print;

  ------------------------------------------------------------------------------
  --
  procedure Test_One (Format : in Output_Format_Type := Default_Format) is

    Results : Test_Results;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Execute a single experiment run.
    --
    function Test_Run
      (Algorithm : access Sort_Interface.Object'Class)
       return Test_Result is

      Arr : Random_Array.Object := Original;

      Start : Ada.Calendar.Time;
      Span  : Duration;

      use type Ada.Calendar.Time;

    begin

      -- Time the given sort algorithm's execution
      Start := Ada.Calendar.Clock;
      Arr   := Algorithm.Sort (Original);
      Span  := Ada.Calendar.Clock - Start;

      return (Test_Name => To_Unbounded_String (Algorithm.Name),
              Run_Time  => Span,
              Output    => Arr);

    end Test_Run;

  begin

    -- Test Runs
    Results (1) := Test_Run (new Insertion_Sort.Object);
    Results (2) := Test_Run (new Selection_Sort.Object);
    Results (3) := Test_Run (new Bubble_Sort.Object);
    Results (4) := Test_Run (new Quick_Sort.Object);
    Results (5) := Test_Run (new Merge_Sort.Object);
    Results (6) := Test_Run (new Heap_Sort.Object);

    Print (Results);

  end Test_One;

end Tests;
