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
with Selection_Sort;
with Quick_Sort;

with Sort_Interface;
with Random_Array;

package body Tests is

  package Asu renames Ada.Strings.Unbounded;


  ------------------------------------------------------------------------------
  -- TEST RUNS
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  --
  -- Description:
  --  Mainline test execution.
  --
  procedure Sort_Test_One (Output_Format : in Output_Format_Type := Test_Output) is

    use type Asu.Unbounded_String;

    -- The random array to sort
    Test_Array : Random_Array.Object := Random_Array.Functions.Generate_Random_Array;

    -- Array of Test Results
    Test_Count : constant Natural := 6;

    type Test_Result is record
      Test_Name : Asu.Unbounded_String;
      Run_Time  : Duration := 0.0;
      Output    : Random_Array.Object (Test_Array'Range);
    end record;

    Test_Results : array (1 .. Test_Count) of Test_Result;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Execute a single experiment run.
    --
    function Test_Run
      (Algorithm : access Sort_Interface.Object'Class)
       return Test_Result is

      Arr : Random_Array.Object := Test_Array;

      Start : Ada.Calendar.Time;
      Span  : Duration;

      use type Ada.Calendar.Time;

    begin

      -- Time the given sort algorithm's execution
      Start := Ada.Calendar.Clock;
      Arr   := Algorithm.Sort (Test_Array);
      Span  := Ada.Calendar.Clock - Start;

      return (Test_Name => Asu.To_Unbounded_String (Algorithm.Name),
              Run_Time  => Span,
              Output    => Arr);

    end Test_Run;

    ----------------------------------------------------------------------------
    --
    procedure Separator is
    begin
      Ada.Text_IO.Put_Line ("------------------------");
    end Separator;

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
      for Index in Test_Results'Range loop
        Ada.Text_IO.Put_Line (Asu.To_String (Test_Results (Index).Test_Name));
        Random_Array.Functions.Print (Test_Results(Index).Output);
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

      for Index in Test_Results'Range loop
        Ada.Text_IO.Put_Line (Asu.To_String (Test_Results (Index).Test_Name) & ": " & Test_Results(Index).Run_Time'Img);
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

      for Index in Test_Results'Range loop
        Ada.Text_IO.Put_Line (Asu.To_String (Test_Results (Index).Test_Name) & ": " & Random_Array.Functions.Is_Sorted (Test_Results(Index).Output)'Img);
      end loop;

    end Print_Sort_Check;

  begin

    -- Print the random array
    Separator;
    Ada.Text_IO.Put_Line ("Random Array:");
    Separator;
    Random_Array.Functions.Print (Test_Array);
    Separator;

    -- Test Runs
    Test_Results (1) := Test_Run (new Insertion_Sort.Object);
    Test_Results (2) := Test_Run (new Selection_Sort.Object);
    Test_Results (3) := Test_Run (new Bubble_Sort.Object);
    Test_Results (4) := Test_Run (new Quick_Sort.Object);
    Test_Results (5) := Test_Run (new Merge_Sort.Object);
    Test_Results (6) := Test_Run (new Heap_Sort.Object);

    -- Print test output according to the given format choice
    case Output_Format is

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

  end Sort_Test_One;

end Tests;
