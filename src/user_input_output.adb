with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
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

package body User_Input_Output is

  Exit_Exception : Exception;

  NL : constant Character := Ada.Characters.Latin_1.LF;

  type Output_Format is
    (Full,
     Time_Summary,
     Result_Summary,
     Sort_Check);

  for Output_Format use
    (Full           => 1,
     Time_Summary   => 2,
     Result_Summary => 3,
     Sort_Check     => 4);

  Print_Format : Output_Format := Full;

  -- Array of Test Results
  Test_Count : constant Natural := 6;

  -- The random array to sort
  Test_Array : Random_Array.Object := Random_Array.Functions.Generate;

  type Test_Result is record
    Test_Name : Unbounded_String;
    Run_Time  : Duration := 0.0;
    Output    : Random_Array.Object (Test_Array'Range);
  end record;

  type Test_Results is array (1 .. Test_Count) of Test_Result;

  Current_Sort : access Sort_Interface.Object'Class := new Insertion_Sort.Object;

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
  procedure Print_Test_Array is
  begin

    Separator;
    Ada.Text_IO.Put_Line ("Random Array:");
    Separator;
    Random_Array.Functions.Print (Test_Array);
    Separator;

  end Print_Test_Array;

  ------------------------------------------------------------------------------
  --
  procedure Generate_Array_Callback is
  begin

    Test_Array := Random_Array.Functions.Generate;
    Print_Test_Array;

  end Generate_Array_Callback;

  ------------------------------------------------------------------------------
  --
  procedure Output_Format_Callback is

    Selection : Integer := 0;

  begin

    Ada.Text_IO.Put_Line (NL & "Select output format: ");

    for Option in Output_Format loop
      Ada.Text_IO.Put_Line (Integer'Image (Output_Format'Enum_Rep (Option)) & ". " & Option'Img);
    end loop;

    Ada.Text_IO.Put ("> ");
    Ada.Integer_Text_IO.Get (Selection);

    Print_Format := Output_Format'Val (Selection - 1);
    Ada.Text_IO.Put_Line ("Setting format: " & Print_Format'Img);

  exception

    when Constraint_Error =>

      Ada.Text_IO.Put_Line ("Invalid option.");

  end Output_Format_Callback;

  ----------------------------------------------------------------------------
  --
  -- Description:
  --   Execute a sort run.
  --
  function Run_Sort
    (Algorithm : access Sort_Interface.Object'Class)
     return Test_Result is

    Arr   : Random_Array.Object := Test_Array;
    Start : Ada.Calendar.Time;
    Span  : Duration;

    use type Ada.Calendar.Time;

  begin

    -- Time the given sort algorithm's execution
    Start := Ada.Calendar.Clock;
    Arr   := Algorithm.Sort (Test_Array);
    Span  := Ada.Calendar.Clock - Start;

    return (Test_Name => To_Unbounded_String (Algorithm.Name),
            Run_Time  => Span,
            Output    => Arr);

  end Run_Sort;

  ------------------------------------------------------------------------------
  --
  procedure Select_Sort_Callback is

    Algorithms : constant array (Positive range 1 .. 6) of access Sort_Interface.Object'Class :=
      (new Insertion_Sort.Object,
       new Selection_Sort.Object,
       new Bubble_Sort.Object,
       new Quick_Sort.Object,
       new Merge_Sort.Object,
       new Heap_Sort.Object);

    Selection : Integer := 0;

  begin

    Ada.Text_IO.Put_Line (NL & "Select sorting algorithm: ");

    for Index in Algorithms'Range loop
      Ada.Text_IO.Put_Line (Index'Img & ". " & Algorithms (Index).Name);
    end loop;

    Ada.Text_IO.Put ("> ");
    Ada.Integer_Text_IO.Get (Selection);

    if Selection in Algorithms'Range then
      Current_Sort := Algorithms (Selection);
    else
      Ada.Text_IO.Put_Line ("Invalid selection.");
    end if;

  end Select_Sort_Callback;

  ------------------------------------------------------------------------------
  --
  procedure Perform_Sort_Callback is

    Result : constant Test_Result := Run_Sort (Current_Sort);

  begin

    Ada.Text_IO.Put_Line ("Name:  " & To_String (Result.Test_Name));

    case Print_Format is

      when Full =>

        Random_Array.Functions.Print (Result.Output);
        Ada.Text_IO.Put_Line ("Time:  " & Result.Run_Time'Img);
        Ada.Text_IO.Put_Line ("Check: " & Boolean'Image (Random_Array.Functions.Is_Sorted (Result.Output)));


      when Time_Summary =>

        Ada.Text_IO.Put_Line ("Time: " & Result.Run_Time'Img);

      when Result_Summary =>

        Ada.Text_IO.Put_Line ("Result:");
        Random_Array.Functions.Print (Result.Output);

      when Sort_Check =>

        Ada.Text_IO.Put_Line ("Check: " & Boolean'Image (Random_Array.Functions.Is_Sorted (Result.Output)));

    end case;



  end Perform_Sort_Callback;

  ------------------------------------------------------------------------------
  --
  procedure Print (Results : in Test_Results) is

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
    Print_Test_Array;

    -- Print test output according to the given format choice
    case Print_Format is

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
  procedure Run_All_Callback is

    Results : Test_Results;

  begin

    -- Test Runs
    Results (1) := Run_Sort (new Insertion_Sort.Object);
    Results (2) := Run_Sort (new Selection_Sort.Object);
    Results (3) := Run_Sort (new Bubble_Sort.Object);
    Results (4) := Run_Sort (new Quick_Sort.Object);
    Results (5) := Run_Sort (new Merge_Sort.Object);
    Results (6) := Run_Sort (new Heap_Sort.Object);

    Print (Results);

  end Run_All_Callback;

  ------------------------------------------------------------------------------
  --
  procedure Exit_Callback is
  begin

    raise Exit_Exception;

  end Exit_Callback;

  ------------------------------------------------------------------------------
  --
  type Option is record
    Name     : Unbounded_String;
    Callback : access procedure;
  end record;

  Options : constant array (Positive range 1 .. 5) of Option :=
    ((To_Unbounded_String ("Generate new random array"), Generate_Array_Callback'Access),
     (To_Unbounded_String ("Change output format"),      Output_Format_Callback'Access),
     (To_Unbounded_String ("Select sorting algorithm"),  Select_Sort_Callback'Access),
     (To_Unbounded_String ("Perform sort"),              Perform_Sort_Callback'Access),
     (To_Unbounded_String ("Exit"),                      Exit_Callback'Access));

  ------------------------------------------------------------------------------
  --
  procedure Print_Menu is
  begin

    Ada.Text_IO.Put_Line ("");

    for Opt in Options'Range loop
      Ada.Text_IO.Put_Line (Opt'Img & ". " & To_String (Options (Opt).Name));
    end loop;

  end Print_Menu;

  ------------------------------------------------------------------------------
  --
  procedure Prompt_User is

    Selection : Integer := 0;

  begin

    Print_Test_Array;

    while True loop

      Print_Menu;

      Ada.Text_Io.Put_Line (NL & "Output Format:     " & Print_Format'Img);
      Ada.Text_Io.Put_Line ("Sorting Algorithm: " & Current_Sort.Name & NL);

      Ada.Text_IO.Put_Line (NL & "Select an option: ");
      Ada.Integer_Text_IO.Get (Selection);

      if Selection in Options'Range then
        Options (Selection).Callback.all;
      else
        Ada.Text_IO.Put_Line ("Invalid option.");
      end if;

    end loop;

  exception

    when Exit_Exception =>

      Ada.Text_IO.Put_Line ("Exiting.");

  end Prompt_User;

end User_Input_Output;
