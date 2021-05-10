with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Sortable_Array;

package body Sort_Pkg is

  package Asu renames Ada.Strings.Unbounded;

  -- Instantiation of the Sortable_Array generic
  package Sortable_Array_Pkg is new Sortable_Array (Min  => 0,
                                                    Max  => 99,
                                                    Size => 500);

  subtype Sortable_Array is Sortable_Array_Pkg.Sortable_Array;

  -- Access type to the sorting algorithm
  type Sort_Function is access function
    (Sort_Array : in Sortable_Array)
     return Sortable_Array;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Insertion Sort.
  --
  -- Notes:
  --   A simple sorting algorithm that takes elements from the list one by
  --   one and inserts them into their correct position in the sorted array.
  --   Insertion tends to be expensive because it requires shifting all
  --   following elements over by one.
  --
  --
  -- Performance:
  --   Worst-case: O(n^2)
  --   Best-case:  O(n)
  --   Average:    O(n^2)
  --
  function Insertion_Sort (Sort_Array : in Sortable_Array)
                           return Sortable_Array is

    Arr : Sortable_Array := Sort_Array;

    J   : Integer := 0;
    Key : Integer := 0;

    Size : constant Natural := Arr'Size/Arr(0)'Size;

  begin

    for Index in 1 .. Size - 1 loop

      Key := Arr (Index);
      J   := Index - 1;

      -- Move elements 0 to i-1 that are greater than the key to one
      -- position ahead of their current position.

      while J >= 0 and then Arr (J) > Key loop
        Arr (J+1) := Arr (J);
        J         := J - 1;
      end loop;

      Arr (J+1) := Key;

    end loop;

    return Arr;

  end Insertion_Sort;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Selection Sort.
  --
  function Selection_Sort (Sort_Array : in Sortable_Array)
                           return Sortable_Array is

    Arr : Sortable_Array := Sort_Array;
    Min : Integer := 0;

  begin

    for Index in Arr'First .. Arr'Last - 1 loop

      Min := Index;

      for I in Index+1 .. Arr'Last loop

        if Arr(I) < Arr(Min) then
          Min := I;
        end if;

      end loop;

      if Min /= Index then
        Sortable_Array_Pkg.Swap (Arr, Index, Min);
      end if;

    end loop;

    return Arr;

  end Selection_Sort;
  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Bubble Sort.
  --
  -- Notes:
  --   Bubble sort repeatedly steps through the array, compares each pair
  --   of adjacent values, and swaps them if they're in the wrong order.
  --
  -- Performance:
  --   Worst-case: O(n^2)
  --   Best-case:  O(n)
  --   Average:    O(n^2)
  --
  function Bubble_Sort (Sort_Array : in Sortable_Array)
                        return Sortable_Array is

    use type Sortable_Array_Pkg.Value;

    Arr     : Sortable_Array := Sort_Array;
    Swapped : Boolean        := True;

  begin

    -- Continue until there are no more elements to swap
    while Swapped loop

      Swapped := False;

      for X in Natural range 1 .. Arr'Length - 1 loop

        if Arr(X-1) > Arr(X) then

          Sortable_Array_Pkg.Swap (Arr, X-1, X);
          Swapped := True;

        end if;

      end loop;

    end loop;

    return Arr;

  end Bubble_Sort;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Perform a quicksort in-place on the given Sortable_Aray.
  --
  -- Notes:
  --   Quicksort is a recursive, divide and conquer algorithm. It uses three
  --   basic steps to sort the array:
  --
  --     1. Choose an element of the array to be the pivot.
  --
  --     2. Reorder the array so that elements less than the pivot come before
  --        it and elements greater than the pivot come after it.
  --
  --     3. Recursively apply the above to sub-array of elements less than the
  --        pivot and to the sub-array of elements greater than the pivot.
  --
  function Quick_Sort (Sort_Array : in Sortable_Array) return Sortable_Array is

    Arr : Sortable_Array := Sort_Array;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Moves the pivot to its position in the sorted array, places all
    --   smaller elements to the left of the pivot, and all greater elements to
    --   the right of the pivot.
    --
    function Partition (P_Arr : in out Sortable_Array;
                        Low   : in Integer;
                        High  : in Integer)
                        return Natural is

      I  : Integer := Low - 1;

      --
      -- The pivot is important to the performance of the quicksort
      -- algorithm. Here we choose the last element in the partition as the
      -- pivot. This tends to result in poor performance on arrays that are
      -- already sorted (these crop up a lot towards the end of the sort).
      --

      Pivot : constant Sortable_Array_Pkg.Value := P_Arr (High);

    begin

      for Index in Natural range Low .. High - 1 loop

        if P_Arr (Index) <= Pivot then

          I := I + 1;
          Sortable_Array_Pkg.Swap (P_Arr, I, Index);

        end if;

      end loop;

      Sortable_Array_Pkg.Swap (P_Arr, I+1, High);

      return I + 1;

    end Partition;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --
    --
    procedure Sort (S_Arr : in out Sortable_Array;
                    Low   : in Integer;
                    High  : in Integer) is
      Split : Integer;
    begin

      if Low < High then

        Split := Partition (S_Arr, Low, High);

        Sort (S_Arr, Low, Split - 1);
        Sort (S_Arr, Split + 1, High);

      end if;

    end Sort;

  begin

    Sort (Arr, Arr'First, Arr'Last);
    return Arr;

  end Quick_Sort;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Merge Sort.
  --
  -- Notes:
  --   Merge Sort is a divide and conquer algorithm. It uses two basic steps
  --   to sort the array:
  --
  --     1. Divide the unsorted array into n sub-arrays, each containing one
  --        element (an array of of one element is considered sorted).
  --
  --     2. Merge sub-arrays by adding the lower current element of either array
  --        to the sorted array. Repeat until only one sorted array remains.

  --
  -- Performance:
  --   Worst-case: O(n*log(n))
  --   Best-case:  O(n*log(n))
  --   Average:    O(n*log(n))
  --
  --
  function Merge_Sort (Sort_Array : in Sortable_Array) return Sortable_Array is

    Merge_Array : Sortable_Array := Sort_Array;

    ----------------------------------------------------------------------------
    --
    function Merge (Left  : in Sortable_Array;
                    Right : in Sortable_Array) return Sortable_Array is

      L : Integer := Left'First;
      R : Integer := Right'First;
      M : Integer := Left'First;

    begin

      -- Loop while both arrays still have elements to merge
      while L <= Left'Last and R <= Right'Last loop

        if Left (L) <= Right (R) then
          Merge_Array (M) := Left (L);
          L := L + 1;
        else
          Merge_Array (M) := Right (R);
          R := R + 1;
        end if;

        M := M + 1;

      end loop;

      -- Add any remaining elements of Left to the merged array
      while L <= Left'Last loop
        Merge_Array (M) := Left (L);
        L := L + 1;
        M := M + 1;
      end loop;

      -- Add any remaining elements of Right to the merged array
      while R <= Right'Last loop
        Merge_Array (M) := Right (R);
        R := R + 1;
        M := M + 1;
      end loop;

      return Merge_Array;

    end Merge;

  begin

    if Sort_Array'First < Sort_Array'Last then

      declare

        Mid : constant Integer := Sort_Array'First + (Sort_Array'Last - Sort_Array'First)/2;

        Left_Sub    : Sortable_Array (Sort_Array'First .. Mid)  := (others => 0);
        Right_Sub   : Sortable_Array (Mid+1 .. Sort_Array'Last) := (others => 0);

      begin

        Left_Sub  := Merge_Sort (Sort_Array (Sort_Array'First .. Mid));
        Right_Sub := Merge_Sort (Sort_Array (Mid+1 .. Sort_Array'Last));

        Merge_Array := Merge (Left_Sub, Right_Sub);

      end;

    end if;

    return Merge_Array;

  end Merge_Sort;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Sort the given array using Heap Sort.
  --
  -- Notes:
  --
  --
  -- Performance:
  --   Worst-Case:
  --   Best-Case:
  --   Average:
  --
  function Heap_Sort (Sort_Array : in Sortable_Array) return Sortable_Array is

    Arr : Sortable_Array := Sort_Array;


    ----------------------------------------------------------------------------
    --
    procedure Heapify (Sort_Array : in out Sortable_Array;
                       N          : in Integer;
                       I          : in Integer) is

      High : Integer := I;
      L    : Integer := 2*I + 1;
      R    : Integer := 2*i + 2;

    begin

      if L < N and then Sort_Array (L) > Sort_Array (High) then
        High := L;
      end if;

      if R < N and then Sort_Array (R) > Sort_Array (High) then
        High := R;
      end if;

      if High /= I then
        Sortable_Array_Pkg.Swap (Sort_Array, I, High);
        Heapify (Sort_Array, N, High);
      end if;

    end Heapify;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --
    --
    procedure Sort (S_Arr : in out Sortable_Array;
                    Size  : in Integer) is
    begin

      for Index in reverse 0 .. Size/2 - 1 loop
        Heapify (S_Arr, Size, Index);
      end loop;

      for Index in reverse 0 .. Size - 1 loop
        Sortable_Array_Pkg.Swap (S_Arr, 0, Index);
        Heapify (S_Arr, Index, 0);
      end loop;

    end Sort;

    -- Size of the heap = size of the array / size of one element
    Heap_Size : constant Natural := Sort_Array'Size / Sort_Array (0)'Size;

  begin

    Sort (Arr, Heap_Size);
    return Arr;

  end Heap_Sort;

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
    Random_Array : Sortable_Array := Sortable_Array_Pkg.Generate_Random_Array;

    -- Array of Test Results
    Test_Count : constant Natural := 6;

    type Test_Result is record
      Test_Name : String (1 .. 15);
      Run_Time  : Duration := 0.0;
      Output    : Sortable_Array (Random_Array'Range);
    end record;

    Test_Results : array (1 .. Test_Count) of Test_Result;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Execute a single experiment run.
    --
    function Test_Run (Name : in String;
                       Sort : in Sort_Function)
                       return Test_Result is

      Arr : Sortable_Array := Random_Array;

      Start : Ada.Calendar.Time;
      Span  : Duration;

      use type Ada.Calendar.Time;

    begin

      -- Time the given sort algorithm's execution
      Start := Ada.Calendar.Clock;
      Arr   := Sort.all (Random_Array);
      Span  := Ada.Calendar.Clock - Start;

      return (Test_Name => Name,
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
        Ada.Text_IO.Put_Line (Test_Results(Index).Test_Name);
        Sortable_Array_Pkg.Print (Test_Results(Index).Output);
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
        Ada.Text_IO.Put_Line (Test_Results(Index).Test_Name & ": " & Test_Results(Index).Run_Time'Img);
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
        Ada.Text_IO.Put_Line (Test_Results(Index).Test_Name & ": " & Sortable_Array_Pkg.Is_Sorted (Test_Results(Index).Output)'Img);
      end loop;

    end Print_Sort_Check;

  begin

    -- Print the random array
    Separator;
    Ada.Text_IO.Put_Line ("Random Array:");
    Separator;
    Sortable_Array_Pkg.Print (Random_Array);
    Separator;

    -- Test Runs
    Test_Results (1) := Test_Run ("Insertion Sort ", Insertion_Sort'Access);
    Test_Results (2) := Test_Run ("Selection Sort ", Selection_Sort'Access);
    Test_Results (3) := Test_Run ("Bubble Sort    ", Bubble_Sort'Access);
    Test_Results (4) := Test_Run ("Quick Sort     ", Quick_Sort'Access);
    Test_Results (5) := Test_Run ("Merge Sort     ", Merge_Sort'Access);
    Test_Results (6) := Test_Run ("Heap Sort      ", Heap_Sort'Access);

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

end Sort_Pkg;
