with Ada.Characters.Latin_1;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Sortable_Array is

  package Asu renames Ada.Strings.Unbounded;
  package Random is new Ada.Numerics.Discrete_Random (Value);

  use type Asu.Unbounded_String;

  -----------------------------------------------------------------------------
  --
  -- Description:
  --   Generates a random Sortable_Array.
  --
  function Generate_Random_Array (Array_Size : in Natural := Size) return Sortable_Array is

    Random_Array : Sortable_Array (0 .. Size) := (others => 0);

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Generate a single random Value.
    --
    function Generate_Random_Number return Value is

      Seed         : Random.Generator;
      Random_Value : Value := 0;

    begin

      Random.Reset (Seed);
      Random_Value := Random.Random(Seed);

      return Random_Value;

    end Generate_Random_Number;

  begin

    -- Loop over each array element and generate a random number in the Sortable_Array_Range.
    for X in Random_Array'Range loop
      Random_Array (X) := Generate_Random_Number;
    end loop;

    return Random_Array;

  end Generate_Random_Array;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Swap the values at the given indices in the Sortable_Array.
  --
  procedure Swap (Object : in out Sortable_Array;
                  Left   : in Integer;
                  Right  : in Integer) is
    Temp : Natural := Object (Left);
  begin
    Object (Left)  := Object (Right);
    Object (Right) := Temp;
  end Swap;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Return a boolean indicating whether the given array is sorted.
  --
  function Is_Sorted (Object : in Sortable_Array) return Boolean is
  begin

    for Index in Object'First .. Object'Last - 1 loop

      if Object (Index) > Object (Index + 1) then
        return False;
      end if;

    end loop;

    return True;

  end Is_Sorted;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Print the Sortable_Array line-by-line.
  --
  procedure Print_By_Line (Object : in Sortable_Array) is
  begin
    for X in Object'Range loop
      Ada.Text_IO.Put_Line (X'Img & " =>" & Object(X)'Img);
    end loop;
  end Print_By_Line;

  ------------------------------------------------------------------------------
  --
  -- Description:
  --   Print the Sortable_Array in rows of a constant size.
  --
  procedure Print_Condensed (Object : in Sortable_Array) is

    Print_String      : Asu.Unbounded_String := Asu.To_Unbounded_String("");
    New_Line          : constant Character   := Character'Val(13);

    Elements_Per_Line : constant Natural := 25;
    Elements_Listed   : Natural := 0;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Return the number of digits in the given value.
    --
    function Count_Digits (Value : in Natural) return Natural is
      Base       : Natural := 10;
      Num_Digits : Natural := 1;
    begin

      while Value mod Base /= Value loop

        Base       := Base * 10;
        Num_Digits := Num_Digits + 1;

      end loop;

      return Num_Digits;

    end Count_Digits;

    ----------------------------------------------------------------------------
    --
    -- Description:
    --   Return a fixed-width string of the given Value.
    --
    function Format_Value (Value : in Natural) return String is

      Max_Digits  : constant Natural := Count_Digits (Max);

    begin

      return Ada.Strings.Fixed.Tail
        (Source => Ada.Strings.Fixed.Trim (Source => Value'Img,
                                           Side   => Ada.Strings.Both),
         Count  => Max_Digits,
         Pad    => '0');

    end Format_Value;

  begin

    for X in Object'Range loop

      if Elements_Listed mod Elements_Per_Line = 0 then
        Ada.Text_IO.Put_Line (Asu.To_String (Print_String));
        Print_String := Asu.To_Unbounded_String("");
      else
        Print_String := Print_String & " " & Format_Value (Object(X));
      end if;

      Elements_Listed := Elements_Listed + 1;

    end loop;

    Ada.Text_IO.Put_Line (Asu.To_String (Print_String));

  end Print_Condensed;

  -------------------------------------------------------------------------------
  --
  -- Description:
  --   Print either line-by-line or condensed according to the given print type.
  --
  procedure Print (Object : in Sortable_Array;
                   Format : in Print_Format_Type := Condensed) is
  begin

    case Format is

      when Line_By_Line =>
        Print_By_Line (Object);

      when Condensed =>
        Print_Condensed (Object);

    end case;

  end Print;

end Sortable_Array;
