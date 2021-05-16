with Ada.Calendar;
with Ada.Real_Time;

use Ada.Real_Time;

package body Real_Time_IO is

  ------------------------------------------------------------------------------
  --
  function Since_Start return Time_Span is
    (Ada.Calendar.Clock - Epoch);

  ------------------------------------------------------------------------------
  --
  function Since_Start return Duration is
    (To_Duration (Since_Start));

  ------------------------------------------------------------------------------
  --
  function Since_Start return String is
    (Duration'Image (Since_Start));

  ------------------------------------------------------------------------------
  --
  procedure Time_Span_Start is
  begin
    Span_Start := Ada.Calendar.Clock;
  end Time_Span_Start;

  ------------------------------------------------------------------------------
  --
  procedure Time_Span_End is
  begin
    Span_End := Ada.Calendar.Clock;
  end Time_Span_End;

  ------------------------------------------------------------------------------
  --
  function Current_Span return Time_Span is
    (Span_End - Span_Start);

  ------------------------------------------------------------------------------
  --
  function Current_Span return Duration is
    (To_Duration (Current_Span));

  ------------------------------------------------------------------------------
  --
  function Current_Span return String is
    (Duration'Image (Current_Span));

end Real_Time_IO;
