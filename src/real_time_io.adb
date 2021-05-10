with Ada.Calendar;
with Ada.Real_Time;

package body Real_Time_IO is

  function Since_Start return Ada.Real_Time.Time_Span is
  begin
    return Ada.Calendar.Clock - Epoch;
  end Since_Start;

  function Since_Start return Duration is
  begin
    return Ada.Real_Time.To_Duration (Since_Start);
  end Since_Start;

  function Since_Start return String is
  begin
    return Duration'Image (Since_Start);
  end Since_Start;

  procedure Time_Span_Start is
  begin
    Span_Start := Ada.Calendar.Clock;
  end Time_Span_Start;

  procedure Time_Span_End is
  begin
    Span_End := Ada.Calendar.Clock;
  end Time_Span_End;

  function Current_Span return Ada.Real_Time.Time_Span is
  begin
    return Span_End - Span_Start;
  end Current_Span;

  function Current_Span return Duration is
  begin
    return Ada.Real_Time.To_Duration (Current_Span);
  end Current_Span;

  function Current_Span return String is
  begin
    return Duration'Image (Current_Span);
  end Current_Span;


end Real_Time_IO;
