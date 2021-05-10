with Ada.Calendar;
with Ada.Real_Time;

package Real_Time_IO is

  function Since_Start return Ada.Real_Time.Time_Span;

  function Since_Start return Duration;

  function Since_Start return String;

  procedure Time_Span_Start;

  procedure Time_Span_End;

  function Current_Span return Ada.Real_Time.Time_Span;

  function Current_Span return Duration;

  function Current_Span return String;

private
  Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of(1970, 01, 01);

  Span_Start : Ada.Calendar.Time := Epoch;
  Span_End   : Ada.Calendar.Time := Epoch;
end Real_Time_IO;
