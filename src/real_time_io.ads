with Ada.Calendar;
with Ada.Real_Time;

--  @summary
--  Routines used to measure durations.
--
--  @description
--  This package defines routines that can be used to measure time periods.
--
package Real_Time_IO is


  --  Return the time span start as a time span.
  --
  --  @return  the time since start as a time span
  --
  function Since_Start return Ada.Real_Time.Time_Span;


  --  Return the time since start as a duration.
  --
  --  @return  the time since start as a duration.
  --
  function Since_Start return Duration;


  --  Return the time since start as a string.
  --
  --  @return  the time since start as a string
  --
  function Since_Start return String;


  --  Set the time span start time to the current time.
  --
  procedure Time_Span_Start;


  --  Set the time span end time to the current time.
  --
  procedure Time_Span_End;


  --  Return the current span time as a time span.
  --
  --  @return  the current span time as a time span.
  --
  function Current_Span return Ada.Real_Time.Time_Span;


  --  Return the current span time as a duration.
  --
  --  @return  the current span time as a duration.
  --
  function Current_Span return Duration;

  --  Return the current span time as a string.
  --
  --  @return  the current span time as a string.
  --
  function Current_Span return String;


private

  --  The epoch start time begins on January 1st, 1970.
  Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1970, 01, 01);

  --  The span start and end times. Both default to the epoch start.
  Span_Start : Ada.Calendar.Time := Epoch;
  Span_End   : Ada.Calendar.Time := Epoch;

end Real_Time_IO;
