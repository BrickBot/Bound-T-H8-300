-- ADDITIONAL CALENDAR FUNCTIONS
   -----------------------------

-- Revision : 18-AUG-1988 by Mats Weber, made type LONG_DURATION private to avoid sign problems.

-- Creation : 19-JAN-1987 by Mats Weber.


with Calendar;

use Calendar;

package Extended_Calendar is
-------------------------

   type Long_Duration is private;

   type Day_Interval is range -366 * Year_Number'Last..366 * Year_Number'Last;

   subtype Natural_Day_Interval is Day_Interval range 0..Day_Interval'Last;

   type Duration_Sign is ('+', '-');


   function To_Long_Duration (Days    : Natural_Day_Interval;
                              Seconds : Day_Duration;
                              Sign    : Duration_Sign := '+') return Long_Duration;

   function To_Long_Duration (Seconds : Day_Duration;
                              Sign    : Duration_Sign := '+') return Long_Duration;

   function To_Long_Duration (Days    : Natural_Day_Interval;
                              Sign    : Duration_Sign := '+') return Long_Duration;

   function Days    (Delta_Time : Long_Duration) return Natural_Day_Interval;
   function Seconds (Delta_Time : Long_Duration) return Day_Duration;
   function Sign    (Delta_Time : Long_Duration) return Duration_Sign;

   function To_Duration (Delta_Time : Long_Duration) return Duration;

   function Zero return Long_Duration;


   function "+" (A : Long_Duration) return Long_Duration;
   function "-" (A : Long_Duration) return Long_Duration;

   function "abs" (A : Long_Duration) return Long_Duration;

   function "+" (A, B : Long_Duration) return Long_Duration;
   function "-" (A, B : Long_Duration) return Long_Duration;

   function "*" (N : Integer; A : Long_Duration) return Long_Duration;
   function "*" (A : Long_Duration; N : Integer) return Long_Duration;
   function "/" (A : Long_Duration; N : Integer) return Long_Duration;

   function "<"  (A, B : Long_Duration) return Boolean;
   function "<=" (A, B : Long_Duration) return Boolean;
   function ">"  (A, B : Long_Duration) return Boolean;
   function ">=" (A, B : Long_Duration) return Boolean;

   function "+" (T : Time; A : Long_Duration) return Time;
   function "+" (A : Long_Duration; T : Time) return Time;
   function "-" (T : Time; A : Long_Duration) return Time;
   function "-" (T1, T2 : Time) return Long_Duration;
      -- Will raise TIME_ERROR if no valid time value can be returned.


   type Week_Day is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

   function Succ (Day : Week_Day) return Week_Day;
   function Pred (Day : Week_Day) return Week_Day;

   function Day_Of_Week (Date : Time := Clock) return Week_Day;

   function Day_Of_Week (Year  : Year_Number;
                         Month : Month_Number;
                         Day   : Day_Number) return Week_Day;

private

   type Long_Duration is
      record
         Days    : Natural_Day_Interval;
         Seconds : Day_Duration;
         Sign    : Duration_Sign;
      end record;

end Extended_Calendar;
