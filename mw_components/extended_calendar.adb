-- ADDITIONAL CALENDAR FUNCTIONS
   -----------------------------

-- Creation : 19-JAN-1987 by Mats Weber.


with Exponentiation_Functions;

package body Extended_Calendar is
------------------------------

   -- Time zero is 1-JAN-0001 at 00:00

   Days_Since_1_Jan : array (Month_Number) of Day_Interval range 0..365;


   function Is_Leap_Year (Year : Year_Number) return Boolean is
   begin
      return Year mod 400 = 0 or (Year mod 100 /= 0 and Year mod 4 = 0);
   end Is_Leap_Year;


   function Duration_Since_1_Jan_1 (Date : Time) return Long_Duration is

      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Seconds     : Day_Duration;

      Number_Of_Days : Natural_Day_Interval;

   begin
      Split(Date    => Date,
            Year    => Year,
            Month   => Month,
            Day     => Day,
            Seconds => Seconds);
      Number_Of_Days := 365 * Day_Interval(Year - 1) +
                        Day_Interval(Year - 1) / 4 -
                        Day_Interval(Year - 1) / 100 +
                        Day_Interval(Year - 1) / 400 +
                        Days_Since_1_Jan(Month) +
                        Day_Interval(Day) - 1;
      if Month > 2 and Is_Leap_Year(Year) then
         Number_Of_Days := Number_Of_Days + 1;
      end if;
      return To_Long_Duration(Days => Number_Of_Days, Seconds => Seconds);
   end Duration_Since_1_Jan_1;


   function Time_Of (Duration_Since_1_Jan_1 : Long_Duration) return Time is

      Lower_Year     : Year_Number := Year_Number'First;
      Upper_Year     : Year_Number := Year_Number'Last;
      Middle_Year    : Year_Number;

      Days_Remaining : Natural_Day_Interval;
      Leap_Year      : Boolean;
      The_Month      : Month_Number;

   begin
      while Upper_Year - Lower_Year > 1 loop
         Middle_Year := (Lower_Year + Upper_Year) / 2;
         if Duration_Since_1_Jan_1 >=
            Extended_Calendar.Duration_Since_1_Jan_1(Time_Of(Year    => Middle_Year,
                                                             Month   => 1,
                                                             Day     => 1,
                                                             Seconds => 0.0))
         then
            Lower_Year := Middle_Year;
         else
            Upper_Year := Middle_Year;
         end if;
      end loop;
      Days_Remaining := Days(Duration_Since_1_Jan_1 -
                             Extended_Calendar.Duration_Since_1_Jan_1
                                (Time_Of(Year    => Lower_Year,
                                         Month   => 1,
                                         Day     => 1,
                                         Seconds => 0.0)));
      Leap_Year := Is_Leap_Year(Lower_Year);
      for Month in reverse Month_Number loop
         if Leap_Year and Month > 2 then
            if Days_Remaining >= Days_Since_1_Jan(Month) + 1 then
               The_Month      := Month;
               Days_Remaining := Days_Remaining - (Days_Since_1_Jan(Month) + 1);
               exit;
            end if;
         else
            if Days_Remaining >= Days_Since_1_Jan(Month) then
               The_Month      := Month;
               Days_Remaining := Days_Remaining - Days_Since_1_Jan(Month);
               exit;
            end if;
         end if;
      end loop;
      return Time_Of(Year    => Lower_Year,
                     Month   => The_Month,
                     Day     => Day_Number(Days_Remaining + 1),
                     Seconds => Seconds(Duration_Since_1_Jan_1));
   end Time_Of;


   function To_Long_Duration (Days    : Natural_Day_Interval;
                              Seconds : Day_Duration;
                              Sign    : Duration_Sign := '+') return Long_Duration is
   begin
      if Days = 0 and Seconds = 0.0 then
         return Zero;
      elsif Seconds = Day_Duration'Last then
         return (Days    => Days + 1,
                 Seconds => 0.0,
                 Sign    => Sign);
      else
         return (Days    => Days,
                 Seconds => Seconds,
                 Sign    => Sign);
      end if;
   end To_Long_Duration;

   function To_Long_Duration (Seconds : Day_Duration;
                              Sign    : Duration_Sign := '+') return Long_Duration is
   begin
      if Seconds = 0.0 then
         return Zero;
      elsif Seconds = Day_Duration'Last then
         return (Days    => 1,
                 Seconds => 0.0,
                 Sign    => Sign);
      else
         return (Days    => 0,
                 Seconds => Seconds,
                 Sign    => Sign);
      end if;
   end To_Long_Duration;

   function To_Long_Duration (Days : Natural_Day_Interval;
                              Sign : Duration_Sign := '+') return Long_Duration is
   begin
      if Days = 0 then
         return Zero;
      else
         return (Days    => Days,
                 Seconds => 0.0,
                 Sign    => Sign);
      end if;
   end To_Long_Duration;


   function Days (Delta_Time : Long_Duration) return Natural_Day_Interval is
   begin
      return Delta_Time.Days;
   end Days;

   function Seconds (Delta_Time : Long_Duration) return Day_Duration is
   begin
      return Delta_Time.Seconds;
   end Seconds;

   function Sign (Delta_Time : Long_Duration) return Duration_Sign is
   begin
      return Delta_Time.Sign;
   end Sign;


   function To_Duration (Delta_Time : Long_Duration) return Duration is

      Result : Duration;

   begin
      Result := Natural(Delta_Time.Days) * Day_Duration'Last + Delta_Time.Seconds;
      case Delta_Time.Sign is
         when '+' =>
            return Result;
         when '-' =>
            return -Result;
      end case;
   end To_Duration;


   function Zero return Long_Duration is
   begin
      return (Days => 0, Seconds => 0.0, Sign => '+');
   end Zero;


   function "+" (A : Long_Duration) return Long_Duration is
   begin
      return A;
   end "+";

   function "-" (A : Long_Duration) return Long_Duration is
   begin
      case A.Sign is
         when '+' =>
            return To_Long_Duration(Days    => A.Days,
                                    Seconds => A.Seconds,
                                    Sign    => '-');
         when '-' =>
            return To_Long_Duration(Days    => A.Days,
                                    Seconds => A.Seconds,
                                    Sign    => '+');
      end case;
   end "-";


   function "abs" (A : Long_Duration) return Long_Duration is
   begin
      case A.Sign is
         when '+' =>
            return A;
         when '-' =>
            return -A;
      end case;
   end "abs";


   function "+" (A, B : Long_Duration) return Long_Duration is
   begin
      if A.Sign = B.Sign then
         begin
            return To_Long_Duration(Days    => A.Days + B.Days,
                                    Seconds => A.Seconds + B.Seconds,
                                    Sign    => A.Sign);
         exception
            when Constraint_Error | Numeric_Error =>
               return To_Long_Duration(Days    => A.Days + B.Days + 1,
                                       Seconds => (A.Seconds - Day_Duration'Last) + B.Seconds,
                                       Sign    => A.Sign);
         end;
      else
         return A - (-B);
      end if;
   end "+";

   function "-" (A, B : Long_Duration) return Long_Duration is
   begin
      if A.Sign = B.Sign then
         if abs A >= abs B then
            begin
               return To_Long_Duration(Days    => A.Days - B.Days,
                                       Seconds => A.Seconds - B.Seconds,
                                       Sign    => A.Sign);
            exception
               when Constraint_Error | Numeric_Error =>
                  return To_Long_Duration(Days    => A.Days - B.Days - 1,
                                          Seconds => A.Seconds + (Day_Duration'Last - B.Seconds),
                                          Sign    => A.Sign);
            end;
         else
            return -(B - A);
         end if;
      else
         return A + (-B);
      end if;
   end "-";


   function "*" (N : Integer; A : Long_Duration) return Long_Duration is

      function Product is
         new Exponentiation_Functions.Exponentiation(Number   => Long_Duration,
                                                     One      => Zero,
                                                     Exponent => Integer,
                                                     "*"      => "+",
                                                     Inverse  => "-");

   begin
      return Product(X => A, N => N);
   end "*";

   function "*" (A : Long_Duration; N : Integer) return Long_Duration is
   begin
      return N * A;
   end "*";

   function "/" (A : Long_Duration; N : Integer) return Long_Duration is
   begin
      if N >= 0 then
         return To_Long_Duration(Days    => A.Days / Day_Interval(N),
                                 Seconds => Integer(A.Days mod Day_Interval(N)) *
                                            (Day_Duration'Last / N) +
                                            A.Seconds / N,
                                 Sign    => A.Sign);
      else
         return (-A) / (-N);
      end if;
   end "/";


   function "<" (A, B : Long_Duration) return Boolean is
   begin
      case A.Sign is
         when '+' =>
            case B.Sign is
               when '+' =>
                  if A.Days /= B.Days then
                     return A.Days < B.Days;
                  else
                     return A.Seconds < B.Seconds;
                  end if;
               when '-' =>
                  return False;
            end case;
         when '-' =>
            case B.Sign is
               when '+' =>
                  return True;
               when '-' =>
                  if A.Days /= B.Days then
                     return A.Days > B.Days;
                  else
                     return A.Seconds > B.Seconds;
                  end if;
            end case;
      end case;
   end "<";

   function "<=" (A, B : Long_Duration) return Boolean is
   begin
      case A.Sign is
         when '+' =>
            case B.Sign is
               when '+' =>
                  if A.Days /= B.Days then
                     return A.Days < B.Days;
                  else
                     return A.Seconds <= B.Seconds;
                  end if;
               when '-' =>
                  return False;
            end case;
         when '-' =>
            case B.Sign is
               when '+' =>
                  return True;
               when '-' =>
                  if A.Days /= B.Days then
                     return A.Days > B.Days;
                  else
                     return A.Seconds >= B.Seconds;
                  end if;
            end case;
      end case;
   end "<=";

   function ">" (A, B : Long_Duration) return Boolean is
   begin
      return B < A;
   end ">";

   function ">=" (A, B : Long_Duration) return Boolean is
   begin
      return B <= A;
   end ">=";


   function "+" (T : Time; A : Long_Duration) return Time is
   begin
      return Time_Of(Duration_Since_1_Jan_1(Date => T) + A);
   exception
      when Constraint_Error | Numeric_Error =>
         raise Time_Error;
   end "+";

   function "+" (A : Long_Duration; T : Time) return Time is
   begin
      return T + A;
   exception
      when Constraint_Error | Numeric_Error =>
         raise Time_Error;
   end "+";

   function "-" (T : Time; A : Long_Duration) return Time is
   begin
      return Time_Of(Duration_Since_1_Jan_1(Date => T) - A);
   exception
      when Constraint_Error | Numeric_Error =>
         raise Time_Error;
   end "-";

   function "-" (T1, T2 : Time) return Long_Duration is
   begin
      return Duration_Since_1_Jan_1(Date => T1) - Duration_Since_1_Jan_1(Date => T2);
   exception
      when Constraint_Error | Numeric_Error =>
         raise Time_Error;
   end "-";


   function Succ (Day : Week_Day) return Week_Day is
   begin
      if Day = Week_Day'Last then
         return Week_Day'First;
      else
         return Week_Day'Succ(Day);
      end if;
   end Succ;

   function Pred (Day : Week_Day) return Week_Day is
   begin
      if Day = Week_Day'First then
         return Week_Day'Last;
      else
         return Week_Day'Pred(Day);
      end if;
   end Pred;


   function Day_Of_Week (Date : Time := Clock) return Week_Day is

      Day_Of_Week_1_Jan_1 : constant Week_Day := Monday;

   begin
      return Week_Day'Val((Week_Day'Pos(Day_Of_Week_1_Jan_1) +
                           Duration_Since_1_Jan_1(Date).Days) mod
                          (Week_Day'Pos(Week_Day'Last) + 1));
   end Day_Of_Week;


   function Day_Of_Week (Year  : Year_Number;
                         Month : Month_Number;
                         Day   : Day_Number) return Week_Day is
   begin
      return Day_Of_Week(Time_Of(Year    => Year,
                                 Month   => Month,
                                 Day     => Day,
                                 Seconds => 0.0));
   end Day_Of_Week;


begin
   declare

      Days_In_Month : constant array (Month_Number) of Day_Interval range 28..31 :=
         (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

   begin
      Days_Since_1_Jan(1) := 0;
      for Month in Month_Number range 2..Month_Number'Last loop
         Days_Since_1_Jan(Month) := Days_Since_1_Jan(Month - 1) + Days_In_Month(Month - 1);
      end loop;
   end;
end Extended_Calendar;
