/* The time zone that Ada.Calendar asks for (#497). Rose has no notion of
   time zones yet, so every time is UTC and the offset is 0. */

void
__gnat_localtime_tzoff (const void *timer, const int *is_historic, long *off)
{
  (void) timer;
  (void) is_historic;
  *off = 0;
}

/* The value that Ada.Calendar.Time_Zones.Local_Time_Offset checks for, to
   tell that the offset couldn't be found (adaint.c). It never is here. */

const int __gnat_invalid_tzoff = 259200;

/* The binder sets this when the program asks for leap seconds, which Rose
   doesn't count (ADR 0020). */

int __gl_leap_seconds_support = 0;
