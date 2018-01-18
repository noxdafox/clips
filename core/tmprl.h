
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  27/06/17            */
   /*                                                     */
   /*             Regular Expression Functions            */
   /*******************************************************/

/*************************************************************/
/* Purpose: provides regular expression functionality.       */
/*    Regular expressions are compiled and cached for better */
/*    performance.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Matteo Cafasso                                       */
/*                                                           */
/*************************************************************/


#include "clips.h"


#define TIME_DATA 65

#define CLOCK_TIME 0
#define REAL_TIME 1
#define MONO_TIME 2
#define USER_TIME 4

struct timeData
  {
   int ClockType;
   double UserTime;
  };

#define TimeData(theEnv)                                        \
    ((struct timeData *) GetEnvironmentData(theEnv, TIME_DATA))

   LOCALE void TimeFunctionDefinitions(void *);
   LOCALE int  EnvGetClockType(void *);
   LOCALE int  EnvSetClockType(void *, int);
   LOCALE void EnvSetTime(void *, double);
   LOCALE void *ClockTypeFunction(void *);
   LOCALE void SetTimeFunction(void *);
LOCALE double EnvTime(void *);
