/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.30  01/12/15            */
/*                                                     */
/*                    Time Functions                   */
/*******************************************************/

/*************************************************************/
/* Purpose: Time wise functions for CLIPS                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Matteo Cafasso                                       */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#include "clips.h"
#include "tmprl.h"

#include <math.h>
#include <time.h>

#if UNIX_V || LINUX || DARWIN
#include <unistd.h>
#include <sys/time.h>
#include <sys/times.h>
#elif UNIX_7 || WIN_GCC || WIN_MVC
#include <sys/types.h>
#include <sys/timeb.h>
#elif MAC_XCD
#include <Carbon/Carbon.h>
#define TWO_POWER_32 4294967296.0
#endif

#define SEC_TO_MIN 60
#define SEC_TO_HOUR 3600
#define SEC_TO_DAY 86400
#define USEC_TO_SEC 1000000.0
#define NSEC_TO_SEC 1000000000.0

static const char *GetClockName(int);
#if defined(_POSIX_TIMERS) && (_POSIX_TIMERS > 0)
static double gettime(clockid_t);
#elif UNIX_7
static double timeofday();
#elif MAC_XCD
static double microseconds();
#endif

globle void TimeFunctionDefinitions(
  void *theEnv)
  {
   AllocateEnvironmentData(theEnv,TIME_DATA,sizeof(SYMBOL_HN),NULL);
#if defined(_POSIX_MONOTONIC_CLOCK)
   TimeData(theEnv)->ClockType = MONO_TIME;
#else
   TimeData(theEnv)->ClockType = REAL_TIME;
#endif
   TimeData(theEnv)->UserTime = 0;

#if ! RUN_TIME
   EnvDefineFunction2(theEnv,"clock-type",'w',
                      PTIEF ClockTypeFunction,"ClockTypeFunction","01w");
   EnvDefineFunction2(theEnv,"set-time",'v',
                      PTIEF SetTimeFunction,"SetTimeFunction","11d");
#endif
  }

/*****************************************/
/* ClockTypeFunction: H/L access routine */
/*   for the clock-type function.        */
/*****************************************/
globle void *ClockTypeFunction(
  void *theEnv)
  {
   DATA_OBJECT theValue;
   const char *argument;
   int argCount, oldType;

   /*========================================*/
   /* The clock-type function accepts either */
   /* zero or one argument.                  */
   /*========================================*/

   argCount = EnvRtnArgCount(theEnv);

   if ((argCount != 0) && (argCount != 1))
     {
      PrintErrorID(theEnv,"TMPRL",1,FALSE);
      EnvPrintRouter(
          theEnv,WERROR,
          "Function clock-type expected either 0 or 1 argument\n");
     }

   /*==================================================*/
   /* If no argument was given, return the clock type. */
   /*==================================================*/

   if (argCount == 0)
     {
      oldType = EnvGetClockType(theEnv);
      return (SYMBOL_HN *) EnvAddSymbol(theEnv,GetClockName(oldType));
     }

   /*================================*/
   /* Ensure the argument is SYMBOL. */
   /*================================*/

   if (EnvArgTypeCheck(theEnv, "clock-type", 1, SYMBOL, &theValue) == FALSE)
     {
      PrintErrorID(theEnv,"TMPRL",1,FALSE);
      EnvPrintRouter(
          theEnv,WERROR,
          "Function clock-type expected argument #1 to be SYMBOL\n");
     }

   argument = DOToString(theValue);

   /*=========================================*/
   /* Set clock type and return previous one. */
   /*=========================================*/

   if (strcmp(argument,"clock-time") == 0)
     oldType = EnvSetClockType(theEnv, CLOCK_TIME);
   else if (strcmp(argument,"mono-time") == 0)
     {
#if defined(_POSIX_MONOTONIC_CLOCK)
     oldType = EnvSetClockType(theEnv, MONO_TIME);
#else
     oldType = EnvGetClockType(theEnv);
     PrintErrorID(theEnv,"TMPRL",1,FALSE);
     EnvPrintRouter(
         theEnv,WERROR,
         "Function clock-type argument mono-time not supported\n");
#endif
     }
   else if (strcmp(argument,"real-time") == 0)
     oldType = EnvSetClockType(theEnv, REAL_TIME);
   else if (strcmp(argument,"user-time") == 0)
     oldType = EnvSetClockType(theEnv, USER_TIME);
   else
     {
      ExpectedTypeError1(theEnv,"clock-type",1,
      "symbol with value mono-time, real-time or user-time");
      oldType = EnvGetClockType(theEnv);
     }

   return (SYMBOL_HN *) EnvAddSymbol(theEnv,GetClockName(oldType));
  }

/**********************************************************/
/* GetClockName: Given the integer value corresponding    */
/*   to a specified clock type, return a character string */
/*   of the clock type's name.                            */
/**********************************************************/
static const char *GetClockName(
  int clock_type)
  {
   const char *cname;

   switch (clock_type)
     {
      case CLOCK_TIME:
        cname = "clock-time";
        break;
      case MONO_TIME:
        cname = "mono-time";
        break;
      case REAL_TIME:
        cname = "real-time";
        break;
      case USER_TIME:
        cname = "user-time";
        break;
      default:
        cname = "unknown";
        break;
     }

   return cname;
  }

/***************************************/
/* SetTimeFunction: H/L access routine */
/*   for the set-time function.        */
/***************************************/
globle void SetTimeFunction(
  void *theEnv)
  {
   DATA_OBJECT theValue;

   /*==============================================*/
   /* The set-time function requires one argument. */
   /*==============================================*/

   if (EnvArgCountCheck(theEnv,"set-time",EXACTLY,1) == -1)
     return;

   if (EnvArgTypeCheck(theEnv,"set-time",1,FLOAT,&theValue) == FALSE)
     return;

   EnvSetTime(theEnv,DOToDouble(theValue));
  }

/*************************************/
/* EnvGetClockType: C access routine */
/*   for the clock-type command.     */
/*************************************/
globle int EnvGetClockType(
  void *theEnv)
  {
   return TimeData(theEnv)->ClockType;
  }

/*************************************/
/* EnvGetClockType: C access routine */
/*   for the clock-type command.     */
/*************************************/
globle int EnvSetClockType(
  void *theEnv, int value)
  {
   int type = EnvGetClockType(theEnv);

   TimeData(theEnv)->ClockType = value;

   return type;
  }

/********************************/
/* EnvSetTime: C access routine */
/*   for the set-time command.  */
/********************************/
globle void EnvSetTime(
  void *theEnv, double value)
  {
   TimeData(theEnv)->UserTime = value;
  }

/*****************************/
/* EnvTime: C access routine */
/*   for the time command.   */
/*****************************/
globle double EnvTime(
  void *theEnv)
  {
   switch(EnvGetClockType(theEnv))
     {
       case USER_TIME:
         return TimeData(theEnv)->UserTime;
       case CLOCK_TIME:
         return((double) clock() / (double) CLOCKS_PER_SEC);
#if defined(_POSIX_TIMERS) && (_POSIX_TIMERS > 0)
       case MONO_TIME:
         return gettime(CLOCK_MONOTONIC);
       case REAL_TIME:
         return gettime(CLOCK_REALTIME);
#elif UNIX_7
         return timeofday();
#elif MAC_XCD
         return microseconds();
#endif
       default:
         return((double) clock() / (double) CLOCKS_PER_SEC);
     }
  }

#if defined(_POSIX_TIMERS) && (_POSIX_TIMERS > 0)
static double gettime(
  clockid_t clock_type)
  {
   struct timespec now;
   clock_gettime(clock_type, &now);

   return (now.tv_nsec / NSEC_TO_SEC) + now.tv_sec;
  }

#elif UNIX_7
static double timeofday()
  {
   struct timeval now;
   gettimeofday(&now, NULL);

   return (now.tv_usec / USEC_TO_SEC) + now.tv_sec;
  }

#elif MAC_XCD
static double microseconds()
  {
   UnsignedWide result;
   Microseconds(&result);

   return(((((double) result.hi) * TWO_POWER_32) + result.lo) / USEC_TO_SEC);
 }
#endif
