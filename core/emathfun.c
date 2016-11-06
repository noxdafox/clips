   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/01/16             */
   /*                                                     */
   /*            EXTENDED MATH FUNCTIONS MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous extended math     */
/*   functions including cos, sin, tan, sec, csc, cot, acos, */
/*   asin, atan, asec, acsc, acot, cosh, sinh, tanh, sech,   */
/*   csch, coth, acosh, asinh, atanh, asech, acsch, acoth,   */
/*   mod, exp, log, log10, sqrt, pi, deg-rad, rad-deg,       */
/*   deg-grad, grad-deg, **, and round.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Renamed EX_MATH compiler flag to               */
/*            EXTENDED_MATH_FUNCTIONS.                       */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include "setup.h"
#include "argacces.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "prntutil.h"
#include "router.h"

#include "emathfun.h"

#if EXTENDED_MATH_FUNCTIONS

#include <math.h>

/***************/
/* DEFINITIONS */
/***************/

#ifndef PI
#define PI   3.14159265358979323846
#endif

#ifndef PID2
#define PID2 1.57079632679489661923 /* PI divided by 2 */
#endif

#define SMALLEST_ALLOWED_NUMBER 1e-15
#define dtrunc(x) (((x) < 0.0) ? ceil(x) : floor(x))

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static bool                    SingleNumberCheck(UDFContext *,UDFValue *);
   static bool                    TestProximity(double,double);
   static void                    DomainErrorMessage(UDFContext *,UDFValue *);
   static void                    ArgumentOverflowErrorMessage(UDFContext *,UDFValue *);
   static void                    SingularityErrorMessage(UDFContext *,UDFValue *);
   static double                  genacosh(double);
   static double                  genasinh(double);
   static double                  genatanh(double);
   static double                  genasech(double);
   static double                  genacsch(double);
   static double                  genacoth(double);

/************************************************/
/* ExtendedMathFunctionDefinitions: Initializes */
/*   the extended math functions.               */
/************************************************/
void ExtendedMathFunctionDefinitions(
  Environment *theEnv)
  {
#if ! RUN_TIME
   EnvAddUDF(theEnv,"cos","d",1,1,"ld",CosFunction,"CosFunction",NULL);
   EnvAddUDF(theEnv,"sin","d",1,1,"ld",SinFunction,"SinFunction",NULL);
   EnvAddUDF(theEnv,"tan","d",1,1,"ld",TanFunction,"TanFunction",NULL);
   EnvAddUDF(theEnv,"sec","d",1,1,"ld",SecFunction,"SecFunction",NULL);
   EnvAddUDF(theEnv,"csc","d",1,1,"ld",CscFunction,"CscFunction",NULL);
   EnvAddUDF(theEnv,"cot","d",1,1,"ld",CotFunction,"CotFunction",NULL);
   EnvAddUDF(theEnv,"acos","d",1,1,"ld",AcosFunction,"AcosFunction",NULL);
   EnvAddUDF(theEnv,"asin","d",1,1,"ld",AsinFunction,"AsinFunction",NULL);
   EnvAddUDF(theEnv,"atan","d",1,1,"ld",AtanFunction,"AtanFunction",NULL);
   EnvAddUDF(theEnv,"asec","d",1,1,"ld",AsecFunction,"AsecFunction",NULL);
   EnvAddUDF(theEnv,"acsc","d",1,1,"ld",AcscFunction,"AcscFunction",NULL);
   EnvAddUDF(theEnv,"acot","d",1,1,"ld",AcotFunction,"AcotFunction",NULL);
   EnvAddUDF(theEnv,"cosh","d",1,1,"ld",CoshFunction,"CoshFunction",NULL);
   EnvAddUDF(theEnv,"sinh","d",1,1,"ld",SinhFunction,"SinhFunction",NULL);
   EnvAddUDF(theEnv,"tanh","d",1,1,"ld",TanhFunction,"TanhFunction",NULL);
   EnvAddUDF(theEnv,"sech","d",1,1,"ld",SechFunction,"SechFunction",NULL);
   EnvAddUDF(theEnv,"csch","d",1,1,"ld",CschFunction,"CschFunction",NULL);
   EnvAddUDF(theEnv,"coth","d",1,1,"ld",CothFunction,"CothFunction",NULL);
   EnvAddUDF(theEnv,"acosh","d",1,1,"ld",AcoshFunction,"AcoshFunction",NULL);
   EnvAddUDF(theEnv,"asinh","d",1,1,"ld",AsinhFunction,"AsinhFunction",NULL);
   EnvAddUDF(theEnv,"atanh","d",1,1,"ld",AtanhFunction,"AtanhFunction",NULL);
   EnvAddUDF(theEnv,"asech","d",1,1,"ld",AsechFunction,"AsechFunction",NULL);
   EnvAddUDF(theEnv,"acsch","d",1,1,"ld",AcschFunction,"AcschFunction",NULL);
   EnvAddUDF(theEnv,"acoth","d",1,1,"ld",AcothFunction,"AcothFunction",NULL);

   EnvAddUDF(theEnv,"mod","ld",2,2,"ld",ModFunction,"ModFunction",NULL);
   EnvAddUDF(theEnv,"exp","d", 1,1,"ld",ExpFunction,"ExpFunction",NULL);
   EnvAddUDF(theEnv,"log","d",1,1,"ld",LogFunction,"LogFunction",NULL);
   EnvAddUDF(theEnv,"log10","d",1,1,"ld",Log10Function,"Log10Function",NULL);
   EnvAddUDF(theEnv,"sqrt","d",1,1,"ld",SqrtFunction,"SqrtFunction",NULL);
   EnvAddUDF(theEnv,"pi","d",0,0,NULL,PiFunction, "PiFunction",NULL);
   EnvAddUDF(theEnv,"deg-rad","d",1,1,"ld",DegRadFunction, "DegRadFunction",NULL);
   EnvAddUDF(theEnv,"rad-deg","d",1,1,"ld",RadDegFunction, "RadDegFunction",NULL);
   EnvAddUDF(theEnv,"deg-grad","d",1,1,"ld",DegGradFunction,"DegGradFunction",NULL);
   EnvAddUDF(theEnv,"grad-deg","d",1,1,"ld",GradDegFunction,"GradDegFunction",NULL);
   EnvAddUDF(theEnv,"**","d",2,2,"ld",PowFunction,"PowFunction",NULL);
   EnvAddUDF(theEnv,"round","l", 1,1,"ld",RoundFunction,"RoundFunction",NULL);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/************************************************************/
/* SingleNumberCheck: Retrieves the numeric argument for    */
/*   extended math functions which expect a single floating */
/*   point argument.                                        */
/************************************************************/
static bool SingleNumberCheck(
  UDFContext *context,
  UDFValue *returnValue)
  {
   /*======================================*/
   /* Check that the argument is a number. */
   /*======================================*/

   if (! UDFNthArgument(context,1,NUMBER_BITS,returnValue))
     {
      returnValue->floatValue = EnvCreateFloat(context->environment,0.0);
      return false;
     }

   return true;
  }

/**************************************************************/
/* TestProximity: Returns true if the specified number falls  */
/*   within the specified range, otherwise false is returned. */
/**************************************************************/
static bool TestProximity(
  double theNumber,
  double range)
  {
   if ((theNumber >= (- range)) && (theNumber <= range)) return true;
   else return false;
  }

/********************************************************/
/* DomainErrorMessage: Generic error message used when  */
/*   a domain error is detected during a call to one of */
/*   the extended math functions.                       */
/********************************************************/
static void DomainErrorMessage(
  UDFContext *context,
  UDFValue *returnValue)
  {
   Environment *theEnv = context->environment;

   PrintErrorID(theEnv,"EMATHFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Domain error for ");
   EnvPrintRouter(theEnv,WERROR,UDFContextFunctionName(context));
   EnvPrintRouter(theEnv,WERROR," function.\n");
   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);
   returnValue->floatValue = EnvCreateFloat(theEnv,0.0);
  }

/************************************************************/
/* ArgumentOverflowErrorMessage: Generic error message used */
/*   when an argument overflow is detected during a call to */
/*   one of the extended math functions.                    */
/************************************************************/
static void ArgumentOverflowErrorMessage(
  UDFContext *context,
  UDFValue *returnValue)
  {
   Environment *theEnv = context->environment;

   PrintErrorID(theEnv,"EMATHFUN",2,false);
   EnvPrintRouter(theEnv,WERROR,"Argument overflow for ");
   EnvPrintRouter(theEnv,WERROR,UDFContextFunctionName(context));
   EnvPrintRouter(theEnv,WERROR," function.\n");
   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);
   returnValue->floatValue = EnvCreateFloat(theEnv,0.0);
  }

/************************************************************/
/* SingularityErrorMessage: Generic error message used when */
/*   a singularity is detected during a call to one of the  */
/*   extended math functions.                               */
/************************************************************/
static void SingularityErrorMessage(
  UDFContext *context,
  UDFValue *returnValue)
  {
   Environment *theEnv = context->environment;

   PrintErrorID(theEnv,"EMATHFUN",3,false);
   EnvPrintRouter(theEnv,WERROR,"Singularity at asymptote in ");
   EnvPrintRouter(theEnv,WERROR,UDFContextFunctionName(context));
   EnvPrintRouter(theEnv,WERROR," function.\n");
   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);
   returnValue->floatValue = EnvCreateFloat(theEnv,0.0);
  }

/*************************************/
/* CosFunction: H/L access routine   */
/*   for the cos function.           */
/*************************************/
void CosFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,cos(CVCoerceToFloat(returnValue)));
  }

/*************************************/
/* SinFunction: H/L access routine   */
/*   for the sin function.           */
/*************************************/
void SinFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,sin(CVCoerceToFloat(returnValue)));
  }

/*************************************/
/* TanFunction: H/L access routine   */
/*   for the tan function.           */
/*************************************/
void TanFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double tv;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   tv = cos(CVCoerceToFloat(returnValue));
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,sin(CVCoerceToFloat(returnValue)) / tv);
  }

/*************************************/
/* SecFunction: H/L access routine   */
/*   for the sec function.           */
/*************************************/
void SecFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double tv;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   tv = cos(CVCoerceToFloat(returnValue));
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,1.0 / tv);
  }

/*************************************/
/* CscFunction: H/L access routine   */
/*   for the csc function.           */
/*************************************/
void CscFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double tv;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   tv = sin(CVCoerceToFloat(returnValue));
   if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
     {
      SingularityErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,1.0 / tv);
  }

/*************************************/
/* CotFunction: H/L access routine   */
/*   for the cot function.           */
/*************************************/
void CotFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double tv;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

    tv = sin(CVCoerceToFloat(returnValue));
    if ((tv < SMALLEST_ALLOWED_NUMBER) && (tv > -SMALLEST_ALLOWED_NUMBER))
      {
       SingularityErrorMessage(context,returnValue);
       return;
      }

    returnValue->floatValue = EnvCreateFloat(theEnv,cos(CVCoerceToFloat(returnValue)) / tv);
  }

/**************************************/
/* AcosFunction: H/L access routine   */
/*   for the acos function.           */
/**************************************/
void AcosFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);

   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

    returnValue->floatValue = EnvCreateFloat(theEnv,acos(num));
  }

/**************************************/
/* AsinFunction: H/L access routine   */
/*   for the asin function.           */
/**************************************/
void AsinFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if ((num > 1.0) || (num < -1.0))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,asin(num));
  }

/**************************************/
/* AtanFunction: H/L access routine   */
/*   for the atan function.           */
/**************************************/
void AtanFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,atan(CVCoerceToFloat(returnValue)));
  }

/**************************************/
/* AsecFunction: H/L access routine   */
/*   for the asec function.           */
/**************************************/
void AsecFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   num = 1.0 / num;
   returnValue->floatValue = EnvCreateFloat(theEnv,acos(num));
  }

/**************************************/
/* AcscFunction: H/L access routine   */
/*   for the acsc function.           */
/**************************************/
void AcscFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if ((num < 1.0) && (num > -1.0))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   num = 1.0 / num;
   returnValue->floatValue = EnvCreateFloat(theEnv,asin(num));
  }

/**************************************/
/* AcotFunction: H/L access routine   */
/*   for the acot function.           */
/**************************************/
void AcotFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (TestProximity(num,1e-25) == true)
     {
      returnValue->floatValue = EnvCreateFloat(theEnv,PID2);
      return;
     }

   num = 1.0 / num;
   returnValue->floatValue = EnvCreateFloat(theEnv,atan(num));
  }

/**************************************/
/* CoshFunction: H/L access routine   */
/*   for the cosh function.           */
/**************************************/
void CoshFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,cosh(CVCoerceToFloat(returnValue)));
  }

/**************************************/
/* SinhFunction: H/L access routine   */
/*   for the sinh function.           */
/**************************************/
void SinhFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,sinh(CVCoerceToFloat(returnValue)));
  }

/**************************************/
/* TanhFunction: H/L access routine   */
/*   for the tanh function.           */
/**************************************/
void TanhFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,tanh(CVCoerceToFloat(returnValue)));
  }

/**************************************/
/* SechFunction: H/L access routine   */
/*   for the sech function.           */
/**************************************/
void SechFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,1.0 / cosh(CVCoerceToFloat(returnValue)));
  }

/**************************************/
/* CschFunction: H/L access routine   */
/*   for the csch function.           */
/**************************************/
void CschFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (num == 0.0)
     {
      SingularityErrorMessage(context,returnValue);
      return;
     }
   else if (TestProximity(num,1e-25) == true)
     {
      ArgumentOverflowErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,1.0 / sinh(num));
  }

/**************************************/
/* CothFunction: H/L access routine   */
/*   for the coth function.           */
/**************************************/
void CothFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (num == 0.0)
     {
      SingularityErrorMessage(context,returnValue);
      return;
     }
   else if (TestProximity(num,1e-25) == true)
     {
      ArgumentOverflowErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,1.0 / tanh(num));
  }

/***************************************/
/* AcoshFunction: H/L access routine   */
/*   for the acosh function.           */
/***************************************/
void AcoshFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (num < 1.0)
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,genacosh(num));
  }

/***************************************/
/* AsinhFunction: H/L access routine   */
/*   for the asinh function.           */
/***************************************/
void AsinhFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,genasinh(CVCoerceToFloat(returnValue)));
  }

/***************************************/
/* AtanhFunction: H/L access routine   */
/*   for the atanh function.           */
/***************************************/
void AtanhFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if ((num >= 1.0) || (num <= -1.0))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,genatanh(num));
  }

/***************************************/
/* AsechFunction: H/L access routine   */
/*   for the asech function.           */
/***************************************/
void AsechFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if ((num > 1.0) || (num <= 0.0))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,genasech(num));
  }

/***************************************/
/* AcschFunction: H/L access routine   */
/*   for the acsch function.           */
/***************************************/
void AcschFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (num == 0.0)
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,genacsch(num));
  }

/***************************************/
/* AcothFunction: H/L access routine   */
/*   for the acoth function.           */
/***************************************/
void AcothFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if ((num <= 1.0) && (num >= -1.0))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,genacoth(num));
  }

/*************************************/
/* ExpFunction: H/L access routine   */
/*   for the exp function.           */
/*************************************/
void ExpFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,exp(CVCoerceToFloat(returnValue)));
  }

/*************************************/
/* LogFunction: H/L access routine   */
/*   for the log function.           */
/*************************************/
void LogFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (num < 0.0)
     {
      DomainErrorMessage(context,returnValue);
      return;
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,log(num));
  }

/***************************************/
/* Log10Function: H/L access routine   */
/*   for the log10 function.           */
/***************************************/
void Log10Function(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (num < 0.0)
     {
      DomainErrorMessage(context,returnValue);
      return;
     }
   else if (num == 0.0)
     {
      ArgumentOverflowErrorMessage(context,returnValue);
      return;
     }

    returnValue->floatValue = EnvCreateFloat(theEnv,log10(num));
   }

/**************************************/
/* SqrtFunction: H/L access routine   */
/*   for the sqrt function.           */
/**************************************/
void SqrtFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double num;

   if (! SingleNumberCheck(context,returnValue))
     { return; }

   num = CVCoerceToFloat(returnValue);
   if (num < 0.00000)
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   returnValue->floatValue = EnvCreateFloat(theEnv,sqrt(num));
  }

/*************************************/
/* PowFunction: H/L access routine   */
/*   for the pow function.           */
/*************************************/
void PowFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue value1, value2;
   double num1, num2;

   /*==================================*/
   /* Check for two numeric arguments. */
   /*==================================*/

   if (! UDFNthArgument(context,1,NUMBER_BITS,&value1))
     { return; }

   if (! UDFNthArgument(context,2,NUMBER_BITS,&value2))
     { return; }

    /*=====================*/
    /* Domain error check. */
    /*=====================*/

    num1 = CVCoerceToFloat(&value1);
    num2 = CVCoerceToFloat(&value2);

    if (((num1 == 0.0) && (num2 <= 0.0)) ||
       ((num1 < 0.0) && (dtrunc(num2) != num2)))
     {
      DomainErrorMessage(context,returnValue);
      return;
     }

   /*============================*/
   /* Compute and set the value. */
   /*============================*/

   returnValue->floatValue = EnvCreateFloat(theEnv,pow(num1,num2));
  }

/*************************************/
/* ModFunction: H/L access routine   */
/*   for the mod function.           */
/*************************************/
void ModFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue item1, item2;
   double fnum1, fnum2;
   long long lnum1, lnum2;

   /*==================================*/
   /* Check for two numeric arguments. */
   /*==================================*/

   if (! UDFNthArgument(context,1,NUMBER_BITS,&item1))
     { return; }

   if (! UDFNthArgument(context,2,NUMBER_BITS,&item2))
     { return; }

   /*===========================*/
   /* Check for divide by zero. */
   /*===========================*/

   if ((CVIsType(&item2,INTEGER_BIT) ? (item2.integerValue->contents == 0L) : false) ||
       (CVIsType(&item2,FLOAT_BIT) ? (item2.floatValue->contents == 0.0) : false))
     {
      DivideByZeroErrorMessage(theEnv,"mod");
      EnvSetEvaluationError(theEnv,true);
      returnValue->integerValue = EnvCreateInteger(theEnv,0);
      return;
     }

   /*===========================*/
   /* Compute the return value. */
   /*===========================*/

   if (CVIsType(&item1,FLOAT_BIT) || CVIsType(&item2,FLOAT_BIT))
     {
      fnum1 = CVCoerceToFloat(&item1);
      fnum2 = CVCoerceToFloat(&item2);
      returnValue->floatValue = EnvCreateFloat(theEnv,fnum1 - (dtrunc(fnum1 / fnum2) * fnum2));
     }
   else
     {
      lnum1 = item1.integerValue->contents;
      lnum2 = item2.integerValue->contents;
      returnValue->integerValue = EnvCreateInteger(theEnv,lnum1 - (lnum1 / lnum2) * lnum2);
     }
  }

/************************************/
/* PiFunction: H/L access routine   */
/*   for the pi function.           */
/************************************/
void PiFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   returnValue->floatValue = EnvCreateFloat(theEnv,acos(-1.0));
  }

/****************************************/
/* DegRadFunction: H/L access routine   */
/*   for the deg-rad function.          */
/****************************************/
void DegRadFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,CVCoerceToFloat(returnValue) * PI / 180.0);
  }

/****************************************/
/* RadDegFunction: H/L access routine   */
/*   for the rad-deg function.          */
/****************************************/
void RadDegFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,CVCoerceToFloat(returnValue) * 180.0 / PI);
  }

/*****************************************/
/* DegGradFunction: H/L access routine   */
/*   for the deg-grad function.          */
/*****************************************/
void DegGradFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,CVCoerceToFloat(returnValue) / 0.9);
  }

/*****************************************/
/* GradDegFunction: H/L access routine   */
/*   for the grad-deg function.          */
/*****************************************/
void GradDegFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   if (! SingleNumberCheck(context,returnValue))
     { return; }

   returnValue->floatValue = EnvCreateFloat(theEnv,CVCoerceToFloat(returnValue) * 0.9);
  }

/***************************************/
/* RoundFunction: H/L access routine   */
/*   for the round function.           */
/***************************************/
void RoundFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   /*======================================*/
   /* Check that the argument is a number. */
   /*======================================*/

   if (! UDFNthArgument(context,1,NUMBER_BITS,returnValue))
     { return; }

   /*==============================*/
   /* Round float type to integer. */
   /*==============================*/

   if (CVIsType(returnValue,FLOAT_BIT))
     { returnValue->integerValue = EnvCreateInteger(theEnv,(long long) ceil(returnValue->floatValue->contents - 0.5)); }
  }

/*******************************************/
/* genacosh: Generic routine for computing */
/*   the hyperbolic arccosine.             */
/*******************************************/
static double genacosh(
  double num)
  {
   return(log(num + sqrt(num * num - 1.0)));
  }

/*******************************************/
/* genasinh: Generic routine for computing */
/*   the hyperbolic arcsine.               */
/*******************************************/
static double genasinh(
  double num)
  {
   return(log(num + sqrt(num * num + 1.0)));
  }

/*******************************************/
/* genatanh: Generic routine for computing */
/*   the hyperbolic arctangent.            */
/*******************************************/
static double genatanh(
  double num)
  {
   return((0.5) * log((1.0 + num) / (1.0 - num)));
  }

/*******************************************/
/* genasech: Generic routine for computing */
/*   the hyperbolic arcsecant.             */
/*******************************************/
static double genasech(
  double num)
  {
   return(log(1.0 / num + sqrt(1.0 / (num * num) - 1.0)));
  }

/*******************************************/
/* genacsch: Generic routine for computing */
/*   the hyperbolic arccosecant.           */
/*******************************************/
static double genacsch(
  double num)
  {
   return(log(1.0 / num + sqrt(1.0 / (num * num) + 1.0)));
  }

/*******************************************/
/* genacoth: Generic routine for computing */
/*   the hyperbolic arccotangent.          */
/*******************************************/
static double genacoth(
  double num)
  {
   return((0.5) * log((num + 1.0) / (num - 1.0)));
  }

#endif

