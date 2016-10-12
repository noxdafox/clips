   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.40  08/06/16          */
   /*                                                     */
   /*                     MAIN MODULE                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Moved UserFunctions and EnvUserFunctions to    */
/*            the new userfunctions.c file.                  */
/*                                                           */
/*      6.40: Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Moved CatchCtrlC to main.c.                    */
/*                                                           */
/*************************************************************/

/***************************************************************************/
/*                                                                         */
/* Permission is hereby granted, free of charge, to any person obtaining   */
/* a copy of this software and associated documentation files (the         */
/* "Software"), to deal in the Software without restriction, including     */
/* without limitation the rights to use, copy, modify, merge, publish,     */
/* distribute, and/or sell copies of the Software, and to permit persons   */
/* to whom the Software is furnished to do so.                             */
/*                                                                         */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS */
/* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF              */
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT   */
/* OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY  */
/* CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN   */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.          */
/*                                                                         */
/***************************************************************************/

#include "clips.h"

#if   UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
#include <signal.h>
#endif

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
   static void                    CatchCtrlC(int);
#endif

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

   static Environment            *mainEnv;

/****************************************/
/* main: Starts execution of the expert */
/*   system development environment.    */
/****************************************/
int main(
  int argc,
  char *argv[])
  {
   FactBuilder *theFB;
   FactModifier *theFM;
   Fact *thePerson;
   Fact *thePoint;
   Multifield *theMF;
   CLIPSValue multifieldArray[3];

   mainEnv = CreateEnvironment();
   
#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC
   signal(SIGINT,CatchCtrlC);
#endif

   EnvBuild(mainEnv,"(deftemplate person"
                    "   (multislot name)"
                    "   (slot age))");

   EnvBuild(mainEnv,"(deftemplate point"
                    "   (slot x (default 0))"
                    "   (slot y (default 0))"
                    "   (slot z (default 0)))");

   EnvWatch(mainEnv,"facts");

   theFB = EnvCreateFactBuilder(mainEnv,"MAIN::point");

   FBPutSlotInteger(theFB,"x",EnvCreateInteger(mainEnv,4));
   FBPutSlotInteger(theFB,"y",EnvCreateInteger(mainEnv,3));
   FBPutSlotInteger(theFB,"z",EnvCreateInteger(mainEnv,1));
   FBAssert(theFB);

   FBPutSlotInteger(theFB,"x",EnvCreateInteger(mainEnv,7));
   FBPutSlotInteger(theFB,"y",EnvCreateInteger(mainEnv,0));
   FBPutSlotInteger(theFB,"z",EnvCreateInteger(mainEnv,5));
   FBAssert(theFB);

   FBPutSlotInteger(theFB,"x",EnvCreateInteger(mainEnv,2));
   FBPutSlotInteger(theFB,"y",EnvCreateInteger(mainEnv,6));
   FBPutSlotInteger(theFB,"z",EnvCreateInteger(mainEnv,8));
   thePoint = FBAssert(theFB);

   FBSetDeftemplate(theFB,"MAIN::person");
   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Gary");
   multifieldArray[1].lexemeValue = EnvCreateSymbol(mainEnv,"Riley");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,2);
   
   FBPutSlotMultifield(theFB,"name",theMF);
   FBPutSlotInteger(theFB,"age",EnvCreateInteger(mainEnv,44));
   thePerson = FBAssert(theFB);
   
   FBDispose(theFB);

   theFM = EnvCreateFactModifier(mainEnv,thePoint);
   FMPutSlotInteger(theFM,"x",EnvCreateInteger(mainEnv,11));
   FMApply(theFM);
   FMPutSlotInteger(theFM,"y",EnvCreateInteger(mainEnv,22));
   FMPutSlotInteger(theFM,"z",EnvCreateInteger(mainEnv,33));
   FMApply(theFM);

   FMPutSlotInteger(theFM,"x",EnvCreateInteger(mainEnv,22));
   FMPutSlotInteger(theFM,"y",EnvCreateInteger(mainEnv,33));
   FMPutSlotInteger(theFM,"z",EnvCreateInteger(mainEnv,44));
   FMAbort(theFM);

   FMPutSlotInteger(theFM,"x",EnvCreateInteger(mainEnv,55));
   FMPutSlotInteger(theFM,"z",EnvCreateInteger(mainEnv,66));
   FMApply(theFM);
   
   FMSetFact(theFM,thePerson);
   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Mark");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,1);
      
   FMPutSlotMultifield(theFM,"name",theMF);
   FMApply(theFM);

   FMPutSlotInteger(theFM,"age",EnvCreateInteger(mainEnv,17));
   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Howard");
   multifieldArray[1].lexemeValue = EnvCreateSymbol(mainEnv,"Wayne");
   multifieldArray[2].lexemeValue = EnvCreateSymbol(mainEnv,"Garner");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,3);
   
   FMPutSlotMultifield(theFM,"name",theMF);
   FMApply(theFM);
      
   FMApply(theFM);

   FMPutSlotInteger(theFM,"age",EnvCreateInteger(mainEnv,17));
   FMPutSlotInteger(theFM,"age",EnvCreateInteger(mainEnv,18));
   FMPutSlotInteger(theFM,"age",EnvCreateInteger(mainEnv,17));

   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Howard");
   multifieldArray[1].lexemeValue = EnvCreateSymbol(mainEnv,"Wayne");
   multifieldArray[2].lexemeValue = EnvCreateSymbol(mainEnv,"Garner");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,3);
   FMPutSlotMultifield(theFM,"name",theMF);

   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Gary");
   multifieldArray[1].lexemeValue = EnvCreateSymbol(mainEnv,"David");
   multifieldArray[2].lexemeValue = EnvCreateSymbol(mainEnv,"Riley");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,3);
   FMPutSlotMultifield(theFM,"name",theMF);
   
   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Howard");
   multifieldArray[1].lexemeValue = EnvCreateSymbol(mainEnv,"Wayne");
   multifieldArray[2].lexemeValue = EnvCreateSymbol(mainEnv,"Garner");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,3);
   FMPutSlotMultifield(theFM,"name",theMF);

   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Fred");
   multifieldArray[1].lexemeValue = EnvCreateSymbol(mainEnv,"Flintstone");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,2);
   FMPutSlotMultifield(theFM,"name",theMF);
   FMPutSlotInteger(theFM,"age",EnvCreateInteger(mainEnv,19));
   
   FMApply(theFM);

   multifieldArray[0].lexemeValue = EnvCreateSymbol(mainEnv,"Barney");
   multifieldArray[1].lexemeValue = EnvCreateSymbol(mainEnv,"Rubble");
   theMF = EnvArrayToMultifield(mainEnv,multifieldArray,2);
   FMPutSlotMultifield(theFM,"name",theMF);
   FMAbort(theFM);

   FMDispose(theFM);

   //RerouteStdin(mainEnv,argc,argv);
   //CommandLoop(mainEnv);

   /*==================================================================*/
   /* Control does not normally return from the CommandLoop function.  */
   /* However if you are embedding CLIPS, have replaced CommandLoop    */
   /* with your own embedded calls that will return to this point, and */
   /* are running software that helps detect memory leaks, you need to */
   /* add function calls here to deallocate memory still being used by */
   /* CLIPS. If you have a multi-threaded application, no environments */
   /* can be currently executing.                                      */
   /*==================================================================*/

   DestroyEnvironment(mainEnv);

   return -1;
  }

#if UNIX_V || LINUX || DARWIN || UNIX_7 || WIN_GCC || WIN_MVC || DARWIN
/***************/
/* CatchCtrlC: */
/***************/
static void CatchCtrlC(
  int sgnl)
  {
   EnvSetHaltExecution(mainEnv,true);
   CloseAllBatchSources(mainEnv);
   signal(SIGINT,CatchCtrlC);
  }
#endif

