   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  02/17/17       */
   /*                                                     */
   /*              WINDOWS CLIPS GLUE MODULE              */
   /*******************************************************/

/**************************************************************/
/* Purpose:                                                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

#include "CLIPSGlue.h"

#include "commline.h"
#include "router.h"
#include "rulecom.h"

/***************************************************************/
/* MatchesAction: The GetFocus function in rulecom.h conflicts */
/*   with a similar function in Windows. Since the Matches     */
/*   command is also defined in rulecom.h, the code that calls */
/*   this function was moved to this file which does not       */
/*   include any windows headers.                              */
/***************************************************************/
void MatchesAction(
   Defrule *theDefrule,
   char *string)
   {
    Environment *theEnv = theDefrule->header.env;
    CLIPSValue result;

    PrintString(theEnv,STDOUT,"(matches ");
    PrintString(theEnv,STDOUT,string );
    PrintString(theEnv,STDOUT,")\n");
    Matches(theDefrule,VERBOSE,&result);
    PrintPrompt(theEnv);
   }
