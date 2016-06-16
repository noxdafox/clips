/*
 *  CLIPSTerminalGlue.c
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */

#import "CLIPSTerminalGlue.h"

#import "AppController.h"
#import "CLIPSTerminalController.h"
#import "CLIPSEnvironment.h"

/*********************************************/
/* ClearEnvironmentWindowCommand: H/L access */
/*   routine for the clear-window command.   */
/*********************************************/
void ClearEnvironmentWindowCommand(
  void *theEnv)
  {
   CLIPSTerminalController *theObject;

   if (EnvArgCountCheck(theEnv,"clear-window",EXACTLY,0) == -1) return;
   theObject = (__bridge CLIPSTerminalController *) GetEnvironmentContext(theEnv);
   
   [theObject clearScrollbackFunction];
  }
  
/**********************************************************/
/* QueryInterfaceRouter: Router function which recognizes */
/*   I/O directed to the display window.                  */
/**********************************************************/
intBool QueryInterfaceRouter(
  void *theEnv,
  const char *logicalName)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif
   
   if ( (strcmp(logicalName,STDOUT) == 0) ||
        (strcmp(logicalName,STDIN) == 0) ||
        (strcmp(logicalName,WPROMPT) == 0) ||
        (strcmp(logicalName,WTRACE) == 0) ||
        (strcmp(logicalName,WERROR) == 0) ||
        (strcmp(logicalName,WWARNING) == 0) ||
        (strcmp(logicalName,WDISPLAY) == 0) ||
        (strcmp(logicalName,WDIALOG) == 0) )
     { return(TRUE); }

    return(FALSE);
  }

/*****************************************/
/* PrintInterfaceRouter: Router function */
/*   which prints to the display window. */
/*****************************************/
int PrintInterfaceRouter(
  void *theEnv,
  const char *logicalName,
  const char *str)
  {
   FILE *fptr;
   CLIPSTerminalController *theObject = (__bridge CLIPSTerminalController *) GetEnvironmentRouterContext(theEnv);

   fptr = FindFptr(theEnv,logicalName);
   if (fptr == stdout)
     { [theObject printC: str]; }
   else
     { fprintf(fptr,"%s",str); } // TBD Is this necessary?
 
   return(TRUE);  // TBD bool?
  }

/*******************************************/
/* GetcInterfaceRouter: Router function to */
/*   get input from the display window and */
/*   process other events.                 */
/*******************************************/
int GetcInterfaceRouter(
  void *theEnv,
  const char *logicalName)
  {
   int theChar;
   CLIPSTerminalController *theObject = (__bridge CLIPSTerminalController *) GetEnvironmentRouterContext(theEnv);

   theChar = [theObject waitForChar];
   
   return(theChar);
  }
  
/*************************************************/
/* ExitInterfaceRouter: Routine to check an exit */
/*   from the dialog window to make sure that    */
/*   the application doesn't exit.               */
/*************************************************/
int ExitInterfaceRouter(
  void *theEnv,
  int num)
  {   
   CLIPSTerminalController *theController = (__bridge CLIPSTerminalController *) GetEnvironmentRouterContext(theEnv);

   [[NSApplication sharedApplication] terminate: NULL];
   [theController exit];
   /* AbortExit(theEnv); */
   return(TRUE);
  }  
    
/************************/
/* MacPeriodicFunction: */
/************************/
void MacPeriodicFunction(
  void *theEnv)
  {
   CLIPSTerminalController *theController = (__bridge CLIPSTerminalController *) GetEnvironmentContext(theEnv);
   NSConditionLock *theAgendaLock, *theFactsLock;
   
   /* TBD See if there are other ways to check the locks */
   
   if ([[theController pauseLock] condition] == EXECUTION_IS_PAUSED)
     {
      [[theController pauseLock] lockWhenCondition: EXECUTION_IS_NOT_PAUSED];
      [[theController pauseLock] unlock];
     }

   /*============================================================*/
   /* If there are debugging windows displaying the state of the */
   /* agenda, then update the agenda if necessary. Acquiring the */
   /* lock as frequently as this function is called can kill     */
   /* performance so we first check to see if there are windows  */
   /* that need the agenda and an agenda fetch has been issued.  */
   /*============================================================*/

   theAgendaLock = [[theController environment] agendaLock];
   if (([[theController environment] agendaListenerCount] != 0) &&
       ([theAgendaLock condition] == FETCH_AGENDA))
     {
      [theAgendaLock lock];
      switch ([theAgendaLock condition])
        {
         case FETCH_AGENDA:
           [[theController environment] fetchAgenda: NO];
           [theAgendaLock unlockWithCondition: AGENDA_FETCHED];
           break;
         
         default:
           [theAgendaLock unlock];
           break;
        }
     }

   theFactsLock = [[theController environment] factsLock];
   if (([[theController environment] factsListenerCount] != 0) &&
       ([theFactsLock condition] == FETCH_FACTS))
     {
      [theFactsLock lock];
      switch ([theFactsLock condition])
        {
         case FETCH_FACTS:
           [[theController environment] fetchFacts: NO];
           [theFactsLock unlockWithCondition: FACTS_FETCHED];
           break;
         
         default:
           [theFactsLock unlock];
           break;
        }
     }
      
   /*==================================*/
   /* Disable periodic functions.      */
   /* They'll be reenabled by a timer. */
   /*==================================*/
    
   EnablePeriodicFunctions(theEnv,false);
  }
    
/**************************************/
/* MacBeforeOpenFunction:             */
/**************************************/
int MacBeforeOpenFunction(
  void *theEnv)
  {
   AppController *theDelegate = [NSApp delegate];
   CLIPSTerminalController *theController = [theDelegate mainTerminal];
   
   NSLock *theLock = [theDelegate fileOpenLock];
   
   [theLock lock];
   
   [[NSFileManager defaultManager] changeCurrentDirectoryPath: [theController currentDirectory]];

   return TRUE;
  }  
  
/*************************/
/* MacAfterOpenFunction: */
/*************************/
int MacAfterOpenFunction( // TBD bool?
  void *theEnv)
  {
   AppController *theDelegate = [NSApp delegate];
   NSLock *theLock = [theDelegate fileOpenLock];
   
   [theLock unlock];

   return TRUE;
  } 
