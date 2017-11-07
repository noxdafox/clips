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
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   CLIPSTerminalController *theObject;
   
   theObject = (__bridge CLIPSTerminalController *) GetEnvironmentContext(theEnv);
   
   [theObject clearScrollbackFunction];
  }
  
/************************************************************/
/* QueryInterfaceCallback: Router function which recognizes */
/*   I/O directed to the display window.                    */
/************************************************************/
bool QueryInterfaceCallback(
  Environment *theEnv,
  const char *logicalName,
  void *context)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif
   
   if ( (strcmp(logicalName,STDOUT) == 0) ||
        (strcmp(logicalName,STDIN) == 0) ||
        (strcmp(logicalName,STDERR) == 0) ||
        (strcmp(logicalName,STDWRN) == 0) )
     { return true; }

    return false;
  }

/*******************************************/
/* WriteInterfaceCallback: Router function */
/*   which prints to the display window.   */
/*******************************************/
void WriteInterfaceCallback(
  Environment *theEnv,
  const char *logicalName,
  const char *str,
  void *context)
  {
   FILE *fptr;
   CLIPSTerminalController *theObject = (__bridge CLIPSTerminalController *) context;

   fptr = FindFptr(theEnv,logicalName);
   if ((fptr == stdout) || (fptr == stderr))
     {
      [theObject ungetClear];
      [theObject printC: str];
     }
   else
     { fprintf(fptr,"%s",str); } // TBD Is this necessary?
  }

/******************************************/
/* ReadInterfaceCallback: Router function */
/*   to get input from the display window */
/*   and process other events.            */
/******************************************/
int ReadInterfaceCallback(
  Environment *theEnv,
  const char *logicalName,
  void *context)
  {
   int theChar;
   CLIPSTerminalController *theObject = (__bridge CLIPSTerminalController *) context;

   if ([theObject ungetCount] > 0)
     { return (int) [theObject popUngetChar]; }
     
   theChar = [theObject waitForChar];
   
   return theChar;
  }

/********************************************/
/* UnreadInterfaceCallback: Router function */
/*   to unget input from the display window */
/*   and process other events.              */
/********************************************/
int UnreadInterfaceCallback(
  Environment *theEnv,
  const char *logicalName,
  int theChar,
  void *context)
  {
   CLIPSTerminalController *theObject = (__bridge CLIPSTerminalController *) context;

   if (theChar == EOF) return EOF;
   
   [theObject pushUngetChar: theChar];
   
   return theChar;
  }

/**********************************************/
/* ExitInterfaceCallback: Routine to check an */
/*   exit from the dialog window to make sure */
/*   that the application doesn't exit.       */
/**********************************************/
void ExitInterfaceCallback(
  Environment *theEnv,
  int num,
  void *context)
  {   
   CLIPSTerminalController *theController = (__bridge CLIPSTerminalController *) context;

   [[NSApplication sharedApplication] terminate: NULL];
   [theController exit];
   /* AbortExit(theEnv); */
  }
    
/************************/
/* MacPeriodicFunction: */
/************************/
void MacPeriodicFunction(
  Environment *theEnv,
  void *context)
  {
   CLIPSTerminalController *theController = (__bridge CLIPSTerminalController *) GetEnvironmentContext(theEnv); // TBD Remove?
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
  Environment *theEnv)
  {
   AppController *theDelegate = [NSApp delegate];
   CLIPSTerminalController *theController = [theDelegate mainTerminal];
   
   NSLock *theLock = [theDelegate fileOpenLock];
   
   [theLock lock];
   
   [[NSFileManager defaultManager] changeCurrentDirectoryPath: [theController currentDirectory]];

   return true;
  }  
  
/*************************/
/* MacAfterOpenFunction: */
/*************************/
int MacAfterOpenFunction( // TBD bool?
  Environment *theEnv)
  {
   AppController *theDelegate = [NSApp delegate];
   NSLock *theLock = [theDelegate fileOpenLock];
   
   [theLock unlock];

   return true;
  } 
