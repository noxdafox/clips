   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                 WINDOWS MAIN MODULE                 */
   /*******************************************************/

/**************************************************************/
/* Purpose: Main startup functions for Windows interface.     */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*       Ernst Bokkelkamp                                     */
/*                                                            */
/* Revision History:                                          */
/*       6.24: Ernst's changes to use stand Windows cursors.  */
/*                                                            */
/**************************************************************/

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

#include <windows.h>

#include "setup.h"

#include "envrnmnt.h"
#include "commline.h"
#include "engine.h"
#include "filertr.h"
#include "router.h"
#include "sysdep.h"
#include "envrnbld.h"
#include "utility.h"

#include "StdSDK.h"    
#include "Initialization.h"  
#include "Frame.h"   
#include "resource.h"  
#include "mdi.h"
#include "SearchDialog.h"

#include "display.h"
#include "status.h"
#include "menu.h"
#include "Registry.h"
#include "Text.h"

#include <winuser.h>

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    SetUpRouters(void *);
   static bool                    QueryInterfaceRouter(Environment *,const char *,void *);
   static void                    PrintInterfaceRouter(Environment *,const char *,const char *,void *);
   static void                    ExitInterfaceRouter(Environment *,int,void *);
   static int                     GetcInterfaceRouter(Environment *,const char *,void *);
   static int                     InterfaceEventFunction(void *);
   static void                    WinPeriodicEvent(Environment *,void *);
   static void                    WinRunEvent(Environment *,Activation *,void *);
   static void                    WinEventAction(void);

   Environment                   *GlobalEnv;

/**************************************************/
/* WinMain: Entry point for the application. This */
/*   function initializes the application and     */
/*   processes the message loop.                  */
/**************************************************/
int WINAPI WinMain(
  HINSTANCE hInstance, 
  HINSTANCE hPrevInstance, 
  LPSTR lpCmdLine, 
  int nCmdShow)
  {   
   Environment *theEnv;
   HWND hEditWnd;
   
   /*=============================*/   
   /* Application initialization. */
   /*=============================*/
   
   theEnv = CreateEnvironment(); 
   GlobalEnv = theEnv;

   if (! initInstance(hInstance,IDR_MAINFRAME,nCmdShow,lpCmdLine)) 
     { return FALSE; }
     
   /*==================================*/
   /* Setup routers for the interface. */
   /*==================================*/
   
   SetUpRouters(theEnv);

   /*================================================================*/
   /* Set up hook between the command loop and interface event loop. */
   /*================================================================*/

#if ! RUN_TIME
   SetEventFunction(theEnv,InterfaceEventFunction);
#endif

   /*====================================*/
   /* Add execution functions to update  */
   /* the interface between rule firings */
   /* and execution of procedural code.  */
   /*====================================*/
   
   AddPeriodicFunction(theEnv,"status_wnd",WinPeriodicEvent,0,NULL);
#if DEFRULE_CONSTRUCT
   AddAfterRuleFiresFunction(theEnv,"run_function",WinRunEvent,0,NULL);
#endif

   /*==================================================*/
   /* Add a function which prevents command execution  */
   /* if we're editing a command in the dialog window. */
   /*==================================================*/
   
   SetBeforeCommandExecutionFunction(theEnv,DisplayBeforeCommandExecution);

   /*====================================*/
   /* Register the function which allows */
   /* the display to be cleared.         */
   /*====================================*/
  
   AddUDF(theEnv,"clear-window","v",0,0,NULL,ClearWindowCommand,"ClearWindowCommand",NULL);
                      
   /*======================================*/
   /* Set the focus to the display window. */
   /*======================================*/
      
   display_OnSetFocus(DialogWindow,NULL);
      
   /*=====================================*/
   /* Read preferences from the registry. */
   /*=====================================*/
   
   ReadRegistryInformation();
   
   /*====================*/
   /* Main message loop. */
   /*====================*/
   
   if ((lpCmdLine != NULL) &&
       (lpCmdLine[0] != 0))
     { 
      char *fileName;
      size_t length;
      
      /* Strip the quotation marks from the command line argument. */
      
      length = strlen(lpCmdLine);
      fileName = (char *) malloc(length - 1);
      if (fileName == NULL) return FALSE;
      strncpy(fileName,&lpCmdLine[1],length-2);
      fileName[length-2] = 0;
      
      if ((hEditWnd = text_New(hMainFrame,fileName)) != NULL)
        { text_Revert(hMainFrame,fileName,hEditWnd); }
        
      free(fileName);
     }
 
   CommandLoop(GlobalEnv);
   
   return TRUE;
  }

/**************************************/
/* SetUpRouters: Sets up routers used */
/*   by the windowed interface.       */
/**************************************/
static void SetUpRouters(
  void *theEnv)
  {  
   AddRouter(theEnv,"InterfaceExit",60,NULL,NULL,NULL,NULL,ExitInterfaceRouter,NULL);
   AddRouter(theEnv,"InterfaceStdIO",10,QueryInterfaceRouter,PrintInterfaceRouter,GetcInterfaceRouter,NULL,NULL,NULL);
  }

/**************************************************/
/* ExitInterfaceRouter: Routine to  check an exit */
/*   from the dialog window to make sure that     */
/*   the user has an opportunity to save files.   */
/**************************************************/
static void ExitInterfaceRouter(
  Environment *theEnv,
  int num,
  void *context)
  {   
   MSG msg;
   if (num >= 0) return;
   
   //DoQuit();
   //AbortExit();
   //return(1);
   
   PostMessage(DialogWindow,WM_COMMAND,ID_APP_EXIT,0);
   exitInstance(&msg); 
  }

/**********************************************************/
/* QueryInterfaceRouter: Router function which recognizes */
/*   I/O directed to the display window.                  */
/**********************************************************/
static bool QueryInterfaceRouter(
  Environment *theEnv,
  const char *logicalName,
  void *context)
  {
   if ( (strcmp(logicalName,STDOUT) == 0) ||
        (strcmp(logicalName,STDIN) == 0) ||
        (strcmp(logicalName,WERROR) == 0) ||
        (strcmp(logicalName,WWARNING) == 0) )
     { return(TRUE); }

    return(FALSE);
  }

/******************************************/
/* PrintInterfaceRouter: Router function  */
/*    which prints to the display window. */
/******************************************/
static void PrintInterfaceRouter(
  Environment *theEnv,
  const char *logicalName,
  const char *str,
  void *context)
  {
   FILE *fptr;

   fptr = FindFptr(theEnv,logicalName);
   if (fptr == stdout)
     { DisplayPrint(DialogWindow,str); }
   else
     { fprintf(fptr,"%s",str); }
  }
  
/*******************************************/
/* GetcInterfaceRouter: Router function to */
/*   get input from the display window and */
/*   process other events.                 */
/*******************************************/
static int GetcInterfaceRouter(
  Environment *theEnv,
  const char *logicalName,
  void *context)
  { 
   FILE *fptr;
   MSG msg;
   static int count = 0;

   fptr = FindFptr(theEnv,logicalName);
   if (fptr != stdin) return(getc(fptr));

   //UpdateCursor(QUESTION_CURSOR);
   SetCursor(LoadCursor(NULL,IDC_HELP));
   SetClassLongPtr(DialogWindow,GCLP_HCURSOR,(LONG_PTR) LoadCursor(NULL,IDC_HELP));

   GetMessage(&msg,NULL,0,0);
   TranslateMessage(&msg);

   while (TRUE)
     {  
      if (msg.message == WM_CHAR)
        {  
         switch(msg.wParam)
           {  
            case VK_BACK:
              GetUserCmd(DialogWindow,(WORD) msg.wParam,TRUE,(unsigned) count);
              count--;
              if (count < 0) count = 0;
              msg.wParam = '\b';
              break;

            case VK_RETURN:
              GetUserCmd(DialogWindow,(WORD) msg.wParam,TRUE,(unsigned) count);
              count = 0;
              //UpdateCursor(ARROW_CURSOR);
	  		  SetCursor(LoadCursor(NULL,IDC_ARROW));
		      SetClassLongPtr(DialogWindow,GCLP_HCURSOR,(LONG_PTR) LoadCursor(NULL,IDC_ARROW));
              msg.wParam = '\n';
              break;

            default:
              count++;
              GetUserCmd(DialogWindow,(WORD) msg.wParam,TRUE,(unsigned) count);
              break;
           }
           
         return((int) msg.wParam);
        }
      
      DispatchMessage(&msg);
      //UpdateCursor(QUESTION_CURSOR);
      SetCursor(LoadCursor(NULL,IDC_HELP));
	  SetClassLongPtr(DialogWindow,GCLP_HCURSOR,(LONG_PTR) LoadCursor(NULL,IDC_HELP));

      GetMessage(&msg,NULL,0,0);
      TranslateMessage(&msg);
     }
  }
  
/****************************************/
/* InterfaceEventFunction: Executes one */
/*   pass of the main program loop.     */
/****************************************/
static int InterfaceEventFunction(
  void *theEnv)
  {  
   MSG msg;
   
     //UpdateCursor(ARROW_CURSOR);
	 //SetCursor(LoadCursor(NULL,IDC_ARROW));
	 //SetClassLong(DialogWindow,GCL_HCURSOR,(LONG) LoadCursor(NULL,IDC_ARROW));
   
   /*============================*/
   /* Update the status windows. */
   /*============================*/
  
   UpdateStatus();
   
   /*========================*/
   /* Update the menu items. */
   /*========================*/
   
   UpdateMenu(hMainFrame);

   /*========================*/
   /* Handle the next event. */
   /*========================*/
   
   GetMessage(&msg,NULL,0,0);
   if (! TranslateAccelerator(hMainFrame,haccel,&msg))
     {  
      TranslateMessage(&msg);
      DispatchMessage(&msg);
     }

   return(TRUE);
  }
  
/******************/
/* WinEventAction */
/******************/
static void WinEventAction()
  {  
   MSG msg;

   UpdateStatus();
   UpdateMenu(hMainFrame);

   while (PeekMessage(&msg,NULL,0,0,PM_REMOVE))
     {  
      if (! TranslateAccelerator(hMainFrame,haccel,&msg))
        {  
         TranslateMessage(&msg);
	      DispatchMessage(&msg);
        }
     }
  }

/***********************************************************/
/* WinPeriodicEvent: Function which is called periodically */
/*   to update the interface while rules are firing or     */
/*   procedural code is executing.                         */
/***********************************************************/
static void WinPeriodicEvent(
  Environment *theEnv,
  void *context)
  { 
   WinEventAction(); 
   }
  
/******************************************************/
/* WinRunEvent: Function which is called periodically */
/*   to update the interface while rules are firing   */
/*   or procedural code is executing.                 */
/******************************************************/
static void WinRunEvent(
  Environment *theEnv,
  Activation *activation,
  void *context)
  {  
   WinEventAction(); 
  }

  
