   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  07/31/15       */
   /*                                                     */
   /*                   DIALOG1 MODULE                    */
   /*******************************************************/

/**************************************************************/
/* Purpose: Contains the callback functions for most of the   */
/*       simple dialogs from the main menu.                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Gary D. Riley                                         */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/*      6.31: Fixed 64 bit architecture issues.               */
/*                                                            */
/**************************************************************/

#define _DIALOG1_SOURCE_

/*-------------------------------+
| Windows & System Include Files |
+-------------------------------*/
#include <windows.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/*------------------------+
| CLIPS 6.0 Include Files |
+------------------------*/
#include "setup.h"
#include "facthsh.h"
#include "crstrtgy.h"
#include "globlcom.h"
#include "watch.h"

/*------------------------+
| Interface Include Files |
+------------------------*/

#include "resource.h"
#include "Registry.h"
#include "mdi.h"

#include "dialog1.h"

int RuleStep = 1;
int Warnings = 1;
int Complete = 1;
   
/**********************************************************************/
/* SetCheckBox: The function selects (places a check  mark next to)   */
/*   or clears (removes a check mark from) a button control (wParam). */
/**********************************************************************/
int SetCheckBox(
  HWND hDlg,
  WORD wParam,
  int value)
  {  
   CheckDlgButton(hDlg,wParam,(unsigned) value);
   return (value);
  }
  
/****************************************************/
/* ExecDlg: Callback function which allows the user */
/*   to set various CLIPS execution options.        */
/****************************************************/
INT_PTR CALLBACK ExecDlg(
  HWND hDlg,
  UINT message,
  WPARAM wParam,
  LPARAM lParam)
  {   
   WPARAM item;
   Environment *theEnv = GlobalEnv;
   unsigned value;
   char Msg[40];
   
   switch (message)
     {  
      case WM_INITDIALOG:  
        SetWindowText(hDlg,(LPSTR)"Execution Options");

        /*===============================*/
        /* Initalize Strategy Combo Box. */
        /*===============================*/
        
        SendDlgItemMessage(hDlg, IDC_EXE_STRATEGY, CB_ADDSTRING,0,(LPARAM)((LPSTR) "Depth"));
        SendDlgItemMessage(hDlg, IDC_EXE_STRATEGY, CB_ADDSTRING,0,(LPARAM)((LPSTR) "Breadth"));
        SendDlgItemMessage(hDlg, IDC_EXE_STRATEGY, CB_ADDSTRING,0,(LPARAM)((LPSTR) "LEX"));
        SendDlgItemMessage(hDlg, IDC_EXE_STRATEGY, CB_ADDSTRING,0,(LPARAM)((LPSTR) "MEA"));
        SendDlgItemMessage(hDlg, IDC_EXE_STRATEGY, CB_ADDSTRING,0,(LPARAM)((LPSTR) "Complexity"));
        SendDlgItemMessage(hDlg, IDC_EXE_STRATEGY, CB_ADDSTRING,0,(LPARAM)((LPSTR) "Simplicity"));
        SendDlgItemMessage(hDlg, IDC_EXE_STRATEGY, CB_ADDSTRING,0,(LPARAM)((LPSTR) "Random"));

        switch (EnvGetStrategy(GlobalEnv))
          {   
           case DEPTH_STRATEGY:
             item = (WPARAM) SendDlgItemMessage(hDlg,IDC_EXE_STRATEGY,CB_FINDSTRING,0,(LPARAM)((LPSTR)"Depth"));
             break;
             
           case BREADTH_STRATEGY:
             item = (WPARAM) SendDlgItemMessage(hDlg,IDC_EXE_STRATEGY,CB_FINDSTRING,0,(LPARAM)((LPSTR)"Breadth"));
             break;
             
           case LEX_STRATEGY:
             item = (WPARAM) SendDlgItemMessage(hDlg,IDC_EXE_STRATEGY,CB_FINDSTRING,0,(LPARAM)((LPSTR)"LEX"));
             break;
             
           case MEA_STRATEGY:
             item = (WPARAM) SendDlgItemMessage(hDlg,IDC_EXE_STRATEGY,CB_FINDSTRING,0,(LPARAM)((LPSTR)"MEA"));
             break;
             
           case COMPLEXITY_STRATEGY:
             item = (WPARAM) SendDlgItemMessage(hDlg,IDC_EXE_STRATEGY,CB_FINDSTRING,0,(LPARAM)((LPSTR)"Complexity"));
             break;
      
           case SIMPLICITY_STRATEGY:
             item = (WPARAM) SendDlgItemMessage(hDlg,IDC_EXE_STRATEGY,CB_FINDSTRING,0,(LPARAM)((LPSTR)"Simplicity"));
             break;
             
           case RANDOM_STRATEGY:
             item = (WPARAM) SendDlgItemMessage(hDlg,IDC_EXE_STRATEGY,CB_FINDSTRING,0,(LPARAM)((LPSTR)"Random"));
             break;
          }

        SendDlgItemMessage (hDlg, IDC_EXE_STRATEGY, CB_SETCURSEL, item, 0);

        /*===============================*/
        /* Initalize Salience Combo Box. */
        /*===============================*/
        
        SendDlgItemMessage (hDlg, IDC_EXE_SALIENCE, CB_ADDSTRING,0,(LPARAM)((LPSTR) "When Defined"));
        SendDlgItemMessage (hDlg, IDC_EXE_SALIENCE, CB_ADDSTRING,0,(LPARAM)((LPSTR) "When Activated"));
        SendDlgItemMessage (hDlg, IDC_EXE_SALIENCE, CB_ADDSTRING,0,(LPARAM)((LPSTR) "Every Cycle"));

        switch (EnvGetSalienceEvaluation(GlobalEnv))
          {   
           case WHEN_DEFINED:
             item = (WPARAM) SendDlgItemMessage (hDlg, IDC_EXE_SALIENCE, CB_FINDSTRING,0,(LPARAM)((LPSTR) "When Defined"));
             break;
             
           case WHEN_ACTIVATED:
             item = (WPARAM) SendDlgItemMessage (hDlg, IDC_EXE_SALIENCE, CB_FINDSTRING,0,(LPARAM)((LPSTR) "When Activated"));
             break;
             
           case EVERY_CYCLE:
             item = (WPARAM) SendDlgItemMessage (hDlg, IDC_EXE_SALIENCE, CB_FINDSTRING,0,(LPARAM)((LPSTR) "Every Cycle"));
             break;
          }

        SendDlgItemMessage (hDlg, IDC_EXE_SALIENCE, CB_SETCURSEL, item, 0);

        /*=====================================*/
        /* Disable Incremental reset check box */
        /* if any rules are in the system.     */
        /*=====================================*/
        
        if (EnvGetNextDefrule (theEnv,NULL) != NULL)
          { EnableWindow(GetDlgItem(hDlg, IDC_WATCH_ALL ), TRUE); }

        /*==============================*/
        /* Initalize Other Check Boxes. */
        /*==============================*/
        
        SetCheckBox(hDlg,IDC_EXE_DYNAMIC,EnvGetDynamicConstraintChecking(theEnv));
#if DEFGLOBAL_CONSTRUCT
        SetCheckBox(hDlg,IDC_EXE_GLOBAL,EnvGetResetGlobals(theEnv));
#endif
#if DEFTEMPLATE_CONSTRUCT
        SetCheckBox(hDlg,IDC_EXE_FACT,EnvGetFactDuplication(theEnv));
#endif
#if (!RUN_TIME)
        SetCheckBox(hDlg,IDC_EXE_SEQUENCE,EnvGetSequenceOperatorRecognition(theEnv));
#endif
        return (TRUE);
      
      case WM_COMMAND:
        switch (wParam)
          {  
           case IDC_OK:

             /*===========================*/
             /* Decode Salience Combo Box */
             /*===========================*/
             
             value = (unsigned) SendDlgItemMessage(hDlg,IDC_EXE_SALIENCE,CB_GETCURSEL,0,0);

             SendDlgItemMessage(hDlg,IDC_EXE_SALIENCE,CB_GETLBTEXT,value,(LPARAM) Msg );

             if (strcmp(Msg,"When Defined") == 0)
               { EnvSetSalienceEvaluation(GlobalEnv,WHEN_DEFINED); }
             else if (strcmp(Msg, "When Activated" ) == 0)
               { EnvSetSalienceEvaluation(GlobalEnv,WHEN_ACTIVATED); }
             else
               { EnvSetSalienceEvaluation(GlobalEnv,EVERY_CYCLE); }

             /*===========================*/
             /* Decode Strategy Combo Box */
             /*===========================*/
             
             value = (unsigned) SendDlgItemMessage (hDlg, IDC_EXE_STRATEGY, CB_GETCURSEL, 0, 0);
             SendDlgItemMessage (hDlg, IDC_EXE_STRATEGY, CB_GETLBTEXT, value,(LPARAM) Msg );

             if ( strcmp (Msg, "Depth"  ) == 0 )
               { EnvSetStrategy(GlobalEnv,DEPTH_STRATEGY); }
             else if (strcmp(Msg, "Breadth" ) == 0)
               { EnvSetStrategy(GlobalEnv,BREADTH_STRATEGY); }
             else if (strcmp(Msg, "LEX") == 0 )
               { EnvSetStrategy(GlobalEnv,LEX_STRATEGY); }
             else if (strcmp(Msg, "MEA") == 0 )
               { EnvSetStrategy(GlobalEnv,MEA_STRATEGY); }
             else if (strcmp(Msg, "Complexity") == 0 )
               { EnvSetStrategy(GlobalEnv,COMPLEXITY_STRATEGY); }
             else if (strcmp(Msg, "Simplicity") == 0 )
               { EnvSetStrategy(GlobalEnv,SIMPLICITY_STRATEGY); }
             else
               { EnvSetStrategy(GlobalEnv,RANDOM_STRATEGY); }

             /*==========================*/
             /* Decode Other Check Boxes */
             /*==========================*/
             
             EnvSetDynamicConstraintChecking(theEnv,(int) IsDlgButtonChecked(hDlg,IDC_EXE_DYNAMIC));

#if DEFGLOBAL_CONSTRUCT
             EnvSetResetGlobals(theEnv,(int) IsDlgButtonChecked(hDlg,IDC_EXE_GLOBAL));
#endif
#if DEFTEMPLATE_CONSTRUCT
             EnvSetFactDuplication(theEnv,(int) IsDlgButtonChecked(hDlg,IDC_EXE_FACT));
#endif
#if INCREMENTAL_RESET & (! RUN_TIME )
             EnvSetIncrementalReset(theEnv,(int) IsDlgButtonChecked(hDlg,IDC_EXE_RESET));
#endif
#if ( ! RUN_TIME )
             EnvSetSequenceOperatorRecognition(theEnv,(int) IsDlgButtonChecked(hDlg,IDC_EXE_SEQUENCE));
#endif
             SaveExecutionInformation();
             EndDialog(hDlg,IDOK);
             return(TRUE);

           case IDC_CANCEL:
             EndDialog(hDlg,IDOK);
             return(TRUE);
     
          /*====================*/
          /* Toggle Check Boxes */
          /*====================*/
          
          case IDC_EXE_DYNAMIC:
          case IDC_EXE_GLOBAL:
          case IDC_EXE_FACT:
          case IDC_EXE_SEQUENCE:
            SetCheckBox(hDlg,(unsigned short) wParam,! IsDlgButtonChecked(hDlg,(int) wParam));
            return (TRUE);
         }
      }
   return (FALSE);
  }

/***********************************************************/
/* OptionDlgProc:  Callback function which allows the user */ 
/*   to set various CLIPS interface execution preferences. */
/***********************************************************/
INT_PTR CALLBACK OptionDlgProc (
  HWND hDlg,
  UINT message,
  WPARAM wParam,
  LPARAM lParam)
  {  
   static char Number[4];  
   int temp;
   int x;
   int flag = 0;

   switch (message)
     {  
      case WM_INITDIALOG: 
        SetCheckBox(hDlg,IDC_WARN, Warnings);
        SetCheckBox(hDlg,IDC_PREF_COMPLETE, Complete );
        SendDlgItemMessage(hDlg,IDC_EDIT,EM_LIMITTEXT,3,0);
        sprintf(Number,"%d",RuleStep);
        SendDlgItemMessage(hDlg, IDC_EDIT, WM_SETTEXT,0, (LPARAM) Number);
        return (TRUE);

      case WM_COMMAND:
        switch (wParam)
          {  
           case IDC_WARN:
           case IDC_PREF_COMPLETE:
             SetCheckBox(hDlg,(unsigned short) wParam,!(IsDlgButtonChecked(hDlg,(int) wParam)));
             return (TRUE);

           case IDC_OK:
             SendDlgItemMessage ( hDlg, IDC_EDIT, WM_GETTEXT,4, (LPARAM) Number);
             temp = atoi (Number);
             for (x=0; x < (int) strlen(Number); x++)
               { 
                if (! isdigit (Number[x]))
                  { flag=1; }
               }
               
             if (temp < 1 || temp > 999 || flag)
               {  
                sprintf(Number,"%d", RuleStep );
                SendDlgItemMessage ( hDlg, IDC_EDIT, WM_SETTEXT,0, (LPARAM) Number);
                MessageBeep(0);
                MessageBox(hDlg,"Must be between 1 and 999 inclusive",
                "Step Rule Firing Increment", MB_ICONHAND|MB_OK);
                SetFocus(GetDlgItem(hDlg, IDC_EDIT));
                break;
               }
            
             RuleStep = temp;

             Warnings = (int) IsDlgButtonChecked (hDlg, IDC_WARN);
             Complete = (int) IsDlgButtonChecked (hDlg, IDC_PREF_COMPLETE);
             SavePreferenceInformation();
             EndDialog(hDlg,IDOK);
             return TRUE;
     
           case IDC_CANCEL:
             EndDialog(hDlg,IDOK);
             return TRUE;
          }  
     }
     
   return ( FALSE );
  }

/*****************************************************/
/* WatchDlgProc:  Callback function which allows the */
/*   user to set watch options for CLIPS to display. */
/*****************************************************/
INT_PTR CALLBACK WatchDlgProc(
  HWND hDlg,
  UINT message,
  WPARAM wParam,
  LPARAM lParam)
  {  
   static int count = 0;
   WORD x;
   Environment *theEnv = GlobalEnv;
   
   switch (message)
     {  
      case WM_INITDIALOG:  
        count = 0;
        count += SetCheckBox(hDlg, IDC_WATCH_COMPILE,    EnvGetWatchItem(theEnv,"compilations"));
        count += SetCheckBox(hDlg, IDC_WATCH_FACTS,      EnvGetWatchItem(theEnv,"facts"));
        count += SetCheckBox(hDlg, IDC_WATCH_INSTANCE,   EnvGetWatchItem(theEnv,"instances"));
        count += SetCheckBox(hDlg, IDC_WATCH_RULES,      EnvGetWatchItem(theEnv,"rules"));
        count += SetCheckBox(hDlg, IDC_WATCH_GENERIC,    EnvGetWatchItem(theEnv,"generic-functions"));
        count += SetCheckBox(hDlg, IDC_WATCH_MESSAGE,    EnvGetWatchItem(theEnv,"messages"));
        count += SetCheckBox(hDlg, IDC_WATCH_DEFFUN,     EnvGetWatchItem(theEnv,"deffunctions"));
        count += SetCheckBox(hDlg, IDC_WATCH_STATS,      EnvGetWatchItem(theEnv,"statistics"));
        count += SetCheckBox(hDlg, IDC_WATCH_GLOBAL,     EnvGetWatchItem(theEnv,"globals"));
        count += SetCheckBox(hDlg, IDC_WATCH_SLOT,       EnvGetWatchItem(theEnv,"slots"));
        count += SetCheckBox(hDlg, IDC_WATCH_ACTIVE,     EnvGetWatchItem(theEnv,"activations"));
        count += SetCheckBox(hDlg, IDC_WATCH_METHOD,     EnvGetWatchItem(theEnv,"methods"));
        count += SetCheckBox(hDlg, IDC_WATCH_FOCUS,      EnvGetWatchItem(theEnv,"focus"));
        count += SetCheckBox(hDlg, IDC_WATCH_MSGHANDLER, EnvGetWatchItem(theEnv,"message-handlers"));
  
        if (count == IDC_WATCH_COUNT)
          { EnableWindow(GetDlgItem(hDlg,IDC_WATCH_ALL ),FALSE); }
        else if ( count == 0 )
          { EnableWindow(GetDlgItem(hDlg,IDC_WATCH_NONE),FALSE); }
        return (TRUE);

      case WM_COMMAND:
        switch (wParam)
          {  
           case IDC_OK:
             EnvSetWatchItem (GlobalEnv,"compilations",     IsDlgButtonChecked (hDlg, IDC_WATCH_COMPILE),NULL);
             EnvSetWatchItem (GlobalEnv,"statistics",       IsDlgButtonChecked (hDlg, IDC_WATCH_STATS),NULL);
#if DEFTEMPLATE_CONSTRUCT
			 EnvSetWatchItem(GlobalEnv, "facts", IsDlgButtonChecked(hDlg, IDC_WATCH_FACTS), NULL);
#endif
#if OBJECT_SYSTEM
			 EnvSetWatchItem(GlobalEnv, "instances", IsDlgButtonChecked(hDlg, IDC_WATCH_INSTANCE), NULL);
			 EnvSetWatchItem(GlobalEnv, "slots", IsDlgButtonChecked(hDlg, IDC_WATCH_SLOT), NULL);
			 EnvSetWatchItem(GlobalEnv, "messages", IsDlgButtonChecked(hDlg, IDC_WATCH_MESSAGE), NULL);
			 EnvSetWatchItem(GlobalEnv, "message-handlers", IsDlgButtonChecked(hDlg, IDC_WATCH_MSGHANDLER), NULL);
#endif
#if DEFFUNCTION_CONSTRUCT
			 EnvSetWatchItem(GlobalEnv, "deffunctions", IsDlgButtonChecked(hDlg, IDC_WATCH_DEFFUN), NULL);
#endif
#if DEFRULE_CONSTRUCT
			 EnvSetWatchItem(GlobalEnv, "rules", IsDlgButtonChecked(hDlg, IDC_WATCH_RULES), NULL);
			 EnvSetWatchItem(GlobalEnv, "activations", IsDlgButtonChecked(hDlg, IDC_WATCH_ACTIVE), NULL);
			 EnvSetWatchItem(GlobalEnv, "focus", IsDlgButtonChecked(hDlg, IDC_WATCH_FOCUS), NULL);
#endif
#if DEFGENERIC_CONSTRUCT
			 EnvSetWatchItem(GlobalEnv, "generic-functions", IsDlgButtonChecked(hDlg, IDC_WATCH_GENERIC), NULL);
			 EnvSetWatchItem(GlobalEnv, "methods", IsDlgButtonChecked(hDlg, IDC_WATCH_METHOD), NULL);
#endif
#if DEFGLOBAL_CONSTRUCT
			 EnvSetWatchItem(GlobalEnv, "globals", IsDlgButtonChecked(hDlg, IDC_WATCH_GLOBAL), NULL);
#endif
             SaveWatchInformation();
             EndDialog(hDlg,IDOK);
             return (TRUE);
             
           case IDC_CANCEL:
             EndDialog(hDlg,IDOK);
             return (TRUE);

           case IDC_WATCH_COMPILE:
           case IDC_WATCH_FACTS:
           case IDC_WATCH_INSTANCE:
           case IDC_WATCH_RULES:
           case IDC_WATCH_GENERIC:
           case IDC_WATCH_MESSAGE:
           case IDC_WATCH_DEFFUN:
           case IDC_WATCH_STATS:
           case IDC_WATCH_GLOBAL:
           case IDC_WATCH_SLOT:
           case IDC_WATCH_METHOD:
           case IDC_WATCH_MSGHANDLER:
           case IDC_WATCH_ACTIVE:
           case IDC_WATCH_FOCUS:
             if (IsDlgButtonChecked(hDlg,(int) wParam))
               {  
                SetCheckBox(hDlg,(unsigned short) wParam,0);
                count--;
               }
             else
               {  
                SetCheckBox(hDlg,(unsigned short) wParam,1);
                count++;
               }
             
             if (count == IDC_WATCH_COUNT )
               { EnableWindow(GetDlgItem(hDlg, IDC_WATCH_ALL ), FALSE); }
             else if ( count == 0 )
               { EnableWindow(GetDlgItem(hDlg, IDC_WATCH_NONE), FALSE); }
             else
               {  
                EnableWindow(GetDlgItem(hDlg, IDC_WATCH_ALL ), TRUE);
                EnableWindow(GetDlgItem(hDlg, IDC_WATCH_NONE ), TRUE);
               }
             return (TRUE);

           case IDC_WATCH_ALL:
             for ( x=IDC_WATCH_START; x<=IDC_WATCH_END; x++)
               { SetCheckBox(hDlg,x,1); }
             EnableWindow(GetDlgItem(hDlg, IDC_WATCH_ALL ), FALSE);
             EnableWindow(GetDlgItem(hDlg, IDC_WATCH_NONE), TRUE);
             count = IDC_WATCH_COUNT;
             return ( TRUE );

           case IDC_WATCH_NONE:
             for ( x=IDC_WATCH_START; x<=IDC_WATCH_END; x++)
               { SetCheckBox(hDlg,x,0); }
             EnableWindow(GetDlgItem(hDlg, IDC_WATCH_NONE), FALSE);
             EnableWindow(GetDlgItem(hDlg, IDC_WATCH_ALL ), TRUE);
             count = 0;
             return ( TRUE );
          }  
     }  

   return ( FALSE );
  }

