   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                DIALOG2 HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides dialogs for Windows interface.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Gary D. Riley                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_dialog2

#define _H_dialog2

#ifdef LOCALE
#undef LOCALE
#endif

#ifdef _DIALOG2_SOURCE_
#define LOCALE
#else
#define LOCALE extern
#endif

   LOCALE INT_PTR CALLBACK               DeffactsManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DeftemplateManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DeffunctionManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DefglobalManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DefglobalManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DefruleManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               AgendaManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DefinstancesManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DefgenericManager(HWND,UINT,WPARAM,LPARAM); 
   LOCALE INT_PTR CALLBACK               DefmethodsManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DefclassManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               DefmessageHandlerManager(HWND,UINT,WPARAM,LPARAM);
   LOCALE INT_PTR CALLBACK               CommandComplete(HWND,UINT,WPARAM,LPARAM);

#ifndef _DIALOG2_SOURCE_
   extern char                    CompleteString[];
   extern struct symbolMatch     *GlobalMatches;
#endif

#endif

