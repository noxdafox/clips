   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*                 EDIT UTILITY MODULE                 */
   /*******************************************************/

/**************************************************************/
/* Purpose:                                                   */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Christopher J. Ortiz                                  */
/*      Gary Riley                                            */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/**************************************************************/

#include <stdlib.h>
//#include <extras.h>

#include "StdSDK.h"
#include "EditUtil.h"

/************************************************/
/* getAppName: Returns pointer to a displayable */
/*   version of the application's file name.    */
/************************************************/
LPCTSTR getAppName()
  {
   static TCHAR AppName [_MAX_FNAME + 1] = TEXT("") ;

   //DWORD   Length ;
   HMODULE hmod ;
   TCHAR   AppFileSpec [_MAX_PATH + 1] ;

   // Get the module handle of this application
   hmod = GetModuleHandle (NULL) ;

   // Get the full path and file name for this application
   GetModuleFileName (hmod, AppFileSpec, DIM (AppFileSpec)) ;

   // Convert to the displayable filename
   // Don't cache the display name in case user changes preferences
   
   VERIFY(GetFileTitle (AppFileSpec, AppName, DIM (AppName))) ;

   // Return the file name component
   return AppName ;
  }

/***************************************/
/* getHelpFileName: Returns pointer to */
/*   full help file path string.       */
/***************************************/
LPCTSTR getHelpFileName(
  char *overrideName)
  {
   static TCHAR HelpFile    [_MAX_PATH] = TEXT("") ;
   HMODULE hmod ;
   TCHAR   AppFileSpec [_MAX_PATH] ;
   TCHAR   AppDrive    [_MAX_DRIVE] ;
   TCHAR   AppPath     [_MAX_DIR] ;
   TCHAR   AppFileName [_MAX_FNAME] ;
   TCHAR  *useName;

   // Return help file name if already known
   if (TEXT('\0') != HelpFile [0])
     { return HelpFile; }

   // Get the module handle of this application
   hmod = GetModuleHandle (NULL) ;

   // Get the full path and file name for this application
   GetModuleFileName (hmod, AppFileSpec, DIM (AppFileSpec)) ;
   //ASSERT (0 != Length) ;

   // Extract the drive, path and file name components ignoring extension
   _splitpath (AppFileSpec, AppDrive, AppPath, AppFileName, NULL) ;
   if (overrideName == NULL)
     { useName = AppFileName; }
   else
     { useName = overrideName; }
   // Combine the drive, path and file name components with a .hlp extension
   _makepath (HelpFile, AppDrive, AppPath, useName, TEXT(".hlp")) ;

   return HelpFile;
  }


/****************************************/
/* centerWindow: Centers a child window */
/*   within its parent window.          */
/****************************************/
BOOL centerWindow(
  HWND hwndCentered, 
  HWND hwndPreferredOwner)
  {
   BOOL  Result ;
   POINT OwnerCenter , CenteredUL ;
   RECT  WindowRect, OwnerRect, WorkArea ;
   SIZE  CenteredWindow ;

   ASSERT (NULL != hwndCentered) ;
   ASSERT (IsWindow (hwndCentered)) ;

   /*===========================================*/
   /* If a preferred owner isn't specified, use */
   /* the owner of the window to be centered.   */
   /*===========================================*/
   
   if (NULL == hwndPreferredOwner)
     { hwndPreferredOwner = GetWindowOwner(hwndCentered); }

   /*=====================================*/     
   /* Get the rectangle for the workarea. */
   /*=====================================*/
   
   Result = SystemParametersInfo (SPI_GETWORKAREA, sizeof (RECT), &WorkArea,  0) ;

   // If the above call failed, the new shell probably isn't running
   // therefore there is no tray and no workarea.

   // Use the screen size as the workarea size.
   if (! Result) 
     {
      SetRect(&WorkArea,0,0, 
              GetSystemMetrics (SM_CXSCREEN),
              GetSystemMetrics (SM_CYSCREEN)) ;
     }

   // Center around the owner window, if one,
   // otherwise center in the work area
   if (NULL != hwndPreferredOwner) 
     {
      ASSERT (IsWindow (hwndPreferredOwner)) ;
      GetWindowRect (hwndPreferredOwner, &OwnerRect) ;
     }
   else
     { OwnerRect = WorkArea; }

   // Preferred center point
   OwnerCenter.x = (OwnerRect.left + OwnerRect.right) / 2 ;
   OwnerCenter.y = (OwnerRect.top + OwnerRect.bottom) / 2 ;

   // Get the centered window's rectangle and compute height/width
   GetWindowRect (hwndCentered, &WindowRect) ;
   CenteredWindow.cx = WindowRect.right - WindowRect.left ;
   CenteredWindow.cy = WindowRect.bottom - WindowRect.top ;

   // Center window in owner horizontally
   // Calculates the left side coordinate
   CenteredUL.x = OwnerCenter.x - CenteredWindow.cx / 2 ;

   // Center window in owner vertically
   // Calculates the top side coordinate
   CenteredUL.y = OwnerCenter.y - CenteredWindow.cy / 2 ;

   // If the left edge of the centered window is clipped by the workarea
   // move the window horizontally to the right until left edge is exposed.
   if (CenteredUL.x < WorkArea.left)
     { CenteredUL.x = WorkArea.left; }

   // If the right edge of the centered window is clipped by the workarea
   // move the window horizontally to the left until right edge is exposed.
   else if (CenteredUL.x + CenteredWindow.cx > WorkArea.right)
     { CenteredUL.x = WorkArea.right - CenteredWindow.cx ; }

   // If the top edge of the centered window is clipped by the workarea
   // move the window vertically down until top edge is exposed.
   if (CenteredUL.y < WorkArea.top)
     { CenteredUL.y = WorkArea.top; }

   // If the bottom edge of the centered window is clipped by the workarea
   // move the window vertically up until bottom edge is exposed.
   else if (CenteredUL.y + CenteredWindow.cy > WorkArea.bottom)
     { CenteredUL.y = WorkArea.bottom - CenteredWindow.cy; }

   /*================================*/
   /* Reposition the window centered */
   /* within visibility constraints. */
   /*================================*/
    
   return SetWindowPos(hwndCentered, NULL, 
                       CenteredUL.x, CenteredUL.y,
                       0, 0, SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOZORDER) ;
  }

