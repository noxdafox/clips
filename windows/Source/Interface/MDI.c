   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*       Microsoft Windows Version 3.0  01/31/02       */
   /*                                                     */
   /*         MULTIPLE DOCUMENT INTERFACE MODULE          */
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

#include "stdSDK.h"

#include "setup.h"

#include "display.h"
#include "resource.h"
#include "Frame.h"
#include "mdi.h"
#include "EditUtil.h"

// If you are going to run under Win32s, you must enable the line below.
// CreateMDIWindow does not exist in Win32s.
//#define USE_WM_MDICREATE
BOOL mdi_DisplayContextMenu(HWND,POINT,HMENU);

static void mdi_ConformMenus(HMENU hMaster, HMENU hPopup);

/********************************************************/
/* mdi_DisplayContextMenu: Pops up a text context menu. */
/********************************************************/
BOOL mdi_DisplayContextMenu(
  HWND hwnd, 
  POINT pt, 
  HMENU context)
  {
   HMENU popup;

   if (context == NULL)
     { return FALSE; }
     
   popup = GetSubMenu(context, 0);

   ClientToScreen(hwnd, &pt);
   mdi_ConformMenus(GetMenu(hMainFrame), popup);

   TrackPopupMenu(popup, TPM_LEFTALIGN | TPM_RIGHTBUTTON,
                                pt.x, pt.y, 0, 
                                hMainFrame, NULL);
   return TRUE;
  }

/*******************************************************/
/* mdi_OnContextMenu: Pops up a context-specific menu. */
/*******************************************************/
BOOL mdi_OnContextMenu(
  HWND hwnd, 
  HWND hwndCtl, 
  int xPos, 
  int yPos, 
  HMENU context)
  {
   POINT pt;
   RECT  rc;

   pt.x = xPos;
   pt.y = yPos;
     
   /*================================*/
   /* Bring the window to the front. */
   /*================================*/
   
   if (IsWindow(hwnd))
     { FORWARD_WM_MDIACTIVATE(MDIClientWnd,FALSE,NULL,hwnd,SendMessage); }
  
   /*===============================*/
   /* Get the bounding rectangle of */
   /* the client area of the child. */
   /*===============================*/
   
   GetClientRect(hwnd,&rc);
   
   /*============================*/
   /* Convert the mouse position */
   /* to client coordinates.     */
   /*============================*/
          
   ScreenToClient(hwnd,&pt);
   
   /*======================================*/
   /* If the mouse click was in the client */
   /* area of this child window, display   */
   /* the appropriate popup menu.          */
   /*======================================*/
                            
   if (PtInRect(&rc,pt))
     {
      if (mdi_DisplayContextMenu(hwnd,pt,context))
        { return TRUE; }
     }

   /*============================*/
   /* Otherwise, tell our caller */
   /* that we don't handle it.   */
   /*============================*/
   
   return FALSE;
  }

/*************************************************************/
/* mdi_ConformMenus: For each menu item in hPopup, its state */
/*   is set to be the same state as found in hMaster, unless */
/*   there is no such state in which case it is left alone.  */
/*   This can only work with API level 4 and above.          */
/*************************************************************/
static void mdi_ConformMenus(
  HMENU hMaster, 
  HMENU hPopup)
  {
   unsigned count = (unsigned) GetMenuItemCount(hPopup);
   unsigned i;
   MENUITEMINFO getpopupinfo;
   MENUITEMINFO setpopupinfo;
   MENUITEMINFO masterinfo;
     
   getpopupinfo.cbSize = sizeof(MENUITEMINFO);
   getpopupinfo.fMask  = MIIM_SUBMENU | MIIM_ID;

   setpopupinfo.cbSize = sizeof(MENUITEMINFO);
   setpopupinfo.fMask  = MIIM_STATE;

   masterinfo.cbSize   = sizeof(MENUITEMINFO);
   masterinfo.fMask    = MIIM_STATE;

   for (i = 0; i < count; i++)
     { /* process each */
      VERIFY(GetMenuItemInfo(hPopup, i, TRUE, &getpopupinfo));
      if (getpopupinfo.hSubMenu != NULL)
        { mdi_ConformMenus(hMaster, getpopupinfo.hSubMenu); }
      else
        { 
         GetMenuItemInfo(hMaster, getpopupinfo.wID, FALSE, &masterinfo);
         setpopupinfo.fState = masterinfo.fState;
         SetMenuItemInfo(hPopup, i, TRUE, &setpopupinfo);
        }
     }
  }

/************************************************************/
/* mdi_Create: Creates an MDI child window. CreateMDIWindow */
/*   only exists in Windows/NT and Windows/96. To use in    */
/*   Win32s, you must define USE_WM_MDICREATE.              */
/************************************************************/                                
HWND mdi_Create(
  HWND hwnd, 
  DWORD styles, 
  unsigned classid, 
  char *title,
  int x,
  int y,
  int width,
  int height)
  {
   HINSTANCE hinst = GetWindowInstance(hwnd);
   HWND hwndChild;
   TCHAR Class[80];
     
   LoadString(hinst,classid,Class,DIM(Class));

   hwndChild = CreateMDIWindow(Class,title,0, 
                               x,y,width,height,   
                               hwnd,hinst,0);
     
   if (hwndChild == NULL)
     { ReportError(GetLastError()); } 
   
   return hwndChild;
  }

/*****************************************************/
/* ReportError: Pops up message box reporting error. */
/*****************************************************/
void ReportError(
  DWORD err)
  {
   LPTSTR str;

   FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                 0, err,
                 0, (LPTSTR)&str,
                 0, NULL);
   MessageBox(NULL, str, _T(""), MB_ICONSTOP | MB_OK | MB_TASKMODAL);
   LocalFree(str);
  }
