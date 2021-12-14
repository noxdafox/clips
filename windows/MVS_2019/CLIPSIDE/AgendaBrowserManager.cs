using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using CLIPSNET;

namespace CLIPSIDE
  {
   class AgendaBrowserManager
     {
      private List<AgendaBrowser> browsers = new List<AgendaBrowser>();
      private FocusStack focusStack;
      private Dictionary<Focus,Agenda> agendaMap;
      private MainWindow ide;
      
      /************************/
      /* AgendaBrowserManager */
      /************************/
      public AgendaBrowserManager(
        MainWindow theIDE) 
        {
         ide = theIDE;
         focusStack = new FocusStack();
         agendaMap = new Dictionary<Focus,Agenda>();
        }
            
      /****************/
      /* BrowserCount */
      /****************/
      public int BrowserCount()
        {
         lock (browsers)
           { 
            return browsers.Count();
           }
        }

      /*****************/  
      /* CreateBrowser */
      /*****************/
      public AgendaBrowser CreateBrowser()
        {
         AgendaBrowser theBrowser = new AgendaBrowser(ide);

         lock(browsers)
           {
            browsers.Add(theBrowser);

            if (! ide.dialog.GetExecuting())
              {
               if (browsers.Count == 1)
                 { FetchData(); }

                theBrowser.UpdateData(focusStack,agendaMap);
              }
           }
         return theBrowser;
        }
      
      /*****************/  
      /* RemoveBrowser */
      /*****************/
      public void RemoveBrowser(
        AgendaBrowser theBrowser)
        {
         lock(browsers)
           {
            browsers.Remove(theBrowser);
            if (browsers.Count == 0)
              {
               focusStack = null;
               agendaMap = null;
              }
           }
        }
    
     /******************/
     /* ManagesBrowser */
     /******************/
     public bool ManagesBrowser(
       AgendaBrowser theBrowser)
       {
        lock(browsers)
          {
           return browsers.Contains(theBrowser);
          }
       }

      /*************/
      /* FetchData */
      /*************/
      private void FetchData()
        {
         focusStack = ide.GetEnvironment().GetFocusStack();
         agendaMap = new Dictionary<Focus, Agenda>();

         foreach (Focus theFocus in focusStack)
           {
            if (! agendaMap.ContainsKey(theFocus))
              {
               Agenda theAgenda = ide.GetEnvironment().GetAgenda(theFocus);
               agendaMap.Add(theFocus,theAgenda);
              }
           } 
        }

      /*****************/
      /* UpdateBrowser */
      /*****************/
      private void UpdateBrowser(
        AgendaBrowser theBrowser)
        {
         theBrowser.UpdateData(focusStack,agendaMap);
        }

      /*********************/
      /* UpdateAllBrowsers */
      /*********************/
      public void UpdateAllBrowsers()
        {
         lock(browsers)
           {
            if (browsers.Count == 0) return;

            FetchData();

            foreach(AgendaBrowser theBrowser in browsers)
              { UpdateBrowser(theBrowser); }
           }
        }

      /******************************/
      /* UpdateAgendaBrowserButtons */
      /******************************/
      public void UpdateAgendaBrowserButtons(
        bool isExecuting)
        {
         lock(browsers)
           {
            if (browsers.Count == 0) return;

            foreach(AgendaBrowser theBrowser in browsers)
              { theBrowser.UpdateButtons(isExecuting); }
           }
        }
      
      /**********************/
      /* UpdateBrowserFonts */
      /**********************/
      public void UpdateBrowserFonts()
        {
         lock(browsers)
           {
            if (browsers.Count == 0) return;

            foreach(AgendaBrowser theBrowser in browsers)
              { theBrowser.SetFontFromPreferences(); }
           }
        }

      /*******/
      /* ... */
      /*******/

     }
  }
