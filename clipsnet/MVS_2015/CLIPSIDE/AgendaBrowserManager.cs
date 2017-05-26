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
         return browsers.Count();
        }

      /*****************/  
      /* CreateBrowser */
      /*****************/
      public AgendaBrowser CreateBrowser()
        {
         AgendaBrowser theBrowser = new AgendaBrowser(ide);

         browsers.Add(theBrowser);

         if (! ide.dialog.GetExecuting())
           {
            if (browsers.Count == 1)
              { FetchData(); }

             theBrowser.UpdateData(focusStack,agendaMap);
           }

         return theBrowser;
        }
      
      /*****************/  
      /* RemoveBrowser */
      /*****************/
      public void RemoveBrowser(
        AgendaBrowser theBrowser)
        {
         browsers.Remove(theBrowser);
         if (browsers.Count == 0)
           {
            focusStack = null;
            agendaMap = null;
           }
        }
    
     /******************/
     /* ManagesBrowser */
     /******************/
     public bool ManagesBrowser(
       AgendaBrowser theBrowser)
       {
        return browsers.Contains(theBrowser);
       }

      /*************/
      /* FetchData */
      /*************/
      private void FetchData() // TBD Synchronized?
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
         if (browsers.Count == 0) return;

         FetchData();

         foreach(AgendaBrowser theBrowser in browsers)
           { UpdateBrowser(theBrowser); }
        }

      /******************************/
      /* UpdateAgendaBrowserButtons */
      /******************************/
      public void UpdateAgendaBrowserButtons(
        bool isExecuting)
        {
         if (browsers.Count == 0) return;

         foreach(AgendaBrowser theBrowser in browsers)
           { theBrowser.UpdateButtons(isExecuting); }
        }

      /*******/
      /* ... */
      /*******/

     }
  }
