using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Collections;

using CLIPSNET;

namespace CLIPSIDE
  {
   class FactBrowserManager
     {
      private List<FactBrowser> browsers = new List<FactBrowser>();
   
      private List<Module> modules;
      private List<FactInstance> entities;
      private Dictionary<ulong,BitArray> scopes;

      private MainWindow ide;

      // private String ENTITY_NAME = "Fact";
      // private static int entityCount = 1;
      
      /**********************/
      /* FactBrowserManager */
      /**********************/
      public FactBrowserManager(
        MainWindow theIDE) 
        {
         ide = theIDE;

         modules = new List<Module>();
         entities = new List<FactInstance>();
         scopes = new Dictionary<ulong,BitArray>();
        }
      
      /*****************/  
      /* CreateBrowser */
      /*****************/
      public FactBrowser CreateBrowser()
        {
         FactBrowser theBrowser = new FactBrowser(ide);

         browsers.Add(theBrowser);

         if (! ide.dialog.GetExecuting())
           {
            if (browsers.Count == 1)
              { FetchData(); }

             theBrowser.UpdateData(modules,entities,scopes);
           }

         return theBrowser;
        }
      
      /*****************/  
      /* RemoveBrowser */
      /*****************/
      public void RemoveBrowser(
        FactBrowser theBrowser)
        {
         browsers.Remove(theBrowser);
         if (browsers.Count == 0)
           {
            modules = null;
            entities = null;
            scopes = null;
           }
        }

      /*************/
      /* FetchData */
      /*************/
      private void FetchData() // TBD Synchronized?
        {
         modules = ide.GetEnvironment().GetModuleList();
         entities = ide.GetEnvironment().GetFactList();
         scopes = ide.GetEnvironment().GetFactScopes();

         /*
         agendaMap = new Dictionary<Focus, Agenda>();

         foreach (Focus theFocus in focusStack)
           {
            if (! agendaMap.ContainsKey(theFocus))
              {
               Agenda theAgenda = ide.GetEnvironment().GetAgenda(theFocus);
               agendaMap.Add(theFocus,theAgenda);
              }
           }
        */
        }

      /*****************/
      /* UpdateBrowser */
      /*****************/
      private void UpdateBrowser(
        FactBrowser theBrowser)
        {
         theBrowser.UpdateData(modules,entities,scopes);
        }

      /*********************/
      /* UpdateAllBrowsers */
      /*********************/
      public void UpdateAllBrowsers()
        {
         if (browsers.Count == 0) return;

         FetchData();

         foreach(FactBrowser theBrowser in browsers)
           { UpdateBrowser(theBrowser); }
        }

      /*******/
      /* ... */
      /*******/

     }
  }
