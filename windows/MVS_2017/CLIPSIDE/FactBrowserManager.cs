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
      private List<EntityBrowser> browsers = new List<EntityBrowser>();
   
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
      
      /****************/
      /* BrowserCount */
      /****************/
      public int BrowserCount()
        {
         lock(browsers)
           {
            return browsers.Count();
           }
        }

      /*****************/  
      /* CreateBrowser */
      /*****************/
      public EntityBrowser CreateBrowser()
        {
         EntityBrowser theBrowser = new EntityBrowser(ide);

         lock(browsers)
           {
            browsers.Add(theBrowser);

            if (! ide.dialog.GetExecuting())
              {
               if (browsers.Count == 1)
                 { FetchData(); }

                theBrowser.UpdateData(modules,entities,scopes);
              }
           }
         return theBrowser;
        }
      
      /*****************/  
      /* RemoveBrowser */
      /*****************/
      public void RemoveBrowser(
        EntityBrowser theBrowser)
        {
         lock(browsers)
           {
            browsers.Remove(theBrowser);
            if (browsers.Count == 0)
              {
               modules = null;
               entities = null;
               scopes = null;
              }
           }
        }
     
     /******************/
     /* ManagesBrowser */
     /******************/
     public bool ManagesBrowser(
       EntityBrowser theBrowser)
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
         modules = ide.GetEnvironment().GetModuleList();
         entities = ide.GetEnvironment().GetFactList();
         scopes = ide.GetEnvironment().GetFactScopes();
        }

      /*****************/
      /* UpdateBrowser */
      /*****************/
      private void UpdateBrowser(
        EntityBrowser theBrowser)
        {
         theBrowser.UpdateData(modules,entities,scopes);
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

            foreach(EntityBrowser theBrowser in browsers)
              { UpdateBrowser(theBrowser); }
           }
        }

      /*******/
      /* ... */
      /*******/

     }
  }
