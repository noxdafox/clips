using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Threading;
using System.Windows.Threading;
using System.ComponentModel;

using CLIPSNET;

namespace CLIPSIDE
  {
   public class FactInstanceCollection : ObservableCollection<FactInstance>
     {
      public FactInstanceCollection(List<FactInstance> theEntityList) : base(theEntityList)
        {
        }
     }

   public class SlotValueCollection : ObservableCollection<SlotValue>
     {
      public SlotValueCollection(List<SlotValue> theSlotValues) : base(theSlotValues)
        {
        }
     }

   public partial class EntityBrowser : UserControl
     { 
      private MainWindow ide = null;
      private List<Module> modules;
      private List<FactInstance> entityList;
      private Dictionary<ulong,BitArray> scopes;
      private CollectionViewSource entitySourceList;
      private CollectionViewSource slotSourceList;
      private FactInstanceCollection entityCollection;
      private SlotValueCollection slotCollection;

      /*****************/
      /* EntityBrowser */
      /*****************/
      public EntityBrowser()
        {
         InitializeComponent();
        }

      public EntityBrowser(MainWindow theMW) : this()
        {
         ide = theMW;
         if (ide != null)
           { 
           }
        }
                
      /*************/
      /* DetachIDE */
      /*************/
      public void DetachIDE()
        {
        }
        
     /**************/
     /* UpdateData */
     /**************/
     public void UpdateData(
        List<Module> theModules,
        List<FactInstance> theEntityList,
        Dictionary<ulong,BitArray> theScopes)
        {
         Application.Current.Dispatcher.Invoke(DispatcherPriority.Normal,
                                                new Action(delegate { AssignData(theModules,theEntityList,theScopes); }));
        }
       
      /**********************/
      /* CreateEntitySource */
      /**********************/
      private void CreateEntitySource()
        {
         entityCollection = new FactInstanceCollection(entityList);
         entitySourceList = new CollectionViewSource() { Source = entityCollection };

         entitySourceList.Filter += EntityFilter;

         entityDataGridView.ItemsSource = entitySourceList.View;
        }  

      /************************/
      /* CreateSlotListSource */
      /************************/
      private void CreateSlotListSource(
        FactInstance theEntity)
        {
         slotCollection = new SlotValueCollection(theEntity.GetSlotValues());
         slotSourceList = new CollectionViewSource() { Source = slotCollection };

         slotSourceList.Filter += SlotFilter;

         slotDataGridView.ItemsSource = slotSourceList.View;
        }  
      
      /**************/
      /* AssignData */
      /**************/
      private void AssignData(
        List<Module> theModules,
        List<FactInstance> theEntityList,
        Dictionary<ulong,BitArray> theScopes)
        {
         modules = theModules;
         entityList = theEntityList;
         scopes = theScopes;

         moduleDataGridView.ItemsSource = theModules;

         CreateEntitySource();
 
         if (theModules.Count == 0)
           { moduleDataGridView.SelectedItem = null; }
         else
           { moduleDataGridView.SelectedItem = theModules.First(); }

         if (entityDataGridView.Items.Count != 0)
           {
            FactInstance theEntity = entityDataGridView.Items[0] as FactInstance;

            entityDataGridView.SelectedItem = theEntity;

            CreateSlotListSource(theEntity);

            if (slotDataGridView.Items.Count != 0)
              { 
               SlotValue theSlotValue = slotDataGridView.Items[0] as SlotValue;
               slotDataGridView.SelectedItem = theSlotValue; 
              }
           }
         else
           { 
            slotDataGridView.ItemsSource = null; 
            slotSourceList = null;
            entitySourceList = null;
            entityCollection = null;
            slotCollection = null;
           }
        }

      /*****************/
      /* ModuleChanged */
      /*****************/
      private void ModuleChanged(object sender,SelectionChangedEventArgs e)
        {
         if (e.RemovedItems.Count != 1) return;
         if (e.AddedItems.Count != 1) return;

         /*===============================================*/
         /* Reapply the entity filter to display the list */
         /* of facts/instances for the selected module.   */
         /*===============================================*/

         if (entitySourceList == null) return;

         entitySourceList.Filter -= EntityFilter;
         entitySourceList.Filter += EntityFilter;

         ReselectEntityAndSlots();
        }

      /*****************/
      /* EntityChanged */
      /*****************/
      private void EntityChanged(object sender,SelectionChangedEventArgs e)
        {
         FactInstance theEntity;

         if (e.RemovedItems.Count != 1) return;
         if (e.AddedItems.Count != 1) return;

         theEntity = (FactInstance) e.AddedItems[0];
         
         CreateSlotListSource(theEntity);

         if (slotDataGridView.Items.Count != 0)
           {
            SlotValue theSV = slotDataGridView.Items[0] as SlotValue;
            slotDataGridView.SelectedItem = theSV;
           }
        }

      /****************/ 
      /* EntityFilter */
      /****************/ 
      private void EntityFilter(object sender, FilterEventArgs e)
        {
         FactInstance fact = e.Item as FactInstance;

         if (fact == null) return;

         int moduleIndex = moduleDataGridView.SelectedIndex;

         if (moduleIndex < 0) 
           { moduleIndex = 0; }

         BitArray theArray = scopes[fact.TypeAddress];

         if (theArray.Get(moduleIndex))
           { 
            if (fact.SearchForString(searchField.Text))
              { e.Accepted = true; }
            else
              { e.Accepted = false; }
           }
         else
           { e.Accepted = false; }
        }

      /**************/ 
      /* SlotFilter */
      /**************/ 
      private void SlotFilter(object sender, FilterEventArgs e)
        {
         SlotValue slot = e.Item as SlotValue;

         if (slot == null) return;

         if (! slot.IsDefault)
           { e.Accepted = true; }
         else if (displayDefaultsCheckBox.IsChecked == true)
           { e.Accepted = true; }
         else 
           { e.Accepted = false; }
        }

      /***********************************/ 
      /* DisplayDefaultedValuesUnchecked */
      /***********************************/ 
      private void DisplayDefaultedValuesUnchecked(object sender, RoutedEventArgs e)
        {
         if (slotSourceList == null) return;
         slotSourceList.Filter -= SlotFilter;
         slotSourceList.Filter += SlotFilter;
        }

      /*********************************/ 
      /* DisplayDefaultedValuesChecked */
      /*********************************/ 
      private void DisplayDefaultedValuesChecked(object sender, RoutedEventArgs e)
        {
         if (slotSourceList == null) return;
         slotSourceList.Filter -= SlotFilter;
         slotSourceList.Filter += SlotFilter;
        }
        
      /**************************/
      /* ReselectEntityAndSlots */
      /**************************/
      private void ReselectEntityAndSlots()
        {
         if (entityDataGridView.Items.Count != 0)
           {
            FactInstance theEntity;

            theEntity = entityDataGridView.Items[0] as FactInstance;

            entityDataGridView.SelectedItem = theEntity;

            CreateSlotListSource(theEntity);
            if (slotDataGridView.Items.Count != 0)
              {
               SlotValue theSV = slotDataGridView.Items[0] as SlotValue;
               slotDataGridView.SelectedItem = theSV;
              }
           }
         else
           {
            slotDataGridView.ItemsSource = null;
            slotSourceList = null;
            slotCollection = null;
           }
        }

      /********************/ 
      /* SearchFieldKeyUp */
      /********************/ 
      private void SearchFieldKeyUp(object sender, KeyEventArgs e)
        {
         if ((e.Key == System.Windows.Input.Key.Return) ||
             (e.Key == System.Windows.Input.Key.Enter))
           {
            if (entitySourceList != null)
              {
               entitySourceList.Filter -= EntityFilter;
               entitySourceList.Filter += EntityFilter;

               ReselectEntityAndSlots();
              }
           }
        }
     } 
  }
