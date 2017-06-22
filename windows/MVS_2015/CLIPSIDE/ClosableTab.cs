using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace CLIPSIDE
  {
   public delegate void TabClosedDelegate(ClosableTab tabItem);

   public class ClosableTab : TabItem
     {
      public event TabClosedDelegate TabClosedEvent;

      /***************/
      /* Constructor */
      /***************/
      public ClosableTab()
        {
         /*========================================*/
         /* Create an instance of the usercontrol. */
         /*========================================*/

         CloseableHeader closableTabHeader = new CloseableHeader();

         /*===========================================*/
         /* Assign the usercontrol to the tab header. */
         /*===========================================*/

         this.Header = closableTabHeader;
      
         /*==========================================================*/
         /* Attach to the CloseableHeader events (Mouse Enter/Leave, */
         /* Button Click, and Label resize).                         */
         /*==========================================================*/

         closableTabHeader.button_close.MouseEnter += new MouseEventHandler(button_close_MouseEnter);
         closableTabHeader.button_close.MouseLeave += new MouseEventHandler(button_close_MouseLeave);
         closableTabHeader.button_close.Click += new RoutedEventHandler(button_close_Click);
         closableTabHeader.label_TabTitle.SizeChanged += new SizeChangedEventHandler(label_TabTitle_SizeChanged);
        }  
     
      /*************************************************/
      /* Property Title: Set the Title of the TabItem. */
      /*************************************************/
      public string Title
        {
         set
           {
            ((CloseableHeader) Header).label_TabTitle.Content = value;
           }
        }
        
      /***********************************************/
      /* Override OnSelected: Show the Close Button. */
      /***********************************************/
      protected override void OnSelected(
        RoutedEventArgs e)
        {
         base.OnSelected(e);
         ((CloseableHeader) Header).button_close.Visibility = Visibility.Visible;
        }
     
       /*************************************************/
       /* Override OnUnSelected: Hide the Close Button. */
       /*************************************************/
       protected override void OnUnselected(
         RoutedEventArgs e)
         {
          base.OnUnselected(e);
          ((CloseableHeader) Header).button_close.Visibility = Visibility.Hidden;
         }
      
       /*************************************************/
       /* Override OnMouseEnter: Show the Close Button. */
       /*************************************************/
       protected override void OnMouseEnter(
         MouseEventArgs e)
         {
          base.OnMouseEnter(e);
          ((CloseableHeader) Header).button_close.Visibility = Visibility.Visible;
         }
      
       /*************************************************************************/
       /* Override OnMouseLeave: Hide the Close Button (If it is NOT selected). */
       /*************************************************************************/
       protected override void OnMouseLeave(
         MouseEventArgs e)
         {
          base.OnMouseLeave(e);
          if (! IsSelected)
            { ((CloseableHeader) Header).button_close.Visibility = Visibility.Hidden; }
         }
      
      /*********************************************/
      /* Button MouseEnter: When the mouse is over */
      /*   the button, change the color to Red.    */
      /*********************************************/
      void button_close_MouseEnter(
        object sender, 
        MouseEventArgs e)
        {
         ((CloseableHeader) Header).button_close.Foreground = Brushes.Red;
        }
     
      /*******************************************************/
      /* Button MouseLeave: When the mouse is no longer over */
      /*   the button, change the color back to black.       */
      /*******************************************************/
      void button_close_MouseLeave(
        object sender, 
        MouseEventArgs e)
        {
         ((CloseableHeader) Header).button_close.Foreground = Brushes.Black;
        }

      /*********************************************************/
      /* Button Close Click: Remove the Tab (or raise an event */
      /*   indicating a "CloseTab" event has occurred).        */
      /*********************************************************/
      void button_close_Click(
        object sender, 
        RoutedEventArgs e)
        { 
         if (TabClosedEvent != null)
           { TabClosedEvent(this); }

         this.Template = null;

         ((TabControl) Parent).Items.Remove(this);
        }
     
      /**************************************************************/
      /* Label SizeChanged: When the Size of the Label changes (due */ 
      /*   to setting the Title) set position of button properly.   */
      /**************************************************************/
      void label_TabTitle_SizeChanged(object sender, SizeChangedEventArgs e)
        {
         ((CloseableHeader) Header).button_close.Margin = new Thickness(((CloseableHeader) Header).label_TabTitle.ActualWidth + 5, 3, 4, 0);
        }
     }
  }