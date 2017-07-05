using System;
using System.Collections.Generic;
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
using System.Globalization;

using CLIPSNET;

namespace AnimalWPFExample
  {
   public partial class MainWindow : Window
     {      
      private enum InterviewState { GREETING, INTERVIEW, CONCLUSION };

      private CLIPSNET.Environment clips = new CLIPSNET.Environment();
      private String lastAnswer = null;
      private String relationAsserted = null;
      private List<string> variableAsserts = new List<string>();
      private List<string> priorAnswers = new List<string>();
      private InterviewState interviewState;

      public MainWindow()
        {
         InitializeComponent();
         prevButton.Tag = "Prev";
         clips.LoadFromResource("AnimalWPFExample","AnimalWPFExample.bcengine.clp");
         clips.LoadFromResource("AnimalWPFExample","AnimalWPFExample.animal.clp");
         clips.LoadFromResource("AnimalWPFExample","AnimalWPFExample.animal_en.clp");
         clips.Reset();  
        }

      private void HandleResponse()
         {
          /*===========================*/
          /* Get the current UI state. */
          /*===========================*/

          String evalStr = "(find-fact ((?f UI-state)) TRUE)";
          FactAddressValue fv =  (FactAddressValue) ((MultifieldValue) clips.Eval(evalStr))[0];

          /*========================================*/
          /* Determine the Next/Prev button states. */
          /*========================================*/

          if (fv.GetSlotValue("state").ToString().Equals("conclusion"))
            {
             interviewState = InterviewState.CONCLUSION;
             nextButton.Tag = "Restart";
             nextButton.Content = "Restart";
             prevButton.Visibility = Visibility.Visible;
             choicesPanel.Visibility = Visibility.Collapsed;
            }
          else if (fv.GetSlotValue("state").ToString().Equals("greeting"))
            {
             interviewState = InterviewState.GREETING;
             nextButton.Tag = "Next";
             nextButton.Content = "Next >";
             prevButton.Visibility = Visibility.Collapsed;
             choicesPanel.Visibility = Visibility.Collapsed;
            }
          else
            {
             interviewState = InterviewState.INTERVIEW;
             nextButton.Tag = "Next";
             nextButton.Content = "Next >";
             prevButton.Visibility = Visibility.Visible;
             choicesPanel.Visibility = Visibility.Visible;
           }

          /*=====================*/
          /* Set up the choices. */
          /*=====================*/

          choicesPanel.Children.Clear();

          MultifieldValue damf = (MultifieldValue) fv.GetSlotValue("display-answers");
          MultifieldValue vamf = (MultifieldValue) fv.GetSlotValue("valid-answers");

          String selected = fv.GetSlotValue("response").ToString();
          RadioButton firstButton = null;

          for (int i = 0; i < damf.Count; i++)
            {
             LexemeValue da = (LexemeValue) damf[i];
             LexemeValue va = (LexemeValue) vamf[i];
             RadioButton rButton;
             String buttonName, buttonText, buttonAnswer;

             buttonName = da.GetLexemeValue();
             buttonText = buttonName.Substring(0,1).ToUpperInvariant() + buttonName.Substring(1);
             buttonAnswer = va.GetLexemeValue();

             rButton = new RadioButton();
             rButton.Content = buttonText;
             if (((lastAnswer != null) && buttonAnswer.Equals(lastAnswer)) ||
                 ((lastAnswer == null) && buttonAnswer.Equals(selected)))
               { rButton.IsChecked = true; }
             else
               { rButton.IsChecked = false; }

             rButton.Tag = buttonAnswer;
             rButton.Visibility = Visibility.Visible;
             rButton.Margin = new Thickness(5);
             choicesPanel.Children.Add(rButton);

             if (firstButton == null)
               { firstButton = rButton; }
            }

          if ((GetCheckedChoiceButton() == null) && (firstButton != null))
            { firstButton.IsChecked = true; }

          /*====================================*/
          /* Set the label to the display text. */
          /*====================================*/

          relationAsserted = ((LexemeValue) fv.GetSlotValue("relation-asserted")).GetLexemeValue();

         /*====================================*/
         /* Set the label to the display text. */
         /*====================================*/
        
          String messageString = ((StringValue) fv.GetSlotValue("display")).GetStringValue();
          double theWidth = ComputeTextBoxWidth(messageString);
          messageTextBox.Width = theWidth;
          messageTextBox.MinWidth = theWidth;
          messageTextBox.Text = messageString;
         }

      private void NextButtonAction() 
         {
          String theString;
          String theAnswer;

          lastAnswer = null;

          switch (interviewState)
            {
             case InterviewState.GREETING:
             case InterviewState.INTERVIEW:
               theAnswer = (String) GetCheckedChoiceButton().Tag;
               theString = "(variable (name " + relationAsserted + ") (value " +  theAnswer + "))";
               variableAsserts.Add(theString);
               priorAnswers.Add(theAnswer);
               break;

             case InterviewState.CONCLUSION:
               variableAsserts.Clear();
               priorAnswers.Clear();
               break;
            }

          ProcessRules();
         }

      private void PrevButtonAction()
        {
         lastAnswer = priorAnswers.ElementAt(priorAnswers.Count - 1);
         variableAsserts.RemoveAt(variableAsserts.Count - 1);
         priorAnswers.RemoveAt(priorAnswers.Count - 1);
         ProcessRules();
        }

     private void ProcessRules()
        {
         clips.Reset();
         foreach (String factString in variableAsserts)
           {
            String assertCommand = "(assert " + factString + ")";
            clips.Eval(assertCommand);
           }
         clips.Run();
         HandleResponse();
        }

      private RadioButton GetCheckedChoiceButton()
        {
         foreach (RadioButton control in choicesPanel.Children) 
           {
            if (control.IsChecked == true)
              { return control; }
           }

         return null;
        }

     private void OnClickButton(object sender, RoutedEventArgs e)
        {
         Button button = sender as Button;

         if (button.Tag.Equals("Next"))
           { NextButtonAction(); }
         else if (button.Tag.Equals("Restart"))
           { NextButtonAction(); }
         else if (button.Tag.Equals("Prev"))
           { PrevButtonAction(); }
        }

     private void OnLoad(object sender, RoutedEventArgs e)
       {
        ProcessRules();
       }

     private double ComputeTextBoxWidth(string theString)
       {
        FormattedText theText = new FormattedText(theString,
                                                  CultureInfo.CurrentUICulture,
                                                  FlowDirection.LeftToRight,
                                                  new Typeface(this.messageTextBox.FontFamily, 
                                                               this.messageTextBox.FontStyle, 
                                                               this.messageTextBox.FontWeight, 
                                                               this.messageTextBox.FontStretch),
                                                  this.messageTextBox.FontSize,
                                                  Brushes.Black);

        double availableWidth = this.Width - 30;
        theText.MaxTextWidth = availableWidth;

        double initialWidth = theText.WidthIncludingTrailingWhitespace;
        double initialHeight = theText.Height;
        int reductions = 0;
        if (initialWidth <= 12.0)
          { return initialWidth; }

        theText.MaxTextWidth = initialWidth - 12.0;

        while ((initialHeight >= theText.Height) && 
               (theText.WidthIncludingTrailingWhitespace > 12.0))
          {
           reductions++;
           theText.MaxTextWidth -= 12.0;
          }

        double finalWidth;
        if (reductions == 0)
          { finalWidth = availableWidth; }
        else
          { finalWidth = initialWidth - (12.0 * reductions); }
        
        return finalWidth + 20;
       }
     }
  }
