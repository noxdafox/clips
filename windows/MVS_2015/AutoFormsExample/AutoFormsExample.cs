using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

using CLIPSNET;

namespace AutoFormsExample
  {
   public partial class AutoFormsExample : Form
     {
      private enum InterviewState { GREETING, INTERVIEW, CONCLUSION };

      private CLIPSNET.Environment clips = new CLIPSNET.Environment();
      private String lastAnswer = null;
      private String relationAsserted = null;
      private List<string> variableAsserts = new List<string>();
      private List<string> priorAnswers = new List<string>();
      private InterviewState interviewState;
      
      public AutoFormsExample()
        {
         InitializeComponent();
         clips.LoadFromResource("AutoFormsExample","AutoFormsExample.auto.clp");
         clips.LoadFromResource("AutoFormsExample","AutoFormsExample.auto_en.clp");
         clips.Reset();  
        }

      protected override void OnLoad(EventArgs e)
         {
          base.OnLoad(e);
          ProcessRules();
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
             nextButton.Text = "Restart";
             prevButton.Visible = true;
             choicesPanel.Visible = false;
            }
          else if (fv.GetSlotValue("state").ToString().Equals("greeting"))
            {
             interviewState = InterviewState.GREETING;
             nextButton.Tag = "Next";
             nextButton.Text = "Next >";
             prevButton.Visible = false;
             choicesPanel.Visible = false;
            }
          else
            {
             interviewState = InterviewState.INTERVIEW;
             nextButton.Tag = "Next";
             nextButton.Text = "Next >";
             prevButton.Visible = true;
             choicesPanel.Visible = true;
           }

          /*=====================*/
          /* Set up the choices. */
          /*=====================*/
          
          choicesPanel.Controls.Clear();

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
             rButton.Text = buttonText;
             if (((lastAnswer != null) && buttonAnswer.Equals(lastAnswer)) ||
                 ((lastAnswer == null) && buttonAnswer.Equals(selected)))
               { rButton.Checked = true; }
             else
               { rButton.Checked = false; }

             rButton.Tag = buttonAnswer;
             rButton.Visible = true;
             rButton.AutoSize = true;
             choicesPanel.Controls.Add(rButton);

             if (firstButton == null)
               { firstButton = rButton; }
            }

         if ((GetCheckedChoiceButton() == null) && (firstButton != null))
           { firstButton.Checked = true; }

         /*====================================*/
         /* Set the label to the display text. */
         /*====================================*/

         relationAsserted = ((LexemeValue) fv.GetSlotValue("relation-asserted")).GetLexemeValue();

         /*====================================*/
         /* Set the label to the display text. */
         /*====================================*/

         String messageString = ((StringValue) fv.GetSlotValue("display")).GetStringValue();
         int preferredWidth = ComputeLabelWidth(messageString);

         messageLabel.Text = messageString;
         messageLabel.Size = new Size(preferredWidth,messageLabel.Height);
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
               theString = "(" + relationAsserted + " " + theAnswer + ")";
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
         foreach (RadioButton control in choicesPanel.Controls) 
           {
            if (control.Checked)
              { return control; }
           }

         return null;
        }

      private void OnClickButton(object sender, EventArgs e)
        {
         Button button = sender as Button;

         if (button.Tag.Equals("Next"))
           { NextButtonAction(); }
         else if (button.Tag.Equals("Restart"))
           { NextButtonAction(); }
         else if (button.Tag.Equals("Prev"))
           { PrevButtonAction(); }
        }

      /*********************/
      /* ComputeLabelWidth */
      /*********************/
      private int ComputeLabelWidth(
        String theString)
        {
         Size theSize;
         Size attemptSize = new Size();
         int reductions = 0;

         /*=============================================================*/
         /* Determine the width and height needed to display the string */
         /* with the maximum width set to the width of the window.      */
         /*=============================================================*/

         attemptSize.Width = this.Width;
         attemptSize.Height = this.messageLabel.Height;

         theSize = TextRenderer.MeasureText(theString,this.messageLabel.Font,attemptSize,TextFormatFlags.WordBreak );
         int initialWidth = theSize.Width;
         int initialHeight = theSize.Height;

         if (initialWidth <= 12) 
           { return initialWidth; }

         /*======================================================*/
         /* Reduce the width until the string can no longer fit  */
         /* within the initial height determined for the string. */
         /*======================================================*/

         attemptSize.Width = initialWidth - 12;
         theSize = TextRenderer.MeasureText(theString,this.messageLabel.Font,attemptSize,TextFormatFlags.WordBreak);

         while ((initialHeight >= theSize.Height) && (attemptSize.Width > 12))
           {
            reductions++;
            attemptSize.Width -= 12;
            theSize = TextRenderer.MeasureText(theString,this.messageLabel.Font,attemptSize,TextFormatFlags.WordBreak);
           }
           
         /*============================================*/
         /* Return the computed width that will evenly */
         /* distribute the string over several lines.  */
         /*============================================*/

         int finalWidth = initialWidth - (12 * reductions);

         return finalWidth;
        }

     }
  }
