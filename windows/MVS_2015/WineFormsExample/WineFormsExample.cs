using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

using CLIPSNET;

namespace WineFormsExample
  {
   public partial class WineFormsExample : Form
     {
      private class WineRecommendation 
        {
         public string WineName { get; set; }
         public int Certainty { get; set; }
        }


      String [] preferredColorNames = { "Don't Care", "Red", "White" };
      String [] preferredBodyNames = { "Don't Care", "Light", "Medium", "Full" };
      String [] preferredSweetnessNames = { "Don't Care", "Dry", "Medium", "Sweet" };

      String [] mainCourseNames = { "Don't Know", "Beef", "Pork", "Lamb", "Turkey", "Chicken", "Duck", "Fish", "Other" };
      String [] sauceNames = { "Don't Know", "None", "Spicy", "Sweet", "Cream", "Other" };
      String [] flavorNames = { "Don't Know", "Delicate", "Average", "Strong" };

      String [] preferredColorChoices;
      String [] preferredBodyChoices;
      String [] preferredSweetnessChoices;

      String [] mainCourseChoices;
      String [] sauceChoices;
      String [] flavorChoices;

      private CLIPSNET.Environment clips;
      private bool formLoaded = false;

      public WineFormsExample()
        {
         InitializeComponent();

         preferredColorChoices = GenerateChoices(preferredColorNames);
         preferredBodyChoices = GenerateChoices(preferredBodyNames);
         preferredSweetnessChoices = GenerateChoices(preferredSweetnessNames);
         
         mainCourseChoices = GenerateChoices(mainCourseNames);
         sauceChoices = GenerateChoices(sauceNames);
         flavorChoices = GenerateChoices(flavorNames);

         colorComboBox.DataSource = preferredColorChoices;
         bodyComboBox.DataSource = preferredBodyChoices;
         sweetnessComboBox.DataSource = preferredSweetnessChoices;

         mainCourseComboBox.DataSource = mainCourseChoices;
         sauceComboBox.DataSource = sauceChoices;
         flavorComboBox.DataSource = flavorChoices;

         clips = new CLIPSNET.Environment();
         clips.LoadFromResource("WineFormsExample","WineFormsExample.wine.clp");
        }

      private String [] GenerateChoices(
        String [] names)
        {
         String [] choices = new String[names.Count()];

         for (int i = 0; i < names.Count(); i++)
           { choices[i] = names[i]; }

         return choices;
        }

      private void OnChange(object sender, EventArgs e)
        {
         if (formLoaded)
           { RunWine(); }
        }

      private void OnLoad(object sender, EventArgs e)
        {
         RunWine();
         formLoaded = true;
        }

      private void RunWine()
        {
         clips.Reset();

         string item = (string) colorComboBox.SelectedValue;

         if (item.Equals("Red")) 
           { clips.AssertString("(attribute (name preferred-color) (value red))"); }
         else if (item.Equals("White"))
           { clips.AssertString("(attribute (name preferred-color) (value white))"); } 
         else
           { clips.AssertString("(attribute (name preferred-color) (value unknown))"); }

         item = (string) bodyComboBox.SelectedValue;

         if (item.Equals("Light")) 
           { clips.AssertString("(attribute (name preferred-body) (value light))"); } 
         else if (item.Equals("Medium")) 
           { clips.AssertString("(attribute (name preferred-body) (value medium))"); }
         else if (item.Equals("Full"))
           { clips.AssertString("(attribute (name preferred-body) (value full))"); } 
         else
           { clips.AssertString("(attribute (name preferred-body) (value unknown))"); }

         item = (string) sweetnessComboBox.SelectedValue;
         
         if (item.Equals("Dry")) 
           { clips.AssertString("(attribute (name preferred-sweetness) (value dry))"); } 
         else if (item.Equals("Medium")) 
           { clips.AssertString("(attribute (name preferred-sweetness) (value medium))"); } 
         else if (item.Equals("Sweet")) 
           { clips.AssertString("(attribute (name preferred-sweetness) (value sweet))"); } 
         else 
           { clips.AssertString("(attribute (name preferred-sweetness) (value unknown))"); }

         item = (string) mainCourseComboBox.SelectedValue;

         if (item.Equals("Beef") ||
             item.Equals("Pork") ||
             item.Equals("Lamb")) 
           {
            clips.AssertString("(attribute (name main-component) (value meat))");
            clips.AssertString("(attribute (name has-turkey) (value no))");
           }
         else if (item.Equals("Turkey"))
           {
            clips.AssertString("(attribute (name main-component) (value poultry))");
            clips.AssertString("(attribute (name has-turkey) (value yes))");
           }
         else if (item.Equals("Chicken") ||
                  item.Equals("Duck"))
           {
            clips.AssertString("(attribute (name main-component) (value poultry))");
            clips.AssertString("(attribute (name has-turkey) (value no))");
           } 
         else if (item.Equals("Fish")) 
           {
            clips.AssertString("(attribute (name main-component) (value fish))");
            clips.AssertString("(attribute (name has-turkey) (value no))");
           } 
         else if (item.Equals("Other")) 
           {
            clips.AssertString("(attribute (name main-component) (value unknown))");
            clips.AssertString("(attribute (name has-turkey) (value no))");
           }
         else
           {
            clips.AssertString("(attribute (name main-component) (value unknown))");
            clips.AssertString("(attribute (name has-turkey) (value unknown))");
           }

         item = (string) sauceComboBox.SelectedValue;

         if (item.Equals("None")) 
           { clips.AssertString("(attribute (name has-sauce) (value no))"); } 
         else if (item.Equals("Spicy")) 
           {
            clips.AssertString("(attribute (name has-sauce) (value yes))");
            clips.AssertString("(attribute (name sauce) (value spicy))");
           }
         else if (item.Equals("Sweet"))
           {
            clips.AssertString("(attribute (name has-sauce) (value yes))");
            clips.AssertString("(attribute (name sauce) (value sweet))");
           }
         else if (item.Equals("Cream"))
           {
            clips.AssertString("(attribute (name has-sauce) (value yes))");
            clips.AssertString("(attribute (name sauce) (value cream))");
           }
         else if (item.Equals("Other"))
           {
            clips.AssertString("(attribute (name has-sauce) (value yes))");
            clips.AssertString("(attribute (name sauce) (value unknown))");
           }
         else
           {
            clips.AssertString("(attribute (name has-sauce) (value unknown))");
            clips.AssertString("(attribute (name sauce) (value unknown))");
           }

         item = (string)flavorComboBox.SelectedValue;
         if (item.Equals("Delicate")) 
           { clips.AssertString("(attribute (name tastiness) (value delicate))"); } 
         else if (item.Equals("Average")) 
           { clips.AssertString("(attribute (name tastiness) (value average))"); } 
         else if (item.Equals("Strong")) 
           { clips.AssertString("(attribute (name tastiness) (value strong))"); } 
         else 
           { clips.AssertString("(attribute (name tastiness) (value unknown))"); }

         clips.Run();

         UpdateWines();
        }

      private void UpdateWines() 
        {
         string evalStr = "(WINES::get-wine-list)";
         List<WineRecommendation> wineList = new List<WineRecommendation>();

         foreach (FactAddressValue fv in clips.Eval(evalStr) as MultifieldValue)
           {
            int certainty = (int) ((NumberValue) fv["certainty"]);

            String wineName = ((LexemeValue) fv["value"]).Value;

            wineList.Add(new WineRecommendation() { WineName = wineName, Certainty = certainty });
           }

         resultsDataGridView.DataSource = wineList;
         resultsDataGridView.ClearSelection();
        }
     }
  }
