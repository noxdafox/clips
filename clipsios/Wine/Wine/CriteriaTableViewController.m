//
//  CriteriaTableViewController.m
//  Wine
//
//  Created by Programmer X on 4/28/16.
//  Copyright © 2016 Secret Society Software. All rights reserved.
//

#import "CriteriaTableViewController.h"
#import "OptionsTableViewController.h"
#import "Wine.h"

#import <CLIPSiOS/clips.h>

@implementation CriteriaTableViewController

@synthesize criteriaTableView;

bool criterionUpdate = NO;
NSArray *criteria;
NSDictionary *criteriaData;
void *clipsEnv;
NSMutableArray *wineList;

NSString *kRestoreColorChoiceKey = @"ColorChoice";
NSString *kRestoreBodyChoiceKey = @"BodyChoice";
NSString *kRestoreSweetnessChoiceKey = @"SweetnessChoice";
NSString *kRestoreMainCourseChoiceKey = @"MainCourseChoice";
NSString *kRestoreSauceChoiceKey = @"SauceChoice";
NSString *kRestoreFlavorChoiceKey = @"FlavorChoice";

/************/
/* saveData */
/************/
- (void) saveData
  {
   [[NSUserDefaults standardUserDefaults]
      setValue: [[criteriaData valueForKey: @"Color"] valueForKey: @"chosen"]
      forKey: kRestoreColorChoiceKey];
   [[NSUserDefaults standardUserDefaults]
      setValue: [[criteriaData valueForKey: @"Body"] valueForKey: @"chosen"]
      forKey: kRestoreBodyChoiceKey];
   [[NSUserDefaults standardUserDefaults]
      setValue: [[criteriaData valueForKey: @"Sweetness"] valueForKey: @"chosen"]
      forKey: kRestoreSweetnessChoiceKey];
   [[NSUserDefaults standardUserDefaults]
      setValue: [[criteriaData valueForKey: @"Main Course"] valueForKey: @"chosen"]
      forKey: kRestoreMainCourseChoiceKey];
   [[NSUserDefaults standardUserDefaults]
      setValue: [[criteriaData valueForKey: @"Sauce"] valueForKey: @"chosen"]
      forKey: kRestoreSauceChoiceKey];
   [[NSUserDefaults standardUserDefaults]
      setValue: [[criteriaData valueForKey: @"Flavor"] valueForKey: @"chosen"]
      forKey: kRestoreFlavorChoiceKey];
  }

/***************/
/* viewDidLoad */
/***************/
- (void) viewDidLoad
  {
   [super viewDidLoad];
       
   criteria = @[ @[ @"Color" , @"Body", @"Sweetness" ] ,
                 @[ @"Main Course", @"Sauce", @"Flavor" ]
               ];

   NSArray *colorChoices = @[ @"Don’t Care", @"Red", @"White" ];
   NSArray *bodyChoices = @[ @"Don’t Care" , @"Light", @"Medium" , @"Full" ];
   NSArray *sweetnessChoices = @[ @"Don’t Care" , @"Dry", @"Medium" , @"Sweet" ];
   NSArray *mainCourseChoices = @[ @"Don’t Know", @"Beef", @"Pork", @"Lamb", @"Turkey",
                                    @"Chicken", @"Duck", @"Fish", @"Other"];
   NSArray *sauceChoices = @[ @"Don’t Know" , @"None", @"Spicy" ,
                               @"Sweet" , @"Cream" , @"Other" ];
   NSArray *flavorChoices = @[ @"Don’t Know" , @"Delicate", @"Average" , @"Strong" ];
    
   criteriaData
            = @{ @"Color" :
                   [[NSMutableDictionary alloc] initWithObjectsAndKeys:
                       @0, @"chosen", colorChoices , @"choices",
                       nil] ,
                 @"Body" :
                   [[NSMutableDictionary alloc] initWithObjectsAndKeys:
                       @0, @"chosen", bodyChoices , @"choices",
                       nil] ,
                 @"Sweetness" :
                   [[NSMutableDictionary alloc] initWithObjectsAndKeys:
                       @0, @"chosen", sweetnessChoices , @"choices",
                       nil] ,
                 @"Main Course" :
                   [[NSMutableDictionary alloc] initWithObjectsAndKeys:
                       @0, @"chosen", mainCourseChoices , @"choices",
                       nil] ,
                 @"Sauce" :
                    [[NSMutableDictionary alloc] initWithObjectsAndKeys:
                       @0, @"chosen", sauceChoices , @"choices",
                       nil] ,
                 @"Flavor" :
                   [[NSMutableDictionary alloc] initWithObjectsAndKeys:
                       @0, @"chosen", flavorChoices , @"choices",
                       nil]
               };

   [[criteriaData valueForKey: @"Color"]
      setValue: [[NSUserDefaults standardUserDefaults] valueForKey: kRestoreColorChoiceKey]
      forKey: @"chosen"];

   [[criteriaData valueForKey: @"Body"]
      setValue: [[NSUserDefaults standardUserDefaults] valueForKey: kRestoreBodyChoiceKey]
      forKey: @"chosen"];
      
   [[criteriaData valueForKey: @"Sweetness"]
      setValue: [[NSUserDefaults standardUserDefaults] valueForKey: kRestoreSweetnessChoiceKey]
      forKey: @"chosen"];
      
   [[criteriaData valueForKey: @"Main Course"]
      setValue: [[NSUserDefaults standardUserDefaults] valueForKey: kRestoreMainCourseChoiceKey]
      forKey: @"chosen"];
    
   [[criteriaData valueForKey: @"Sauce"]
      setValue: [[NSUserDefaults standardUserDefaults] valueForKey: kRestoreSauceChoiceKey]
      forKey: @"chosen"];

   [[criteriaData valueForKey: @"Flavor"]
      setValue: [[NSUserDefaults standardUserDefaults] valueForKey: kRestoreFlavorChoiceKey]
      forKey: @"chosen"];

   wineList = [[NSMutableArray alloc] init];
   
   clipsEnv = CreateEnvironment();
   if (clipsEnv == NULL) return;

   NSString *filePath = [[NSBundle mainBundle] pathForResource: @"wine" ofType: @"clp"];
   char *cFilePath = (char *) [filePath UTF8String];
   EnvLoad(clipsEnv,cFilePath);
   
   [self runWine];
  }

/***********/
/* runWine */
/***********/
- (void) runWine
  {
   NSString *item;
   NSDictionary *criterion;
   NSNumber *theChoice;
   NSArray *choices;
   
   EnvReset(clipsEnv);
   
   criterion = [criteriaData valueForKey: @"Color"];
   theChoice = [criterion valueForKey: @"chosen"];
   choices = [criterion valueForKey: @"choices"];
   item = [choices objectAtIndex: [theChoice intValue]];
      
   if ([item isEqualToString: @"Red"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-color) (value red))"); }
   else if ([item isEqualToString: @"White"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-color) (value white))"); }
   else
     { EnvAssertString(clipsEnv,"(attribute (name preferred-color) (value unknown))"); }

   criterion = [criteriaData valueForKey: @"Body"];
   theChoice = [criterion valueForKey: @"chosen"];
   choices = [criterion valueForKey: @"choices"];
   item = [choices objectAtIndex: [theChoice intValue]];

   if ([item isEqualToString: @"Light"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-body) (value light))"); }
   else if ([item isEqualToString: @"Medium"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-body) (value medium))"); }
   else if([item isEqualToString: @"Full"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-body) (value full))"); }
   else
     { EnvAssertString(clipsEnv,"(attribute (name preferred-body) (value unknown))"); }
 
   criterion = [criteriaData valueForKey: @"Sweetness"];
   theChoice = [criterion valueForKey: @"chosen"];
   choices = [criterion valueForKey: @"choices"];
   item = [choices objectAtIndex: [theChoice intValue]];
   
   if ([item isEqualToString: @"Dry"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-sweetness) (value dry))"); }
   else if ([item isEqualToString: @"Medium"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-sweetness) (value medium))"); }
   else if ([item isEqualToString: @"Sweet"])
     { EnvAssertString(clipsEnv,"(attribute (name preferred-sweetness) (value sweet))"); }
   else
     { EnvAssertString(clipsEnv,"(attribute (name preferred-sweetness) (value unknown))"); }

   criterion = [criteriaData valueForKey: @"Main Course"];
   theChoice = [criterion valueForKey: @"chosen"];
   choices = [criterion valueForKey: @"choices"];
   item = [choices objectAtIndex: [theChoice intValue]];
   
   if ([item isEqualToString: @"Beef"] ||
       [item isEqualToString: @"Pork"] ||
       [item isEqualToString: @"Lamb"])
     {
      EnvAssertString(clipsEnv,"(attribute (name main-component) (value meat))");
      EnvAssertString(clipsEnv,"(attribute (name has-turkey) (value no))");
     }
   else if ([item isEqualToString: @"Turkey"])
     {
      EnvAssertString(clipsEnv,"(attribute (name main-component) (value poultry))");
      EnvAssertString(clipsEnv,"(attribute (name has-turkey) (value yes))");
     }
   else if ([item isEqualToString: @"Chicken"] ||
            [item isEqualToString: @"Duck"])
     {
      EnvAssertString(clipsEnv,"(attribute (name main-component) (value poultry))");
      EnvAssertString(clipsEnv,"(attribute (name has-turkey) (value no))");
     }
   else if ([item isEqualToString: @"Fish"])
     {
      EnvAssertString(clipsEnv,"(attribute (name main-component) (value fish))");
      EnvAssertString(clipsEnv,"(attribute (name has-turkey) (value no))");
     }
   else if ([item isEqualToString: @"Other"])
     {
      EnvAssertString(clipsEnv,"(attribute (name main-component) (value unknown))");
      EnvAssertString(clipsEnv,"(attribute (name has-turkey) (value no))");
     }
   else
     {
      EnvAssertString(clipsEnv,"(attribute (name main-component) (value unknown))");
      EnvAssertString(clipsEnv,"(attribute (name has-turkey) (value unknown))");
     }

   criterion = [criteriaData valueForKey: @"Sauce"];
   theChoice = [criterion valueForKey: @"chosen"];
   choices = [criterion valueForKey: @"choices"];
   item = [choices objectAtIndex: [theChoice intValue]];

   if ([item isEqualToString: @"None"])
     { EnvAssertString(clipsEnv,"(attribute (name has-sauce) (value no))"); }
   else if ([item isEqualToString: @"Spicy"])
     {
      EnvAssertString(clipsEnv,"(attribute (name has-sauce) (value yes))");
      EnvAssertString(clipsEnv,"(attribute (name sauce) (value spicy))");
     }
   else if ([item isEqualToString: @"Sweet"])
     {
      EnvAssertString(clipsEnv,"(attribute (name has-sauce) (value yes))");
      EnvAssertString(clipsEnv,"(attribute (name sauce) (value sweet))");
     }
   else if ([item isEqualToString: @"Cream"])
     {
      EnvAssertString(clipsEnv,"(attribute (name has-sauce) (value yes))");
       EnvAssertString(clipsEnv,"(attribute (name sauce) (value cream))");
     }
   else if ([item isEqualToString: @"Other"])
     {
      EnvAssertString(clipsEnv,"(attribute (name has-sauce) (value yes))");
      EnvAssertString(clipsEnv,"(attribute (name sauce) (value unknown))");
     }
   else
     {
      EnvAssertString(clipsEnv,"(attribute (name has-sauce) (value unknown))");
      EnvAssertString(clipsEnv,"(attribute (name sauce) (value unknown))");
     }

   criterion = [criteriaData valueForKey: @"Flavor"];
   theChoice = [criterion valueForKey: @"chosen"];
   choices = [criterion valueForKey: @"choices"];
   item = [choices objectAtIndex: [theChoice intValue]];

   if ([item isEqualToString: @"Delicate"])
     { EnvAssertString(clipsEnv,"(attribute (name tastiness) (value delicate))"); }
   else if ([item isEqualToString: @"Average"])
     { EnvAssertString(clipsEnv,"(attribute (name tastiness) (value average))"); }
   else if ([item isEqualToString: @"Strong"])
     { EnvAssertString(clipsEnv,"(attribute (name tastiness) (value strong))"); }
   else
     { EnvAssertString(clipsEnv,"(attribute (name tastiness) (value unknown))"); }
     
   EnvRun(clipsEnv,-1L);
   
   [self updateWines];
  }

/***************/
/* updateWines */
/***************/
- (void) updateWines
  {
   DATA_OBJECT theResult, theSlot;
   struct multifield *theMultifield;
   void *theFact;
   NSString *wineName;
   NSNumber *certainty;
   Wine *theWine;
   
   [wineList removeAllObjects];
   
   EnvEval(clipsEnv,"(WINES::get-wine-list)",&theResult);
      
   if (GetType(theResult) != MULTIFIELD) return;
   
   theMultifield = GetValue(theResult);
   
   for (int i = 1; i <= GetDOLength(theResult); i++)
     {
      if (GetMFType(theMultifield,i) != FACT_ADDRESS) continue;
      
      theFact = GetMFValue(theMultifield,i);

      EnvGetFactSlot(clipsEnv,theFact,"value",&theSlot);
      
      if ((GetType(theSlot) == SYMBOL) || (GetType(theSlot) == STRING))
        { wineName = [NSString stringWithCString: DOToString(theSlot) encoding: NSUTF8StringEncoding]; }
      else
        { wineName = @"Unknown"; }
     
      EnvGetFactSlot(clipsEnv,theFact,"certainty",&theSlot);
      
      if (GetType(theSlot) == INTEGER)
        { certainty = [NSNumber numberWithInteger: DOToInteger(theSlot)]; }
      else if (GetType(theSlot) == FLOAT)
        { certainty = [NSNumber numberWithFloat: DOToFloat(theSlot)]; }
      else
        { certainty = [NSNumber numberWithInteger: 0]; }
        
      theWine = [[Wine alloc] init];
      theWine.name = wineName;
      theWine.certainty = certainty;
      
      [wineList addObject: theWine];
     }
  }

/********************************/
/* numberOfSectionsInTableView: */
/********************************/
- (NSInteger) numberOfSectionsInTableView: (UITableView *) tableView
  {
   return [criteria count] + 1;
  }

/************************************/
/* tableView:numberOfRowsInSection: */
/************************************/
- (NSInteger) tableView: (UITableView *) tableView
              numberOfRowsInSection: (NSInteger) section
  {
   switch (section)
     {
      case 0:
      case 1:
        return [[criteria objectAtIndex: section] count];

      case 2:
        return [wineList count];
     }
     
   return 1;
  }

/************************************/
/* tableView:cellForRowAtIndexPath: */
/************************************/
- (UITableViewCell *) tableView: (UITableView *) tableView
                      cellForRowAtIndexPath: (NSIndexPath *) indexPath
  {
   static NSString *simpleTableIdentifier = @"CriteriaCell";
    
   UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: simpleTableIdentifier];
    
   if (cell == nil)
     {
      cell = [[UITableViewCell alloc] initWithStyle: UITableViewCellStyleDefault
                                      reuseIdentifier: simpleTableIdentifier];
     }

   if (indexPath.section == 2)
     {
      Wine *theWine = [wineList objectAtIndex: indexPath.row];
   
      cell.textLabel.text = theWine.name;
      cell.detailTextLabel.text =
        [NSString stringWithFormat: @"%ld",[theWine.certainty longValue]];
      cell.accessoryType = UITableViewCellAccessoryNone;

      /*
      cell.textLabel.text = @"Recommendations";
      cell.detailTextLabel.text =
        [NSString stringWithFormat: @"%ld",[wineList count]];
      */
     }
   else
     {
      NSString *title = [[criteria objectAtIndex: indexPath.section] objectAtIndex: indexPath.row];
      NSDictionary *criterion = [criteriaData valueForKey: title];
      NSNumber *theChoice = [criterion valueForKey: @"chosen"];
      NSArray *choices = [criterion valueForKey: @"choices"];
   
      cell.textLabel.text = title;
      cell.detailTextLabel.text = [choices objectAtIndex: [theChoice intValue]];
      cell.accessoryType = UITableViewCellAccessoryDisclosureIndicator;
     }
   
   return cell;
  }

/**************************************/
/* tableView:didSelectRowAtIndexPath: */
/**************************************/
- (void) tableView: (UITableView *) tableView
         didSelectRowAtIndexPath: (NSIndexPath *) indexPath
  {
   switch (indexPath.section)
     {
      case 0:
      case 1:
        criterionUpdate = YES;
        [self performSegueWithIdentifier: @"optionsSegue" sender: self];
        break;
/*
      case 2:
        [self performSegueWithIdentifier: @"recommendationsSegue" sender: self];
        break;
*/
     }
  }

/**************************************/
/* tableView:titleForHeaderInSection: */
/**************************************/
- (NSString *) tableView: (UITableView *) tableView titleForHeaderInSection: (NSInteger) section
  {
   switch (section)
	 {
      case 0:
        return @"Preferences";

      case 1:
        return @"Meal";
        
      case 2:
        return @"Recommendations";
	 }
     
   return @"";
  }

/********************************************/
/* tableView:shouldHighlightRowAtIndexPath: */
/********************************************/
- (BOOL) tableView: (UITableView *) tableView
         shouldHighlightRowAtIndexPath: (NSIndexPath *) indexPath
  {
   switch (indexPath.section)
     {
      case 0:
      case 1:
        return YES;
        break;

      case 2:
        return NO;
        break;
     }
     
   return NO;
  }

/***************************/
/* prepareForSegue:sender: */
/***************************/
- (void) prepareForSegue: (UIStoryboardSegue *) segue
         sender: (id) sender
  {
   if ([segue.identifier isEqualToString:@"optionsSegue"])
     {
      NSIndexPath *indexPath = [self.tableView indexPathForSelectedRow];
      NSString *title = [[criteria objectAtIndex: indexPath.section] objectAtIndex: indexPath.row];
      NSMutableDictionary *criterion = [criteriaData valueForKey: title];
      
      OptionsTableViewController *destViewController = segue.destinationViewController;
      destViewController.criterion = criterion;
      destViewController.title = title;
     }
  }

/*******************/
/* viewWillAppear: */
/*******************/
- (void) viewWillAppear: (BOOL) animated
  {
   if (criterionUpdate)
     {
      [self runWine];
      [criteriaTableView reloadData];
      criterionUpdate = NO;
     }
     
   [super viewWillAppear: animated];
  }

@end
