//
//  OptionsTableViewController.m
//  Wine
//
//  Created by Programmer X on 4/28/16.
//  Copyright © 2016 Secret Society Software. All rights reserved.
//

#import "OptionsTableViewController.h"

@implementation OptionsTableViewController

@synthesize criterion;

/************************************/
/* tableView:numberOfRowsInSection: */
/************************************/
- (NSInteger) tableView: (UITableView *) tableView
              numberOfRowsInSection: (NSInteger) section
  {
   return [[criterion valueForKey: @"choices"] count];
  }

/************************************/
/* tableView:cellForRowAtIndexPath: */
/************************************/
- (UITableViewCell *) tableView: (UITableView *) tableView
                      cellForRowAtIndexPath: (NSIndexPath *) indexPath
  {
   static NSString *simpleTableIdentifier = @"OptionsCell";
    
   UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier: simpleTableIdentifier];
    
   if (cell == nil)
     {
      cell = [[UITableViewCell alloc] initWithStyle: UITableViewCellStyleDefault
                                      reuseIdentifier: simpleTableIdentifier];
     }
    
   cell.textLabel.text = [[criterion valueForKey: @"choices"] objectAtIndex: indexPath.row];

   if ([[criterion valueForKey: @"chosen"] intValue] == indexPath.row)
     { cell.accessoryType = UITableViewCellAccessoryCheckmark; }
   else
     { cell.accessoryType = UITableViewCellAccessoryNone; }

   return cell;
  }

/**************************************/
/* tableView:didSelectRowAtIndexPath: */
/**************************************/
- (void) tableView: (UITableView *) tableView
         didSelectRowAtIndexPath: (NSIndexPath *) indexPath
  {
   [criterion setValue: [NSNumber numberWithLong: indexPath.row] forKey: @"chosen"];
   [[self navigationController] popViewControllerAnimated: YES];
  }

@end
