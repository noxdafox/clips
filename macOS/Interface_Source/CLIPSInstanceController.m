//
//  CLIPSInstanceController.m
//  CLIPS
//
//  Created by Gary Riley on 3/3/08.
//

#import "CLIPSInstanceController.h"

#import "AppController.h"
#import "CLIPSAgendaController.h"
#import "CLIPSEnvironment.h"
#import "CLIPSFactInstance.h"
#import "CLIPSModule.h"
#import "ModuleArrayController.h"

#include <CLIPS/clips.h>

@implementation CLIPSInstanceController

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"CLIPSInstanceBrowser"];

   if (self)
     {
      [self setValue: [NSNumber numberWithInt: 10] forKey: @"fontSize"]; 
      [self setValue: [NSNumber numberWithInt: 13] forKey: @"rowHeight"]; 
      lastModuleRow = -1;
      lastModule = NULL;
      lastInstance = NULL;
      lastInstanceRow = -1;
     }

   return self;
  }

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   AppController *theDelegate = [NSApp delegate];

   /*====================================================================*/
   /* Determine the environment to which this window should be attached. */
   /*====================================================================*/

   [self setValue: [theDelegate mainEnvironment] forKey: @"environment"];

   [self setValue: [NSNumber numberWithInt: 10] forKey: @"fontSize"]; 
   [self setValue: [NSNumber numberWithInt: 13] forKey: @"rowHeight"]; 
   
   /*=====================================================*/
   /* Initialize with the last setting the user chose for */
   /* whether defaulted values should be displayed.       */
   /*=====================================================*/
    
   NSUserDefaults *theValues;
   
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
   
   if ([[theValues valueForKey: @"instancesDisplayDefaultedValues"] boolValue]) 
     { 
      [self setValue: nil forKey: @"slotFilter"]; 
      [displayDefaultedValuesButton setState: NSOnState];
     }
   else
     {
      NSPredicate *predicate = [NSPredicate predicateWithFormat: @"slotDefault = TRUE"]; 
      [self setValue: predicate forKey: @"slotFilter"]; 
      [displayDefaultedValuesButton setState: NSOffState];
     }

   /*=================================================*/
   /* This setting for this attribute isn't preserved */
   /* when set in Interface Builder.                  */
   /*=================================================*/
   
   [executionIndicator setDisplayedWhenStopped: NO];
        
   /*========================================*/
   /* If we can get the execution lock, then */
   /* the environment isn't executing, so we */
   /* can directly retrieve the instances.   */
   /*========================================*/
   
   if ([[environment executionLock] tryLock]) 
     {
      [environment fetchInstances: YES];
      [environment transferInstances: YES];
      [[environment executionLock] unlock];
     }
   else
     {
      [self startExecutionIndicator];
     }
  }

/***************************/
/* startExecutionIndicator */
/***************************/
- (void) startExecutionIndicator
  {
   if ([NSThread isMainThread])
     { [executionIndicator startAnimation: nil]; }
   else
     {
      dispatch_sync(dispatch_get_main_queue(),
                    ^{ [executionIndicator startAnimation: nil]; });
     }
  }

/**************************/
/* stopExecutionIndicator */
/**************************/
- (void) stopExecutionIndicator
  {
   if ([NSThread isMainThread])
     { [executionIndicator stopAnimation: nil]; }
   else
     {
      dispatch_sync(dispatch_get_main_queue(),
                    ^{ [executionIndicator stopAnimation: nil]; });
     }
  }

/******************/
/* saveSelection: */
/******************/
- (void) saveSelection
  {
   NSInteger theRow;
   NSArray *theArray;
   
   theRow = [moduleList selectedRow];
   if (theRow != -1)
     {
      theArray = [moduleListController arrangedObjects];
      CLIPSModule *theModule;

      theModule = [theArray objectAtIndex: (NSUInteger) theRow];
      lastModule = [theModule moduleName];
      lastModuleRow = theRow;
     }
   else
     {
      lastModule = NULL;
      lastModuleRow = -1;
     }

   theRow = [instanceList selectedRow];
   if (theRow != -1)
     {
      theArray = [instanceListController arrangedObjects];
      CLIPSFactInstance *theInstance;

      theInstance = [theArray objectAtIndex: (NSUInteger) theRow];
      lastInstance = [theInstance name];
      lastInstanceRow = theRow;
     }
   else
     {
      lastInstance = NULL;
      lastInstanceRow = -1;
     }
  }

/*********************/
/* restoreSelection: */
/*********************/
- (void) restoreSelection
  {
   NSArray *theArray;
   NSUInteger i, count;
   BOOL found;
   
   if (lastModuleRow == -1)
     { [moduleList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO]; }
   else
     {
      CLIPSModule *theModule;
      theArray = [moduleListController arrangedObjects];
      count = [theArray count];
      found = NO;
      
      if (((NSUInteger) lastModuleRow) < count)
        {

         theModule = [theArray objectAtIndex: (NSUInteger) lastModuleRow];
         
         if ([[theModule moduleName] isEqualToString: lastModule])
           {
            [moduleList selectRowIndexes: [NSIndexSet indexSetWithIndex: (NSUInteger) lastModuleRow] byExtendingSelection: NO];
            found = YES;
           }
        }
        
      if (found == NO)
        {
         for (i = 0; i < count; i++)
           {
            theModule = [theArray objectAtIndex: i];
            if ([[theModule moduleName] isEqualToString: lastModule])
              {
               found = YES;
               [moduleList selectRowIndexes: [NSIndexSet indexSetWithIndex: i] byExtendingSelection: NO];
               break;
              }
           }
        }
        
      if (found == NO)
        {
         lastInstance = NULL;
         lastInstanceRow = -1;
         if (count > 0)
           {
            if (lastModuleRow < (NSInteger) count)
              { [moduleList selectRowIndexes: [NSIndexSet indexSetWithIndex: (NSUInteger) lastModuleRow] byExtendingSelection: NO]; }
            else
              { [moduleList selectRowIndexes: [NSIndexSet indexSetWithIndex: (NSUInteger) (count - 1)] byExtendingSelection: NO]; }
           }
         else
           { [moduleList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO]; }
        }
     }
     
   if (lastInstanceRow == -1)
     { [instanceList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO]; }
   else
     {
      CLIPSFactInstance *theInstance;
      theArray = [instanceListController arrangedObjects];
      count = [theArray count];
      found = NO;
      
      if (((NSUInteger) lastInstanceRow) < count)
        {

         theInstance = [theArray objectAtIndex: (NSUInteger) lastInstanceRow];
         
         if ([[theInstance name] isEqualToString: lastInstance])
           {
            [instanceList selectRowIndexes: [NSIndexSet indexSetWithIndex: (NSUInteger) lastInstanceRow] byExtendingSelection: NO];
            found = YES;
           }
        }
        
      if (found == NO)
        {
         for (i = 0; i < count; i++)
           {
            theInstance = [theArray objectAtIndex: i];
            if ([[theInstance name] isEqualToString: lastInstance])
              {
               found = YES;
               [instanceList selectRowIndexes: [NSIndexSet indexSetWithIndex: i] byExtendingSelection: NO];
               break;
              }
           }
        }
        
      if (found == NO)
        {
         if (count > 0)
           {
            if (lastInstanceRow < (NSInteger) count)
              { [instanceList selectRowIndexes: [NSIndexSet indexSetWithIndex: (NSUInteger) lastInstanceRow] byExtendingSelection: NO]; }
            else
              { [instanceList selectRowIndexes: [NSIndexSet indexSetWithIndex: (NSUInteger) (count - 1)] byExtendingSelection: NO]; }
           }
         else
           { [instanceList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO]; }
        }
     }
     
   lastModuleRow = -1;
   lastModule = NULL;
   lastInstance = NULL;
   lastInstanceRow = -1;
  }

/***************************/
/* observeValueForKeyPath: */
/***************************/
- (void) observeValueForKeyPath: (NSString *) keyPath 
                       ofObject: (id) object 
                        change: (NSDictionary *) change 
                       context: (void *) context 
  {
   if ([keyPath isEqual:@"instancesSaveSelection"])
     { [self saveSelection]; }
   else if ([keyPath isEqual:@"instancesChanged"])
     { [self restoreSelection]; }
   else if ([keyPath isEqual:@"executing"])
     { 
      if ([[change valueForKey: NSKeyValueChangeKindKey] intValue] == NSKeyValueChangeSetting)
        {
         if ([[change valueForKey: NSKeyValueChangeNewKey] intValue])
           { [self startExecutionIndicator]; }
         else
           { [self stopExecutionIndicator]; }
        }
     }
  }
   
/**************************************************/
/* splitView:constraintMinCoordinate:ofSubviewAt: */
/**************************************************/
- (CGFloat) splitView: (NSSplitView *) sender
          constrainMinCoordinate: (CGFloat) proposedMin
          ofSubviewAt: (NSInteger) offset
  {
   return 100.0;
  }

/**************************************************/
/* splitView:constraintMaxCoordinate:ofSubviewAt: */
/**************************************************/
- (CGFloat) splitView: (NSSplitView *) sender
          constrainMaxCoordinate: (float) proposedMax 
          ofSubviewAt: (NSInteger) offset
  {
   return [sender bounds].size.width - 200.0;
  }

/*********************************/
/* splitView:canCollapseSubview: */
/*********************************/
- (BOOL) splitView: (NSSplitView *) sender 
         canCollapseSubview: (NSView *) subview
  {
   return YES;
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

/******************************************************************/
/* displayDefaultedValues: Handles the "Display Defaulted Values" */
/*   checkbox. If checked, then all of the slot values of an      */
/*   instance are displayed by the inspector. If unchecked, then  */
/*   the slot values that are the same as the static default for  */
/*   the slot are not displayed.                                  */
/******************************************************************/
- (IBAction) displayDefaultedValues: (id) sender
  {
   NSUserDefaults *theValues;
   NSNumber *displayValue;
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];
   
   theValues = [theDefaultsController values];

   if ([sender state])
     { 
      [self setValue: nil forKey: @"slotFilter"]; 
      displayValue = [NSNumber numberWithBool:YES];
     }
   else
     {
      NSPredicate *predicate = [NSPredicate predicateWithFormat: @"slotDefault = TRUE"]; 
      [self setValue: predicate forKey: @"slotFilter"]; 
      displayValue = [NSNumber numberWithBool:NO];
     }
     
   [theValues setValue: displayValue forKey: @"instancesDisplayDefaultedValues"];
   [theDefaultsController save: self];
  }

/***********/  
/* search: */
/***********/  
- (IBAction) search: (id) sender
 {
  [instanceListController search: sender];
/*
  [self setSearchString: [sender stringValue]];
  [self rearrangeObjects];
*/
 } 
  
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Window Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/

/********************/
/* validateMenuItem: */
/********************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   if ([menuItem action] == @selector(showDefrule:))
     {
      if ([instanceList selectedRow] == -1) 
        { return NO; } 
     }
     
   return YES;
  }
  
/********************/
/* windowWillClose: */
/********************/
- (void) windowWillClose: (NSNotification *) aNotification
  {
   AppController *theDelegate = [NSApp delegate];

   [theDelegate removeInstanceController: self];
   [self setValue: nil forKey: @"environment"];
  }

/****************************************************/
/* tableViewSelectionDidChange: Called when the     */
/*   selection changes in the instance NSTableView. */
/****************************************************/
- (void) tableViewSelectionDidChange: (NSNotification *) aNotification
  {
   NSInteger theRow;
   NSString *thePPForm = nil;
   AppController *theDelegate = [NSApp delegate];
   
   if ([aNotification object] == moduleList)
     {
      theRow = [moduleList selectedRow];

      [instanceListController setModuleIndex: theRow];
     }
    
   theRow = [instanceList selectedRow];
   if (theRow != -1)
     {
      Environment *theEnvironment = [environment environment];
      Instance *clipsInstance = NULL;
      char *theInstanceName;
      NSArray *theArray = [instanceListController arrangedObjects];
      CLIPSFactInstance *theInstance = [theArray objectAtIndex: (NSUInteger) theRow];
      
      /*===============================================================*/
      /* Use the instance name stored with the GUI fact object to find */
      /* the actual CLIPS instance referenced. TBD: It would be more   */
      /* efficient to directly store the pointer to the instance with  */
      /* the GUI instance object.                                      */
      /*===============================================================*/
      
      theInstanceName = (char *) [[theInstance name] UTF8String];
      if (theInstanceName != NULL)
        { clipsInstance = FindInstance(theEnvironment,NULL,theInstanceName,true); }
      
      /*===========================================================*/
      /* If we were able to find the corresponding CLIPS instance, */
      /* then retrieve the pretty print form of the defclass       */
      /* associated with the instance.                             */
      /*===========================================================*/
      
      if ((clipsInstance != NULL) && 
          DefclassPPForm(InstanceClass(clipsInstance)) != NULL)
        { thePPForm = [NSString stringWithUTF8String: DefclassPPForm(InstanceClass(clipsInstance))]; }
     }
     
   [theDelegate setValue: thePPForm forKey: @"constructInspectorText"];
  }

/*%%%%%%%%%%%%%%%%%%%%%%*/
/* Notification Methods */
/*%%%%%%%%%%%%%%%%%%%%%%*/

/*********************************/
/* targetEnvironmentDeallocated: */
/*********************************/
- (void) targetEnvironmentDeallocated: (NSNotification *) note
  {
   [self setValue: nil forKey: @"environment"];
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*******************/
/* setEnvironment: */
/*******************/
- (void) setEnvironment: (CLIPSEnvironment *) theEnvironment
  {
   /*=====================================*/
   /* Stop observing the old environment. */
   /*=====================================*/
   
   NSNotificationCenter  *nc;
   nc = [NSNotificationCenter defaultCenter];
   
   if (environment != nil)
     {
      [nc removeObserver: self
          name:@"CLIPSEnvironmentWillBeDestroyed"
          object: environment];
          
      [environment removeObserver: self 
                       forKeyPath: @"instancesChanged"];
         
      [environment removeObserver: self
                       forKeyPath: @"instancesSaveSelection"];
         
      [environment removeObserver: self 
                       forKeyPath: @"executing"]; 
                    
      [environment decrementInstancesListeners];
     }

   /*======================================*/
   /* Start observing the new environment. */
   /*======================================*/

   if (theEnvironment != nil)
     {
      [nc addObserver: self 
          selector: @selector(targetEnvironmentDeallocated:)
          name: @"CLIPSEnvironmentWillBeDestroyed"
          object: theEnvironment];
          
      [theEnvironment addObserver: self 
                      forKeyPath: @"instancesChanged" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 

     [theEnvironment addObserver: self
                      forKeyPath: @"instancesSaveSelection"
                      options: (NSKeyValueObservingOptionNew |
                                NSKeyValueObservingOptionOld)
                      context: nil];

      [theEnvironment addObserver: self
                      forKeyPath: @"executing" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 
            
      /*==============================================*/
      /* Fetch the instance list if no other instance */ 
      /* controllers are attached to the environment. */
      /*==============================================*/
   
      if ([theEnvironment instancesListenerCount] == 0)
        {
         /*==========================================================*/
         /* If we can get the execution lock, then the environment   */
         /* isn't executing, so we can directly retrieve the list    */
         /* of instances.                                            */
         /*==========================================================*/

         if ([[theEnvironment executionLock] tryLock]) 
           {
            [theEnvironment fetchInstances: YES];
            [theEnvironment transferInstances: YES];
            [[theEnvironment executionLock] unlock];
           }
        }

      /*===============================================*/
      /* If the new environment is not executing, then */
      /* the reset/run/step buttons are available,     */
      /* otherwise they aren't.                        */
      /*===============================================*/
      
      if ([[theEnvironment executionLock] tryLock]) 
        {
         [self stopExecutionIndicator];
         [[theEnvironment executionLock] unlock];
        }
      else
        {
         [self startExecutionIndicator];
        }
      
      /*=====================================================*/
      /* Increment the count of instance list controllers    */
      /* watching the specified enviroment. For performance, */
      /* the instance list isn't retrieved unless there are  */
      /* controllers interested in the list of instances.    */
      /*=====================================================*/
      
      [theEnvironment incrementInstancesListeners];
     }
   
   /*====================================================================*/
   /* Otherwise, the instances browser isn't attached to an environment. */
   /* The buttons and execution indicator should be disabled.            */
   /*====================================================================*/
   
   else
     { [self stopExecutionIndicator]; }

   /*=============================*/
   /* Release the old environment */
   /* and assign the new value.   */
   /*=============================*/
   
   environment = theEnvironment;
  }

/****************/
/* environment: */
/****************/
- (CLIPSEnvironment *) environment
  {
   return environment;
  }

/*****************************/
/* setSlotFilter: */
/*****************************/
- (void) setSlotFilter: (NSPredicate *) theSlotFilter
  {
   slotFilter = theSlotFilter;
  }

/**************************/
/* slotFilter: */
/**************************/
- (NSPredicate *) slotFilter
  {
   return slotFilter;
  }
  
@end
