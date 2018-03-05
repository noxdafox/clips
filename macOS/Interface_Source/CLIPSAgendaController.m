//
//  CLIPSAgendaController.m
//  CLIPS
//
//  Created by Gary Riley on 3/10/06.
//

#import "CLIPSAgendaController.h"

#import "AppController.h"
#import "CLIPSEnvironment.h"
#import "CLIPSActivation.h"

#include <CLIPS/clips.h>

@implementation CLIPSAgendaController

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"CLIPSAgendaBrowser"];

   if (self)
     {
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

   /*=================================================*/
   /* This setting for this attribute isn't preserved */
   /* when set in Interface Builder.                  */
   /*=================================================*/
   
   [executionIndicator setDisplayedWhenStopped: NO];
        
   /*==========================================================*/
   /* If we can get the execution lock, then the environment   */
   /* isn't executing, so we can directly retrieve the agenda. */
   /*==========================================================*/
   
   if ([[environment executionLock] tryLock]) 
     {
      [environment fetchAgenda: YES];
      [environment transferAgenda: YES];
      [runButton setEnabled: YES];
      [resetButton setEnabled: YES];
      [stepButton setEnabled: YES]; 
      [haltButton setEnabled: NO];
      [[environment executionLock] unlock];
     }
   else
     {
      [runButton setEnabled: NO];
      [resetButton setEnabled: NO];
      [stepButton setEnabled: NO];
      [haltButton setEnabled: YES];
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

/***************************/
/* observeValueForKeyPath: */
/***************************/
- (void) observeValueForKeyPath: (NSString *) keyPath 
                       ofObject: (id) object 
                         change: (NSDictionary *) change 
                        context: (void *) context 
  {
   if ([keyPath isEqual:@"agendaChanged"])
     {
      if ([NSThread isMainThread])
        {
         [focusStack selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
         [agendaList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
         [self updateAgendaInspectorText];
        }
      else
        {
         dispatch_sync(dispatch_get_main_queue(),
                    ^{
                      [focusStack selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
                      [agendaList selectRowIndexes: [NSIndexSet indexSetWithIndex: 0] byExtendingSelection: NO];
                      [self updateAgendaInspectorText];
                    });
        }
     }
   else if ([keyPath isEqual:@"executing"])
     { 
      if ([[change valueForKey: NSKeyValueChangeKindKey] intValue] == NSKeyValueChangeSetting)
        {
         if ([[change valueForKey: NSKeyValueChangeNewKey] intValue])
           {
            if ([NSThread isMainThread])
              {
               [runButton setEnabled: NO];
               [resetButton setEnabled: NO];
               [stepButton setEnabled: NO];
               [haltButton setEnabled: YES];
               [self startExecutionIndicator];
               [self updateAgendaInspectorText];
              }
            else
              {
               dispatch_sync(dispatch_get_main_queue(),
                    ^{
                      [runButton setEnabled: NO];
                      [resetButton setEnabled: NO];
                      [stepButton setEnabled: NO];
                      [haltButton setEnabled: YES];
                      [self startExecutionIndicator];
                      [self updateAgendaInspectorText];
                    });
              }
           }
         else
           {
            if ([NSThread isMainThread])
              {
               [runButton setEnabled: YES];
               [resetButton setEnabled: YES];
               [stepButton setEnabled: YES];
               [haltButton setEnabled: NO];
               [self stopExecutionIndicator];
               [self updateAgendaInspectorText];
              }
            else
              {
               dispatch_sync(dispatch_get_main_queue(),
                    ^{
                      [runButton setEnabled: YES];
                      [resetButton setEnabled: YES];
                      [stepButton setEnabled: YES];
                      [haltButton setEnabled: NO];
                      [self stopExecutionIndicator];
                      [self updateAgendaInspectorText];
                    });
              }
           }
        }
      else
        {
         if ([NSThread isMainThread])
           { [self updateAgendaInspectorText]; }
         else
           {
            dispatch_sync(dispatch_get_main_queue(),
                    ^{ [self updateAgendaInspectorText];  });
           }
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
          constrainMaxCoordinate: (CGFloat) proposedMax
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

/***********************************/  
/* reset: Handle the reset button. */
/***********************************/  
- (IBAction) reset: (id) sender
  {
   /*======================================================*/
   /* Disable the browser buttons. These will be reenabled */
   /* when the environment finishes executing or changes.  */
   /*======================================================*/
   
   [runButton setEnabled: NO];
   [resetButton setEnabled: NO];
   [stepButton setEnabled: NO]; 
   [haltButton setEnabled: NO]; 
   
   /*======================================*/
   /* Send the command to the environment. */
   /*======================================*/

   [environment doCommand: @"(reset)\n"];
  }
  
/*******************************/  
/* run: Handle the run button. */
/*******************************/  
- (IBAction) run: (id) sender
  {
   /*======================================================*/
   /* Disable the browser buttons. These will be reenabled */
   /* when the environment finishes executing or changes.  */
   /*======================================================*/
   
   [runButton setEnabled: NO];
   [resetButton setEnabled: NO];
   [stepButton setEnabled: NO]; 
   [haltButton setEnabled: NO]; 
   
   /*======================================*/
   /* Send the command to the environment. */
   /*======================================*/

   [environment doCommand: @"(run)\n"];
  }
  
/**********************************/  
/* step: Handles the step button. */
/**********************************/  
- (IBAction) step: (id) sender
  {
   /*======================================================*/
   /* Disable the browser buttons. These will be reenabled */
   /* when the environment finishes executing or changes.  */
   /*======================================================*/
   
   [runButton setEnabled: NO];
   [resetButton setEnabled: NO];
   [stepButton setEnabled: NO]; 
   [haltButton setEnabled: NO]; 
   
   /*======================================*/
   /* Send the command to the environment. */
   /*======================================*/
   
   [environment doCommand: @"(run 1)\n"];
  }

/****************/  
/* showDefrule: */
/****************/  
- (IBAction) showDefrule: (id) sender
  {
  }
 
/*********/
/* halt: */
/*********/
- (IBAction) halt: (id) sender
  {
   SetHaltRules([environment environment],true);
  }

/*********************/    
/* haltImmediately: */
/*********************/    
- (IBAction) haltImmediately: (id) sender
  {
   /* TBD Need to abort waitForChar */
   /* TBD Need to abort batch */
   SetHaltCommandLoopBatch([environment environment],true);
   SetHaltExecution([environment environment],true);
  }
 
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Window Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/

/********************/
/* validateMenuItem: */
/********************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   /*===================================================*/
   /* The Halt and Halt Immediately menu items are only */
   /* available if the CLIPS environment is executing.  */
   /*===================================================*/
   
   if (([menuItem action] == @selector(halt:)) ||
       ([menuItem action] == @selector(haltImmediately:)))
     {
      if (environment == nil)
        { return NO; }
        
      if ([[environment executionLock] tryLock])
        {
         [[environment executionLock] unlock];
         return NO;
        }
      else
        { return YES; }
     }

   if ([menuItem action] == @selector(showDefrule:))
     {
      if ([agendaList selectedRow] == -1) 
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
   
   [theDelegate removeAgendaController: self];
   [self setValue: nil forKey: @"environment"];
  }

/**************************************************/
/* tableViewSelectionDidChange: Called when the   */
/*   selection changes in the agenda NSTableView. */
/**************************************************/
- (void) tableViewSelectionDidChange: (NSNotification *) aNotification
  {
   if (([aNotification object] == agendaList) ||
       ([aNotification object] == focusStack))
     {
      [self updateAgendaInspectorText];
     }
  }

/*******************************/
/* updateAgendaInspectorText:  */
/*******************************/
- (void) updateAgendaInspectorText
  {
   NSInteger theRow = [agendaList selectedRow];
   AppController *theDelegate = [NSApp delegate];

   /*===============================================*/
   /* If the environment is executing, don't update */
   /* the construct inspector since the activation  */
   /* may be stale.                                 */
   /*===============================================*/

   if (! [[environment executionLock] tryLock]) 
     { return; }
   [[environment executionLock] unlock];

   if (theRow == -1)
     { [theDelegate setValue: nil forKey: @"constructInspectorText"]; }
   else
     {   
      NSArray *theArray = [focusStackController valueForKeyPath: @"selection.agenda"];
         
      Activation *theActivation = (Activation *) [[theArray objectAtIndex: (NSUInteger) theRow] activation];
         
      NSString *thePPForm = [NSString stringWithUTF8String: DefrulePPForm(theActivation->theRule)];

      [theDelegate setValue: thePPForm forKey: @"constructInspectorText"];
     }
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
                       forKeyPath: @"agendaChanged"]; 

      [environment removeObserver: self 
                       forKeyPath: @"executing"]; 
                    
      [environment decrementAgendaListeners];
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
                      forKeyPath: @"agendaChanged" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 

      [theEnvironment addObserver: self 
                      forKeyPath: @"executing" 
                      options: (NSKeyValueObservingOptionNew | 
                                NSKeyValueObservingOptionOld) 
                      context: nil]; 
            
      /*=================================================*/
      /* Fetch the agenda if no other agenda controllers */
      /* are attached to the environment.                */
      /*=================================================*/
   
      if ([theEnvironment agendaListenerCount] == 0)
        {
         /*==========================================================*/
         /* If we can get the execution lock, then the environment   */
         /* isn't executing, so we can directly retrieve the agenda. */
         /*==========================================================*/

         if ([[theEnvironment executionLock] tryLock]) 
           {
            [theEnvironment fetchAgenda: YES];
            [theEnvironment transferAgenda: YES];
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
         [runButton setEnabled: YES];
         [resetButton setEnabled: YES];
         [stepButton setEnabled: YES]; 
         [haltButton setEnabled: NO];
         [self stopExecutionIndicator];
         [[theEnvironment executionLock] unlock];
        }
      else
        {
         [runButton setEnabled: NO];
         [resetButton setEnabled: NO];
         [stepButton setEnabled: NO]; 
         [haltButton setEnabled: YES]; 
         [self startExecutionIndicator];
        }
      
      /*=========================================================*/
      /* Increment the count of agenda controllers watching the  */
      /* specified enviroment. For performance, the agenda isn't */
      /* retrieved unless there are controllers interested in    */
      /* the content of the agenda.                              */
      /*=========================================================*/
      
      [theEnvironment incrementAgendaListeners];
     }
   
   /*=================================================================*/
   /* Otherwise, the agenda browser isn't attached to an environment. */
   /* The buttons and execution indicator should be disabled.         */
   /*=================================================================*/
   
   else
     {
      [runButton setEnabled: NO];
      [resetButton setEnabled: NO];
      [stepButton setEnabled: NO];         
      [haltButton setEnabled: NO]; 
      [self stopExecutionIndicator];
     }

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

@end
