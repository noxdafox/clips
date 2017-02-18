//
//  GenericController.m
//  CLIPS
//
//  Created by Gary Riley on 3/22/06.
//

#import "GenericController.h"

#import "AppController.h"
#import "CLIPSEnvironment.h"
#import "EnvController.h"

@implementation GenericController

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"GenericInspector"];

   if (self)
     {
     }
     
   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   [super dealloc];
  }

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   NSArrayController *theArrayController;
   NSMutableDictionary *bindingOptions = [NSMutableDictionary dictionary];
   AppController *theDelegate = [NSApp delegate];

   /*=====================================================================*/
   /* Create the binding for the environment displayed in the popup menu. */
   /*=====================================================================*/

   [bindingOptions setObject:@"Unattached" forKey:@"NSNullPlaceholder"];
   [bindingOptions setObject: [NSNumber numberWithBool:YES] forKey:@"NSInsertsNullPlaceholder"];

   [environmentList bind: @"content" 
                    toObject: [[theDelegate envController] environmentArrayController]
                    withKeyPath: @"arrangedObjects"
                    options: bindingOptions];
   
   /*=============================================================*/
   /* Locate and assign the application's environment controller. */
   /*=============================================================*/
  
   [self setValue: [theDelegate envController] forKey: @"environmentController"];
   
   /*====================================================================*/
   /* Determine the environment to which this window should be attached. */
   /*====================================================================*/
    
   theArrayController 
      = [[theDelegate envController] environmentArrayController];
      
   NSArray *theArray;
   NSUInteger theIndex;

   theArray = [theArrayController arrangedObjects];
   theIndex = [theArrayController selectionIndex]; 
   
   if (theIndex != NSNotFound)
     { [self setValue: [theArray objectAtIndex: theIndex] forKey: @"environment"]; }
   else if ([theArray count] != 0)
     { [self setValue: [theArray objectAtIndex: 0] forKey: @"environment"]; }
   else
     { [self setValue: nil forKey: @"environment"]; }
  }
    
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Window Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/

/********************/
/* windowWillClose: */
/********************/
- (void) windowWillClose: (NSNotification *) aNotification
  {
   [self setValue: nil forKey: @"environmentController"];

   [self autorelease];
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
  
/*****************************/
/* setEnvironmentController: */
/*****************************/
- (void) setEnvironmentController: (EnvController *) theController
  {
   [theController retain];
   [environmentController release];
   environmentController = theController;
  }

/**************************/
/* environmentController: */
/**************************/
- (EnvController *) environmentController
  {
   return environmentController;
  }
  
/*******************/
/* setEnvironment: */
/*******************/
- (void) setEnvironment: (CLIPSEnvironment *) theEnvironment
  {
   /*=============================*/
   /* Retain the new environment. */
   /*=============================*/
   
   [theEnvironment retain];

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
     }

   /*=============================*/
   /* Release the old environment */
   /* and assign the new value.   */
   /*=============================*/
   
   [environment release];
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
