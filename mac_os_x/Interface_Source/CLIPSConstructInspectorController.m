#import "CLIPSConstructInspectorController.h"

#import "AppController.h"

@implementation CLIPSConstructInspectorController

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"CLIPSConstructInspector"];

   return self;
  }

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   AppController *theDelegate = [NSApp delegate];
   
   /*======================================================*/
   /* Places a few pixels of white space between the edges */
   /* of the window and the rectangle in which the text is */
   /* displayed in the construct inspector.                */
   /*======================================================*/
   
   NSSize theSize = { 3, 3 };
   [textView setTextContainerInset: theSize];   
   
   /*=============================================================*/
   /* Locate and assign the application's environment controller. */
   /*=============================================================*/
  
   [self setAppController: theDelegate];

   /*==================================*/   
   /* Set up the horizontal scrollbar. */
   /*==================================*/   
     
   NSScrollView *textScrollView = [textView enclosingScrollView];

   [textScrollView setHasHorizontalScroller: YES];
   [textScrollView setAutoresizingMask: (NSViewWidthSizable | NSViewHeightSizable)];
   
   [textView setMaxSize: NSMakeSize(FLT_MAX, FLT_MAX)];
   [textView setHorizontallyResizable: YES];
   [textView setAutoresizingMask: (NSViewWidthSizable | NSViewHeightSizable)];
   
   [[textView textContainer] setContainerSize: NSMakeSize(FLT_MAX, FLT_MAX)];
   [[textView textContainer] setWidthTracksTextView: NO];
  }
  
/**************/
/* showPanel: */
/**************/
- (void) showPanel
  {
   NSWindow *panel = [self window];
   
   [panel setExcludedFromWindowsMenu:YES];
   [panel setMenu:nil];
     
   /*====================*/
   /* Display the panel. */
   /*====================*/
        
   [panel makeKeyAndOrderFront:nil];
  }

/*%%%%%%%%%%%%%%%%%%*/
/* Delegate Methods */
/*%%%%%%%%%%%%%%%%%%*/
/*
- (BOOL) shouldDrawInsertionPoint
  {
   return NO;
  }
*/  
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
  
/*********************/
/* setAppController: */
/*********************/
- (void) setAppController: (AppController *) theController
  {
   appController = theController;
  }

/******************/
/* appController: */
/******************/
- (AppController *) appController
  {
   return appController;
  }

@end
