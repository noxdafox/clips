//
//  PreferenceController.m
//  CLIPS
//
//  Created by Gary Riley on 2/25/06.
//

#import "CLIPSTerminalController.h"
#import "AppController.h"
#import "CLIPSEnvironment.h"

#import "PreferenceController.h"

@implementation PreferenceController

/***************/
/* initialize: */
/***************/
+ (void) initialize
  { 
   /*======================================================*/
   /* Create and register the font name value transformer. */
   /* In the Preferences panel, the font name value will   */
   /* be converted to the display name for the font.       */
   /*======================================================*/
    
   NSValueTransformer *transformer = [[FontNameToDisplayNameTransformer alloc] init];
   [NSValueTransformer setValueTransformer:transformer forName:@"FontNameToDisplayNameTransformer"];
  } 

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super initWithWindowNibName:@"Preferences"];
   return self;
  }

/******************/
/* awakeFromNib: */
/******************/
- (void) awakeFromNib
  {
  }

/******************/
/* windowDidLoad: */
/******************/
- (void) windowDidLoad
  {
  }
  
/**************/
/* showPanel: */
/**************/
- (void) showPanel
  {
   NSWindow *panel = [self window];
   
   [panel setHidesOnDeactivate:NO];
   [panel setExcludedFromWindowsMenu:YES];
   [panel setMenu:nil];
   [panel center];

   /*====================*/
   /* Display the panel. */
   /*====================*/
        
   [panel makeKeyAndOrderFront:nil];
  }
    
/*************************************************************************/  
/* changeFont: Handles the changeFont action sent from the font manager. */
/*   Relies on the following connections:                                */
/*   1) The delegate of the Preferences panel (from Preferences.nib)     */
/*      should be the File's Owner (the Preferences class).              */
/*************************************************************************/  
- (void) changeFont: (id) fontManager
  {
   NSUserDefaults *theValues;
   NSFont *selectedFont, *panelFont;
   
   selectedFont = [fontManager selectedFont];
   if (selectedFont == nil)
	 { selectedFont = [NSFont userFixedPitchFontOfSize:0.0]; }

   panelFont = [fontManager convertFont:selectedFont];
   
   if (panelFont != nil)
     { 
      theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
      if ([[[tabView selectedTabViewItem] identifier] isEqualToString: @"dialog"])
        {
         [theValues setValue: [panelFont fontName] forKey: @"dialogTextFontName"];
         [theValues setValue: [NSNumber numberWithFloat: [panelFont pointSize]] forKey: @"dialogTextFontSize"];
        }
      else
        {
         [theValues setValue: [panelFont fontName] forKey: @"editorTextFontName"];
         [theValues setValue: [NSNumber numberWithFloat: [panelFont pointSize]] forKey: @"editorTextFontSize"];
        }

     }
  }

/*********************************************************************/
/* changeEditorFont: Handles the action generated when the Change... */
/*   button is clicked in the Editor Preferences tab.                */
/*********************************************************************/
- (void) changeEditorFont: (id) sender
  {
   NSFontManager *fontManager;
   NSFont *selectedFont;
   NSUserDefaults *theValues;
   NSWindow *panel = [self window];
   
   /*============================*/
   /* Retrieve the font manager. */
   /*============================*/
   
   fontManager = [NSFontManager sharedFontManager];
      
   /*==============================*/
   /* Make it the first responder. */
   /*==============================*/
   
   [panel makeFirstResponder:panel];

   /*======================================*/
   /* First determine if a font is already */
   /* selected in the font manager.        */
   /*======================================*/
   
   selectedFont = [fontManager selectedFont];
   
   /*==============================================*/
   /* If no font is selected, try finding the font */
   /* specified in the user's preferences.         */
   /*==============================================*/
   
   if (selectedFont == nil)
     {
      theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
      
      if ([[[tabView selectedTabViewItem] identifier] isEqualToString: @"dialog"])
        {
         selectedFont = [NSFont fontWithName: [theValues valueForKey: @"dialogTextFontName"]
                                        size: [[theValues valueForKey: @"dialogTextFontSize"] floatValue]];
        }
      else
        {
         selectedFont = [NSFont fontWithName: [theValues valueForKey: @"editorTextFontName"]
                                        size: [[theValues valueForKey: @"editorTextFontSize"] floatValue]];
        }
     }
   
   /*=================================*/
   /* If all else fails, just use the */
   /* default fixed width font.       */
   /*=================================*/
   
   if (selectedFont == nil)
	 { selectedFont = [NSFont userFixedPitchFontOfSize:0.0]; }
     
   /*============================================*/
   /* Set the selected font in the font manager. */
   /*============================================*/
        
   if (selectedFont != nil)
     { [fontManager setSelectedFont: selectedFont isMultiple:NO]; }

   /*======================================*/
   /* Bring the font manager to the front. */
   /*======================================*/
      
   [fontManager orderFrontFontPanel:self];
  }
  
/**********************/
/* windowShouldClose: */
/**********************/  
- (BOOL) windowShouldClose: (id) sender
  {
   NSUserDefaultsController *theDefaultsController;

   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   if (! [theDefaultsController hasUnappliedChanges])
     { return YES; }

   NSAlert *alert = [[NSAlert alloc] init];
   alert.messageText =  @"Do you want to save changes to your Preferences?";
   [alert addButtonWithTitle: @"Save"];
   [alert addButtonWithTitle: @"Cancel"];
   [alert addButtonWithTitle: @"Don't Save"];
   
   [alert beginSheetModalForWindow: [self window]
                 completionHandler: ^(NSInteger returnCode)
                 {
                  NSUserDefaultsController *theDefaultsController;
                  NSWindow *panel = [self window];
                  theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];
                  if (returnCode == NSAlertFirstButtonReturn) // Save
                    {
                     [theDefaultsController save: self];
                     [panel close];
                    }
                  else if (returnCode == NSAlertSecondButtonReturn)
                    { [panel makeKeyAndOrderFront: nil]; }
                  else if (returnCode == NSAlertThirdButtonReturn)
                    {
                     [theDefaultsController revert: self];
                     [panel close];
                    }
                 }];
 
   return NO;
  }
  
/************/
/* doCancel: */
/************/
- (IBAction) doCancel: (id) sender
  {
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   [theDefaultsController revert: self];
   
   [[self window] close];
  }
  
/*********/
/* doOK: */
/*********/
- (IBAction) doOK: (id) sender
  {
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   [theDefaultsController save: self];
   
   [[self window] close];
  }

/************************************/
/* reviewPreferencesBeforeQuitting: */
/************************************/  
- (NSApplicationTerminateReply) reviewPreferencesBeforeQuitting
  {
   NSUserDefaultsController *theDefaultsController;
      
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];

   if (! [theDefaultsController hasUnappliedChanges])
     { return NSTerminateNow; }

   NSAlert *alert = [[NSAlert alloc] init];
   alert.messageText =  @"Do you want to save changes to your Preferences?";
   [alert addButtonWithTitle: @"Save"];
   [alert addButtonWithTitle: @"Cancel"];
   [alert addButtonWithTitle: @"Don't Save"];
   
   [alert beginSheetModalForWindow: [self window]
                 completionHandler: ^(NSInteger returnCode)
                 {
                  NSUserDefaultsController *theDefaultsController;
                  NSWindow *panel = [self window];
                  theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];
                  if (returnCode == NSAlertFirstButtonReturn) // Save
                    {
                     [panel close];
                     [theDefaultsController save: self];
                     [theDefaultsController setAppliesImmediately: YES]; // Saving does not appear to work reliably
                     [theDefaultsController setAppliesImmediately: NO];  // Hence the need to force the save
                     [NSApp replyToApplicationShouldTerminate: YES];
                    }
                  else if (returnCode == NSAlertSecondButtonReturn) // Cancel
                    {
                      [panel makeKeyAndOrderFront: nil];
                      [NSApp replyToApplicationShouldTerminate: NO];
                     }
                  else if (returnCode == NSAlertThirdButtonReturn)
                    {
                     [theDefaultsController revert: self];
                     [panel close];
                     [NSApp replyToApplicationShouldTerminate: YES];
                    }
                 }];

   return NSTerminateLater;  
  }

@end

/*##################################*/
/* FontNameToDisplayNameTransformer */
/*##################################*/

@implementation FontNameToDisplayNameTransformer

+ (Class) transformedValueClass
  {
   return [NSString class];
  }

+ (BOOL) allowsReverseTransformation
  {
   return NO;
  }

- (id) transformedValue: (id) aValue
  {
   NSFont *font = [NSFont fontWithName:aValue size:12];
   return [font displayName];
  }

@end
