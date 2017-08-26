#import "AppController.h"
#import "PreferenceController.h"
#import "CLIPSTerminalController.h"
#import "CLIPSAgendaController.h"
#import "CLIPSFactController.h"
#import "CLIPSInstanceController.h"
#import "CLIPSConstructInspectorController.h"
#import "CLIPSEnvironment.h"
#import <CLIPS/clips.h>

@implementation AppController

@synthesize mainEnvironment, mainTerminal, constructInspectorText;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/***********************************************/
/* initialize: Set up the default preferences. */
/***********************************************/
+ (void) initialize
  { 
   NSDictionary *appDefaults; 
   NSUserDefaults *defaults; 
   NSFont *theFont;
   
   theFont = [NSFont userFixedPitchFontOfSize:0.0];

   appDefaults = 
      [NSDictionary dictionaryWithObjectsAndKeys:

         [theFont fontName],                              @"editorTextFontName",
         [NSNumber numberWithDouble:[theFont pointSize]], @"editorTextFontSize",
         [NSNumber numberWithBool:YES],                   @"editorBalanceParens",

         [theFont fontName],                              @"dialogTextFontName",
         [NSNumber numberWithDouble:[theFont pointSize]], @"dialogTextFontSize",
         [NSNumber numberWithBool:YES],                   @"dialogBalanceParens",
       
         nil]; 
 
   defaults = [NSUserDefaults standardUserDefaults]; 
   [defaults registerDefaults: appDefaults]; 

   [[NSUserDefaultsController sharedUserDefaultsController] setInitialValues:appDefaults];
  } 

/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   factControllers = [[NSMutableArray alloc] init];
   instanceControllers = [[NSMutableArray alloc] init];
   agendaControllers = [[NSMutableArray alloc] init];
   fileOpenLock = [[NSLock alloc] init];
  }

/**************************/
/* setWatchMenuItemState: */
/**************************/
- (void) setWatchMenuItemState: (NSMenuItem *) menuItem forItem: (WatchItem) watchItem
  {
   if ([mainEnvironment getWatchState: watchItem])
     { [menuItem setState: NSOnState]; }
   else
     { [menuItem setState: NSOffState]; }
  }

/*********************/
/* validateMenuItem: */
/*********************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   /*===================================*/
   /* Set the state of the watch items. */
   /*===================================*/

   if ([menuItem action] == @selector(watchCompilations:))
     { [self setWatchMenuItemState: menuItem forItem: COMPILATIONS]; }

   if ([menuItem action] == @selector(watchStatistics:))
     { [self setWatchMenuItemState: menuItem forItem: STATISTICS]; }

   if ([menuItem action] == @selector(watchFacts:))
     { [self setWatchMenuItemState: menuItem forItem: FACTS]; }

   if ([menuItem action] == @selector(watchRules:))
     { [self setWatchMenuItemState: menuItem forItem: RULES]; }

   if ([menuItem action] == @selector(watchActivations:))
     { [self setWatchMenuItemState: menuItem forItem: ACTIVATIONS]; }

   if ([menuItem action] == @selector(watchFocus:))
     { [self setWatchMenuItemState: menuItem forItem: FOCUS]; }

   if ([menuItem action] == @selector(watchGlobals:))
     { [self setWatchMenuItemState: menuItem forItem: GLOBALS]; }

   if ([menuItem action] == @selector(watchDeffunctions:))
     { [self setWatchMenuItemState: menuItem forItem: DEFFUNCTIONS]; }

   if ([menuItem action] == @selector(watchGenericFunctions:))
     { [self setWatchMenuItemState: menuItem forItem: GENERIC_FUNCTIONS]; }

   if ([menuItem action] == @selector(watchMethods:))
     { [self setWatchMenuItemState: menuItem forItem: METHODS]; }

   if ([menuItem action] == @selector(watchInstances:))
     { [self setWatchMenuItemState: menuItem forItem: INSTANCES]; }

   if ([menuItem action] == @selector(watchSlots:))
     { [self setWatchMenuItemState: menuItem forItem: SLOTS]; }

   if ([menuItem action] == @selector(watchMessageHandlers:))
     { [self setWatchMenuItemState: menuItem forItem: MESSAGE_HANDLERS]; }

   if ([menuItem action] == @selector(watchMessages:))
     { [self setWatchMenuItemState: menuItem forItem: MESSAGES]; }

   /*===================================================*/
   /* The Halt and Halt Immediately menu items are only */
   /* available if the CLIPS environment is executing.  */
   /*===================================================*/
   
   if (([menuItem action] == @selector(haltRules:)) ||
       ([menuItem action] == @selector(haltExecution:)))
     {
      if ([[mainEnvironment executionLock] tryLock])
        {
         [[mainEnvironment executionLock] unlock];
         return NO;
        }
      else
        { return YES; }
     }
     
   /*=====================================================*/
   /* The Load Constructs, Load Batch, Set Directory, and */
   /* Clear Scrollback menu items are only available if   */
   /* the CLIPS environment is not executing.             */
   /*=====================================================*/

   else if (([menuItem action] == @selector(clear:)) ||
            ([menuItem action] == @selector(loadConstructs:)) ||
            ([menuItem action] == @selector(loadBatch:)) ||
            ([menuItem action] == @selector(setDirectory:)) ||
            ([menuItem action] == @selector(reset:)) ||
            ([menuItem action] == @selector(run:)) ||
            ([menuItem action] == @selector(clearScrollback:)))
     {
      if ([[mainEnvironment executionLock] tryLock])
        {
         [[mainEnvironment executionLock] unlock];
         return YES;
        }
      else
        { return NO; }
     }

   /*===================================*/
   /* Otherwise the menu item is valid. */
   /*===================================*/
   
   return YES;
  }
  
/**********************/
/* addFactController: */
/**********************/
- (void) addFactController: (CLIPSFactController *) theController
  {
   [factControllers addObject: theController];
  }

/*************************/
/* removeFactController: */
/*************************/
- (void) removeFactController: (CLIPSFactController *) theController
  {
   [factControllers removeObject: theController];
  }

/**************************/
/* addInstanceController: */
/**************************/
- (void) addInstanceController: (CLIPSInstanceController *) theController
  {
   [instanceControllers addObject: theController];
  }

/*****************************/
/* removeInstanceController: */
/*****************************/
- (void) removeInstanceController: (CLIPSInstanceController *) theController
  {
   [instanceControllers removeObject: theController];
  }

/************************/
/* addAgendaController: */
/************************/
- (void) addAgendaController: (CLIPSAgendaController *) theController
  {
   [agendaControllers addObject: theController];
  }

/***************************/
/* removeAgendaController: */
/***************************/
- (void) removeAgendaController: (CLIPSAgendaController *) theController
  {
   [agendaControllers removeObject: theController];
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

/*********/
/* clear */
/*********/
- (IBAction) clear: (id) sender
  {
   [mainEnvironment doCommand: @"(clear)\n"];
  }

/*******************/
/* loadConstructs: */
/*******************/
- (IBAction) loadConstructs: (id) sender
  {
   [mainTerminal loadConstructs: self];
  }

/**************/
/* loadBatch: */
/**************/
- (IBAction) loadBatch: (id) sender
  {
   [mainTerminal loadBatch: self];
  }

/*****************/
/* setDirectory: */
/*****************/
- (IBAction) setDirectory: (id) sender
  {
   [mainTerminal setDirectory: self];
  }

/*********/
/* reset */
/*********/
- (IBAction) reset: (id) sender
  {
   [mainEnvironment doCommand: @"(reset)\n"];
  }

/*******/
/* run */
/*******/
- (IBAction) run: (id) sender
  {
   [mainEnvironment doCommand: @"(run)\n"];
  }

/**************/
/* haltRules: */
/**************/
- (IBAction) haltRules: (id) sender
  {
   SetHaltRules([mainEnvironment environment],true);
  }

/******************/
/* haltExecution: */
/******************/
- (IBAction) haltExecution: (id) sender
  {
   /* Need to abort waitForChar */
   /* Need to abort batch */
   SetHaltCommandLoopBatch([mainEnvironment environment],true);
   SetHaltExecution([mainEnvironment environment],true);
  }

/********************/
/* clearScrollback: */
/********************/
- (IBAction) clearScrollback: (id) sender
  {
   [mainTerminal clearScrollback: self];
  }

/************************/
/* showPreferencePanel: */
/************************/
- (IBAction) showPreferencePanel: (id) sender
  {
   if (! preferenceController)
     { preferenceController = [[PreferenceController alloc] init]; }
    
   [preferenceController showPanel];
  }

/*******************/
/* toggleWatchItem */
/*******************/
- (void) toggleWatchItem: (WatchItem) watchItem
  {
   [mainEnvironment setWatchState: watchItem
                         toValue: ! [mainEnvironment getWatchState: watchItem]];
  }

/*****************/
/* setWatchState */
/*****************/
- (void) setWatchState: (WatchItem) watchItem toValue: (bool) value
  {
   [mainEnvironment setWatchState: watchItem
                         toValue: value];
  }

/*********************/
/* watchCompilations */
/*********************/
- (IBAction) watchCompilations: (id) sender
  {
   [self toggleWatchItem: COMPILATIONS];
  }

/*******************/
/* watchStatistics */
/*******************/
- (IBAction) watchStatistics: (id) sender
  {
   [self toggleWatchItem: STATISTICS];
  }

/**************/
/* watchFacts */
/**************/
- (IBAction) watchFacts: (id) sender
  {
   [self toggleWatchItem: FACTS];
  }

/**************/
/* watchRules */
/**************/
- (IBAction) watchRules: (id) sender
  {
   [self toggleWatchItem: RULES];
  }

/********************/
/* watchActivations */
/********************/
- (IBAction) watchActivations: (id) sender
  {
   [self toggleWatchItem: ACTIVATIONS];
  }

/**************/
/* watchFocus */
/**************/
- (IBAction) watchFocus: (id) sender
  {
   [self toggleWatchItem: FOCUS];
  }

/****************/
/* watchGlobals */
/****************/
- (IBAction) watchGlobals: (id) sender
  {
   [self toggleWatchItem: GLOBALS];
  }

/*********************/
/* watchDeffunctions */
/*********************/
- (IBAction) watchDeffunctions: (id) sender
  {
   [self toggleWatchItem: DEFFUNCTIONS];
  }

/*************************/
/* watchGenericFunctions */
/*************************/
- (IBAction) watchGenericFunctions: (id) sender
  {
   [self toggleWatchItem: GENERIC_FUNCTIONS];
  }

/****************/
/* watchMethods */
/****************/
- (IBAction) watchMethods: (id) sender
  {
   [self toggleWatchItem: METHODS];
  }

/******************/
/* watchInstances */
/******************/
- (IBAction) watchInstances: (id) sender
  {
   [self toggleWatchItem: INSTANCES];
  }

/**************/
/* watchSlots */
/**************/
- (IBAction) watchSlots: (id) sender
  {
   [self toggleWatchItem: SLOTS];
  }

/*************/
/* watchAll: */
/*************/
- (IBAction) watchAll: (id) sender
  {
   [self setWatchState: COMPILATIONS toValue: YES];
   [self setWatchState: FACTS toValue: YES];
   [self setWatchState: RULES toValue: YES];
   [self setWatchState: STATISTICS toValue: YES];
   [self setWatchState: ACTIVATIONS toValue: YES];
   [self setWatchState: FOCUS toValue: YES];
   [self setWatchState: GLOBALS toValue: YES];
   [self setWatchState: DEFFUNCTIONS toValue: YES];
   [self setWatchState: GENERIC_FUNCTIONS toValue: YES];
   [self setWatchState: METHODS toValue: YES];
   [self setWatchState: INSTANCES toValue: YES];
   [self setWatchState: SLOTS toValue: YES];
   [self setWatchState: MESSAGE_HANDLERS toValue: YES];
   [self setWatchState: MESSAGES toValue: YES];
  }
  
/**************/
/* watchNone: */
/**************/
- (IBAction) watchNone: (id) sender
  {
   [self setWatchState: COMPILATIONS toValue: NO];
   [self setWatchState: FACTS toValue: NO];
   [self setWatchState: RULES toValue: NO];
   [self setWatchState: STATISTICS toValue: NO];
   [self setWatchState: ACTIVATIONS toValue: NO];
   [self setWatchState: FOCUS toValue: NO];
   [self setWatchState: GLOBALS toValue: NO];
   [self setWatchState: DEFFUNCTIONS toValue: NO];
   [self setWatchState: GENERIC_FUNCTIONS toValue: NO];
   [self setWatchState: METHODS toValue: NO];
   [self setWatchState: INSTANCES toValue: NO];
   [self setWatchState: SLOTS toValue: NO];
   [self setWatchState: MESSAGE_HANDLERS toValue: NO];
   [self setWatchState: MESSAGES toValue: NO];
  }

/************************/
/* watchMessageHandlers */
/************************/
- (IBAction) watchMessageHandlers: (id) sender
  {
   [self toggleWatchItem: MESSAGE_HANDLERS];
  }

/*****************/
/* watchMessages */
/*****************/
- (IBAction) watchMessages: (id) sender
  {
   [self toggleWatchItem: MESSAGES];
  }

/******************/
/* agendaBrowser: */
/******************/
- (IBAction) agendaBrowser: (id) sender
  {
   CLIPSAgendaController *theController;
      
   theController = [[CLIPSAgendaController alloc] init];
   
   [self addAgendaController: theController];
      
   [theController showWindow: self];
  }

/****************/
/* factBrowser: */
/****************/
- (IBAction) factBrowser: (id) sender
  {
   CLIPSFactController *theController;
 
   theController = [[CLIPSFactController alloc] init];

   [self addFactController: theController];
   
   [theController showWindow: self];
  }

/********************/
/* instanceBrowser: */
/********************/
- (IBAction) instanceBrowser: (id) sender
  {
   CLIPSInstanceController *theController;
      
   theController = [[CLIPSInstanceController alloc] init]; 

   [self addInstanceController: theController];
      
   [theController showWindow: self];
  }

/***********************/
/* constructInspector: */
/***********************/
- (IBAction) constructInspector: (id) sender
  {
   if (! constructInspectorController)
     { constructInspectorController = [[CLIPSConstructInspectorController alloc] init]; }
    
   [constructInspectorController showPanel];
  }

/***************************************************************/
/* showCLIPSDocumentation: Opens the CLIPS Documentation Page. */
/***************************************************************/
- (IBAction) showCLIPSDocumentation: (id) sender
  {
   [[NSWorkspace sharedWorkspace] 
       openURL: [NSURL URLWithString: @"http://www.clipsrules.net/?q=Documentation"]];
  }

/*************************************************/
/* showCLIPSHomePage: Opens the CLIPS Home Page. */
/*************************************************/
- (IBAction) showCLIPSHomePage: (id) sender
  {
   [[NSWorkspace sharedWorkspace] 
       openURL: [NSURL URLWithString: @"http://www.clipsrules.net/"]];
  }

/*****************************************************/
/* showCLIPSExamples: Opens the CLIPS Examples Page. */
/*****************************************************/
- (IBAction) showCLIPSExamples: (id) sender
  {
   [[NSWorkspace sharedWorkspace] 
       openURL: [NSURL URLWithString: @"https://sourceforge.net/p/clipsrules/code/HEAD/tree/branches/64x/examples/"]];
  }

/*****************************************/
/* showCLIPSExpertSystemGroup: Opens the */
/*   CLIPS Expert System Group Web Page. */
/*****************************************/
- (IBAction) showCLIPSExpertSystemGroup: (id) sender
  {
   [[NSWorkspace sharedWorkspace] 
       openURL: [NSURL URLWithString: @"http://groups.google.com/group/CLIPSESG/"]];
  }

/********************************************/
/* showCLIPSSourceForgeForums: Opens the    */
/*   CLIPS SourceForge Discussion Web Page. */
/********************************************/
- (IBAction) showCLIPSSourceForgeDiscussion: (id) sender
  {
   [[NSWorkspace sharedWorkspace] 
       openURL: [NSURL URLWithString: @"http://sourceforge.net/p/clipsrules/discussion"]];
  }

/************************************/
/* showCLIPStackOverflow: Opens the */
/*   CLIPS Stack Overflow Page.     */
/************************************/
- (IBAction) showCLIPSStackOverflow: (id) sender
  {
   [[NSWorkspace sharedWorkspace] 
       openURL: [NSURL URLWithString: @"http://stackoverflow.com/questions/tagged/clips"]];
  }

/*********************************/
/* setPreferencesFromWatchFlags: */
/*********************************/
- (void) setPreferencesFromWatchFlags
  {
   NSUserDefaults *theValues;
   NSNumber *watchValue;
   AppController *theDelegate = [NSApp delegate];
   CLIPSTerminalController *theController = [theDelegate mainTerminal];
   CLIPSEnvironment *theEnv = [theController environment];
   NSUserDefaultsController *theDefaultsController;
   
   theDefaultsController = [NSUserDefaultsController sharedUserDefaultsController];
   theValues = [theDefaultsController values];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: COMPILATIONS]];
   [theValues setValue: watchValue forKey: @"compilations"];
   
   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: FACTS]];
   [theValues setValue: watchValue forKey: @"facts"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: RULES]];
   [theValues setValue: watchValue forKey: @"rules"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: STATISTICS]];
   [theValues setValue: watchValue forKey: @"statistics"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: ACTIVATIONS]];
   [theValues setValue: watchValue forKey: @"activations"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: FOCUS]];
   [theValues setValue: watchValue forKey: @"focus"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: GLOBALS]];
   [theValues setValue: watchValue forKey: @"globals"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: DEFFUNCTIONS]];
   [theValues setValue: watchValue forKey: @"deffunctions"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: GENERIC_FUNCTIONS]];
   [theValues setValue: watchValue forKey: @"generic-functions"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: METHODS]];
   [theValues setValue: watchValue forKey: @"methods"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: INSTANCES]];
   [theValues setValue: watchValue forKey: @"instances"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: SLOTS]];
   [theValues setValue: watchValue forKey: @"slots"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: MESSAGE_HANDLERS]];
   [theValues setValue: watchValue forKey: @"message-handlers"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchState: MESSAGES]];
   [theValues setValue: watchValue forKey: @"messages"];
   
   [theDefaultsController save: self];
  }

/*%%%%%%%%%%%%%%%%%%*/
/* Delegate Methods */
/*%%%%%%%%%%%%%%%%%%*/

/**********************************/
/* applicationDidFinishLaunching: */
/**********************************/
- (void) applicationDidFinishLaunching: (NSNotification *) aNotification
  {
   mainEnvironment = [[CLIPSEnvironment alloc] init];
   mainTerminal = [[CLIPSTerminalController alloc] initWithEnvironment: mainEnvironment];
   
   [mainTerminal showWindow: self];
   [[mainTerminal window] makeKeyAndOrderFront: self];
  }

/***********************************************************/
/* applicationShouldOpenUntitledFile: This delegate method */
/*   is used to indicate that an untitled file should not  */
/*   be opened when the application is launched.           */
/***********************************************************/
- (BOOL) applicationShouldOpenUntitledFile: (NSApplication *) sender
  {
   return NO;
  }
        
/*******************************/
/* applicationShouldTerminate: */
/*******************************/
- (NSApplicationTerminateReply) applicationShouldTerminate: (NSApplication *) app
  {
   [self setPreferencesFromWatchFlags];
   
   if (preferenceController != nil)
     { return [preferenceController reviewPreferencesBeforeQuitting]; }
     
   return NSTerminateNow;
  }
  
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
 
/****************************/
/* fileOpenLock: */
/****************************/
- (NSLock *) fileOpenLock
  {
   return fileOpenLock;
  }

@end
