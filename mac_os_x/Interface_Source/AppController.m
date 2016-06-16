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

         [theFont fontName],                             @"editorTextFontName",
         [NSNumber numberWithFloat:[theFont pointSize]], @"editorTextFontSize", 
         [NSNumber numberWithBool:YES],                  @"editorBalanceParens",

         [theFont fontName],                             @"dialogTextFontName",
         [NSNumber numberWithFloat:[theFont pointSize]], @"dialogTextFontSize",
         [NSNumber numberWithBool:YES],                  @"dialogBalanceParens",
       
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
- (void) setWatchMenuItemState: (NSMenuItem *) menuItem forItem: (char *) watchItem
  {
   if ([mainEnvironment getWatchItem: watchItem])
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
     { [self setWatchMenuItemState: menuItem forItem: "compilations"]; }

   if ([menuItem action] == @selector(watchStatistics:))
     { [self setWatchMenuItemState: menuItem forItem: "statistics"]; }

   if ([menuItem action] == @selector(watchFacts:))
     { [self setWatchMenuItemState: menuItem forItem: "facts"]; }

   if ([menuItem action] == @selector(watchRules:))
     { [self setWatchMenuItemState: menuItem forItem: "rules"]; }

   if ([menuItem action] == @selector(watchActivations:))
     { [self setWatchMenuItemState: menuItem forItem: "activations"]; }

   if ([menuItem action] == @selector(watchFocus:))
     { [self setWatchMenuItemState: menuItem forItem: "focus"]; }

   if ([menuItem action] == @selector(watchGlobals:))
     { [self setWatchMenuItemState: menuItem forItem: "globals"]; }

   if ([menuItem action] == @selector(watchDeffunctions:))
     { [self setWatchMenuItemState: menuItem forItem: "deffunctions"]; }

   if ([menuItem action] == @selector(watchGenericFunctions:))
     { [self setWatchMenuItemState: menuItem forItem: "generic-functions"]; }

   if ([menuItem action] == @selector(watchMethods:))
     { [self setWatchMenuItemState: menuItem forItem: "methods"]; }

   if ([menuItem action] == @selector(watchInstances:))
     { [self setWatchMenuItemState: menuItem forItem: "instances"]; }

   if ([menuItem action] == @selector(watchSlots:))
     { [self setWatchMenuItemState: menuItem forItem: "slots"]; }

   if ([menuItem action] == @selector(watchMessageHandlers:))
     { [self setWatchMenuItemState: menuItem forItem: "message-handlers"]; }

   if ([menuItem action] == @selector(watchMessages:))
     { [self setWatchMenuItemState: menuItem forItem: "messages"]; }

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
   EnvSetHaltRules([mainEnvironment environment],true);
  }

/******************/
/* haltExecution: */
/******************/
- (IBAction) haltExecution: (id) sender
  {
   /* Need to abort waitForChar */
   /* Need to abort batch */
   SetHaltCommandLoopBatch([mainEnvironment environment],true);
   EnvSetHaltExecution([mainEnvironment environment],true);
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
- (void) toggleWatchItem: (char *) watchItem
  {
   [mainEnvironment setWatchItem: watchItem
                         toValue: ! [mainEnvironment getWatchItem: watchItem]];
  }

/****************/
/* setWatchItem */
/****************/
- (void) setWatchItem: (char *) watchItem toValue: (bool) value
  {
   [mainEnvironment setWatchItem: watchItem
                         toValue: value];
  }

/*********************/
/* watchCompilations */
/*********************/
- (IBAction) watchCompilations: (id) sender
  {
   [self toggleWatchItem: "compilations"];
  }

/*******************/
/* watchStatistics */
/*******************/
- (IBAction) watchStatistics: (id) sender
  {
   [self toggleWatchItem: "statistics"];
  }

/**************/
/* watchFacts */
/**************/
- (IBAction) watchFacts: (id) sender
  {
   [self toggleWatchItem: "facts"];
  }

/**************/
/* watchRules */
/**************/
- (IBAction) watchRules: (id) sender
  {
   [self toggleWatchItem: "rules"];
  }

/********************/
/* watchActivations */
/********************/
- (IBAction) watchActivations: (id) sender
  {
   [self toggleWatchItem: "activations"];
  }

/**************/
/* watchFocus */
/**************/
- (IBAction) watchFocus: (id) sender
  {
   [self toggleWatchItem: "focus"];
  }

/****************/
/* watchGlobals */
/****************/
- (IBAction) watchGlobals: (id) sender
  {
   [self toggleWatchItem: "globals"];
  }

/*********************/
/* watchDeffunctions */
/*********************/
- (IBAction) watchDeffunctions: (id) sender
  {
   [self toggleWatchItem: "deffunctions"];
  }

/*************************/
/* watchGenericFunctions */
/*************************/
- (IBAction) watchGenericFunctions: (id) sender
  {
   [self toggleWatchItem: "generic-functions"];
  }

/****************/
/* watchMethods */
/****************/
- (IBAction) watchMethods: (id) sender
  {
   [self toggleWatchItem: "methods"];
  }

/******************/
/* watchInstances */
/******************/
- (IBAction) watchInstances: (id) sender
  {
   [self toggleWatchItem: "instances"];
  }

/**************/
/* watchSlots */
/**************/
- (IBAction) watchSlots: (id) sender
  {
   [self toggleWatchItem: "slots"];
  }

/*************/
/* watchAll: */
/*************/
- (IBAction) watchAll: (id) sender
  {
   [self setWatchItem: "compilations" toValue: YES];
   [self setWatchItem: "facts" toValue: YES];
   [self setWatchItem: "rules" toValue: YES];
   [self setWatchItem: "statistics" toValue: YES];
   [self setWatchItem: "activations" toValue: YES];
   [self setWatchItem: "focus" toValue: YES];
   [self setWatchItem: "globals" toValue: YES];
   [self setWatchItem: "deffunctions" toValue: YES];
   [self setWatchItem: "generic-functions" toValue: YES];
   [self setWatchItem: "methods" toValue: YES];
   [self setWatchItem: "instances" toValue: YES];
   [self setWatchItem: "slots" toValue: YES];
   [self setWatchItem: "message-handlers" toValue: YES];
   [self setWatchItem: "messages" toValue: YES];
  }
  
/**************/
/* watchNone: */
/**************/
- (IBAction) watchNone: (id) sender
  {
   [self setWatchItem: "compilations" toValue: NO];
   [self setWatchItem: "facts" toValue: NO];
   [self setWatchItem: "rules" toValue: NO];
   [self setWatchItem: "statistics" toValue: NO];
   [self setWatchItem: "activations" toValue: NO];
   [self setWatchItem: "focus" toValue: NO];
   [self setWatchItem: "globals" toValue: NO];
   [self setWatchItem: "deffunctions" toValue: NO];
   [self setWatchItem: "generic-functions" toValue: NO];
   [self setWatchItem: "methods" toValue: NO];
   [self setWatchItem: "instances" toValue: NO];
   [self setWatchItem: "slots" toValue: NO];
   [self setWatchItem: "message-handlers" toValue: NO];
   [self setWatchItem: "messages" toValue: NO];
  }

/************************/
/* watchMessageHandlers */
/************************/
- (IBAction) watchMessageHandlers: (id) sender
  {
   [self toggleWatchItem: "message-handlers"];
  }

/*****************/
/* watchMessages */
/*****************/
- (IBAction) watchMessages: (id) sender
  {
   [self toggleWatchItem: "messages"];
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
       openURL: [NSURL URLWithString: @"https://sourceforge.net/p/clipsrules/code/HEAD/tree/examples/"]];
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

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "compilations"]];
   [theValues setValue: watchValue forKey: @"compilations"];
   
   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "facts"]];
   [theValues setValue: watchValue forKey: @"facts"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "rules"]];
   [theValues setValue: watchValue forKey: @"rules"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "statistics"]];
   [theValues setValue: watchValue forKey: @"statistics"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "activations"]];
   [theValues setValue: watchValue forKey: @"activations"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "focus"]];
   [theValues setValue: watchValue forKey: @"focus"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "globals"]];
   [theValues setValue: watchValue forKey: @"globals"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "deffunctions"]];
   [theValues setValue: watchValue forKey: @"deffunctions"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "generic-functions"]];
   [theValues setValue: watchValue forKey: @"generic-functions"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "methods"]];
   [theValues setValue: watchValue forKey: @"methods"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "instances"]];
   [theValues setValue: watchValue forKey: @"instances"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "slots"]];
   [theValues setValue: watchValue forKey: @"slots"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "message-handlers"]];
   [theValues setValue: watchValue forKey: @"message-handlers"];

   watchValue = [NSNumber numberWithBool: [theEnv getWatchItem: "messages"]];
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
