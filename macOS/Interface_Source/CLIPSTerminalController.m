//
//  CLIPSTerminalController.m
//  CLIPS
//
//  Created by Gary Riley on 2/25/06.
//

#import "CLIPSTerminalView.h"
#import "CLIPSTerminalController.h"
#import "AppController.h"
#import "CLIPSEnvironment.h"
#import "CLIPSTerminalGlue.h"

#include <CLIPS/clips.h>

#define BUFFER_SIZE 32768
#define CLIPS_TERMINAL_LINE_MAX 10000
#define DELETE_AT_LEAST 1000

@implementation CLIPSTerminalController

   static int                     dialogCount = 1;
   /*
   static int                     bufferMax = 0;
   static int                     lineMax = 0;
   */

#define DEFAULT_COMMAND_MAX 20

/****************/
/* description: */
/****************/
- (NSString *) description
  {
   return [[self window] title];
  }
     
/*********/
/* init: */
/*********/
- (id) initWithEnvironment: (CLIPSEnvironment *) theEnvironment
  {
   self = [super initWithWindowNibName:@"CLIPSTerminal"];
   
   if (self)
     {
      environment = theEnvironment;
      outputBuffer = [NSMutableString stringWithCapacity: BUFFER_SIZE];
      [outputBuffer setString: @""];
      
      outputBufferLock = [[NSConditionLock alloc] initWithCondition: BUFFER_IS_EMPTY];
      
      lineCount = 1;
      lastDumpPosition = 0;
      
      /*===================================*/
      /* Set up the prior command history. */
      /*===================================*/
   
      self->maxCommandCount = DEFAULT_COMMAND_MAX;
      self->currentCommandCount = 1;
      self->topCommand = (struct priorCommand *) malloc(sizeof(struct priorCommand));
      self->currentCommand = self->topCommand;
      self->bottomCommand = self->topCommand;
      self->topCommand->next = NULL;
      self->topCommand->prev = NULL;
      self->topCommand->command = (char *) malloc(1);
      self->topCommand->command[0] = '\0';
     }
     
   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   Environment *theEnvironment = [environment environment];
   struct priorCommand *nextCommand;
   
   /*=================================*/
   /* Deallocate the command history. */
   /*=================================*/
   
   while (self->topCommand != NULL)
     {
      nextCommand = self->topCommand->next;
      
      free(topCommand->command);
      free(topCommand);
      
      self->topCommand = nextCommand;
     }

   SetBeforeOpenFunction(theEnvironment,NULL);
   SetAfterOpenFunction(theEnvironment,NULL);
  }
    
/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   Environment *theEnvironment = [environment environment];
   AppController *theDelegate = [NSApp delegate];

   [textView setEnvironment: environment];
   
   /*=====================================*/
   /* Set the delegate for the text view. */
   /*=====================================*/
   
   textView.delegate = self;
   
   /*===========================================*/
   /* Disable automatic text substitution (such */
   /* as replacing quotes with curly quotes).   */
   /*===========================================*/
   
   [textView setEnabledTextCheckingTypes: 0];
   
   /*======================================================*/
   /* Places a few pixels of white space between the edges */
   /* of the window and the rectangle in which the text is */
   /* displayed in the editor window.                      */
   /*======================================================*/
   
   NSSize theSize = { 3, 3 };
   [textView setTextContainerInset: theSize];

   /*====================================================*/
   /* Add an index value to the window names that are    */
   /* created (except for the first) so that the windows */
   /* will be named Dialog, Dialog 2, Dialog 3, ...      */
   /*====================================================*/
                  
   if (dialogCount > 1)
     { 
      [[self window] setTitle:[NSString stringWithFormat:@"Dialog %d", dialogCount]]; // TBD Autorelease?
     }
     
    dialogCount++;

   /*===================================================*/
   /* Give the environment the same name as the window. */
   /*===================================================*/
       
   [environment setName: [[self window] title]];

   /*=================================================*/
   /* This setting for this attribute isn't preserved */
   /* when set in Interface Builder.                  */
   /*=================================================*/
   
   [executionIndicator setDisplayedWhenStopped: NO];
   
   /*===================================================*/
   /* This attribute can't be set in Interface Builder. */
   /*===================================================*/
   
   [textView setUsesRuler: NO];
   
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
   
   /*============================*/
   /* Set the current directory. */
   /*============================*/

   NSLock *theLock = [theDelegate fileOpenLock];
   
   [theLock lock];

   NSUserDefaults *theValues;
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
   
   NSString *theDirectory = [theValues valueForKey: @"currentDirectory"];

   if (theDirectory == nil)
     { theDirectory = [[NSFileManager defaultManager] currentDirectoryPath]; }
   
   [self setCurrentDirectory: theDirectory];
   
   [self setValue: [[self currentDirectory] stringByAbbreviatingWithTildeInPath]
         forKey: @"displayDirectory"];
   
   [theLock unlock];
     
   /*=================*/
   /* CLIPS Glue Code */
   /*=================*/
   
   SetEnvironmentContext(theEnvironment,(__bridge void *)(self)); // TBD Remove?
   
   AddRouter(theEnvironment,"CLIPSTerminalController",10,QueryInterfaceRouter,PrintInterfaceRouter,
             GetcInterfaceRouter,NULL,ExitInterfaceRouter,(__bridge void *)(self));
        
   SetBeforeOpenFunction(theEnvironment,MacBeforeOpenFunction);
   SetAfterOpenFunction(theEnvironment,MacAfterOpenFunction);
   
   /*===================================*/
   /* Create the lock used for pausing. */ // TBD Why here and not init?
   /*===================================*/
   
   pauseLock = [[NSConditionLock alloc] initWithCondition: EXECUTION_IS_NOT_PAUSED];

   /*===================================*/
   /* Define the clear-window function. */
   /*===================================*/
   
   AddUDF(theEnvironment,"clear-window","v",0,0,NULL,ClearEnvironmentWindowCommand,"ClearEnvironmentWindowCommand",NULL);

   AddPeriodicFunction(theEnvironment,"mac-pf",MacPeriodicFunction,0,NULL);

   commandTimer = [NSTimer scheduledTimerWithTimeInterval: 0.2 // TBD Need to lock input once command found.
                     target: self
                     selector: @selector(lookForCommand:)
                     userInfo: nil
                     repeats: YES];

   scrollTimer = [NSTimer scheduledTimerWithTimeInterval: 0.1 // TBD Need to lock input once command found. was 0.1
                     target: self
                     selector: @selector(scrollToEndCheck:)
                     userInfo: nil
                     repeats: YES];

   updateTimer = [NSTimer scheduledTimerWithTimeInterval: 0.1
                     target: self
                     selector: @selector(lookForUpdates:)
                     userInfo: nil
                     repeats: YES];
                         
   /*===========================================*/
   /* Retrieve the watch flags settings for new */
   /* environments from the user's preferences. */
   /*===========================================*/
   
   [self setWatchFlagsFromPreferences];
      
   PrintBanner(theEnvironment);
   PrintPrompt(theEnvironment);
   RouterData(theEnvironment)->CommandBufferInputCount = 0;
  }

/*************************/
/* updateCommandHistory: */
/*************************/
- (void) updateCommandHistory
  {
   char *theCommand;
   size_t i, length, lastCR;
   struct priorCommand *nextCommand;

   /*=================================================*/
   /* Replace the first command with the contents of  */
   /* the command string, up to but not including the */ 
   /* last carriage return which initiated execution  */
   /* of the command. Removing the last carriage      */
   /* will prevent the command from being immediately */
   /* executed when the command is recalled by the    */
   /* up/down arrow keys (i.e. the user must hit the  */
   /* final carriage return again to execute the      */
   /* recalled command).                              */
   /*=================================================*/
      
   free(self->topCommand->command);

   theCommand = GetCommandString([environment environment]);
   length = strlen(theCommand);
   
   for (i = 0, lastCR = length; i < length; i++)
     {
      if (theCommand[i] == '\n')
        { lastCR = i; }
     }   

   self->topCommand->command = (char *) malloc(lastCR + 1);
   strncpy(self->topCommand->command,theCommand,lastCR);
   self->topCommand->command[lastCR] = '\0';
   
   /*====================================================*/
   /* If this command is identical to the prior command, */
   /* don't add it to the command history.               */
   /*====================================================*/
    
   if ((self->topCommand->next != NULL) &&
       (strcmp(self->topCommand->command,self->topCommand->next->command) == 0))
     {
      free(self->topCommand->command);
      self->topCommand->command = (char *) malloc(1);
      self->topCommand->command[0] = '\0';
	  self->currentCommand = self->topCommand;
      return;
     }
     
   /*=================================================*/
   /* Add a new empty command to the top of the stack */
   /* in preparation for the next user command.       */
   /*=================================================*/

   nextCommand = (struct priorCommand *) malloc(sizeof(struct priorCommand));
   nextCommand->next = self->topCommand;
   nextCommand->prev = NULL;
   nextCommand->command = (char *) malloc(1);
   nextCommand->command[0] = '\0';
   
   self->topCommand->prev = nextCommand;
   self->topCommand = nextCommand;
   self->currentCommand = nextCommand;
   
   self->currentCommandCount++;
   
   /*=============================================*/
   /* Remove commands at the end of the command   */
   /* history if the maximum number of remembered */
   /* commands is exceeded.                       */
   /*=============================================*/
   
   while (self->currentCommandCount > self->maxCommandCount)
     {
      if (self->bottomCommand->prev == NULL)
        { break; }
        
      self->bottomCommand->prev->next = NULL;
      nextCommand = self->bottomCommand;
      self->bottomCommand = self->bottomCommand->prev;
      free(nextCommand->command);
      free(nextCommand);
      
      self->currentCommandCount--;
     }

  }

/*************************/
/* SwitchCommandFrom:To: */
/*************************/
- (void) SwitchCommandFrom: (struct priorCommand *) oldCommand
                        To: (struct priorCommand *) newCommand
  {
   const char *theCommand;
   size_t length;

   NSString *newInput = [NSString stringWithCString: newCommand->command encoding: NSUTF8StringEncoding];

   /*==========================================*/
   /* Retrieve the current command from CLIPS. */
   /*==========================================*/
   
   theCommand = GetCommandString([environment environment]);
   if (theCommand == NULL)
     { theCommand = ""; }
   length = strlen(theCommand);
 
   /*=================================================*/
   /* Determine the range which must be changed. This */
   /* is done using the old CLIPS command string.     */
   /*=================================================*/
   
   NSUInteger charOffset = [self->textView inputStringOffset];
   NSUInteger textLength = [[self->textView string] length];
   NSUInteger inputStart = textLength - charOffset;

   NSRange theRange = { inputStart, charOffset };
   
   /*==============================================*/
   /* Replace the old command with the contents of */
   /* the command string, which will now include   */
   /* any edits the user made.                     */
   /*==============================================*/
   
   free(oldCommand->command);
   oldCommand->command = (char *) malloc(length + 1);
   strncpy(oldCommand->command,theCommand,length + 1);

   /*======================*/
   /* Use the new command. */
   /*======================*/
   
   SetCommandString([environment environment],newCommand->command);
   self->currentCommand = newCommand;

   /*============================================*/
   /* Replace the current command in the window. */
   /*============================================*/
   
   [self->textView replaceCharactersInRange: theRange withString: newInput];
   
   /*===========================================*/
   /* Scroll to the end of the terminal output. */
   /*===========================================*/
   
   NSRange endRange = { [[textView string] length], 0 };
   [textView setSelectedRange: endRange];
   [textView scrollRangeToVisible: endRange];
  }

/*******************/
/* lookForCommand: */
/*******************/
- (void) lookForCommand: (NSTimer *) theTimer
  {
   if ([environment exited])
     {
      [[self window] performClose: self];
      return;
     }
     
   /*======================================================================*/
   /* If we can't get the execution lock, then CLIPS is currently running. */
   /* The pause button should be enabled, and if execution isn't paused    */
   /* the execution indicator should be animated.                          */
   /* If we do get the execution lock, unlock it temporarily so that we    */
   /* can check for changes to the agenda, facts, and other items for      */
   /* which access is limited during execution.                            */
   /*======================================================================*/
   
   if (! [[environment executionLock] tryLock]) 
     {
      if ([pauseButton state] == NSOffState)
        { [executionIndicator startAnimation: nil]; }
      [pauseButton setEnabled: YES];   
      return;
     }
   [[environment executionLock] unlock];

   /*==============================================*/
   /* Check for changes to the agenda, facts, etc. */
   /*==============================================*/
                                                                
   [environment checkForChanges];
   
   /*============================================================*/
   /* Dump any output left in the output buffer to the terminal. */
   /*============================================================*/
   
   [self dumpOutputBuffer];
     
   /*=============================================*/
   /* Don't check for commands if the environment */
   /* is in the process of exiting.               */
   /*=============================================*/
   
   if (exit)
     {
      [executionIndicator stopAnimation: nil];     
      return;
     }

   /*====================================================*/
   /* If the user has moved the caret into the center of */
   /* the command, don't execute the command as the user */
   /* may be temporarily adding/removing parentheses     */
   /* that cause a completed command to be formed.       */
   /*====================================================*/

/*
   NSRange selectionRange = [textView selectedRange];
   
   if ((selectionRange.length != 0) || ([[textView string] length] != selectionRange.location))
     { return; }
*/
   
   /*============================================================*/
   /* Update the command history if there is a complete command. */
   /*============================================================*/
   
   if ((! BatchActive([environment environment])) && CommandCompleteAndNotEmpty([environment environment]))
     { [self updateCommandHistory]; }

   /*===================================================*/
   /* Perform the command if present, otherwise return. */
   /*===================================================*/
     
   [[environment executionLock] lock];

   if (! [environment performCommandIfPresent]) 
     {
      [executionIndicator stopAnimation: nil];     
      [pauseButton setEnabled: NO];   
      [[environment executionLock] unlock];
      return;
     }

   [[environment executionLock] unlock];

   /*=====================================================*/
   /* Dump any remaining output generated by the command. */
   /*=====================================================*/
        
   [self dumpOutputBuffer];

   /*===========================================*/
   /* Scroll to the end of the terminal output. */
   /*===========================================*/
   
   NSRange theRange = { [[textView string] length], 0 };
   [textView setSelectedRange: theRange];   
   [textView scrollRangeToVisible: theRange]; 
  }
  
/*******************/
/* lookForUpdates: */
/*******************/
- (void) lookForUpdates: (NSTimer *) theTimer
  {

   if ([environment exited])
     { return; }
        
   if (! [[environment executionLock] tryLock]) 
     {
      [[environment agendaLock] lock];
      switch ([[environment agendaLock] condition])
        {
         case AGENDA_FETCHED:
           [environment transferAgenda: NO];
           [[environment agendaLock] unlockWithCondition: FETCH_AGENDA];
           break;
           
         default:
           [[environment agendaLock] unlockWithCondition: FETCH_AGENDA];
           break;
        }

      [[environment factsLock] lock];
      switch ([[environment factsLock] condition])
        {
         case AGENDA_FETCHED:
           [environment transferFacts: NO];
           [[environment factsLock] unlockWithCondition: FETCH_FACTS];
           break;
           
         default:
           [[environment factsLock] unlockWithCondition: FETCH_FACTS];
           break;
        }
        
      EnablePeriodicFunctions([environment environment],true);
      return;
     }

   [[environment executionLock] unlock];
  }

/*********************/
/* scrollToEndCheck: */
/*********************/
- (void) scrollToEndCheck: (NSTimer *) theTimer
  {   
   if ((bufferCount == 0) && (scrollToEnd == NO)) return;
   
   /*===========================================================*/
   /* Remember the position of the horizontal scrollbar's knob. */
   /*===========================================================*/
   
   NSScrollView *textScrollView = [textView enclosingScrollView];
   float hValue = [[textScrollView horizontalScroller] floatValue];

   /*================================================================*/
   /* Dump the contents of the output buffer to the terminal window. */
   /*================================================================*/
   
   [self dumpOutputBuffer];
   NSRange theRange = { [[textView string] length], 0 };
   [textView setSelectedRange: theRange];   
   [textView scrollRangeToVisible: theRange]; 
   scrollToEnd = NO;
   
   /*==================================================================*/
   /* If the horizontal scroller was originally set to zero, and it's  */
   /* been changed by adding the contents of the output buffer, reset  */
   /* it to zero. This prevents the knob of the scrollbar from hopping */
   /* back and forth as text is added to the terminal window.          */
   /*==================================================================*/
   
   if ((hValue == 0.0) &&
       [[textScrollView horizontalScroller] floatValue] != 0.0)
     {
      [[textScrollView horizontalScroller] setFloatValue: 0.0];
     }
  }

/*********/
/* clear */
/*********/
- (IBAction) clear: (id) sender
  {
   [environment doCommand: @"(clear)\n"];
  }

/************************************************************/
/* loadConstructs: Initiates the Load Constructs... command */
/*   for a CLIPS environment window (terminal).             */
/************************************************************/
- (IBAction) loadConstructs: (id) sender
  {
   NSString *theDirectory = [self currentDirectory];

   /*======================================*/
   /* Allow CLIPS/text files to be loaded. */
   /*======================================*/

   NSArray *fileTypes = [NSArray arrayWithObjects: @"clp", @"txt", @"text",
                         NSFileTypeForHFSTypeCode( 'TEXT' ), nil];

   /*===================*/
   /* Create the panel. */
   /*===================*/

   NSOpenPanel *oPanel = [NSOpenPanel openPanel];
   
   /*======================================================*/
   /* Change the OK button to "Load" and caption the panel */
   /* to indicate a constructs file should be selected.    */  
   /*======================================================*/
   
   [oPanel setPrompt: @"Load"];
   [oPanel setMessage: @"Select a text file containing constructs to load"];
   
   /*=================================================*/
   /* The directory in which the open panel starts    */
   /* should be the last directory visited, otherwise */
   /* use the home directory.                         */
   /*=================================================*/
   
   if (theDirectory == nil)
     { theDirectory = NSHomeDirectory(); }

   /*==================================================*/
   /* Begin the open sheet for the environment window. */
   /*==================================================*/

   [oPanel setAllowedFileTypes: fileTypes];
   
   [oPanel setDirectoryURL: [NSURL fileURLWithPath: theDirectory]];
   
   [oPanel beginSheetModalForWindow: [self window]
                  completionHandler: ^(NSInteger returnCode)
                                        { [self loadConstructPanelDidEnd: oPanel returnCode: returnCode]; }];
   }

/****************************************************/
/* loadConstructPanelDidEnd:returnCode:contextInfo: */
/****************************************************/
- (void) loadConstructPanelDidEnd: (NSOpenPanel *) sheet 
         returnCode: (NSInteger) returnCode
  {
   if (returnCode != NSModalResponseOK) return;

   NSArray *filesToOpen = [sheet URLs];
   NSString *theFileName;
   Environment *theEnv = [environment environment];
   AppController *theDelegate = [NSApp delegate];
      
   if ([filesToOpen count] != 1) return;
      
   theFileName = [[filesToOpen objectAtIndex: 0] path];

   /*======================================*/
   /* Remember this directory for the next */
   /* time the open panel is invoked.      */
   /*======================================*/
   
   [self setCurrentDirectory: 
         [theFileName stringByDeletingLastPathComponent]];
   
   [self setValue: [[self currentDirectory] stringByAbbreviatingWithTildeInPath]
         forKey: @"displayDirectory"];

   NSLock *theLock = [theDelegate fileOpenLock];
   
   [theLock lock];
   // This is probably unnecessary because of the change directory CLIPS glue function
   [[NSFileManager defaultManager] changeCurrentDirectoryPath: [self currentDirectory]];
   
   [theLock unlock];
      
   FlushCommandString(theEnv);

   [textView insertText: @"(load \"" replacementRange: [textView selectedRange]];
   [textView insertText: [theFileName lastPathComponent] replacementRange: [textView selectedRange]];
   [textView insertText: @"\")\n" replacementRange: [textView selectedRange]];
  }

/**************************************************/
/* loadBatch: Initiates the Load Batch... command */
/*   for a CLIPS environment window (terminal).   */
/**************************************************/
- (IBAction) loadBatch: (id) sender
  {
   NSString *theDirectory = [self currentDirectory];

   /*======================================*/
   /* Allow batch/text files to be loaded. */
   /*======================================*/
   
   NSArray *fileTypes = [NSArray arrayWithObjects: @"txt", @"text",
                                                   @"bat", @"tst",
                         NSFileTypeForHFSTypeCode( 'TEXT' ), nil];

   /*===================*/
   /* Create the panel. */
   /*===================*/
   
   NSOpenPanel *oPanel = [NSOpenPanel openPanel];

   /*====================================================*/
   /* Change the OK button to "Load" and caption the     */
   /* panel to indicate a batch file should be selected. */  
   /*====================================================*/
    
   [oPanel setPrompt: @"Load"];
   [oPanel setMessage: @"Select a text file containing commands to batch process"];

   /*=================================================*/
   /* The directory in which the open panel starts    */
   /* should be the last directory visited, otherwise */
   /* use the home directory.                         */
   /*=================================================*/
   
   if (theDirectory == nil)
     { theDirectory = NSHomeDirectory(); }

   /*==================================================*/
   /* Begin the open sheet for the environment window. */
   /*==================================================*/
   
   [oPanel setAllowedFileTypes: fileTypes];
   
   [oPanel setDirectoryURL: [NSURL fileURLWithPath: theDirectory]];
   
   [oPanel beginSheetModalForWindow: [self window]
                  completionHandler: ^(NSInteger returnCode)
                                        { [self loadBatchPanelDidEnd: oPanel returnCode: returnCode]; }];
  }
  
/************************************************/
/* loadBatchPanelDidEnd:returnCode:contextInfo: */
/************************************************/
- (void) loadBatchPanelDidEnd: (NSOpenPanel *) sheet 
         returnCode: (NSInteger) returnCode
  {
   if (returnCode != NSModalResponseOK) return;

   NSArray *filesToOpen = [sheet URLs];
   NSString *theFileName;
   Environment *theEnv = [environment environment];
      
   if ([filesToOpen count] != 1) return;
      
   theFileName = [[filesToOpen objectAtIndex: 0] path];

   [self setCurrentDirectory: 
         [theFileName stringByDeletingLastPathComponent]];
   
   [self setValue: [[self currentDirectory] stringByAbbreviatingWithTildeInPath]
         forKey: @"displayDirectory"];

   //NSLock *theLock = [theDelegate fileOpenLock];
   
   //[theLock lock];
   // This is probably unnecessary because of the change directory CLIPS glue function   
   //[[NSFileManager defaultManager] changeCurrentDirectoryPath: [self currentDirectory]];
   
   //[theLock unlock];
      
   FlushCommandString(theEnv);

   [textView insertText: @"(batch \"" replacementRange: [textView selectedRange]];
   [textView insertText: [theFileName lastPathComponent] replacementRange: [textView selectedRange]];
   [textView insertText: @"\")\n" replacementRange: [textView selectedRange]];
  }

/********************************************************/
/* setDirectory: Initiates the Set Directory... command */
/*   for a CLIPS environment window (terminal).         */
/********************************************************/
- (IBAction) setDirectory: (id) sender
  {
   NSString *theDirectory = [self currentDirectory];

   /*===================*/
   /* Create the panel. */
   /*===================*/
   
   NSOpenPanel *oPanel = [NSOpenPanel openPanel];

   /*===================================================*/
   /* Change the OK button to "Set" and caption the     */
   /* panel to indicate a directory should be selected. */  
   /*===================================================*/
    
   [oPanel setPrompt: @"Set"];
   [oPanel setMessage: @"Select a directory"];
   [oPanel setCanChooseDirectories: YES];
   [oPanel setCanChooseFiles: NO];

   /*=================================================*/
   /* The directory in which the open panel starts    */
   /* should be the last directory visited, otherwise */
   /* use the home directory.                         */
   /*=================================================*/
   
   if (theDirectory == nil)
     { theDirectory = NSHomeDirectory(); }

   /*==================================================*/
   /* Begin the open sheet for the environment window. */
   /*==================================================*/
     
   [oPanel setDirectoryURL: [NSURL fileURLWithPath: theDirectory]];
   
   [oPanel beginSheetModalForWindow: [self window]
                  completionHandler: ^(NSInteger returnCode)
                                        { [self setDirectoryPanelDidEnd: oPanel returnCode: returnCode]; }];
  }

/************************************************/
/* setDirectoryPanelDidEnd:returnCode:contextInfo: */
/************************************************/
- (void) setDirectoryPanelDidEnd: (NSOpenPanel *) sheet 
         returnCode: (NSInteger) returnCode 
  {
   if (returnCode != NSModalResponseOK) return;
   
   NSArray *filesToOpen = [sheet URLs];
   NSString *theFileName;
      
   if ([filesToOpen count] != 1) return;
      
   theFileName = [[filesToOpen objectAtIndex: 0] path];
   [self setCurrentDirectory: theFileName];
   
   [self setValue: [[self currentDirectory] stringByAbbreviatingWithTildeInPath]
         forKey: @"displayDirectory"];
  }

/*********/
/* reset */
/*********/
- (IBAction) reset: (id) sender
  {
   [environment doCommand: @"(reset)\n"];
  }

/*******/
/* run */
/*******/
- (IBAction) run: (id) sender
  {
   [environment doCommand: @"(run)\n"];
  }

/******************/
/* pauseContinue: */
/******************/
- (IBAction) pauseContinue: (id) sender
  {
   if ([sender state] == NSOnState)
     {
      [executionIndicator setDisplayedWhenStopped: YES];
      [executionIndicator stopAnimation: nil];  
      [pauseLock lock];
      [pauseLock unlockWithCondition: EXECUTION_IS_PAUSED];
     }
   else
     {      
      [executionIndicator setDisplayedWhenStopped: NO];
      [executionIndicator startAnimation: nil];  
      [pauseLock lock];
      [pauseLock unlockWithCondition: EXECUTION_IS_NOT_PAUSED];
     }
  }

/*************************************************/
/* clearScrollback: Removes the output currently */
/*   display in the dialog window.               */
/*************************************************/
- (IBAction) clearScrollback: (id) sender
  {
   char *theCString;
   NSString *theStr;
   Environment *theEnvironment = [environment environment];
   
   /*==============================================*/
   /* Select all of the text in the dialog window. */
   /*==============================================*/
   
   NSRange theRange = { 0, [[textView string] length] };
   
   /*====================================================*/
   /* Print the prompt. By doing this before the current */
   /* content of the dialog window is deleted, the font  */
   /* is preserved (more bindings strangeness).          */
   /*====================================================*/
   
   PrintPrompt(theEnvironment);
   [self dumpOutputBuffer];

   /*=================================================*/
   /* Delete the prior contents of the dialog window. */
   /*=================================================*/
   
   clearWindow = YES;
   [textView replaceCharactersInRange: theRange withString:@""];
   clearWindow = NO;
  
   lineCount = 1;
   lastDumpPosition = 0;
   
   /*=========================================*/
   /* Get the contents of the command buffer. */
   /*=========================================*/
           
   theCString = GetCommandString(theEnvironment);
   
   /*===============================================*/
   /* If there was a partially completed command in */
   /* the buffer, then add it after the prompt.     */
   /*===============================================*/
   
   if (theCString != NULL)
     {
      clearWindow = YES;
      theStr = [NSString stringWithCString: theCString encoding: NSUTF8StringEncoding];
      [self print: theStr];
      clearWindow = NO;
     }
  }

/*********************************************************/
/* clearScrollbackFunction: Removes the output currently */
/*   display in the dialog window. This version is to be */
/*   called by CLIPS clear-window function.              */
/*********************************************************/
- (void) clearScrollbackFunction
  {
   [outputBufferLock lock];
   clearWindow = YES;
   [outputBuffer setString: @""];
   bufferCount = 0;
   [outputBufferLock unlockWithCondition: BUFFER_IS_EMPTY];
  }

/*****************************************************************/
/* setWatchFlagsFromPreferences: Reads the settings of the watch */
/*   items from the application preferences and assigns them to  */
/*   the CLIPS environment associated with this terminal.        */
/*****************************************************************/
- (void) setWatchFlagsFromPreferences
  {
   NSUserDefaultsController *theValues;
   Environment *theEnv = [environment environment];

   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];

   SetWatchState(theEnv,COMPILATIONS,[[theValues valueForKey: @"compilations"] boolValue]);
   SetWatchState(theEnv,FACTS,[[theValues valueForKey: @"facts"] boolValue]);
   SetWatchState(theEnv,RULES,[[theValues valueForKey: @"rules"] boolValue]);
   SetWatchState(theEnv,STATISTICS,[[theValues valueForKey: @"statistics"] boolValue]);
   SetWatchState(theEnv,ACTIVATIONS,[[theValues valueForKey: @"activations"] boolValue]);
   SetWatchState(theEnv,FOCUS,[[theValues valueForKey: @"focus"] boolValue]);
   SetWatchState(theEnv,GLOBALS,[[theValues valueForKey: @"globals"] boolValue]);
   SetWatchState(theEnv,DEFFUNCTIONS,[[theValues valueForKey: @"deffunctions"] boolValue]);
   SetWatchState(theEnv,GENERIC_FUNCTIONS,[[theValues valueForKey: @"generic-functions"] boolValue]);
   SetWatchState(theEnv,METHODS,[[theValues valueForKey: @"methods"] boolValue]);
   SetWatchState(theEnv,INSTANCES,[[theValues valueForKey: @"instances"] boolValue]);
   SetWatchState(theEnv,SLOTS,[[theValues valueForKey: @"slots"] boolValue]);
   SetWatchState(theEnv,MESSAGE_HANDLERS,[[theValues valueForKey: @"message-handlers"] boolValue]);
   SetWatchState(theEnv,MESSAGES,[[theValues valueForKey: @"messages"] boolValue]);
  }
  
/**********/    
/* print: */
/**********/    
- (void) print: (NSString *) theString
  {
   [textView print: theString];
  }

/*********************/    
/* convertAndPrintC: */
/*********************/    
- (void) convertAndPrintC: (char *) theString
  {
   NSString *theStr;
   
   theStr = [NSString stringWithCString: theString encoding: NSUTF8StringEncoding];
   [textView print: theStr];
  }

/***********/    
/* printC: */
/***********/    
- (void) printC: (const char *) theString
  {
   NSString *theStr = [NSString stringWithCString: theString encoding: NSUTF8StringEncoding];
   
   if (theStr == nil)
     { return; }
     
   /*=================================================================*/
   /* If the buffer has grown too large, wait for it to be dumped by  */
   /* the UI before adding any more text to it. For print intensive   */
   /* programs, this increases the responsiveness of the application. */
   /*=================================================================*/
   
   if (bufferCount > BUFFER_SIZE)
     {   
      [outputBufferLock lockWhenCondition: BUFFER_IS_EMPTY];
      [outputBufferLock unlockWithCondition: BUFFER_IS_EMPTY];
     }

   /*=======================================================*/
   /* Keep track of the number of characters in the buffer. */
   /*=======================================================*/
   
   bufferCount += strlen(theString);
   
   [outputBufferLock lock];

   [outputBuffer appendString: theStr];

   [outputBufferLock unlockWithCondition: BUFFER_IS_NOT_EMPTY]; 
         
   return;
  }

/*********************/    
/* dumpOutputBuffer: */
/*********************/    
- (void) dumpOutputBuffer
  {
   NSUInteger saveDumpPosition;
   NSUInteger currentPosition;
   NSUInteger difference;
   /*
   int startLines = lineCount;
   */
   /*==============================================*/
   /* Secure the output buffer lock so that the    */
   /* executing CLIPS thread does not write to the */ 
   /* output buffer while it's being accessed.     */
   /*==============================================*/
   
   [outputBufferLock lock];     

   /*==========================================================*/
   /* If the window was cleared with a (clear-window) command, */
   /* remove the text from the window before any text that was */
   /* added after the command is dumped.                       */
   /*==========================================================*/
   
   if (clearWindow)
     {
      [textView clearTerminal];
      lineCount = 1;
      lastDumpPosition = 0;
      clearWindow = NO;
     }
   
   /*===================================================*/
   /* An empty output buffer doesn't need to be dumped. */
   /*===================================================*/
   
   if ([outputBuffer isEqualToString: @""])
     {
      bufferCount = 0;
      [outputBufferLock unlockWithCondition: BUFFER_IS_EMPTY];
      return;
     }

   /*========================================================*/
   /* Check for a difference between the text length at the  */
   /* time of the last dump and the current text length.     */
   /* The difference, if any, is input from the user, either */
   /* from commands entered at the command prompt or in      */
   /* response to a read from the terminal.                  */
   /*========================================================*/
   
   saveDumpPosition = lastDumpPosition;
   currentPosition = [[textView textStorage] length];
   difference = currentPosition - saveDumpPosition;
   
   /*=========================================================*/
   /* If a difference is found, determine the number of lines */
   /* added by the input and add this to the line count.      */
   /*=========================================================*/
           
   if (difference > 0)
     {
      NSRange theRange;
   
      theRange.location = saveDumpPosition;
      theRange.length = difference;
      
      NSString *otherInput = [[[textView textStorage] string] substringWithRange: theRange];
      lineCount += [self lineCountIncrease: otherInput];
     }

   lastDumpPosition = [textView print: outputBuffer];
  
   lineCount += [self lineCountIncrease: outputBuffer];
/*
   if ((lineCount - startLines) > lineMax)
     {
      lineMax = lineCount - startLines;
     }


   if ([outputBuffer length] > bufferMax)
     {
      bufferMax = [outputBuffer length];
     }
*/
   bufferCount = 0;
   [outputBuffer setString: @""];
         
   [outputBufferLock unlockWithCondition: BUFFER_IS_EMPTY];
     
   /*====================================================*/
   /* If the number of lines in the terminal exceeds the */
   /* limit, delete the necessary number of lines at the */
   /* beginning of the terminal output.                  */
   /*====================================================*/
   
   [self deleteExtraLines];
   
   return;
  }
    
/****************/    
/* waitForChar: */
/****************/    
- (int) waitForChar
  {
   if ([NSThread isMainThread])
     { [self dumpOutputBuffer]; }
   else
     {
      dispatch_async(dispatch_get_main_queue(),
       ^{ [self dumpOutputBuffer]; });
     }

   return [textView waitForChar];
  }
  
/**********************/
/* lineCountIncrease: */
/**********************/
- (unsigned int) lineCountIncrease: (NSString *) theString
  {  
   NSRange theRange = { 0, 0 };
   NSUInteger position = 0;
   NSUInteger length = [theString length];
   unsigned lineIncrease = 0;
   unichar theChar;
   
   if (length == 0) return 0;
   
   for (lineIncrease = 0; position < length; lineIncrease++)
     {
      position = NSMaxRange([theString lineRangeForRange: theRange]);
      theRange.location = position;
     }

   theChar = [theString characterAtIndex: length - 1];
   if ((theChar != '\n') && (theChar != '\r'))
     { lineIncrease--; }
      
   return lineIncrease;
  }

/*********************/
/* deleteExtraLines: */
/*********************/
- (void) deleteExtraLines
  {  
   NSRange theRange = { 0, 0 };
   NSUInteger position = 0;
   unsigned linesToDelete;
   
   /*==============================================*/
   /* If we're within the terminal's line maximum, */
   /* then no lines need to be deleted.            */
   /*==============================================*/
   
   if (lineCount <= CLIPS_TERMINAL_LINE_MAX + DELETE_AT_LEAST)
     { return; }

   linesToDelete = lineCount - CLIPS_TERMINAL_LINE_MAX;
   
   /*==================================================*/
   /* Determine the range for the lines to be deleted. */
   /*==================================================*/
   
   NSString *outputString = [[textView textStorage] string];
   
   while (linesToDelete > 0)
     {
      position = NSMaxRange([outputString lineRangeForRange: theRange]);
      theRange.location = position;
      linesToDelete--;
     }
 
   /*=========================*/
   /* Delete the extra lines. */
   /*=========================*/
   
   theRange.location = 0;
   theRange.length = position;
   
   [[textView textStorage] deleteCharactersInRange: theRange];
   [textView didChangeText];
 
   /*==================================*/
   /* Update the line count variables. */
   /*==================================*/
   
   lastDumpPosition = [[textView textStorage] length];
   lineCount = CLIPS_TERMINAL_LINE_MAX;
  }
  
/*******************************/
/* allowExecution:ofCommand: */
/*******************************/
- (BOOL) allowExecution: (id) sender
         ofCommand: (NSString *) theCommand
  {
   return YES;
  }

/*******************************/
/* beforeExecution:ofCommand: */
/*******************************/
- (void) beforeExecution: (id) sender
         ofCommand: (NSString *) theCommand
  {
  }

/*******************************/
/* clearCurrentCommand: */
/*******************************/
- (void) clearCurrentCommand: (id) sender
  {
   size_t commandLength;
   char *theCommand;
   NSRange theRange = { 0, 0 };
   
   theCommand = GetCommandString([environment environment]);
   if (theCommand == NULL) 
     { return; }
   
   commandLength = strlen(theCommand);
   if (commandLength == 0)
     { return; }
   
   theRange.location = [[textView textStorage] length] - commandLength;
   theRange.length = commandLength;
   
   [[textView textStorage] deleteCharactersInRange: theRange];
   [textView didChangeText];   
  }
      
/*********/
/* exit: */
/*********/
- (void) exit
  {
   exit = YES;
   [self haltImmediately: self];
  }

/*******************************************************/
/* cancelOperation: This is an NSResponder method that */
/*    gets called when the ESC key is pressed.         */
/*******************************************************/
- (void) cancelOperation: (id)sender
  {
   NSRange theRange = { [[textView string] length], 0 };
   [textView setSelectedRange: theRange];   
   [textView scrollRangeToVisible: theRange];
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
   /* Need to abort waitForChar */
   /* Need to abort batch */
   SetHaltCommandLoopBatch([environment environment],true);
   SetHaltExecution([environment environment],true);
  }
      
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Window Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/

/**********************/
/* windowShouldClose: */
/**********************/  
- (BOOL) windowShouldClose: (id) sender
  {
   /*===========================================*/
   /* If the CLIPS environment isn't executing, */
   /* then it's OK to close the window.         */
   /*===========================================*/
   
   if ([[environment executionLock] tryLock]) 
     {
      [[environment executionLock] unlock];
      return YES;
     }

   NSAlert *alert = [[NSAlert alloc] init];
   alert.messageText =  @"This window may not be closed while a command is executing. "
                        @"You must wait for the command to complete or attempt to halt execution of the command.";
   [alert addButtonWithTitle: @"Wait"];
   [alert addButtonWithTitle: @"Halt Immediately"];
   [alert addButtonWithTitle: @"Halt"];
   
   [alert beginSheetModalForWindow: [self window]
                 completionHandler: ^(NSInteger returnCode)
                 {
                  NSWindow *theWindow = [self window];
                  if (returnCode == NSAlertFirstButtonReturn) // Wait
                    {
                     [theWindow makeKeyAndOrderFront: nil];
                    }
                  else if (returnCode == NSAlertSecondButtonReturn) // Halt Immediately
                    {
                     [self haltImmediately: self];
                     [theWindow makeKeyAndOrderFront: nil];
                    }
                  else if (returnCode == NSAlertThirdButtonReturn) // Halt
                    {
                     [self halt: self];
                     [theWindow makeKeyAndOrderFront: nil];
                    }
                 }];
   return NO;
  }
  
/********************/
/* windowWillClose: */
/********************/
- (void) windowWillClose: (NSNotification *) aNotification
  {
   /*=====================*/
   /* Release the timers. */
   /*=====================*/
   
   [commandTimer invalidate];
   commandTimer = nil;

   [scrollTimer invalidate];
   scrollTimer = nil;

   [haltTimer invalidate];
   haltTimer = nil;

   [updateTimer invalidate];
   updateTimer = nil;
   
   /*======================================*/
   /* Get the default notification center. */
   /*======================================*/
      
   NSNotificationCenter *nc;
   nc = [NSNotificationCenter defaultCenter];
  }

/************************************************/
/* validateMenuItem: Indicates which menu items */
/*   from the Environment menu are available.   */
/************************************************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   /*===================================================*/
   /* The Halt and Halt Immediately menu items are only */
   /* available if the CLIPS environment is executing.  */
   /*===================================================*/
   
   if (([menuItem action] == @selector(halt:)) ||
       ([menuItem action] == @selector(haltImmediately:)))
     {
      if ([[environment executionLock] tryLock])
        {
         [[environment executionLock] unlock];
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
      if ([[environment executionLock] tryLock])
        {
         [[environment executionLock] unlock];
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

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/******************/
/* environment: */
/******************/ 
- (CLIPSEnvironment *) environment
  {
   return environment;
  }

/************************/
/* setCurrentDirectory: */
/************************/
- (void) setCurrentDirectory: (NSString *) theValue
  {
   NSUserDefaults *theValues;
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
   
   [theValues setValue: theValue forKey: @"currentDirectory"];

   currentDirectory = theValue;
  }

/*********************/
/* currentDirectory: */
/*********************/  
- (NSString *) currentDirectory
  {
   return currentDirectory;
  }

/************************/
/* setDisplayDirectory: */
/************************/
- (void) setDisplayDirectory: (NSString *) theValue
  {
   displayDirectory = theValue;
  }

/*********************/
/* displayDirectory: */
/*********************/  
- (NSString *) displayDirectory
  {
   return displayDirectory;
  }

/******************/
/* pauseLock: */
/******************/  
- (NSConditionLock *) pauseLock
  {
   return pauseLock;
  }
  
/*********************/
/* setScrollToEnd: */
/*********************/
- (void) setScrollToEnd: (BOOL) theValue
  {
   scrollToEnd = theValue;
  }

/******************/
/* scrollToEnd: */
/******************/  
- (BOOL) scrollToEnd
  {
   return scrollToEnd;
  }

/*********************/
/* setClearWindow: */
/*********************/
- (void) setClearWindow: (BOOL) theValue
  {
   clearWindow = theValue;
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* TextView Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/**********************************************************************/
/* textView:willChangeSelectionFromCharacterRanges:toCharacterRanges: */
/**********************************************************************/
- (NSArray *) textView: (NSTextView *) aTextView
              willChangeSelectionFromCharacterRanges: (NSArray *) oldSelectedCharRanges
              toCharacterRanges: (NSArray *) newSelectedCharRanges
   {
    NSValue *theValue, *inputCaret = NULL, *extraSelection = NULL;
    NSUInteger textLength;
    NSArray *returnArray;

    if (clearWindow)
      { return newSelectedCharRanges; }

    /*=======================================================*/
    /* If the output buffer hasn't been dumped, don't try to */
    /* modify the selection because we're in the process of  */
    /* attempting to print information.                      */
    /*=======================================================*/
    
    if (! [outputBuffer isEqualToString: @""])
      {
       return newSelectedCharRanges;
      }

    /*=======================================*/
    /* Determine the number of characters in */
    /* the display that are currently input. */
    /*=======================================*/
    
    NSUInteger charOffset = [textView inputStringOffset];
    
    /*================================*/
    /* Determine the start and end of */
    /* the input text in the buffer.  */
    /*================================*/
    
    textLength = [[textView string] length];
    NSUInteger inputEnd = textLength;
    NSUInteger inputStart = textLength - charOffset;
        
    /*==================================*/
    /* Look for the original selection. */
    /*==================================*/
    
    for (theValue in oldSelectedCharRanges)
      {
       NSRange theRange = [theValue rangeValue];
       
       /*====================================================*/
       /* The original selection should fall entirely within */
       /* the input area and there should only be one.       */
       /*====================================================*/
       
       if (theRange.location >= inputStart)
         { inputCaret = theValue; }
      }

    for (theValue in newSelectedCharRanges)
      {
       NSRange theRange = [theValue rangeValue];

       if (theRange.location >= inputStart)
         { inputCaret = theValue; }
       else if (theRange.length != 0)
         { extraSelection = theValue; }
      }
   
    if (inputCaret == nil)
      {
       NSRange theRange = { inputEnd , 0 };
       
       inputCaret = [NSValue valueWithRange: theRange];
      }
    
    if (extraSelection == nil)
      { returnArray = [NSArray arrayWithObject: inputCaret]; }
    else
      { returnArray = [NSArray arrayWithObject: extraSelection]; }
    
    /*==========================================================================*/
    /* An array containing the proposed character ranges for the new selection. */
    /* This must be a non-nil, non-empty array of objects responding to the     */
    /* NSValue method rangeValue, and in addition its elements must be sorted,  */
    /* non-overlapping, non-contiguous, and (except for the case of a single    */
    /* range) have non-zero-length.                                             */
    /*==========================================================================*/
   
    return returnArray;
   }

/*%%%%%%%%%%%%%%%%*/
/* Unused Methods */
/*%%%%%%%%%%%%%%%%*/

/**************************************************************/    
/* windowDidBecomeMain: Keeps track of whether an environment */
/*   window is the main window so that the appropriate menu   */
/*   items in the Environment menu can be enabled.            */
/**************************************************************/
/*
- (void) windowDidBecomeMain: (NSNotification *) aNotification
  {
  }
*/

/***************************************************************/    
/* windowDidResignMain:  Keeps track of whether an environment */
/*   window is the main window so that the appropriate menu    */
/*   items in the Environment menu can be disabled.            */
/***************************************************************/    
/*
- (void) windowDidResignMain: (NSNotification *) aNotification
  {   
  }
*/

@end



