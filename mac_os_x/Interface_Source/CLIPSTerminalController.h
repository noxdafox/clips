//
//  CLIPSTerminalController.h
//  CLIPS
//
//  Created by Gary Riley on 2/25/06.
//

#import <Cocoa/Cocoa.h>

#define EXECUTION_IS_PAUSED 0
#define EXECUTION_IS_NOT_PAUSED 1

#define BUFFER_IS_EMPTY 0
#define BUFFER_IS_NOT_EMPTY 1

@class CLIPSEnvironment;
@class CLIPSTerminalView;
@class EnvController;

struct priorCommand
  {
   struct priorCommand *next;
   struct priorCommand *prev;
   char *command;
  };

@interface CLIPSTerminalController : NSWindowController <NSTextViewDelegate>
  {
   IBOutlet CLIPSEnvironment *environment;
   IBOutlet CLIPSTerminalView *textView;
   IBOutlet NSProgressIndicator *executionIndicator;
   IBOutlet NSButton* pauseButton;
   EnvController *envController;
   NSTimer *commandTimer;
   NSTimer *scrollTimer;
   NSTimer *haltTimer; /* TBD Needed */
   NSTimer *updateTimer;
   NSString *currentDirectory;
   NSString *displayDirectory;
   NSMutableString *outputBuffer;
   NSConditionLock *outputBufferLock;
   int bufferCount;
   unsigned lineCount;
   unsigned lastDumpPosition;
   NSConditionLock *pauseLock;
   BOOL scrollToEnd;
   BOOL exit;
   BOOL clearWindow;
@public
   struct priorCommand *topCommand;
@public
   struct priorCommand *bottomCommand;
@public
   struct priorCommand *currentCommand;
   int maxCommandCount;
   int currentCommandCount;
  }

- (IBAction)           loadConstructs: (id) sender;
- (IBAction)           loadBatch: (id) sender;
- (IBAction)           setDirectory: (id) sender;

- (IBAction)           clearScrollback: (id) sender;
- (IBAction)           pauseContinue: (id) sender;
- (void)               clearScrollbackFunction;
- (void)               deleteExtraLines;

- (IBAction)           halt: (id) sender;
- (IBAction)           haltImmediately: (id) sender;

- (void)               setWatchFlagsFromPreferences;
- (void)               print: (NSString *) theString;
- (void)               printC: (const char *) theString;

- (void)               loadConstructPanelDidEnd: (NSOpenPanel *) sheet 
                       returnCode: (int) returnCode;

- (void)               loadBatchPanelDidEnd: (NSOpenPanel *) sheet 
                       returnCode: (int) returnCode;

- (void)               setDirectoryPanelDidEnd: (NSOpenPanel *) sheet 
                       returnCode: (int) returnCode;

- (unsigned int)       lineCountIncrease: (NSString *) theString;
                       
- (void) lookForCommand: (NSTimer *) theTimer;
- (void) scrollToEndCheck: (NSTimer *) theTimer;
- (void) lookForUpdates: (NSTimer *) theTimer;

- (BOOL) allowExecution: (id) sender
         ofCommand: (NSString *) theCommand;

- (void) beforeExecution: (id) sender
         ofCommand: (NSString *) theCommand;

- (void) clearCurrentCommand: (id) sender;

- (int) waitForChar;
- (void) dumpOutputBuffer;
- (void) exit;
- (void) convertAndPrintC: (char *) theString;

- (void) SwitchCommandFrom: (struct priorCommand *) oldCommand
                        To: (struct priorCommand *) newCommand;

/*%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Window Delegate Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (BOOL) windowShouldClose: (id) sender;
- (void) sheetDidEndShouldClose: (NSWindow *) sheet
         returnCode: (int) returnCode
         contextInfo: (void *) contextInfo;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

//- (void)                 setTextView: (CLIPSTerminalView *) theTextView;
//- (CLIPSTerminalView *)  textView;

//- (void)                 setEnvironment: (CLIPSEnvironment *) theEnvironment;
- (CLIPSEnvironment *)   environment;

- (void)                 setEnvController: (EnvController *) theController;
- (EnvController *)      envController;

- (void)                 setCurrentDirectory: (NSString *) theValue;
- (NSString *)           currentDirectory;

- (void)                 setDisplayDirectory: (NSString *) theValue;
- (NSString *)           displayDirectory;

- (NSConditionLock *)    pauseLock;

- (void)                 setScrollToEnd: (BOOL) theValue;
- (BOOL)                 scrollToEnd;

- (void)                 setClearWindow: (BOOL) theValue;

@end
