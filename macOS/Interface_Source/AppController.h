/* Controller */

#import <Cocoa/Cocoa.h>

@class Preferences;
@class PreferenceController;
@class CLIPSEnvironment;
@class CLIPSTerminalController;
@class CLIPSFactController;
@class CLIPSInstanceController;
@class CLIPSAgendaController;
@class CLIPSConstructInspectorController;

@interface AppController : NSObject
  {
   PreferenceController *preferenceController;
   CLIPSEnvironment *mainEnvironment;
   CLIPSTerminalController *mainTerminal;
   CLIPSConstructInspectorController *constructInspectorController;
   NSString *constructInspectorText;
   NSMutableArray *factControllers;
   NSMutableArray *instanceControllers;
   NSMutableArray *agendaControllers;
   NSLock *fileOpenLock;
  }

- (void) addFactController: (CLIPSFactController *) theController;
- (void) removeFactController: (CLIPSFactController *) theController;

- (void) addInstanceController: (CLIPSInstanceController *) theController;
- (void) removeInstanceController: (CLIPSInstanceController *) theController;

- (void) addAgendaController: (CLIPSAgendaController *) theController;
- (void) removeAgendaController: (CLIPSAgendaController *) theController;

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

- (IBAction) showPreferencePanel: (id) sender;

- (IBAction) clear: (id) sender;
- (IBAction) loadBatch: (id) sender;
- (IBAction) loadConstructs: (id) sender;
- (IBAction) setDirectory: (id) sender;

- (IBAction) reset: (id) sender;
- (IBAction) run: (id) sender;
- (IBAction) haltRules: (id) sender;
- (IBAction) haltExecution: (id) sender;

- (IBAction) agendaBrowser: (id) sender;
- (IBAction) factBrowser: (id) sender;
- (IBAction) instanceBrowser: (id) sender;
- (IBAction) constructInspector: (id) sender;

- (IBAction) watchCompilations: (id) sender;
- (IBAction) watchStatistics: (id) sender;
- (IBAction) watchFacts: (id) sender;
- (IBAction) watchRules: (id) sender;
- (IBAction) watchActivations: (id) sender;
- (IBAction) watchFocus: (id) sender;
- (IBAction) watchGlobals: (id) sender;
- (IBAction) watchDeffunctions: (id) sender;
- (IBAction) watchGenericFunctions: (id) sender;
- (IBAction) watchMethods: (id) sender;
- (IBAction) watchInstances: (id) sender;
- (IBAction) watchSlots: (id) sender;
- (IBAction) watchMessageHandlers: (id) sender;
- (IBAction) watchMessages: (id) sender;
- (IBAction) watchAll: (id) sender;
- (IBAction) watchNone: (id) sender;

- (IBAction) showCLIPSHomePage: (id) sender;
- (IBAction) showCLIPSDocumentation: (id) sender;
- (IBAction) showCLIPSExamples: (id) sender;
- (IBAction) showCLIPSExpertSystemGroup: (id) sender;
- (IBAction) showCLIPSSourceForgeDiscussion: (id) sender;
- (IBAction) showCLIPSStackOverflow: (id) sender;

/*%%%%%%%%%%%%%%%%%%*/
/* Delegate Methods */
/*%%%%%%%%%%%%%%%%%%*/

- (NSApplicationTerminateReply) applicationShouldTerminate: (NSApplication *) app;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (NSLock *)               fileOpenLock;

@property (NS_NONATOMIC_IOSONLY, strong) CLIPSEnvironment *mainEnvironment;
@property (NS_NONATOMIC_IOSONLY, strong) CLIPSTerminalController *mainTerminal;
@property (NS_NONATOMIC_IOSONLY, strong) NSString *constructInspectorText;

@end
