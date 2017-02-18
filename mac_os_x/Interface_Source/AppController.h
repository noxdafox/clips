/* Controller */

#import <Cocoa/Cocoa.h>

@class Preferences;
@class PreferenceController;
@class EnvController;
@class CLIPSTextMenu;

@interface AppController : NSObject
  {
   PreferenceController *preferenceController;
   EnvController *envController;
   CLIPSTextMenu *textMenu;
  }

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

- (IBAction) showPreferencePanel: (id) sender;

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

- (void)               setEnvController: (EnvController *) theController;
- (EnvController *)    envController;

@end
