/* CLIPSConstructInspectorController */

#import <Cocoa/Cocoa.h>

@class AppController;

@interface CLIPSConstructInspectorController : NSWindowController
  {
   IBOutlet NSTextView *textView;
   AppController *appController;
  }

- (void) showPanel;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

- (void)                         setAppController: (AppController *) theController;
- (AppController *)              appController;

@end
