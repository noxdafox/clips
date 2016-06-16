//
//  PreferenceController.h
//  CLIPS
//
//  Created by Gary Riley on 2/25/06.
//

#import <Cocoa/Cocoa.h>


@interface PreferenceController : NSWindowController
  {
   IBOutlet NSTabView *tabView;
  }
  
- (IBAction) changeEditorFont: (id) sender;
- (IBAction) doCancel: (id) sender;
- (IBAction) doOK: (id) sender;
- (void) changeFont: (id) fontManager;
- (NSApplicationTerminateReply) reviewPreferencesBeforeQuitting;
- (void) showPanel;

@end

@interface FontNameToDisplayNameTransformer : NSValueTransformer
  {
  }

@end
