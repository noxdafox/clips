//
//  CLIPSTerminalView.h
//  CLIPS
//
//  Created by Gary Riley on 2/23/06.
//

#import <Cocoa/Cocoa.h>

@class CLIPSEnvironment;

@interface CLIPSTerminalView : NSTextView
  {
   CLIPSEnvironment *__weak environment;
   IBOutlet id dialogWindow;
   BOOL routerPrint;
   BOOL balancingDisabled;
   //BOOL waitingForChar;
   int charFound;
   char *inputBuffer;
   size_t inputPos;
   NSConditionLock *inputCharLock;
   NSDictionary *hiliteColor;
  }
  
- (void) resetBackgroundColour: (id) sender;
- (NSUInteger) print: (NSString *) theString;
- (void) clearTerminal;
- (void) balanceParentheses;
- (int) waitForChar;
- (BOOL) readStringFromPasteboard: (NSPasteboard *) pb;
- (NSUInteger) inputStringOffset;

@property (NS_NONATOMIC_IOSONLY, weak) CLIPSEnvironment *environment;
@property (nonatomic, assign) BOOL balancingDisabled;

@end
