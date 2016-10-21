//
//  CLIPSTextDocument.h
//  CLIPSEditor
//
//  Created by Gary Riley on 2/14/06.
//


#import <Cocoa/Cocoa.h>

@class CLIPSTextView;

@interface CLIPSTextDocument : NSDocument
  {
   NSString *string;
   IBOutlet CLIPSTextView *textView;
   IBOutlet NSButton *popupActivator;
   NSDictionary *hiliteColor;
  }

- (void) balanceIt: (NSString *) theText
         leftMiddle: (unsigned int) leftMiddle
         rightMiddle: (unsigned int) rightMiddle
         leftCount: (int) leftCount
         rightCount: (int) rightCount
         textLength: (unsigned int) textLength;

/*%%%%%%%%%%%%%%%%*/
/* Action Methods */
/*%%%%%%%%%%%%%%%%*/

- (IBAction)           loadSelection: (id) sender;
- (IBAction)           batchSelection: (id) sender;
- (IBAction)           loadBuffer: (id) sender;
- (IBAction)           balance: (id) sender;
- (IBAction)           comment: (id) sender;
- (IBAction)           uncomment: (id) sender;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

@end
