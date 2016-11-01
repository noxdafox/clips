//
//  ViewController.h
//  Animal
//
//  Created by Gary Riley
//

#import <UIKit/UIKit.h>

@interface ViewController : UIViewController <UIPickerViewDelegate, UIPickerViewDataSource>

@property (strong, nonatomic) IBOutlet UILabel *displayLabel;
@property (strong, nonatomic) IBOutlet UIButton *prevButton;
@property (strong, nonatomic) IBOutlet UIButton *nextButton;
@property (strong, nonatomic) IBOutlet UIPickerView *answerPickerView;

- (IBAction) nextButtonAction: (id) sender;
- (IBAction) prevButtonAction: (id) sender;
- (void) saveData;

@end

