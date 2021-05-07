//
//  ViewController.m
//  Auto
//
//  Created by Gary Riley on 5/4/21.
//

#import "ViewController.h"

#import <CLIPSiOS/clips.h>

@interface ViewController ()
  {
   void *clipsEnv;
   NSString *relationAsserted;
   NSMutableArray *validAnswers;
   NSMutableArray *displayAnswers;
   NSMutableArray *variableAsserts;
   NSMutableArray *priorAnswers;
   NSInteger interviewState;
  }

@end

@implementation ViewController

@synthesize prevButton, nextButton, displayLabel, answerPickerView;

enum interviewStateValues
  {
   kGreeting = 0,
   kInterview,
   kConclusion
  };

NSString *kRestoreVariableAssertsKey = @"VariableAsserts";
NSString *kRestorePriorAnswersKey = @"PriorAnswers";
NSString *kRestoreCurrentAnswerKey = @"CurrentAnswer";

/***************************************/
/* pickerView:numberOfRowsInComponent: */
/***************************************/
- (NSInteger) pickerView: (UIPickerView *) pickerView numberOfRowsInComponent: (NSInteger) component
  {
   return [displayAnswers count];
  }

/***********************************/
/* numberOfComponentsInPickerView: */
/***********************************/
- (NSInteger) numberOfComponentsInPickerView: (UIPickerView *) pickerView
  {
   return 1;
  }

/***************************************************/
/* pickerView:viewForRow:forComponent:reusingView: */
/***************************************************/
- (UIView *) pickerView: (UIPickerView *) pickerView viewForRow: (NSInteger) row
             forComponent: (NSInteger) component reusingView: (UIView *) view
  {
   CGRect frame;
   
   frame = CGRectMake(0.0, 0.0, pickerView.frame.size.width, 40.0);
     
   if (view == nil)
     { view =  [[UILabel alloc] initWithFrame: frame];  }
     
   view.backgroundColor = [UIColor clearColor];
   UILabel *theLabel = (UILabel *) view;
   
   theLabel.text = [displayAnswers objectAtIndex: row];

   theLabel.textAlignment = NSTextAlignmentCenter;
     
   ((UILabel *) view).font = [UIFont boldSystemFontOfSize: 22];
   
   return view;
  }
 
/***************/
/* evalString */
/***************/
- (void) evalString: (NSString *) evalString
  {
   char *cEvalString;
   
   cEvalString = (char *) [evalString UTF8String];
   
   Eval(clipsEnv,cEvalString,NULL);
  }

/******************/
/* handleResponse */
/******************/
- (void) handleResponse
  {
   CLIPSValue theCV;
   Multifield *theMultifield;
   Fact *theFact;
   const char *theString;

   Eval(clipsEnv,"(find-fact ((?f UI-state)) TRUE)",&theCV);
   
   if ((theCV.header->type != MULTIFIELD_TYPE) ||
       (theCV.multifieldValue->length == 0))
     { return; }
   
   theMultifield = theCV.multifieldValue;
   if (theMultifield->contents[0].header->type != FACT_ADDRESS_TYPE) return;
   
   theFact = theMultifield->contents[0].factValue;
   
   /*=================================*/
   /* Process state slot of response. */
   /*=================================*/
   
   GetFactSlot(theFact,"state",&theCV);
   if ((theCV.header->type == SYMBOL_TYPE) ||
       (theCV.header->type == STRING_TYPE))
     { theString = theCV.lexemeValue->contents; }
   else
     { theString = ""; }
   
   if (strcmp(theString,"greeting") == 0)
     {
      interviewState = kGreeting;
      [prevButton setHidden: YES];
      [nextButton setHidden: NO];
      [nextButton setTitle: @"Next" forState: UIControlStateNormal];
      [answerPickerView setHidden: YES];
     }
   else if (strcmp(theString,"interview") == 0)
     {
      interviewState = kInterview;
      [prevButton setHidden: NO];
      [nextButton setHidden: NO];
      [nextButton setTitle: @"Next" forState: UIControlStateNormal];
      [answerPickerView setHidden: NO];
     }
   else if (strcmp(theString,"conclusion") == 0)
     {
      interviewState = kConclusion;
      [prevButton setHidden: NO];
      [nextButton setHidden: NO];
      [nextButton setTitle: @"Restart" forState: UIControlStateNormal];
      [answerPickerView setHidden: YES];
     }
 
   /*===================================*/
   /* Process display slot of response. */
   /*===================================*/
  
   GetFactSlot(theFact,"display",&theCV);
   
   if ((theCV.header->type == SYMBOL_TYPE) ||
       (theCV.header->type == STRING_TYPE))
     { theString = theCV.lexemeValue->contents; }
   else
     { theString = ""; }
   
   self.displayLabel.text = [NSString stringWithCString: theString encoding: NSUTF8StringEncoding];

   /*=============================================*/
   /* Process relation-asserted slot of response. */
   /*=============================================*/

   GetFactSlot(theFact,"relation-asserted",&theCV);
   
   if ((theCV.header->type == SYMBOL_TYPE) ||
       (theCV.header->type == STRING_TYPE))
     { theString = theCV.lexemeValue->contents; }
   else
     { theString = ""; }

   relationAsserted = [NSString stringWithCString: theString encoding: NSUTF8StringEncoding];

   /*=========================================*/
   /* Process valid-answers slot of response. */
   /*=========================================*/

   [validAnswers removeAllObjects];
   
   GetFactSlot(theFact,"valid-answers",&theCV);

   if (theCV.header->type == MULTIFIELD_TYPE)
     {
      int i;
   
      theMultifield = theCV.multifieldValue;
      
      for (i = 0; i < theMultifield->length; i++)
        {
         if ((theMultifield->contents[i].header->type == SYMBOL_TYPE) ||
             (theMultifield->contents[i].header->type == STRING_TYPE))
           {
            theString = theMultifield->contents[i].lexemeValue->contents;
            [validAnswers addObject: [NSString stringWithCString: theString encoding: NSUTF8StringEncoding]];
           }
        }
     }

   /*=========================================*/
   /* Process valid-answers slot of response. */
   /*=========================================*/

   [displayAnswers removeAllObjects];
   
   GetFactSlot(theFact,"display-answers",&theCV);

   if (theCV.header->type == MULTIFIELD_TYPE)
     {
      int i;
   
      theMultifield = theCV.multifieldValue;
      
      for (i = 0; i < theMultifield->length; i++)
        {
         if ((theMultifield->contents[i].header->type == SYMBOL_TYPE) ||
             (theMultifield->contents[i].header->type == STRING_TYPE))
           {
            theString = theMultifield->contents[i].lexemeValue->contents;
            [displayAnswers addObject: [NSString stringWithCString: theString encoding: NSUTF8StringEncoding]];
           }
        }
     }

   [answerPickerView reloadComponent: 0];
  }

/****************/
/* processRules */
/****************/
- (void) processRules
  {
   NSString *filePath;
   char *cFilePath;
   long long rulesFired;
   NSString *factString, *assertCommand;
   
   /*==============*/
   /* Reset CLIPS. */
   /*==============*/
   
   Reset(clipsEnv);

   /*======================*/
   /* Load the auto facts. */
   /*======================*/
   
   filePath = [[NSBundle mainBundle] pathForResource: @"auto_en" ofType: @"fct"];
   cFilePath = (char *) [filePath UTF8String];
   LoadFacts(clipsEnv,cFilePath);
   
   for (factString in variableAsserts)
     {
      assertCommand = [NSString stringWithFormat: @"(assert %@)",factString];
      [self evalString: assertCommand];
     }
      
   rulesFired = Run(clipsEnv,-1);
   
   [self handleResponse];
  }

/*********************/
/* nextButtonAction: */
/*********************/
- (IBAction) nextButtonAction: (id) sender
  {
   NSString *theString;
   NSInteger theAnswer;
     
   switch (interviewState)
     {
      /* Handle Next button. */
      case kGreeting:
      case kInterview:
        theAnswer = [answerPickerView selectedRowInComponent: 0];
        theString = [NSString stringWithFormat: @"(%@ %@)",
                                                relationAsserted,
                                                [validAnswers objectAtIndex: theAnswer]];
        [variableAsserts addObject: theString];
        [priorAnswers addObject: [NSNumber numberWithInteger: theAnswer]];
        break;
        
      /* Handle Restart button. */
      case kConclusion:
        [variableAsserts removeAllObjects];
        [priorAnswers removeAllObjects];
        break;
     }

   [self processRules];
   
   [answerPickerView selectRow: 0 inComponent: 0 animated: NO];
  }

/*********************/
/* prevButtonAction: */
/*********************/
- (IBAction) prevButtonAction: (id) sender
  {
   NSInteger lastAnswer;
   
   lastAnswer = [[priorAnswers lastObject] integerValue];
   
   [variableAsserts removeLastObject];
   [priorAnswers removeLastObject];
   
   [self processRules];

   [answerPickerView selectRow: lastAnswer inComponent: 0 animated: NO];
  }

/***************/
/* viewDidLoad */
/***************/
- (void) viewDidLoad
  {
   NSString *filePath;
   char *cFilePath;
   NSNumber *currentAnswer;
   
   [super viewDidLoad];
   
   variableAsserts = [[[NSUserDefaults standardUserDefaults] valueForKey: kRestoreVariableAssertsKey] mutableCopy];
   priorAnswers = [[[NSUserDefaults standardUserDefaults] valueForKey: kRestorePriorAnswersKey] mutableCopy];
   currentAnswer = [[NSUserDefaults standardUserDefaults] valueForKey: kRestoreCurrentAnswerKey];
   
   if (variableAsserts == nil)
     { variableAsserts = [NSMutableArray arrayWithCapacity: 10]; }
   if (priorAnswers == nil)
     { priorAnswers = [NSMutableArray arrayWithCapacity: 10]; }

   clipsEnv = CreateEnvironment();
   if (clipsEnv == NULL) return;
   
   filePath = [[NSBundle mainBundle] pathForResource: @"auto" ofType: @"clp"];
   cFilePath = (char *) [filePath UTF8String];
   Load(clipsEnv,cFilePath);
   
   validAnswers = [NSMutableArray arrayWithCapacity: 2];
   displayAnswers = [NSMutableArray arrayWithCapacity: 2];
      
   [self processRules];
   
   [answerPickerView selectRow: [currentAnswer integerValue] inComponent: 0 animated: NO];
  }

/************/
/* saveData */
/************/
- (void) saveData
  {
   [[NSUserDefaults standardUserDefaults] setValue: variableAsserts forKey: kRestoreVariableAssertsKey];
   [[NSUserDefaults standardUserDefaults] setValue: priorAnswers forKey: kRestorePriorAnswersKey];
   [[NSUserDefaults standardUserDefaults] setValue: [NSNumber numberWithInteger: [answerPickerView selectedRowInComponent: 0]]
                                            forKey: kRestoreCurrentAnswerKey];
  }

/***************************/
/* didReceiveMemoryWarning */
/***************************/
- (void) didReceiveMemoryWarning
  {
   [super didReceiveMemoryWarning];
   // Dispose of any resources that can be recreated.
  }

@end
