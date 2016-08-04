//
//  CLIPSTerminalView.m
//  CLIPS
//
//  Created by Gary Riley on 2/23/06.
//

#import "CLIPSEnvironment.h"
#import "CLIPSTerminalView.h"
#import "CLIPSTerminalController.h"
#import <CLIPS/clips.h>

#define DOES_NOT_CHAR  0
#define NEEDS_CHAR     1 
#define HAS_CHAR       2

@implementation CLIPSTerminalView

@synthesize environment;

/********************************/
/* initWithFrame:textContainer: */
/********************************/
- (id) initWithFrame: (NSRect) frameRect textContainer: (NSTextContainer *) aTextContainer
  {
   self = [super initWithFrame: frameRect textContainer: aTextContainer];
   
   if (self)
     { }
     
   return self;
  }
   
/******************/
/* initWithFrame: */
/******************/
- (id) initWithFrame: (NSRect) frameRect
  {
   self = [super initWithFrame: frameRect];
   
   if (self)
     { }
     
   return self;
  }
  
/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super init];
   
   if (self) 
     { inputBuffer = NULL; }
   
   return self;
  }
  
/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   if (inputBuffer != NULL)
     { free(inputBuffer); }
  }
   
/*****************/
/* awakeFromNib: */
/*****************/
- (void) awakeFromNib
  {
   inputCharLock = [[NSConditionLock alloc] initWithCondition: DOES_NOT_CHAR];
   hiliteColor = [[NSDictionary alloc] initWithObjectsAndKeys: [NSColor selectedTextBackgroundColor], NSBackgroundColorAttributeName, nil];
  }

/*************************/
/* performDragOperation: */
/*************************/
- (BOOL) performDragOperation: (id <NSDraggingInfo>) sender
  {
   /*============================================*/
   /* Dropping text in the terminal window isn't */
   /* isn't supported when CLIPS is executing    */
   /* a command and expecting input.             */
   /*============================================*/
   
   if ([inputCharLock condition] == NEEDS_CHAR)
     { return NO; }

   /*===============================================================*/
   /* If the insertion point is before the beginning of the current */
   /* command, then place it at the end of the terminal windows,    */
   /* otherwise place the insertion point at the drag point.        */
   /*===============================================================*/

   NSPoint draggingLocation = [self convertPoint: [sender draggingLocation] fromView: nil];
   NSUInteger location = [self characterIndexForInsertionAtPoint: draggingLocation];
   
   NSUInteger textLength;
   NSString *theTerminalText;
   NSUInteger inputOffset = [self inputStringOffset];
      
   theTerminalText = [self string];
   textLength = [theTerminalText length];
   NSUInteger inputStart = textLength - inputOffset;
   if (location < inputStart)
     {
      // TBD The drag cursor leaves an artifact at the original location
      NSRange theRange = { textLength, 0 };
      [super setSelectedRange: theRange];
     }
   else
     {
      NSRange theRange = { location, 0 };
      [super setSelectedRange: theRange];
     }

   NSPasteboard *pb = [sender draggingPasteboard];
   if (! [self readStringFromPasteboard: pb])
     { return NO; }
   
   return YES;
  }

/***********/
/* delete: */
/***********/
- (IBAction) delete: (id) sender
  {
   char *str;
   NSUInteger textLength;
   NSString *theTerminalText;
   NSUInteger inputOffset = [self inputStringOffset];
      
   theTerminalText = [self string];
      
   NSString *theCommand, *preCommand, *postCommand;
   textLength = [theTerminalText length];
   NSUInteger inputStart = textLength - inputOffset;
         
   NSRange selectionRange = [super selectedRange];

   if (selectionRange.location < inputStart)
     { return; }

   theCommand = [theTerminalText substringFromIndex: inputStart];
   
   preCommand = [theCommand substringToIndex: (selectionRange.location - inputStart)];
   
   postCommand = [theCommand substringFromIndex: (selectionRange.location + selectionRange.length - inputStart)];

   str = (char *) [preCommand UTF8String];
   SetCommandString([environment environment],str);
      
   str = (char *) [postCommand UTF8String];
   AppendCommandString([environment environment],str);
   
   [super delete: sender];
  }

/**********/
/* paste: */
/**********/
- (IBAction) paste: (id) sender
  {
   // InsertText should handle changing selection
   //NSRange theRange = { [[super string] length], 0 };
   //[super setSelectedRange: theRange];

   NSPasteboard *pb = [NSPasteboard generalPasteboard];
   
   [self readStringFromPasteboard: pb];
  }

/*****************************/
/* readStringFromPasteboard: */
/*****************************/
- (BOOL) readStringFromPasteboard: (NSPasteboard *) pb
  {
   NSString *theText;
   NSString *type;
   
   // Is there a string on the pasteboard?
   
   type = [pb availableTypeFromArray: [NSArray arrayWithObject: NSStringPboardType]];
   
   if (type)
     {
      // Read the string from the pasteboard
      theText = [pb stringForType: NSStringPboardType];
      
      [self insertText: theText replacementRange: [self selectedRange]];
      
      [self scrollRangeToVisible: [self selectedRange]];
      
      return YES;
     }
     
   return NO;
  }
  
/**********/    
/* print: */
/**********/    
- (unsigned int) print: (NSString *) theString
  {
   NSRange theRange = { [[super string] length], 0 };

   NSMutableAttributedString *terminalStorage = [self textStorage];

   [super setSelectedRange: theRange];   

   /*==================================================================*/  
   /* If the terminal window doesn't contain any text, use insertText  */
   /* to add the text, otherwise use replaceCharactersInRange. If the  */
   /* terminal window doesn't contain text, it uses the wrong font it  */
   /* the latter method is used, so this test is a workaround for that */
   /* problem. Another possible workaround is to use an attributed     */
   /* string with the font attribute added.                            */
   /*==================================================================*/  
          
   if ([terminalStorage isEqualToAttributedString: [[NSAttributedString new] initWithString: @""]])
     { [super insertText: theString replacementRange: [self selectedRange]]; }
   else
     {
      [terminalStorage replaceCharactersInRange: theRange withString: theString];
      [self didChangeText];
     }
   
   /*================================================*/
   /* Let the terminal controller know that text has */
   /* been added and it should be made visible.      */
   /*================================================*/
   
   [[dialogWindow windowController] setScrollToEnd: YES];
   
   return [[super string] length];
  }

/******************/    
/* clearTerminal: */
/******************/    
- (void) clearTerminal
  {
   NSMutableAttributedString *terminalStorage = [self textStorage];

   [terminalStorage setAttributedString: [[NSAttributedString new] initWithString: @""]];
   [self didChangeText];
   
   /*================================================*/
   /* Let the terminal controller know that text has */
   /* been changed and it should be made visible.    */
   /*================================================*/
   
   [[dialogWindow windowController] setScrollToEnd: YES];
  }

/***********************/    
/* balanceParentheses: */
/***********************/    
- (void) balanceParentheses
  {
   NSRange selectionRange;
   unsigned int cursorLocation;
   int commandLength;
   unichar characterToCheck;
   unsigned short nestingDepth;
   NSString *theText = [super string];
   NSUserDefaults *theValues;
   
   /*==============================================================*/
   /* Check the defaults to see if parentheses should be balanced. */
   /*==============================================================*/
   
   theValues = [[NSUserDefaultsController sharedUserDefaultsController] values];
    
   if (! [[theValues valueForKey: @"dialogBalanceParens"] boolValue]) 
     { return; } 

  /*================================================*/
   /* Don't balance parentheses if there is no text. */
   /*================================================*/
   
   if ([theText length] == 0)
     { return; }
     
   /*=======================================================*/
   /* Don't balance the parentheses if there is no command. */
   /*=======================================================*/
   
   commandLength = RouterData([environment environment])->CommandBufferInputCount;
   if (commandLength <= 0) 
     { return; }

   /*=================================*/
   /* Retrieve the current selection. */
   /*=================================*/
          
   selectionRange = [super selectedRange];
   
   /*======================*/
   /* Where is the cursor? */
   /*======================*/
    
   cursorLocation = selectionRange.location;
   
   if (cursorLocation == 0) return;
   
   cursorLocation--;
   
   /*===============================================*/
   /* What is the character at the cursor location? */
   /*===============================================*/
    
   characterToCheck = [theText characterAtIndex: cursorLocation];

   /*======================================*/
   /* We only balance a right parenthesis. */
   /*======================================*/
   
   if (characterToCheck != ')') return;

   /*======================================================================*/
   /* The nesting depth will start at zero. Each time a ')' is encountered */
   /* the nesting depth is incremented by one and each time a '(' is       */
   /* encountered the nesting depth is decremented by one. If a '(' is     */
   /* encountered when the nesting depth is zero (the starting value), the */
   /* matching parenthesis has been found.                                 */
   /*======================================================================*/
   
   nestingDepth = 0;

   /*==================================================*/
   /* Start looking for the matching left parenthesis. */
   /*==================================================*/
      
   while (cursorLocation-- && commandLength--) 
     {
      characterToCheck = [theText characterAtIndex: cursorLocation];
      if (characterToCheck == '(') 
        {
         if (nestingDepth == 0) 
           {
            [[super layoutManager] addTemporaryAttributes: hiliteColor forCharacterRange: NSMakeRange(cursorLocation, 1)];
		    [self performSelector: @selector(resetBackgroundColour:) withObject: NSStringFromRange(NSMakeRange(cursorLocation, 1)) afterDelay: 0.12];
		    return;
		   }
         else
		   { nestingDepth--; }
	    }
      else if (characterToCheck == ')') 
        { nestingDepth++; }
     }

   /*================================================*/
   /* Beep to indicate a matching ')' was not found. */
   /*================================================*/
   
   NSBeep();
  }

/***************/
/* insertText: */
/***************/
- (void) insertText: (NSString *) theText replacementRange: (NSRange) replacementRange
  {
   Environment *theEnvironment = [environment environment];
   
   /*================================================*/
   /* If the environment is executing a command (the */
   /* executionLock has been set), then don't allow  */
   /* any input unless the executing environment is  */
   /* requesting input.                              */
   /*================================================*/
   
   if ([[environment executionLock] tryLock])
     { [[environment executionLock] unlock]; }
   else if ([inputCharLock condition] != NEEDS_CHAR)
     { return; }
     
   /*================================*/
   /* Dump any text remaining in the */ 
   /* output buffer to the terminal. */
   /*================================*/
     
   [[dialogWindow windowController] dumpOutputBuffer];

   /*========================================================*/
   /* If the inputCharLock is set to NEEDS_CHAR, then we're  */
   /* waiting for input for functions like read or readline. */
   /* In this case we want to add any new input to the end   */
   /* of the terminal as we don't support inline editing of  */
   /* input to these functions.                              */
   /*========================================================*/

   if ([inputCharLock condition] == NEEDS_CHAR)
     {
      char *str; 
      size_t len;
      str = (char *) [theText UTF8String];
      charFound = str[0];
 
      NSRange theRange = { [[super string] length], 0 };
      [super setSelectedRange: theRange];

      [super insertText: theText replacementRange: replacementRange];
      
      if ((len = strlen(str)) >  1)
        {
         /* TBD Concatenate to existing buffer */
         if (inputBuffer != NULL)
           { free(inputBuffer); }
           
         inputBuffer = (char *) malloc(len);
         strcpy(inputBuffer,&str[1]);
         inputPos = 0;
        }
      
      [inputCharLock lock];
      [inputCharLock unlockWithCondition: HAS_CHAR];
      return;
     }
          
   if (! routerPrint)
     { 
      char *str;
      NSUInteger textLength;

      NSString *theTerminalText;
      NSUInteger inputOffset = [self inputStringOffset];
      
      theTerminalText = [self string];
      
      NSString *theCommand, *preCommand, *postCommand;
      textLength = [theTerminalText length];
      NSUInteger inputStart = textLength - inputOffset;
         
      NSRange selectionRange = [super selectedRange];

      if (selectionRange.location < inputStart)
        {
         NSRange theRange = { [[super string] length], 0 };
         [super setSelectedRange: theRange];
         selectionRange = theRange;
        }

      theCommand = [theTerminalText substringFromIndex: inputStart];
   
      preCommand = [theCommand substringToIndex: (selectionRange.location - inputStart)];
   
      postCommand = [theCommand substringFromIndex: (selectionRange.location + selectionRange.length - inputStart)];

      /*================================================================*/
      /* The string returned by UTF8String is placed in the autorelease */
      /* pool, so there is no need to release the str in this function. */
      /*================================================================*/

      str = (char *) [preCommand UTF8String];
      SetCommandString([environment environment],str);
      
      str = (char *) [theText UTF8String];
      AppendCommandString(theEnvironment,str);
      
      str = (char *) [postCommand UTF8String];
      AppendCommandString([environment environment],str);
     }
   else
     {
      NSRange theRange = { [[super string] length], 0 };
      [super setSelectedRange: theRange];
     }

   /*=======================================*/
   /* Add the text to the terminal at the   */
   /* current insertion point or selection. */
   /*=======================================*/
   
   [super insertText: theText replacementRange: replacementRange];
   
   if (! routerPrint)
     {
      [self balanceParentheses];
      
      if (CommandCompleteAndNotEmpty(theEnvironment))
        {
         // TBD At this point lock command input.
        }
     }
  }

/*************************************************************/
/* moveUpAndModifySelection: Overide of the up arrow + shift */
/*   keys to allow cycling through command history.          */
/*************************************************************/
- (void) moveUpAndModifySelection: (id) sender
  {
   CLIPSTerminalController *theController = (CLIPSTerminalController *) [self delegate];

   if (theController == nil)
     { [super moveUpAndModifySelection: sender]; }
   else if (theController->currentCommand->next != NULL)
     {
      [theController SwitchCommandFrom: theController->currentCommand
                                    To: theController->bottomCommand];
     }
  }

/*****************************************************************/
/* moveDownAndModifySelection: Overide of the down arrow + shift */
/*   keys to allow cycling through command history.              */
/*****************************************************************/
- (void) moveDownAndModifySelection: (id) sender
  {
   CLIPSTerminalController *theController = (CLIPSTerminalController *) [self delegate];

   if (theController == nil)
     { [super moveDownAndModifySelection: sender]; }
   else if (theController->currentCommand->prev != NULL)
     {
      [theController SwitchCommandFrom: theController->currentCommand
                                    To: theController->topCommand];
     }
  }

/*********************************************/
/* moveUp: Overide of the up arrow key to   */
/*   allow cycling through command history. */
/********************************************/
- (void) moveUp: (id) sender
   {
    CLIPSTerminalController *theController = (CLIPSTerminalController *) [self delegate];

    if (theController == nil)
      { [super moveUp: sender]; }
    else if (theController->currentCommand->next != NULL)
      {
       [theController SwitchCommandFrom: theController->currentCommand
                                     To: theController->currentCommand->next];
      }
   }

/***********************************************/
/* moveDown: Override of the down arrow key to */
/*   allow cycling through command history.    */
/***********************************************/
- (void) moveDown: (id) sender
   {
    CLIPSTerminalController *theController = (CLIPSTerminalController *) [self delegate];

    if (theController == nil)
      { [super moveUp: sender]; }
    else if (theController->currentCommand->prev != NULL)
      {
       [theController SwitchCommandFrom: theController->currentCommand
                                     To: theController->currentCommand->prev];
      }
   }

/*******************/
/* deleteBackward: */
/*******************/
- (void) deleteBackward: (id) sender
  {
   NSUInteger inputOffset = [self inputStringOffset];
   NSUInteger textLength;
   NSString *theTerminalText;
   char *str;

   /*====================================================*/
   /* If the input buffer is empty, then there are no    */
   /* characters to delete. Remove the current selection */
   /* and move the cursor to the end of the terminal.    */
   /*====================================================*/
   
   if (RouterData([environment environment])->CommandBufferInputCount <= 0) 
     {
      NSRange theRange = { [[super string] length], 0 };
      [super setSelectedRange: theRange];
      return;
     }

   /*==========================================================*/
   /* Is a running CLIPS function or program (such as the read */
   /* function) waiting for character input from stdin. If so, */
   /* move the cursor to the end of the terminal and just      */
   /* delete the last character.                               */
   /*==========================================================*/
   
   if ([inputCharLock condition] == NEEDS_CHAR)
     {
      NSRange theRange = { [[super string] length], 0 };
      [super setSelectedRange: theRange];
      [super deleteBackward: sender];
      charFound = '\b';
      [inputCharLock lock];
      [inputCharLock unlockWithCondition: HAS_CHAR];
      return;
     }

   /*=================================*/
   /* Retrieve the current selection. */
   /*=================================*/
          
   NSRange selectionRange = [super selectedRange];

   /*========================*/
   /* Determine the start of */
   /* the current command.   */
   /*========================*/
   
   theTerminalText = [self string];
   textLength = [theTerminalText length];
   NSUInteger inputStart = textLength - inputOffset;
  
   /*=============================================*/
   /* If the selection contains any prior output, */
   /* then remove the selection and move the      */
   /* cursor to the end of the terminal.          */
   /*=============================================*/
    
   if (selectionRange.location < inputStart)
     {
      NSRange theRange = { [[super string] length], 0 };
      [super setSelectedRange: theRange];
      return;
     }
 
   /*======================================*/
   /* If the cursor is at the beginning of */
   /* input, then deleting has no effect.  */
   /*======================================*/
   
   if ((selectionRange.location == inputStart) && selectionRange.length == 0)
     { return; }
 
   NSString *theCommand, *preCommand, *postCommand;
   
   theCommand = [theTerminalText substringFromIndex: inputStart];
   
   if (selectionRange.length == 0)
     { preCommand = [theCommand substringToIndex: ((selectionRange.location - inputStart) - 1)]; }
   else
     {
      preCommand = [theCommand substringToIndex: (selectionRange.location - inputStart)];
     }
   
   postCommand = [theCommand substringFromIndex: (selectionRange.location + selectionRange.length - inputStart)];

   str = (char *) [preCommand UTF8String];
   SetCommandString([environment environment],str);
   str = (char *) [postCommand UTF8String];
   AppendCommandString([environment environment],str);

   /*==============================================*/
   /* Process the backspace against the unexecuted */
   /* command being entered at the command prompt. */
   /*==============================================*/

   [super deleteBackward: sender];
   
   [self balanceParentheses];
  }
  
/***********************************************************/
/* waitForChar: Waits for the user to enter a character in */
/*   response to CLIPS router getc request from stdin.     */
/***********************************************************/
- (int) waitForChar
  {
   NSRange theRange = { [[super string] length], 0 };
   charFound = -1;

   /*============================================*/
   /* Bring the bottom of the terminal into view */
   /* so we can see the text being entered.      */
   /*============================================*/
   
   [super scrollRangeToVisible: theRange]; 

   /*=====================================*/
   /* Check for queued input from a UTF-8 */
   /* character or a pasted string.       */
   /*=====================================*/
   
   if (inputBuffer != NULL)
     {
      charFound = inputBuffer[inputPos];
      if (inputBuffer[++inputPos] == '\0')
        {
         free(inputBuffer);
         inputBuffer = NULL;
        }
        
      return charFound;
     }
     
   /*==============================================*/
   /* Change the condition on the lock to indicate */
   /* that an input character is needed.           */
   /*==============================================*/
            
   [inputCharLock lock];
   [inputCharLock unlockWithCondition: NEEDS_CHAR];

   /*=========================================================*/
   /* Block this thread until an input character is detected. */ 
   /*=========================================================*/
     
   [inputCharLock lockWhenCondition: HAS_CHAR];
   [inputCharLock unlockWithCondition: DOES_NOT_CHAR];
   
   /*=============================*/
   /* Return the character found. */
   /*=============================*/
   
   return charFound;
  }

/**************************/
/* resetBackgroundColour: */
/**************************/
- (void) resetBackgroundColour: (id) sender
  {
   [[super layoutManager] removeTemporaryAttribute: NSBackgroundColorAttributeName forCharacterRange: NSRangeFromString(sender)];
  }

/************************************************************************/
/* inputStringOffset: Determines the number of characters in the output */
/*    buffer that are associated with the input that user has currently */
/*    entered. This will typically be the command being entered after   */
/*    the CLIPS> prompt, but could also be the characters entered for   */
/*    an input function such as (read) or (readline). The environment   */
/*    value CommandBufferInputCount contains the number of bytes taken  */
/*    up by the current input, but we need to determine the number of   */
/*    UTF8 characters this represents. We compute this by looking at    */
/*    the last portion of input in the buffer adding one character at a */
/*    time until we get a number of bytes equal to the                  */
/*    CommandBufferInputCount.                                          */
/************************************************************************/
- (NSUInteger) inputStringOffset
  {
    NSString *theString;
    NSUInteger textLength;

    textLength = [[self string] length];
    NSUInteger inputByteCount = RouterData([environment environment])->CommandBufferInputCount;
    NSUInteger bufferByteCount = 0;
    NSUInteger charOffset = 0;
    
    while (bufferByteCount < inputByteCount)
      {
       charOffset++;
       theString = [[self string] substringFromIndex: (textLength - charOffset)];
       bufferByteCount = [theString lengthOfBytesUsingEncoding: NSUTF8StringEncoding];
      }
   
   return charOffset;
  }

/*%%%%%%%%%%%%%%%%%%%%*/
/* Overridden Methods */
/*%%%%%%%%%%%%%%%%%%%%*/

/*************************/
/* performKeyEquivalent: */
/*************************/
/*
- (BOOL) performKeyEquivalent: (NSEvent *) theEvent
  {
   if ([inputCharLock condition] == NEEDS_CHAR)
     { return NO; }
        
   if (([theEvent modifierFlags] & NSCommandKeyMask) &&
       [[theEvent characters] isEqual: @"."])
     {
      Environment *theEnvironment = [environment environment];
      
      if ([theEvent modifierFlags] & NSShiftKeyMask)
        {
         EnvSetHaltExecution(theEnvironment,true);
         CloseAllBatchSources(theEnvironment);
        }
      else
        {
#if DEFRULE_CONSTRUCT
         if (EngineData(theEnvironment)->ExecutingRule != NULL)
           { EngineData(theEnvironment)->HaltRules = true; }
         else
#endif
           {
            EnvSetHaltExecution(theEnvironment,true);
            CloseAllBatchSources(theEnvironment);
           }
        }
       
      waitingForChar = NO;
      charFound = -1;
      [NSApp stopModal];

      return YES;
     }
     
   return NO;
  }
*/
/************************************************/
/* validateMenuItem: The cut and delete actions */
/*   are not allowed in the terminal window.    */
/************************************************/
- (BOOL) validateMenuItem: (NSMenuItem *) menuItem
  {
   NSString *selectorString;
   selectorString = NSStringFromSelector([menuItem action]);
   
   /*============================================================*/
   /* The paste, cut, and delete actions are only allowed in the */
   /* terminal window when the selection is in the input area.   */
   /*============================================================*/
   
   if (([menuItem action] == @selector(paste:)) ||
       ([menuItem action] == @selector(cut:)) ||
       ([menuItem action] == @selector(delete:)))
     {
      NSUInteger textLength;

      NSString *theTerminalText;
      NSUInteger inputOffset = [self inputStringOffset];
      
      theTerminalText = [self string];
      
      textLength = [theTerminalText length];
      NSUInteger inputStart = textLength - inputOffset;
         
      NSRange selectionRange = [super selectedRange];

      if (selectionRange.location < inputStart)
        { return NO; }
     }
     
   /*====================================*/
   /* Otherwise, allow the superclass to */
   /* determine the valid menu items.    */
   /*====================================*/
   
   return [super validateMenuItem: menuItem];
  }
  
@end
