//
//  CLIPSActivation.h
//  CLIPS
//
//  Created by Gary Riley on 3/13/06.
//

#import <Cocoa/Cocoa.h>
#import <CLIPS/clips.h>

@interface CLIPSActivation : NSObject 
  {
   NSNumber *salience;
   NSString *ruleName;
   NSString *bindings;
   void *activation;
  }
  
- (void)                         setSalience: (NSNumber *) theSalience;
- (NSNumber *)                   salience;

- (void)                         setRuleName: (NSString *) theRuleName;
- (NSString *)                   ruleName;

- (void)                         setBindings: (NSString *) theBindings;
- (NSString *)                   bindings;

- (void)                         setActivation: (Activation *) theActivation;
- (Activation *)                 activation;

@end
