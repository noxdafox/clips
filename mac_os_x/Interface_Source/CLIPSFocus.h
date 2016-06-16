//
//  CLIPSFocus.h
//  CLIPS
//
//  Created by Gary Riley on 3/13/06.
//

#import <Cocoa/Cocoa.h>

@interface CLIPSFocus : NSObject 
  {
   NSString *moduleName;
   NSArray *agenda;
  }

- (NSString *) description;

- (void)                         setModuleName: (NSString *) theModuleName;
- (NSString *)                   moduleName;

- (void)                         setAgenda: (NSArray *) theAgenda;
- (NSArray *)                    agenda;

@end
