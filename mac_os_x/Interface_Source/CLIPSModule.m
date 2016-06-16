//
//  CLIPSModule.m
//  CLIPS
//
//  Created by Gary Riley on 3/15/07.
//

#import "CLIPSModule.h"


@implementation CLIPSModule

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super init];
   if (self) 
     {    
     }
   
   return self;
  }

/****************/
/* description: */
/****************/
- (NSString *) description
  {
   return moduleName;
  }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/******************/
/* setModuleName: */
/******************/
- (void) setModuleName: (NSString *) theModuleName
  {
   moduleName = theModuleName;
  }

/***************/
/* moduleName: */
/***************/
- (NSString *) moduleName
  {
   return moduleName;
  }

@end
