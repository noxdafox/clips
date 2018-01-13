//
//  CLIPSFocus.m
//  CLIPS
//
//  Created by Gary Riley on 3/13/06.
//

#import "CLIPSFocus.h"

@implementation CLIPSFocus

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

/************************/
/* setAgenda: */
/************************/
- (void) setAgenda: (NSArray *) theAgenda
  {
   agenda = theAgenda;
  }

/*********************/
/* agenda: */
/*********************/
- (NSArray *) agenda
  {
   return agenda;
  }

@end
