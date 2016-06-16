//
//  CLIPSActivation.m
//  CLIPS
//
//  Created by Gary Riley on 3/13/06.
//

#import "CLIPSActivation.h"


@implementation CLIPSActivation


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

/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/****************/
/* setSalience: */
/****************/
- (void) setSalience: (NSNumber *) theSalience
  {
   salience = theSalience;
  }

/*************/
/* salience: */
/*************/
- (NSNumber *) salience
  {
   return salience;
  }

/****************/
/* setRuleName: */
/****************/
- (void) setRuleName: (NSString *) theRuleName
  {
   ruleName = theRuleName;
  }

/*************/
/* ruleName: */
/*************/
- (NSString *) ruleName
  {
   return ruleName;
  }

/****************/
/* setBindings: */
/****************/
- (void) setBindings: (NSString *) theBindings
  {
   bindings = theBindings;
  }

/*************/
/* bindings: */
/*************/
- (NSString *) bindings
  {
   return bindings;
  }

/******************/
/* setActivation: */
/******************/
- (void) setActivation: (void *) theActivation
  {
   activation = theActivation;
  }

/***************/
/* activation: */
/***************/
- (void *) activation
  {
   return activation;
  }

@end
