//
//  CLIPSFactInstance.m
//  CLIPS
//
//  Created by Gary Riley on 3/19/06.
//

#import "CLIPSFactInstance.h"

@implementation CLIPSFactInstance

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Initialization/Deallocation Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*****************/
/* initWithFact: */
/*****************/
- initWithFact: (Fact *) theFact
  fromEnvironment: (void *) theEnvironment
  {
   if (self = [super init])
     {
      int i;
      CLIPSValue slotNames;
      UDFValue slotValue;
      UDFValue defaultValue;
      CLIPSValue cv;
      NSMutableArray *tempArray;
      NSMutableDictionary *theMD;
      NSNumber *theNumber;
      CLIPSBitMap *theScopeMap;

      [self setValue: [NSString stringWithUTF8String: FactRelation(theFact)->contents] forKey: @"relationName"];
      
      [self setValue: [NSString stringWithFormat:@"%lld", FactIndex(theEnvironment,theFact)] forKey: @"name"];

      [self setValue: [NSNumber numberWithLongLong: FactIndex(theEnvironment,theFact)] forKey: @"index"]; 

      theCPointer = theFact; 
            
      FactSlotNames(theEnvironment,theFact,&slotNames);
      
      environment = theEnvironment;

      theScopeMap = (CLIPSBitMap *) CreateDeftemplateScopeMap(theEnvironment,theFact->whichDeftemplate);
      
      scopeMap = malloc(theScopeMap->size);
      
      memcpy(scopeMap,theScopeMap->contents,theScopeMap->size);
      
      DecrementBitMapCount(theEnvironment,theScopeMap);

      tempArray = [NSMutableArray arrayWithCapacity: (unsigned) slotNames.multifieldValue->length];

      for (i = 0; i < slotNames.multifieldValue->length; i++)
        {
         const char *theCSlotName = slotNames.multifieldValue->contents[i].lexemeValue->contents;
         NSString *theSlotName = [NSString stringWithUTF8String: theCSlotName];
         
         FactSlotValue(theEnvironment,theFact,theCSlotName,&cv);
         CLIPSToUDFValue(&cv,&slotValue);

         NSString *theSlotValue = [NSString stringWithUTF8String: DataObjectToString(theEnvironment,&slotValue)];

         /* Only static defaults will be excluded from display. */

         if (DeftemplateSlotDefaultP(FactDeftemplate(theEnvironment,theFact),theCSlotName) == STATIC_DEFAULT)
           {
            DeftemplateSlotDefaultValue(FactDeftemplate(theEnvironment,theFact),
                                        theCSlotName,&cv);
            CLIPSToUDFValue(&cv,&defaultValue);
            if (DOsEqual(&slotValue,&defaultValue))
              { theNumber = [NSNumber numberWithBool: NO]; }
            else
              { theNumber = [NSNumber numberWithBool: YES]; }
           }
         else
           { theNumber = [NSNumber numberWithBool: YES]; }
         
         theMD = [NSMutableDictionary dictionaryWithObjectsAndKeys: theSlotName, @"slotName", theSlotValue, @"slotValue",
                                       theNumber, @"slotDefault", nil];
          
         [tempArray addObject: theMD];
        }
      
      [self setValue: tempArray forKey: @"attributeValues"];
      
      /* IncrementFactCount(theEnvironment,theFact); */
     }
     
   return self;
  }

/*********************/
/* initWithInstance: */
/*********************/
- initWithInstance: (Instance *) theInstance
  fromEnvironment: (void *) theEnvironment
  {
   if (self = [super init])
     {
      int i;
      CLIPSValue slotNames;
      UDFValue slotValue;
      UDFValue defaultValue;
      NSMutableArray *tempArray;
      NSMutableDictionary *theMD;
      NSNumber *theNumber;
      CLIPSBitMap *theScopeMap;
      Defclass *theClass;
      CLIPSValue result;

      theClass = InstanceClass(theInstance);
      [self setValue: [NSString stringWithUTF8String: DefclassName(theClass)] forKey: @"relationName"]; 
      
      [self setValue: [NSString stringWithUTF8String: InstanceName(theInstance)] forKey: @"name"]; 

      /*====================================================*/
      /* An index of -1 indicates that this is an instance. */
      /*====================================================*/
      
      [self setValue: [NSNumber numberWithLongLong: -1] forKey: @"index"]; 
      
      theCPointer = theInstance; 
            
      ClassSlots(theClass,&slotNames,true);
      
      environment = theEnvironment;

      theScopeMap = (CLIPSBitMap *) CreateClassScopeMap(theEnvironment,theClass);
      
      scopeMap = malloc(theScopeMap->size);
      
      memcpy(scopeMap,theScopeMap->contents,theScopeMap->size);
      
      DecrementBitMapCount(theEnvironment,theScopeMap);

      tempArray = [NSMutableArray arrayWithCapacity: (unsigned) slotNames.multifieldValue->length];

      for (i = 0; i < slotNames.multifieldValue->length; i++)
        {
         const char *theCSlotName = slotNames.multifieldValue->contents[i].lexemeValue->contents;
         NSString *theSlotName = [NSString stringWithUTF8String: theCSlotName];
         
         DirectGetSlot(theEnvironment,theInstance,theCSlotName,&result);
         CLIPSToUDFValue(&result,&slotValue);
         
         NSString *theSlotValue = [NSString stringWithUTF8String: DataObjectToString(theEnvironment,&slotValue)];

         /* Only static defaults will be excluded from display. */
 
         if (SlotDefaultP(theEnvironment,theClass,theCSlotName) == STATIC_DEFAULT)
           {
            SlotDefaultValue(theClass,theCSlotName,&result);
            CLIPSToUDFValue(&result,&defaultValue);
                                            
            if (DOsEqual(&slotValue,&defaultValue))
              { theNumber = [NSNumber numberWithBool: NO]; }
            else
              { theNumber = [NSNumber numberWithBool: YES]; }
           }
         else
           { theNumber = [NSNumber numberWithBool: YES]; }
         
         theMD = [NSMutableDictionary dictionaryWithObjectsAndKeys: theSlotName, @"slotName", theSlotValue, @"slotValue",
                                       theNumber, @"slotDefault", nil];
          
         [tempArray addObject: theMD];
        }
      
      [self setValue: tempArray forKey: @"attributeValues"];

      /* IncrementInstanceCount(theEnvironment,theInstance); */
     }
     
   return self;
  }

/*********/
/* init: */
/*********/
- (id) init
  {
   self = [super init];
   if (self) 
     { scopeMap = NULL; }
   
   return self;
  }

/************/    
/* dealloc: */
/************/    
- (void) dealloc
  {
   if (scopeMap != NULL)
     { free(scopeMap); }
/*
   if ([index longLongValue] == -1)
     { DecremenInstanceCount(environment,theCPointer); }
   else
     { DecrementFactCount(environment,theCPointer); }
*/  
  }

/********************/
/* searchForString: */
/********************/
- (BOOL) searchForString: (NSString *) searchString
  {
   NSRange range;
   NSString *tempString;
   
   if (relationName != nil)
     {
      range = [relationName rangeOfString: searchString
                            options: NSCaseInsensitiveSearch];
   
      if (range.location != NSNotFound) 
        { return YES; }
     }
    
   if (index != nil)
     {
      if ([index longLongValue] == -1)
        { tempString = [NSString stringWithFormat:@"[%@]",name]; }
      else
        { tempString = [NSString stringWithFormat:@"f-%lld",[index longLongValue]]; }
      
      range = [tempString rangeOfString: searchString
                          options: NSCaseInsensitiveSearch];
   
      if (range.location != NSNotFound) 
        { return YES; }
     }

   if (attributeValues != nil)
     {
      NSEnumerator *enumerator = [attributeValues objectEnumerator];
      NSDictionary *item;
        
      while (item = [enumerator nextObject])
        {
         tempString = [NSString stringWithFormat:@"%@ %@ %@",relationName,[item valueForKey: @"slotName"],[item valueForKey: @"slotValue"]];

         range = [tempString rangeOfString: searchString
                             options: NSCaseInsensitiveSearch];
   
         if (range.location != NSNotFound) 
           { return YES; }
        }
     }
     
   return NO;
  }
  
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/* Key-Value Coding Methods */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/***********************/
/* setAttributeValues: */
/***********************/
- (void) setAttributeValues: (NSArray *) theAttributeValues
  {
   attributeValues = theAttributeValues;
  }
  
/********************/
/* attributeValues: */
/********************/
- (NSArray *) attributeValues
  {
   return attributeValues;
  }

/***********************/
/* setRelationName: */
/***********************/
- (void) setRelationName: (NSString *) theRelationName
  {
   relationName = theRelationName;
  }
  
/*****************/
/* relationName: */
/*****************/
- (NSString *) relationName
  {
   return relationName;
  }

/************/
/* setName: */
/************/
- (void) setName: (NSString *) theName
  {
   name = theName;
  }
  
/********************/
/* name: */
/********************/
- (NSString *) name
  {
   return name;
  }

/*************/
/* setIndex: */
/*************/
- (void) setIndex: (NSNumber *) theIndex
  {
   index = theIndex;
  }
  
/********************/
/* index: */
/********************/
- (NSNumber *) index
  {
   return index;
  }
  
/*****************/
/* setScopeMap: */
/*****************/
- (void) setScopeMap: (void *) theValue
  {
   scopeMap = theValue;
  }

/**************/
/* scopeMap: */
/**************/
- (void *) scopeMap
  {
   return scopeMap;
  }

/**************/
/* CPointer: */
/**************/
- (void *) CPointer
  {
   return theCPointer;
  }
  
@end
