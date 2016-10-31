   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/30/16            */
   /*                                                     */
   /*                ENTITIES HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Created to store key data structures.          */
/*                                                           */
/*************************************************************/

#ifndef _H_entities

#pragma once

#define _H_entities

typedef struct voidHashNode CLIPSVoid;
typedef struct symbolHashNode CLIPSLexeme;
typedef struct floatHashNode CLIPSFloat;
typedef struct integerHashNode CLIPSInteger;
typedef struct bitMapHashNode CLIPSBitMap;
typedef struct externalAddressHashNode CLIPSExternalAddress;
typedef struct typeHeader TypeHeader;

typedef struct field Field;
typedef struct multifield Multifield;

typedef struct clipsValue CLIPSValue;
typedef struct udfValue UDFValue;

typedef struct fact Fact;
typedef struct instance Instance;

typedef struct expr Expression;
typedef struct functionDefinition FunctionDefinition;
typedef struct udfContext UDFContext;

/**************/
/* typeHeader */
/**************/

struct typeHeader
  {
   unsigned short type;
  };

/****************/
/* voidHashNode */
/****************/
struct voidHashNode
  {
   TypeHeader th;
  };

/******************/
/* symbolHashNode */
/******************/
struct symbolHashNode
  {
   TypeHeader th;
   struct symbolHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededSymbol : 1;
   unsigned int bucket : 29;
   const char *contents;
  };

/*****************/
/* floatHashNode */
/*****************/
struct floatHashNode
  {
   TypeHeader th;
   struct floatHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededFloat : 1;
   unsigned int bucket : 29;
   double contents;
  };

/******************************/
/* integerHashNode STRUCTURE: */
/******************************/
struct integerHashNode
  {
   TypeHeader th;
   struct integerHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededInteger : 1;
   unsigned int bucket : 29;
   long long contents;
  };

/******************/
/* bitMapHashNode */
/******************/
struct bitMapHashNode
  {
   TypeHeader th;
   struct bitMapHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededBitMap : 1;
   unsigned int bucket : 29;
   const char *contents;
   unsigned short size;
  };

/***************************/
/* externalAddressHashNode */
/***************************/
struct externalAddressHashNode
  {
   TypeHeader th;
   struct externalAddressHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededPointer : 1;
   unsigned int bucket : 29;
   void *contents;
   unsigned short type;
  };

/*********/
/* field */
/*********/
struct field
  {
   union
     {
      void *value;
      TypeHeader *header;
      CLIPSLexeme *lexemeValue;
      CLIPSFloat *floatValue;
      CLIPSInteger *integerValue;
      CLIPSVoid *voidValue;
      Fact *factValue;
      Instance *instanceValue;
      Multifield *multifieldValue;
      CLIPSExternalAddress *externalAddressValue;
     };
  };

/**************/
/* multifield */
/**************/
struct multifield
  {
   TypeHeader th;
   unsigned busyCount;
   long length;
   struct multifield *next;
   struct field theFields[1];
  };

/**************/
/* clipsValue */
/**************/
struct clipsValue
  {
   union
     {
      void *value;
      TypeHeader const *header;
      Fact *factValue;
      Instance *instanceValue;
      CLIPSLexeme *lexemeValue;
      CLIPSFloat *floatValue;
      CLIPSInteger *integerValue;
      CLIPSVoid *voidValue;
      Multifield *multifieldValue;
      CLIPSExternalAddress *externalAddressValue;
     };
  };

/************/
/* udfValue */
/************/
struct udfValue
  {
   void *supplementalInfo;
   union
     {
      void *value;
      TypeHeader const *header;
      Fact *factValue;
      Instance *instanceValue;
      CLIPSLexeme *lexemeValue;
      CLIPSFloat *floatValue;
      CLIPSInteger *integerValue;
      CLIPSVoid *voidValue;
      Multifield *multifieldValue;
      CLIPSExternalAddress *externalAddressValue;
     };
   long begin;
   long range;
   struct udfValue *next;
  };

/**************/
/* udfContext */
/**************/
struct udfContext
  {
   Environment *environment;
   FunctionDefinition *theFunction;
   int lastPosition;
   Expression *lastArg;
   UDFValue *returnValue;
  };

#endif /* _H_entities */


