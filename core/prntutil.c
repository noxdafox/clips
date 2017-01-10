   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/01/16             */
   /*                                                     */
   /*                PRINT UTILITY MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for printing various items      */
/*   and messages.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Link error occurs for the SlotExistError       */
/*            function when OBJECT_SYSTEM is set to 0 in     */
/*            setup.h. DR0865                                */
/*                                                           */
/*            Added DataObjectToString function.             */
/*                                                           */
/*            Added SlotExistError function.                 */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Support for DATA_OBJECT_ARRAY primitive.       */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS_TYPE.       */
/*                                                           */
/*            Used gensprintf and genstrcat instead of       */
/*            sprintf and strcat.                            */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added code for capturing errors/warnings.      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*            File name/line count displayed for errors      */
/*            and warnings during load command.              */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <string.h>

#include "setup.h"

#include "argacces.h"
#include "constant.h"
#include "cstrcpsr.h"
#include "envrnmnt.h"
#include "evaluatn.h"
#include "factmngr.h"
#include "inscom.h"
#include "insmngr.h"
#include "memalloc.h"
#include "multifun.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "sysdep.h"
#include "utility.h"

#include "prntutil.h"

/*****************************************************/
/* InitializePrintUtilityData: Allocates environment */
/*    data for print utility routines.               */
/*****************************************************/
void InitializePrintUtilityData(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,PRINT_UTILITY_DATA,sizeof(struct printUtilityData),NULL);
  }

/***********************************************************/
/* PrintInChunks:  Prints a string in chunks to accomodate */
/*   systems which have a limit on the maximum size of a   */
/*   string which can be printed.                          */
/***********************************************************/
void PrintInChunks(
  Environment *theEnv,
  const char *logicalName,
  const char *bigString)
  {
   /*=====================================================*/
   /* This function was originally added because VMS had  */
   /* a bug that didn't allow printing a string greater   */
   /* than 512 bytes. Since this was over 25 years ago,   */
   /* we'll assume no modern compiler has this limitation */
   /* and just print the entire string.                   */
   /*=====================================================*/

   PrintString(theEnv,logicalName,bigString);

/*
   char tc, *subString;

   subString = bigString;

   if (subString == NULL) return;

   while (((int) strlen(subString)) > 500)
     {
      if (EvaluationData(theEnv)->HaltExecution) return;
      tc = subString[500];
      subString[500] = EOS;
      PrintString(theEnv,logicalName,subString);
      subString[500] = tc;
      subString += 500;
     }

   PrintString(theEnv,logicalName,subString);
*/
  }

/************************************************************/
/* PrintFloat: Controls printout of floating point numbers. */
/************************************************************/
void PrintFloat(
  Environment *theEnv,
  const char *fileid,
  double number)
  {
   const char *theString;

   theString = FloatToString(theEnv,number);
   PrintString(theEnv,fileid,theString);
  }

/************************************************/
/* PrintInteger: Controls printout of integers. */
/************************************************/
void PrintInteger(
  Environment *theEnv,
  const char *logicalName,
  long long number)
  {
   char printBuffer[32];

   gensprintf(printBuffer,"%lld",number);
   PrintString(theEnv,logicalName,printBuffer);
  }

/**************************************/
/* PrintAtom: Prints an atomic value. */
/**************************************/
void PrintAtom(
  Environment *theEnv,
  const char *logicalName,
  int type,
  void *value)
  {
   CLIPSExternalAddress *theAddress;
   char buffer[20];

   switch (type)
     {
      case FLOAT_TYPE:
        PrintFloat(theEnv,logicalName,((CLIPSFloat *) value)->contents);
        break;
      case INTEGER_TYPE:
        PrintInteger(theEnv,logicalName,((CLIPSInteger *) value)->contents);
        break;
      case SYMBOL_TYPE:
        PrintString(theEnv,logicalName,((CLIPSLexeme *) value)->contents);
        break;
      case STRING_TYPE:
        if (PrintUtilityData(theEnv)->PreserveEscapedCharacters)
          { PrintString(theEnv,logicalName,StringPrintForm(theEnv,((CLIPSLexeme *) value)->contents)); }
        else
          {
           PrintString(theEnv,logicalName,"\"");
           PrintString(theEnv,logicalName,((CLIPSLexeme *) value)->contents);
           PrintString(theEnv,logicalName,"\"");
          }
        break;

      case EXTERNAL_ADDRESS_TYPE:
        theAddress = (CLIPSExternalAddress *) value;

        if (PrintUtilityData(theEnv)->AddressesToStrings) PrintString(theEnv,logicalName,"\"");

        if ((EvaluationData(theEnv)->ExternalAddressTypes[theAddress->type] != NULL) &&
            (EvaluationData(theEnv)->ExternalAddressTypes[theAddress->type]->longPrintFunction != NULL))
          { (*EvaluationData(theEnv)->ExternalAddressTypes[theAddress->type]->longPrintFunction)(theEnv,logicalName,value); }
        else
          {
           PrintString(theEnv,logicalName,"<Pointer-");

           gensprintf(buffer,"%d-",theAddress->type);
           PrintString(theEnv,logicalName,buffer);

           gensprintf(buffer,"%p",((CLIPSExternalAddress *) value)->contents);
           PrintString(theEnv,logicalName,buffer);
           PrintString(theEnv,logicalName,">");
          }

        if (PrintUtilityData(theEnv)->AddressesToStrings) PrintString(theEnv,logicalName,"\"");
        break;

#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
        PrintString(theEnv,logicalName,"[");
        PrintString(theEnv,logicalName,((CLIPSLexeme *) value)->contents);
        PrintString(theEnv,logicalName,"]");
        break;
#endif

      case VOID_TYPE:
        break;

      default:
        if (EvaluationData(theEnv)->PrimitivesArray[type] == NULL) break;
        if (EvaluationData(theEnv)->PrimitivesArray[type]->longPrintFunction == NULL)
          {
           PrintString(theEnv,logicalName,"<unknown atom type>");
           break;
          }
        (*EvaluationData(theEnv)->PrimitivesArray[type]->longPrintFunction)(theEnv,logicalName,value);
        break;
     }
  }

/**********************************************************/
/* PrintTally: Prints a tally count indicating the number */
/*   of items that have been displayed. Used by functions */
/*   such as list-defrules.                               */
/**********************************************************/
void PrintTally(
  Environment *theEnv,
  const char *logicalName,
  long long count,
  const char *singular,
  const char *plural)
  {
   if (count == 0) return;

   PrintString(theEnv,logicalName,"For a total of ");
   PrintInteger(theEnv,logicalName,count);
   PrintString(theEnv,logicalName," ");

   if (count == 1) PrintString(theEnv,logicalName,singular);
   else PrintString(theEnv,logicalName,plural);

   PrintString(theEnv,logicalName,".\n");
  }

/********************************************/
/* PrintErrorID: Prints the module name and */
/*   error ID for an error message.         */
/********************************************/
void PrintErrorID(
  Environment *theEnv,
  const char *module,
  int errorID,
  bool printCR)
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   FlushParsingMessages(theEnv);
   SetErrorFileName(theEnv,GetParsingFileName(theEnv));
   ConstructData(theEnv)->ErrLineNumber = GetLineCount(theEnv);
#endif
   if (printCR) PrintString(theEnv,WERROR,"\n");
   PrintString(theEnv,WERROR,"[");
   PrintString(theEnv,WERROR,module);
   PrintInteger(theEnv,WERROR,errorID);
   PrintString(theEnv,WERROR,"] ");

   /*==================================================*/
   /* Print the file name and line number if available */
   /* and there is no callback for errors/warnings.    */
   /*==================================================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   if ((ConstructData(theEnv)->ParserErrorCallback == NULL) &&
       (GetLoadInProgress(theEnv) == true))
     {
      const char *fileName;

      fileName = GetParsingFileName(theEnv);
      if (fileName != NULL)
        {
         PrintString(theEnv,WERROR,fileName);
         PrintString(theEnv,WERROR,", Line ");
         PrintInteger(theEnv,WERROR,GetLineCount(theEnv));
         PrintString(theEnv,WERROR,": ");
        }
     }
#endif
  }

/**********************************************/
/* PrintWarningID: Prints the module name and */
/*   warning ID for a warning message.        */
/**********************************************/
void PrintWarningID(
  Environment *theEnv,
  const char *module,
  int warningID,
  bool printCR)
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   FlushParsingMessages(theEnv);
   SetWarningFileName(theEnv,GetParsingFileName(theEnv));
   ConstructData(theEnv)->WrnLineNumber = GetLineCount(theEnv);
#endif
   if (printCR) PrintString(theEnv,WWARNING,"\n");
   PrintString(theEnv,WWARNING,"[");
   PrintString(theEnv,WWARNING,module);
   PrintInteger(theEnv,WWARNING,warningID);
   PrintString(theEnv,WWARNING,"] ");

   /*==================================================*/
   /* Print the file name and line number if available */
   /* and there is no callback for errors/warnings.    */
   /*==================================================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   if ((ConstructData(theEnv)->ParserErrorCallback == NULL) &&
       (GetLoadInProgress(theEnv) == true))
     {
      const char *fileName;

      fileName = GetParsingFileName(theEnv);
      if (fileName != NULL)
        {
         PrintString(theEnv,WERROR,fileName);
         PrintString(theEnv,WERROR,", Line ");
         PrintInteger(theEnv,WERROR,GetLineCount(theEnv));
         PrintString(theEnv,WERROR,", ");
        }
     }
#endif

   PrintString(theEnv,WWARNING,"WARNING: ");
  }

/***************************************************/
/* CantFindItemErrorMessage: Generic error message */
/*  when an "item" can not be found.               */
/***************************************************/
void CantFindItemErrorMessage(
  Environment *theEnv,
  const char *itemType,
  const char *itemName)
  {
   PrintErrorID(theEnv,"PRNTUTIL",1,false);
   PrintString(theEnv,WERROR,"Unable to find ");
   PrintString(theEnv,WERROR,itemType);
   PrintString(theEnv,WERROR," ");
   PrintString(theEnv,WERROR,itemName);
   PrintString(theEnv,WERROR,".\n");
  }

/*****************************************************/
/* CantFindItemInFunctionErrorMessage: Generic error */
/*  message when an "item" can not be found.         */
/*****************************************************/
void CantFindItemInFunctionErrorMessage(
  Environment *theEnv,
  const char *itemType,
  const char *itemName,
  const char *func)
  {
   PrintErrorID(theEnv,"PRNTUTIL",1,false);
   PrintString(theEnv,WERROR,"Unable to find ");
   PrintString(theEnv,WERROR,itemType);
   PrintString(theEnv,WERROR," ");
   PrintString(theEnv,WERROR,itemName);
   PrintString(theEnv,WERROR," in function ");
   PrintString(theEnv,WERROR,func);
   PrintString(theEnv,WERROR,".\n");
  }

/*****************************************************/
/* CantDeleteItemErrorMessage: Generic error message */
/*  when an "item" can not be deleted.               */
/*****************************************************/
void CantDeleteItemErrorMessage(
  Environment *theEnv,
  const char *itemType,
  const char *itemName)
  {
   PrintErrorID(theEnv,"PRNTUTIL",4,false);
   PrintString(theEnv,WERROR,"Unable to delete ");
   PrintString(theEnv,WERROR,itemType);
   PrintString(theEnv,WERROR," ");
   PrintString(theEnv,WERROR,itemName);
   PrintString(theEnv,WERROR,".\n");
  }

/****************************************************/
/* AlreadyParsedErrorMessage: Generic error message */
/*  when an "item" has already been parsed.         */
/****************************************************/
void AlreadyParsedErrorMessage(
  Environment *theEnv,
  const char *itemType,
  const char *itemName)
  {
   PrintErrorID(theEnv,"PRNTUTIL",5,true);
   PrintString(theEnv,WERROR,"The ");
   if (itemType != NULL) PrintString(theEnv,WERROR,itemType);
   if (itemName != NULL) PrintString(theEnv,WERROR,itemName);
   PrintString(theEnv,WERROR," has already been parsed.\n");
  }

/*********************************************************/
/* SyntaxErrorMessage: Generalized syntax error message. */
/*********************************************************/
void SyntaxErrorMessage(
  Environment *theEnv,
  const char *location)
  {
   PrintErrorID(theEnv,"PRNTUTIL",2,true);
   PrintString(theEnv,WERROR,"Syntax Error");
   if (location != NULL)
     {
      PrintString(theEnv,WERROR,":  Check appropriate syntax for ");
      PrintString(theEnv,WERROR,location);
     }

   PrintString(theEnv,WERROR,".\n");
   SetEvaluationError(theEnv,true);
  }

/****************************************************/
/* LocalVariableErrorMessage: Generic error message */
/*  when a local variable is accessed by an "item"  */
/*  which can not access local variables.           */
/****************************************************/
void LocalVariableErrorMessage(
  Environment *theEnv,
  const char *byWhat)
  {
   PrintErrorID(theEnv,"PRNTUTIL",6,true);
   PrintString(theEnv,WERROR,"Local variables can not be accessed by ");
   PrintString(theEnv,WERROR,byWhat);
   PrintString(theEnv,WERROR,".\n");
  }

/******************************************/
/* SystemError: Generalized error message */
/*   for major internal errors.           */
/******************************************/
void SystemError(
  Environment *theEnv,
  const char *module,
  int errorID)
  {
   PrintErrorID(theEnv,"PRNTUTIL",3,true);

   PrintString(theEnv,WERROR,"\n*** ");
   PrintString(theEnv,WERROR,APPLICATION_NAME);
   PrintString(theEnv,WERROR," SYSTEM ERROR ***\n");

   PrintString(theEnv,WERROR,"ID = ");
   PrintString(theEnv,WERROR,module);
   PrintInteger(theEnv,WERROR,errorID);
   PrintString(theEnv,WERROR,"\n");

   PrintString(theEnv,WERROR,APPLICATION_NAME);
   PrintString(theEnv,WERROR," data structures are in an inconsistent or corrupted state.\n");
   PrintString(theEnv,WERROR,"This error may have occurred from errors in user defined code.\n");
   PrintString(theEnv,WERROR,"**************************\n");
  }

/*******************************************************/
/* DivideByZeroErrorMessage: Generalized error message */
/*   for when a function attempts to divide by zero.   */
/*******************************************************/
void DivideByZeroErrorMessage(
  Environment *theEnv,
  const char *functionName)
  {
   PrintErrorID(theEnv,"PRNTUTIL",7,false);
   PrintString(theEnv,WERROR,"Attempt to divide by zero in ");
   PrintString(theEnv,WERROR,functionName);
   PrintString(theEnv,WERROR," function.\n");
  }

/*******************************************************/
/* FloatToString: Converts number to KB string format. */
/*******************************************************/
const char *FloatToString(
  Environment *theEnv,
  double number)
  {
   char floatString[40];
   int i;
   char x;
   CLIPSLexeme *thePtr;

   gensprintf(floatString,"%.15g",number);

   for (i = 0; (x = floatString[i]) != '\0'; i++)
     {
      if ((x == '.') || (x == 'e'))
        {
         thePtr = CreateString(theEnv,floatString);
         return thePtr->contents;
        }
     }

   genstrcat(floatString,".0");

   thePtr = CreateString(theEnv,floatString);
   return thePtr->contents;
  }

/*******************************************************************/
/* LongIntegerToString: Converts long integer to KB string format. */
/*******************************************************************/
const char *LongIntegerToString(
  Environment *theEnv,
  long long number)
  {
   char buffer[50];
   CLIPSLexeme *thePtr;

   gensprintf(buffer,"%lld",number);

   thePtr = CreateString(theEnv,buffer);
   return thePtr->contents;
  }

/******************************************************************/
/* DataObjectToString: Converts a UDFValue to KB string format. */
/******************************************************************/
const char *DataObjectToString(
  Environment *theEnv,
  UDFValue *theDO)
  {
   CLIPSLexeme *thePtr;
   const char *theString;
   char *newString;
   const char *prefix, *postfix;
   size_t length;
   CLIPSExternalAddress *theAddress;
   char buffer[30];

   switch (theDO->header->type)
     {
      case MULTIFIELD_TYPE:
         prefix = "(";
         theString = ImplodeMultifield(theEnv,theDO)->contents;
         postfix = ")";
         break;

      case STRING_TYPE:
         prefix = "\"";
         theString = theDO->lexemeValue->contents;
         postfix = "\"";
         break;

      case INSTANCE_NAME_TYPE:
         prefix = "[";
         theString = theDO->lexemeValue->contents;
         postfix = "]";
         break;

      case SYMBOL_TYPE:
         return theDO->lexemeValue->contents;

      case FLOAT_TYPE:
         return(FloatToString(theEnv,theDO->floatValue->contents));

      case INTEGER_TYPE:
         return(LongIntegerToString(theEnv,theDO->integerValue->contents));

      case VOID_TYPE:
         return("");

#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS_TYPE:
         if (theDO->instanceValue == &InstanceData(theEnv)->DummyInstance)
           { return("<Dummy Instance>"); }

         if (theDO->instanceValue->garbage)
           {
            prefix = "<Stale Instance-";
            theString = theDO->instanceValue->name->contents;
            postfix = ">";
           }
         else
           {
            prefix = "<Instance-";
            theString = GetFullInstanceName(theEnv,theDO->instanceValue)->contents;
            postfix = ">";
           }

        break;
#endif

      case EXTERNAL_ADDRESS_TYPE:
        theAddress = theDO->externalAddressValue;
        /* TBD Need specific routine for creating name string. */
        gensprintf(buffer,"<Pointer-%d-%p>",(int) theAddress->type,theDO->value);
        thePtr = CreateString(theEnv,buffer);
        return thePtr->contents;

#if DEFTEMPLATE_CONSTRUCT
      case FACT_ADDRESS_TYPE:
         if (theDO->factValue == &FactData(theEnv)->DummyFact)
           { return("<Dummy Fact>"); }

         gensprintf(buffer,"<Fact-%lld>",theDO->factValue->factIndex);
         thePtr = CreateString(theEnv,buffer);
         return thePtr->contents;
#endif

      default:
         return("UNK");
     }

   length = strlen(prefix) + strlen(theString) + strlen(postfix) + 1;
   newString = (char *) genalloc(theEnv,length);
   newString[0] = '\0';
   genstrcat(newString,prefix);
   genstrcat(newString,theString);
   genstrcat(newString,postfix);
   thePtr = CreateString(theEnv,newString);
   genfree(theEnv,newString,length);
   return thePtr->contents;
  }

/************************************************************/
/* SalienceInformationError: Error message for errors which */
/*   occur during the evaluation of a salience value.       */
/************************************************************/
void SalienceInformationError(
  Environment *theEnv,
  const char *constructType,
  const char *constructName)
  {
   PrintErrorID(theEnv,"PRNTUTIL",8,true);
   PrintString(theEnv,WERROR,"This error occurred while evaluating the salience");
   if (constructName != NULL)
     {
      PrintString(theEnv,WERROR," for ");
      PrintString(theEnv,WERROR,constructType);
      PrintString(theEnv,WERROR," ");
      PrintString(theEnv,WERROR,constructName);
     }
   PrintString(theEnv,WERROR,".\n");
  }

/**********************************************************/
/* SalienceRangeError: Error message that is printed when */
/*   a salience value does not fall between the minimum   */
/*   and maximum salience values.                         */
/**********************************************************/
void SalienceRangeError(
  Environment *theEnv,
  int min,
  int max)
  {
   PrintErrorID(theEnv,"PRNTUTIL",9,true);
   PrintString(theEnv,WERROR,"Salience value out of range ");
   PrintInteger(theEnv,WERROR,min);
   PrintString(theEnv,WERROR," to ");
   PrintInteger(theEnv,WERROR,max);
   PrintString(theEnv,WERROR,".\n");
  }

/***************************************************************/
/* SalienceNonIntegerError: Error message that is printed when */
/*   a rule's salience does not evaluate to an integer.        */
/***************************************************************/
void SalienceNonIntegerError(
  Environment *theEnv)
  {
   PrintErrorID(theEnv,"PRNTUTIL",10,true);
   PrintString(theEnv,WERROR,"Salience value must be an integer value.\n");
  }

/***************************************************/
/* SlotExistError: Prints out an appropriate error */
/*   message when a slot cannot be found for a     */
/*   function. Input to the function is the slot   */
/*   name and the function name.                   */
/***************************************************/
void SlotExistError(
  Environment *theEnv,
  const char *sname,
  const char *func)
  {
   PrintErrorID(theEnv,"INSFUN",3,false);
   PrintString(theEnv,WERROR,"No such slot ");
   PrintString(theEnv,WERROR,sname);
   PrintString(theEnv,WERROR," in function ");
   PrintString(theEnv,WERROR,func);
   PrintString(theEnv,WERROR,".\n");
   SetEvaluationError(theEnv,true);
  }
