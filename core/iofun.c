   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*                I/O FUNCTIONS MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several I/O functions      */
/*   including printout, read, open, close, remove, rename,  */
/*   format, and readline.                                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*      Gary D. Riley                                        */
/*      Bebe Ly                                              */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added the get-char, set-locale, and            */
/*            read-number functions.                         */
/*                                                           */
/*            Modified printing of floats in the format      */
/*            function to use the locale from the set-locale */
/*            function.                                      */
/*                                                           */
/*            Moved IllegalLogicalNameMessage function to    */
/*            argacces.c.                                    */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Removed the undocumented use of t in the       */
/*            printout command to perform the same function  */
/*            as crlf.                                       */
/*                                                           */
/*            Replaced EXT_IO and BASIC_IO compiler flags    */
/*            with IO_FUNCTIONS compiler flag.               */
/*                                                           */
/*            Added rb and ab and removed r+ modes for the   */
/*            open function.                                 */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Added put-char function.                       */
/*                                                           */
/*            Added SetFullCRLF which allows option to       */
/*            specify crlf as \n or \r\n.                    */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */             
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Added STDOUT and STDIN logical name            */
/*            definitions.                                   */
/*                                                           */
/*      6.40: Modified ReadTokenFromStdin to capture         */
/*            carriage returns in the input buffer so that   */
/*            input buffer count will accurately reflect     */
/*            the number of characters typed for GUI         */
/*            interfaces that support deleting carriage      */
/*            returns.                                       */
/*                                                           */
/*            Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
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
/*************************************************************/

#include "setup.h"

#if IO_FUNCTIONS
#include <locale.h>
#include <stdlib.h>
#include <ctype.h>
#endif

#include <stdio.h>
#include <string.h>

#include "argacces.h"
#include "commline.h"
#include "constant.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "filertr.h"
#include "memalloc.h"
#include "router.h"
#include "scanner.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "utility.h"

#include "iofun.h"

/***************/
/* DEFINITIONS */
/***************/

#define FORMAT_MAX 512
#define FLAG_MAX    80

/********************/
/* ENVIRONMENT DATA */
/********************/

#define IO_FUNCTION_DATA 64

struct IOFunctionData
  { 
   void *locale;
   bool useFullCRLF;
  };

#define IOFunctionData(theEnv) ((struct IOFunctionData *) GetEnvironmentData(theEnv,IO_FUNCTION_DATA))

/****************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS  */
/****************************************/

#if IO_FUNCTIONS
   static void             ReadTokenFromStdin(Environment *,struct token *);
   static const char      *ControlStringCheck(Environment *,int);
   static char             FindFormatFlag(const char *,size_t *,char *,size_t);
   static const char      *PrintFormatFlag(Environment *,const char *,int,int);
   static char            *FillBuffer(Environment *,const char *,size_t *,size_t *);
   static void             ReadNumber(Environment *,const char *,struct token *,bool);
#endif

/**************************************/
/* IOFunctionDefinitions: Initializes */
/*   the I/O functions.               */
/**************************************/
void IOFunctionDefinitions(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,IO_FUNCTION_DATA,sizeof(struct IOFunctionData),NULL);

#if IO_FUNCTIONS
   IOFunctionData(theEnv)->useFullCRLF = false;
   IOFunctionData(theEnv)->locale = (SYMBOL_HN *) EnvAddSymbol(theEnv,setlocale(LC_ALL,NULL));
   IncrementSymbolCount(IOFunctionData(theEnv)->locale);
#endif

#if ! RUN_TIME
#if IO_FUNCTIONS
   EnvAddUDF(theEnv,"printout","v",1,UNBOUNDED,NULL,PrintoutFunction,"PrintoutFunction",NULL);
   EnvAddUDF(theEnv,"read","synldfie",0,1,NULL,ReadFunction,"ReadFunction",NULL);
   EnvAddUDF(theEnv,"open","b",2,3,"*;sy",OpenFunction,"OpenFunction",NULL);
   EnvAddUDF(theEnv,"close","b",0,1,NULL,CloseFunction,"CloseFunction",NULL);
   EnvAddUDF(theEnv,"get-char","l",0,1,NULL,GetCharFunction,"GetCharFunction",NULL);
   EnvAddUDF(theEnv,"put-char","v",1,2,NULL,PutCharFunction,"PutCharFunction",NULL);
   EnvAddUDF(theEnv,"remove","b",1,1,"sy",RemoveFunction,"RemoveFunction",NULL);
   EnvAddUDF(theEnv,"rename","b",2,2,"sy",RenameFunction,"RenameFunction",NULL);
   EnvAddUDF(theEnv,"format","s",2,UNBOUNDED,"*;*;s",FormatFunction,"FormatFunction",NULL);
   EnvAddUDF(theEnv,"readline","sy",0,1,NULL,ReadlineFunction,"ReadlineFunction",NULL);
   EnvAddUDF(theEnv,"set-locale","sy",0,1,NULL,SetLocaleFunction,"SetLocaleFunction",NULL);
   EnvAddUDF(theEnv,"read-number","syld",0,1,NULL,ReadNumberFunction,"ReadNumberFunction",NULL);
#endif
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

#if IO_FUNCTIONS

/******************************************/
/* PrintoutFunction: H/L access routine   */
/*   for the printout function.           */
/******************************************/
void PrintoutFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   const char *dummyid;
   int i, argCount;
   CLIPSValue theArgument;
   
   /*=======================================================*/
   /* The printout function requires at least one argument. */
   /*=======================================================*/

   if ((argCount = EnvArgCountCheck(theEnv,"printout",AT_LEAST,1)) == -1) return;

   /*=====================================================*/
   /* Get the logical name to which output is to be sent. */
   /*=====================================================*/

   dummyid = GetLogicalName(theEnv,1,STDOUT);
   if (dummyid == NULL)
     {
      IllegalLogicalNameMessage(theEnv,"printout");
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   /*============================================================*/
   /* Determine if any router recognizes the output destination. */
   /*============================================================*/

   if (strcmp(dummyid,"nil") == 0)
     { return; }
   else if (QueryRouters(theEnv,dummyid) == false)
     {
      UnrecognizedRouterMessage(theEnv,dummyid);
      return;
     }

   /*===============================================*/
   /* Print each of the arguments sent to printout. */
   /*===============================================*/

   for (i = 2; i <= argCount; i++)
     {
      EnvRtnUnknown(theEnv,i,&theArgument);
      if (EvaluationData(theEnv)->HaltExecution) break;

      switch(GetType(theArgument))
        {
         case SYMBOL:
           if (strcmp(DOToString(theArgument),"crlf") == 0)
             {    
              if (IOFunctionData(theEnv)->useFullCRLF)
                { EnvPrintRouter(theEnv,dummyid,"\r\n"); }
              else
                { EnvPrintRouter(theEnv,dummyid,"\n"); }
             }
           else if (strcmp(DOToString(theArgument),"tab") == 0)
             { EnvPrintRouter(theEnv,dummyid,"\t"); }
           else if (strcmp(DOToString(theArgument),"vtab") == 0)
             { EnvPrintRouter(theEnv,dummyid,"\v"); }
           else if (strcmp(DOToString(theArgument),"ff") == 0)
             { EnvPrintRouter(theEnv,dummyid,"\f"); }
             /*
           else if (strcmp(DOToString(theArgument),"t") == 0)
             { 
              if (IOFunctionData(theEnv)->useFullCRLF)
                { EnvPrintRouter(theEnv,dummyid,"\r\n"); }
              else
                { EnvPrintRouter(theEnv,dummyid,"\n"); }
             }
             */
           else
             { EnvPrintRouter(theEnv,dummyid,DOToString(theArgument)); }
           break;

         case STRING:
           EnvPrintRouter(theEnv,dummyid,DOToString(theArgument));
           break;

         default:
           PrintDataObject(theEnv,dummyid,&theArgument);
           break;
        }
     }
  }

/*****************************************************/
/* SetFullCRLF: Set the flag which indicates whether */
/*   crlf is treated just as '\n' or '\r\n'.         */
/*****************************************************/
bool SetFullCRLF(
  Environment *theEnv,
  bool value)
  {
   bool oldValue = IOFunctionData(theEnv)->useFullCRLF;
   
   IOFunctionData(theEnv)->useFullCRLF = value;
   
   return(oldValue);
  }

/*************************************************************/
/* ReadFunction: H/L access routine for the read function.   */
/*************************************************************/
void ReadFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   struct token theToken;
   int numberOfArguments;
   const char *logicalName = NULL;

   /*===============================================*/
   /* Check for an appropriate number of arguments. */
   /*===============================================*/

   if ((numberOfArguments = EnvArgCountCheck(theEnv,"read",NO_MORE_THAN,1)) == -1)
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   /*======================================================*/
   /* Determine the logical name from which input is read. */
   /*======================================================*/

   if (numberOfArguments == 0)
     { logicalName = STDIN; }
   else if (numberOfArguments == 1)
     {
      logicalName = GetLogicalName(theEnv,1,STDIN);
      if (logicalName == NULL)
        {
         IllegalLogicalNameMessage(theEnv,"read");
         EnvSetHaltExecution(theEnv,true);
         EnvSetEvaluationError(theEnv,true);
         returnValue->type = STRING;
         returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
         return;
        }
     }

   /*============================================*/
   /* Check to see that the logical name exists. */
   /*============================================*/

   if (QueryRouters(theEnv,logicalName) == false)
     {
      UnrecognizedRouterMessage(theEnv,logicalName);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   /*=======================================*/
   /* Collect input into string if the read */
   /* source is stdin, else just get token. */
   /*=======================================*/

   if (strcmp(logicalName,STDIN) == 0)
     { ReadTokenFromStdin(theEnv,&theToken); }
   else
     { GetToken(theEnv,logicalName,&theToken); }

   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = false;

   /*====================================================*/
   /* Copy the token to the return value data structure. */
   /*====================================================*/

   returnValue->type = theToken.type;
   if ((theToken.type == FLOAT) || (theToken.type == STRING) ||
#if OBJECT_SYSTEM
       (theToken.type == INSTANCE_NAME) ||
#endif
       (theToken.type == SYMBOL) || (theToken.type == INTEGER))
     { returnValue->value = theToken.value; }
   else if (theToken.type == STOP)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvAddSymbol(theEnv,"EOF");
     }
   else if (theToken.type == UNKNOWN_VALUE)
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
     }
   else
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,theToken.printForm);
     }

   return;
  }

/********************************************************/
/* ReadTokenFromStdin: Special routine used by the read */
/*   function to read a token from standard input.      */
/********************************************************/
static void ReadTokenFromStdin(
  Environment *theEnv,
  struct token *theToken)
  {
   char *inputString;
   size_t inputStringSize;
   int inchar;
   
   /*===========================================*/
   /* Initialize the variables used for storing */
   /* the characters retrieved from stdin.      */
   /*===========================================*/

   inputString = NULL;
   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = true;
   inputStringSize = 0;

   /*=============================================*/
   /* Continue processing until a token is found. */
   /*=============================================*/

   theToken->type = STOP;
   while (theToken->type == STOP)
     {
      /*========================================================*/
      /* Continue reading characters until a carriage return is */
      /* entered or the user halts execution (usually with      */
      /* control-c). Waiting for the carriage return prevents   */
      /* the input from being prematurely parsed (such as when  */
      /* a space is entered after a symbol has been typed).     */
      /*========================================================*/

      inchar = EnvGetcRouter(theEnv,STDIN);
     
      while ((inchar != '\n') && (inchar != '\r') && (inchar != EOF) &&
             (! EnvGetHaltExecution(theEnv)))
        {
         inputString = ExpandStringWithChar(theEnv,inchar,inputString,&RouterData(theEnv)->CommandBufferInputCount,
                                            &inputStringSize,inputStringSize + 80);
         inchar = EnvGetcRouter(theEnv,STDIN);
        }

      /*====================================================*/
      /* Add the final carriage return to the input buffer. */
      /*====================================================*/
      
      if  ((inchar == '\n') || (inchar == '\r'))
        {
         inputString = ExpandStringWithChar(theEnv,inchar,inputString,&RouterData(theEnv)->CommandBufferInputCount,
                                            &inputStringSize,inputStringSize + 80);
        }
        
      /*==================================================*/
      /* Open a string input source using the characters  */
      /* retrieved from stdin and extract the first token */
      /* contained in the string.                         */
      /*==================================================*/

      OpenStringSource(theEnv,"read",inputString,0);
      GetToken(theEnv,"read",theToken);
      CloseStringSource(theEnv,"read");

      /*===========================================*/
      /* Pressing control-c (or comparable action) */
      /* aborts the read function.                 */
      /*===========================================*/

      if (EnvGetHaltExecution(theEnv))
        {
         theToken->type = STRING;
         theToken->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
        }

      /*====================================================*/
      /* Return the EOF symbol if the end of file for stdin */
      /* has been encountered. This typically won't occur,  */
      /* but is possible (for example by pressing control-d */
      /* in the UNIX operating system).                     */
      /*====================================================*/

      if ((theToken->type == STOP) && (inchar == EOF))
        {
         theToken->type = SYMBOL;
         theToken->value = EnvAddSymbol(theEnv,"EOF");
        }
     }
     
   if (inputStringSize > 0) rm(theEnv,inputString,inputStringSize);
  }

/*************************************************************/
/* OpenFunction: H/L access routine for the open function.   */
/*************************************************************/
void OpenFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   int numberOfArguments;
   const char *fileName, *logicalName, *accessMode = NULL;
   CLIPSValue theArgument;

   returnValue->type = SYMBOL;
   
   /*========================================*/
   /* Check for a valid number of arguments. */
   /*========================================*/

   if ((numberOfArguments = EnvArgRangeCheck(theEnv,"open",2,3)) == -1)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*====================*/
   /* Get the file name. */
   /*====================*/

   if ((fileName = GetFileName(theEnv,"open",1)) == NULL)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*=======================================*/
   /* Get the logical name to be associated */
   /* with the opened file.                 */
   /*=======================================*/

   logicalName = GetLogicalName(theEnv,2,NULL);
   if (logicalName == NULL)
     {
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      IllegalLogicalNameMessage(theEnv,"open");
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*==================================*/
   /* Check to see if the logical name */
   /* is already in use.               */
   /*==================================*/

   if (FindFile(theEnv,logicalName))
     {
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      PrintErrorID(theEnv,"IOFUN",2,false);
      EnvPrintRouter(theEnv,WERROR,"Logical name ");
      EnvPrintRouter(theEnv,WERROR,logicalName);
      EnvPrintRouter(theEnv,WERROR," already in use.\n");
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*===========================*/
   /* Get the file access mode. */
   /*===========================*/

   if (numberOfArguments == 2)
     { accessMode = "r"; }
   else if (numberOfArguments == 3)
     {
      if (EnvArgTypeCheck(theEnv,"open",3,STRING,&theArgument) == false)
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
      accessMode = DOToString(theArgument);
     }

   /*=====================================*/
   /* Check for a valid file access mode. */
   /*=====================================*/

   if ((strcmp(accessMode,"r") != 0) &&
       (strcmp(accessMode,"w") != 0) &&
       (strcmp(accessMode,"a") != 0) &&
       (strcmp(accessMode,"rb") != 0) &&
       (strcmp(accessMode,"wb") != 0) &&
       (strcmp(accessMode,"ab") != 0))
     {
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      ExpectedTypeError1(theEnv,"open",3,"string with value \"r\", \"w\", \"a\", \"rb\", \"wb\", or \"ab\"");
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*================================================*/
   /* Open the named file and associate it with the  */
   /* specified logical name. Return TRUE if the     */
   /* file was opened successfully, otherwise FALSE. */
   /*================================================*/

   if (OpenAFile(theEnv,fileName,accessMode,logicalName))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/***************************************************************/
/* CloseFunction: H/L access routine for the close function.   */
/***************************************************************/
void CloseFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   int numberOfArguments;
   const char *logicalName;

   returnValue->type = SYMBOL;
   
   /*======================================*/
   /* Check for valid number of arguments. */
   /*======================================*/

   if ((numberOfArguments = EnvArgCountCheck(theEnv,"close",NO_MORE_THAN,1)) == -1)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*=====================================================*/
   /* If no arguments are specified, then close all files */
   /* opened with the open command. Return true if all    */
   /* files were closed successfully, otherwise false.    */
   /*=====================================================*/

   if (numberOfArguments == 0)
     {
      if (CloseAllFiles(theEnv))
        { returnValue->value = EnvTrueSymbol(theEnv); }
      else
        { returnValue->value = EnvFalseSymbol(theEnv); }
      return;
     }

   /*================================*/
   /* Get the logical name argument. */
   /*================================*/

   logicalName = GetLogicalName(theEnv,1,NULL);
   if (logicalName == NULL)
     {
      IllegalLogicalNameMessage(theEnv,"close");
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*========================================================*/
   /* Close the file associated with the specified logical   */
   /* name. Return true if the file was closed successfully, */
   /* otherwise false.                                       */
   /*========================================================*/

   if (CloseFile(theEnv,logicalName))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/***************************************/
/* GetCharFunction: H/L access routine */
/*   for the get-char function.        */
/***************************************/
void GetCharFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   int numberOfArguments;
   const char *logicalName;

   returnValue->type = INTEGER;
   
   if ((numberOfArguments = EnvArgCountCheck(theEnv,"get-char",NO_MORE_THAN,1)) == -1)
     {
      returnValue->value = EnvAddLong(theEnv,-1);
      return;
     }

   if (numberOfArguments == 0 )
     { logicalName = STDIN; }
   else
     {
      logicalName = GetLogicalName(theEnv,1,STDIN);
      if (logicalName == NULL)
        {
         IllegalLogicalNameMessage(theEnv,"get-char");
         EnvSetHaltExecution(theEnv,true);
         EnvSetEvaluationError(theEnv,true);
         returnValue->value = EnvAddLong(theEnv,-1);
         return;
        }
     }

   if (QueryRouters(theEnv,logicalName) == false)
     {
      UnrecognizedRouterMessage(theEnv,logicalName);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      returnValue->value = EnvAddLong(theEnv,-1);
      return;
     }

   returnValue->value = EnvAddLong(theEnv,EnvGetcRouter(theEnv,logicalName));
  }

/***************************************/
/* PutCharFunction: H/L access routine */
/*   for the put-char function.        */
/***************************************/
void PutCharFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   int numberOfArguments;
   const char *logicalName;
   CLIPSValue theValue;
   long long theChar;
   FILE *theFile;
   
   if ((numberOfArguments = EnvArgRangeCheck(theEnv,"put-char",1,2)) == -1)
     { return; }
     
   /*=======================*/
   /* Get the logical name. */
   /*=======================*/
   
   if (numberOfArguments == 1)
     { logicalName = STDOUT; }
   else
     {
      logicalName = GetLogicalName(theEnv,1,STDOUT);
      if (logicalName == NULL)
        {
         IllegalLogicalNameMessage(theEnv,"put-char");
         EnvSetHaltExecution(theEnv,true);
         EnvSetEvaluationError(theEnv,true);
         return;
        }
     }

   if (QueryRouters(theEnv,logicalName) == false)
     {
      UnrecognizedRouterMessage(theEnv,logicalName);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   /*===========================*/
   /* Get the character to put. */
   /*===========================*/
   
   if (numberOfArguments == 1)
     { if (EnvArgTypeCheck(theEnv,"put-char",1,INTEGER,&theValue) == false) return; }
   else
     { if (EnvArgTypeCheck(theEnv,"put-char",2,INTEGER,&theValue) == false) return; }
     
   theChar = DOToLong(theValue);
   
   /*===================================================*/
   /* If the "fast load" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* we can bypass the router and directly output the  */
   /* value.                                            */
   /*===================================================*/
      
   theFile = FindFptr(theEnv,logicalName);
   if (theFile != NULL)
     { putc((int) theChar,theFile); }
  }

/****************************************/
/* RemoveFunction: H/L access routine   */
/*   for the remove function.           */
/****************************************/
void RemoveFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   const char *theFileName;

   returnValue->type = SYMBOL;
   
   /*====================*/
   /* Get the file name. */
   /*====================*/

   if ((theFileName = GetFileName(theEnv,"remove",1)) == NULL)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*==============================================*/
   /* Remove the file. Return true if the file was */
   /* sucessfully removed, otherwise false.        */
   /*==============================================*/

   if (genremove(theFileName))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/****************************************/
/* RenameFunction: H/L access routine   */
/*   for the rename function.           */
/****************************************/
void RenameFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   const char *oldFileName, *newFileName;

   returnValue->type = SYMBOL;
   
   /*===========================*/
   /* Check for the file names. */
   /*===========================*/

   if ((oldFileName = GetFileName(theEnv,"rename",1)) == NULL)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   if ((newFileName = GetFileName(theEnv,"rename",2)) == NULL)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*==============================================*/
   /* Rename the file. Return true if the file was */
   /* sucessfully renamed, otherwise false.        */
   /*==============================================*/

   if (genrename(oldFileName,newFileName))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/****************************************/
/* FormatFunction: H/L access routine   */
/*   for the format function.           */
/****************************************/
void FormatFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   int argCount;
   size_t start_pos;
   const char *formatString;
   const char *logicalName;
   char formatFlagType;
   int  f_cur_arg = 3;
   size_t form_pos = 0;
   char percentBuffer[FLAG_MAX];
   char *fstr = NULL;
   size_t fmaxm = 0;
   size_t fpos = 0;
   const char *theString;

   
   /*======================================*/
   /* Set default return value for errors. */
   /*======================================*/

   returnValue->type = STRING;
   returnValue->value = EnvAddSymbol(theEnv,"");

   /*=========================================*/
   /* Format requires at least two arguments: */
   /* a logical name and a format string.     */
   /*=========================================*/

   if ((argCount = EnvArgCountCheck(theEnv,"format",AT_LEAST,2)) == -1)
     { return; }

   /*========================================*/
   /* First argument must be a logical name. */
   /*========================================*/

   if ((logicalName = GetLogicalName(theEnv,1,STDOUT)) == NULL)
     {
      IllegalLogicalNameMessage(theEnv,"format");
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   if (strcmp(logicalName,"nil") == 0)
     { /* do nothing */ }
   else if (QueryRouters(theEnv,logicalName) == false)
     {
      UnrecognizedRouterMessage(theEnv,logicalName);
      return;
     }

   /*=====================================================*/
   /* Second argument must be a string.  The appropriate  */
   /* number of arguments specified by the string must be */
   /* present in the argument list.                       */
   /*=====================================================*/

   if ((formatString = ControlStringCheck(theEnv,argCount)) == NULL)
     { return; }

   /*========================================*/
   /* Search the format string, printing the */
   /* format flags as they are encountered.  */
   /*========================================*/

   while (formatString[form_pos] != '\0')
     {
      if (formatString[form_pos] != '%')
        {
         start_pos = form_pos;
         while ((formatString[form_pos] != '%') &&
                (formatString[form_pos] != '\0'))
           { form_pos++; }
         fstr = AppendNToString(theEnv,&formatString[start_pos],fstr,form_pos-start_pos,&fpos,&fmaxm);
        }
      else
        {
		 form_pos++;
         formatFlagType = FindFormatFlag(formatString,&form_pos,percentBuffer,FLAG_MAX);
         if (formatFlagType != ' ')
           {
            if ((theString = PrintFormatFlag(theEnv,percentBuffer,f_cur_arg,formatFlagType)) == NULL)
              {
               if (fstr != NULL) rm(theEnv,fstr,fmaxm);
               return;
              }
            fstr = AppendToString(theEnv,theString,fstr,&fpos,&fmaxm);
            if (fstr == NULL) return;
            f_cur_arg++;
           }
         else
           {
            fstr = AppendToString(theEnv,percentBuffer,fstr,&fpos,&fmaxm);
            if (fstr == NULL) return;
           }
        }
     }

   if (fstr != NULL)
     {
      returnValue->value = EnvAddSymbol(theEnv,fstr);
      if (strcmp(logicalName,"nil") != 0) EnvPrintRouter(theEnv,logicalName,fstr);
      rm(theEnv,fstr,fmaxm);
     }
  }

/*********************************************************************/
/* ControlStringCheck:  Checks the 2nd parameter which is the format */
/*   control string to see if there are enough matching arguments.   */
/*********************************************************************/
static const char *ControlStringCheck(
  Environment *theEnv,
  int argCount)
  {
   CLIPSValue t_ptr;
   const char *str_array;
   char print_buff[FLAG_MAX];
   size_t i;
   int per_count;
   char formatFlag;

   if (EnvArgTypeCheck(theEnv,"format",2,STRING,&t_ptr) == false) return NULL;

   per_count = 0;
   str_array = ValueToString(t_ptr.value);
   for (i= 0 ; str_array[i] != '\0' ; )
     {
      if (str_array[i] == '%')
        {
         i++;
         formatFlag = FindFormatFlag(str_array,&i,print_buff,FLAG_MAX);
         if (formatFlag == '-')
           { 
            PrintErrorID(theEnv,"IOFUN",3,false);
            EnvPrintRouter(theEnv,WERROR,"Invalid format flag \"");
            EnvPrintRouter(theEnv,WERROR,print_buff);
            EnvPrintRouter(theEnv,WERROR,"\" specified in format function.\n");
            EnvSetEvaluationError(theEnv,true);
            return (NULL);
           }
         else if (formatFlag != ' ')
           { per_count++; }
        }
      else
        { i++; }
     }

   if (per_count != (argCount - 2))
     {
      ExpectedCountError(theEnv,"format",EXACTLY,per_count+2);
      EnvSetEvaluationError(theEnv,true);
      return (NULL);
     }

   return(str_array);
  }

/***********************************************/
/* FindFormatFlag:  This function searches for */
/*   a format flag in the format string.       */
/***********************************************/
static char FindFormatFlag(
  const char *formatString,
  size_t *a,
  char *formatBuffer,
  size_t bufferMax)
  {
   char inchar, formatFlagType;
   size_t copy_pos = 0;

   /*====================================================*/
   /* Set return values to the default value. A blank    */
   /* character indicates that no format flag was found  */
   /* which requires a parameter.                        */
   /*====================================================*/

   formatFlagType = ' ';

   /*=====================================================*/
   /* The format flags for carriage returns, line feeds,  */
   /* horizontal and vertical tabs, and the percent sign, */
   /* do not require a parameter.                         */
   /*=====================================================*/

   if (formatString[*a] == 'n')
     {
      gensprintf(formatBuffer,"\n");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == 'r')
     {
      gensprintf(formatBuffer,"\r");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == 't')
     {
      gensprintf(formatBuffer,"\t");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == 'v')
     {
      gensprintf(formatBuffer,"\v");
      (*a)++;
      return(formatFlagType);
     }
   else if (formatString[*a] == '%')
     {
      gensprintf(formatBuffer,"%%");
      (*a)++;
      return(formatFlagType);
     }

   /*======================================================*/
   /* Identify the format flag which requires a parameter. */
   /*======================================================*/

   formatBuffer[copy_pos++] = '%';
   formatBuffer[copy_pos] = '\0';
   while ((formatString[*a] != '%') &&
          (formatString[*a] != '\0') &&
          (copy_pos < (bufferMax - 5)))
     {
      inchar = formatString[*a];
      (*a)++;

      if ( (inchar == 'd') ||
           (inchar == 'o') ||
           (inchar == 'x') ||
           (inchar == 'u'))
        {
         formatFlagType = inchar;
         formatBuffer[copy_pos++] = 'l';
         formatBuffer[copy_pos++] = 'l';
         formatBuffer[copy_pos++] = inchar;
         formatBuffer[copy_pos] = '\0';
         return(formatFlagType);
        }
      else if ( (inchar == 'c') ||
                (inchar == 's') ||
                (inchar == 'e') ||
                (inchar == 'f') ||
                (inchar == 'g') )
        {
         formatBuffer[copy_pos++] = inchar;
         formatBuffer[copy_pos] = '\0';
         formatFlagType = inchar;
         return(formatFlagType);
        }
      
      /*=======================================================*/
      /* If the type hasn't been read, then this should be the */
      /* -M.N part of the format specification (where M and N  */
      /* are integers).                                        */
      /*=======================================================*/
      
      if ( (! isdigit(inchar)) &&
           (inchar != '.') &&
           (inchar != '-') )
        { 
         formatBuffer[copy_pos++] = inchar;
         formatBuffer[copy_pos] = '\0';
         return('-'); 
        }

      formatBuffer[copy_pos++] = inchar;
      formatBuffer[copy_pos] = '\0';
     }

   return(formatFlagType);
  }

/**********************************************************************/
/* PrintFormatFlag:  Prints out part of the total format string along */
/*   with the argument for that part of the format string.            */
/**********************************************************************/
static const char *PrintFormatFlag(
  Environment *theEnv,
  const char *formatString,
  int whichArg,
  int formatType)
  {
   CLIPSValue theResult;
   const char *theString;
   char *printBuffer;
   size_t theLength;
   void *oldLocale;
      
   /*=================*/
   /* String argument */
   /*=================*/

   switch (formatType)
     {
      case 's':
        if (EnvArgTypeCheck(theEnv,"format",whichArg,SYMBOL_OR_STRING,&theResult) == false) return NULL;
        theLength = strlen(formatString) + strlen(ValueToString(theResult.value)) + 200;
        printBuffer = (char *) gm2(theEnv,(sizeof(char) * theLength));
        gensprintf(printBuffer,formatString,ValueToString(theResult.value));
        break;

      case 'c':
        EnvRtnUnknown(theEnv,whichArg,&theResult);
        if ((GetType(theResult) == STRING) ||
            (GetType(theResult) == SYMBOL))
          {
           theLength = strlen(formatString) + 200;
           printBuffer = (char *) gm2(theEnv,(sizeof(char) * theLength));
           gensprintf(printBuffer,formatString,(ValueToString(theResult.value))[0]);
          }
        else if (GetType(theResult) == INTEGER)
          {
           theLength = strlen(formatString) + 200;
           printBuffer = (char *) gm2(theEnv,(sizeof(char) * theLength));
           gensprintf(printBuffer,formatString,(char) DOToLong(theResult));
          }
        else
          {
           ExpectedTypeError1(theEnv,"format",whichArg,"symbol, string, or integer");
           return NULL;
          }
        break;

      case 'd':
      case 'x':
      case 'o':
      case 'u':
        if (EnvArgTypeCheck(theEnv,"format",whichArg,INTEGER_OR_FLOAT,&theResult) == false) return NULL;
        theLength = strlen(formatString) + 200;
        printBuffer = (char *) gm2(theEnv,(sizeof(char) * theLength));
        
        oldLocale = EnvAddSymbol(theEnv,setlocale(LC_NUMERIC,NULL));
        setlocale(LC_NUMERIC,ValueToString(IOFunctionData(theEnv)->locale));

        if (GetType(theResult) == FLOAT)
          { gensprintf(printBuffer,formatString,(long long) ValueToDouble(theResult.value)); }
        else
          { gensprintf(printBuffer,formatString,(long long) ValueToLong(theResult.value)); }
          
        setlocale(LC_NUMERIC,ValueToString(oldLocale));
        break;

      case 'f':
      case 'g':
      case 'e':
        if (EnvArgTypeCheck(theEnv,"format",whichArg,INTEGER_OR_FLOAT,&theResult) == false) return NULL;
        theLength = strlen(formatString) + 200;
        printBuffer = (char *) gm2(theEnv,(sizeof(char) * theLength));

        oldLocale = EnvAddSymbol(theEnv,setlocale(LC_NUMERIC,NULL));
        
        setlocale(LC_NUMERIC,ValueToString(IOFunctionData(theEnv)->locale));

        if (GetType(theResult) == FLOAT)
          { gensprintf(printBuffer,formatString,ValueToDouble(theResult.value)); }
        else
          { gensprintf(printBuffer,formatString,(double) ValueToLong(theResult.value)); }
        
        setlocale(LC_NUMERIC,ValueToString(oldLocale));
        
        break;

      default:
         EnvPrintRouter(theEnv,WERROR," Error in format, the conversion character");
         EnvPrintRouter(theEnv,WERROR," for formatted output is not valid\n");
         return NULL;
     }

   theString = ValueToString(EnvAddSymbol(theEnv,printBuffer));
   rm(theEnv,printBuffer,sizeof(char) * theLength);
   return(theString);
  }

/******************************************/
/* ReadlineFunction: H/L access routine   */
/*   for the readline function.           */
/******************************************/
void ReadlineFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   char *buffer;
   size_t line_max = 0;
   int numberOfArguments;
   const char *logicalName;

   returnValue->type = STRING;

   if ((numberOfArguments = EnvArgCountCheck(theEnv,"readline",NO_MORE_THAN,1)) == -1)
     {
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   if (numberOfArguments == 0 )
     { logicalName = STDIN; }
   else
     {
      logicalName = GetLogicalName(theEnv,1,STDIN);
      if (logicalName == NULL)
        {
         IllegalLogicalNameMessage(theEnv,"readline");
         EnvSetHaltExecution(theEnv,true);
         EnvSetEvaluationError(theEnv,true);
         returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
         return;
        }
     }

   if (QueryRouters(theEnv,logicalName) == false)
     {
      UnrecognizedRouterMessage(theEnv,logicalName);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = true;
   buffer = FillBuffer(theEnv,logicalName,&RouterData(theEnv)->CommandBufferInputCount,&line_max);
   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = false;

   if (EnvGetHaltExecution(theEnv))
     {
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      if (buffer != NULL) rm(theEnv,buffer,(int) sizeof (char) * line_max);
      return;
     }

   if (buffer == NULL)
     {
      returnValue->value = EnvAddSymbol(theEnv,"EOF");
      returnValue->type = SYMBOL;
      return;
     }

   returnValue->value = EnvAddSymbol(theEnv,buffer);
   rm(theEnv,buffer,(int) sizeof (char) * line_max);
   return;
  }

/*************************************************************/
/* FillBuffer: Read characters from a specified logical name */
/*   and places them into a buffer until a carriage return   */
/*   or end-of-file character is read.                       */
/*************************************************************/
static char *FillBuffer(
  Environment *theEnv,
  const char *logicalName,
  size_t *currentPosition,
  size_t *maximumSize)
  {
   int c;
   char *buf = NULL;

   /*================================*/
   /* Read until end of line or eof. */
   /*================================*/

   c = EnvGetcRouter(theEnv,logicalName);

   if (c == EOF)
     { return NULL; }

   /*==================================*/
   /* Grab characters until cr or eof. */
   /*==================================*/

   while ((c != '\n') && (c != '\r') && (c != EOF) &&
          (! EnvGetHaltExecution(theEnv)))
     {
      buf = ExpandStringWithChar(theEnv,c,buf,currentPosition,maximumSize,*maximumSize+80);
      c = EnvGetcRouter(theEnv,logicalName);
     }

   /*==================*/
   /* Add closing EOS. */
   /*==================*/

   buf = ExpandStringWithChar(theEnv,EOS,buf,currentPosition,maximumSize,*maximumSize+80);
   return (buf);
  }
  
/*****************************************/
/* SetLocaleFunction: H/L access routine */
/*   for the set-locale function.        */
/*****************************************/
void SetLocaleFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theResult;
   int numArgs;
   
   /*======================================*/
   /* Check for valid number of arguments. */
   /*======================================*/
   
   if ((numArgs = EnvArgCountCheck(theEnv,"set-locale",NO_MORE_THAN,1)) == -1)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
     
   /*=================================*/
   /* If there are no arguments, just */
   /* return the current locale.      */
   /*=================================*/
   
   if (numArgs == 0)
     {
      returnValue->type = STRING;
      returnValue->value = IOFunctionData(theEnv)->locale;
      return;
     }

   /*=================*/
   /* Get the locale. */
   /*=================*/
   
   if (EnvArgTypeCheck(theEnv,"set-locale",1,STRING,&theResult) == false)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
     
   /*=====================================*/
   /* Return the old value of the locale. */
   /*=====================================*/
   
   returnValue->type = STRING;
   returnValue->value = IOFunctionData(theEnv)->locale;
   
   /*======================================================*/
   /* Change the value of the locale to the one specified. */
   /*======================================================*/
   
   DecrementSymbolCount(theEnv,(struct symbolHashNode *) IOFunctionData(theEnv)->locale);
   IOFunctionData(theEnv)->locale = DOToPointer(theResult);
   IncrementSymbolCount(IOFunctionData(theEnv)->locale);
  }

/******************************************/
/* ReadNumberFunction: H/L access routine */
/*   for the read-number function.        */
/******************************************/
void ReadNumberFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   struct token theToken;
   int numberOfArguments;
   const char *logicalName = NULL;

   /*===============================================*/
   /* Check for an appropriate number of arguments. */
   /*===============================================*/

   if ((numberOfArguments = EnvArgCountCheck(theEnv,"read",NO_MORE_THAN,1)) == -1)
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   /*======================================================*/
   /* Determine the logical name from which input is read. */
   /*======================================================*/

   if (numberOfArguments == 0)
     { logicalName = STDIN; }
   else if (numberOfArguments == 1)
     {
      logicalName = GetLogicalName(theEnv,1,STDIN);
      if (logicalName == NULL)
        {
         IllegalLogicalNameMessage(theEnv,"read");
         EnvSetHaltExecution(theEnv,true);
         EnvSetEvaluationError(theEnv,true);
         returnValue->type = STRING;
         returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
         return;
        }
     }

   /*============================================*/
   /* Check to see that the logical name exists. */
   /*============================================*/

   if (QueryRouters(theEnv,logicalName) == false)
     {
      UnrecognizedRouterMessage(theEnv,logicalName);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      return;
     }

   /*=======================================*/
   /* Collect input into string if the read */
   /* source is stdin, else just get token. */
   /*=======================================*/

   if (strcmp(logicalName,STDIN) == 0)
     { ReadNumber(theEnv,logicalName,&theToken,true); }
   else
     { ReadNumber(theEnv,logicalName,&theToken,false); }

   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = false;

   /*====================================================*/
   /* Copy the token to the return value data structure. */
   /*====================================================*/

   returnValue->type = theToken.type;
   if ((theToken.type == FLOAT) || (theToken.type == STRING) ||
#if OBJECT_SYSTEM
       (theToken.type == INSTANCE_NAME) ||
#endif
       (theToken.type == SYMBOL) || (theToken.type == INTEGER))
     { returnValue->value = theToken.value; }
   else if (theToken.type == STOP)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvAddSymbol(theEnv,"EOF");
     }
   else if (theToken.type == UNKNOWN_VALUE)
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
     }
   else
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,theToken.printForm);
     }

   return;
  }
  
/********************************************/
/* ReadNumber: Special routine used by the  */
/*   read-number function to read a number. */
/********************************************/
static void ReadNumber(
  Environment *theEnv,
  const char *logicalName,
  struct token *theToken,
  bool isStdin)
  {
   char *inputString;
   char *charPtr = NULL;
   size_t inputStringSize;
   int inchar;
   long long theLong;
   double theDouble;
   void *oldLocale;

   theToken->type = STOP;

   /*===========================================*/
   /* Initialize the variables used for storing */
   /* the characters retrieved from stdin.      */
   /*===========================================*/

   inputString = NULL;
   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = true;
   inputStringSize = 0;
   inchar = EnvGetcRouter(theEnv,logicalName);
            
   /*====================================*/
   /* Skip whitespace before any number. */
   /*====================================*/
      
   while (isspace(inchar) && (inchar != EOF) && 
          (! EnvGetHaltExecution(theEnv)))
     { inchar = EnvGetcRouter(theEnv,logicalName); }

   /*=============================================================*/
   /* Continue reading characters until whitespace is found again */
   /* (for anything other than stdin) or a CR/LF (for stdin).     */
   /*=============================================================*/

   while ((((! isStdin) && (! isspace(inchar))) || 
          (isStdin && (inchar != '\n') && (inchar != '\r'))) &&
          (inchar != EOF) &&
          (! EnvGetHaltExecution(theEnv)))
     {
      inputString = ExpandStringWithChar(theEnv,inchar,inputString,&RouterData(theEnv)->CommandBufferInputCount,
                                         &inputStringSize,inputStringSize + 80);
      inchar = EnvGetcRouter(theEnv,logicalName);
     }

   /*===========================================*/
   /* Pressing control-c (or comparable action) */
   /* aborts the read-number function.          */
   /*===========================================*/

   if (EnvGetHaltExecution(theEnv))
     {
      theToken->type = STRING;
      theToken->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
      if (inputStringSize > 0) rm(theEnv,inputString,inputStringSize);
      return;
     }

   /*====================================================*/
   /* Return the EOF symbol if the end of file for stdin */
   /* has been encountered. This typically won't occur,  */
   /* but is possible (for example by pressing control-d */
   /* in the UNIX operating system).                     */
   /*====================================================*/

   if (inchar == EOF)
     {
      theToken->type = SYMBOL;
      theToken->value = EnvAddSymbol(theEnv,"EOF");
      if (inputStringSize > 0) rm(theEnv,inputString,inputStringSize);
      return;
     }

   /*==================================================*/
   /* Open a string input source using the characters  */
   /* retrieved from stdin and extract the first token */
   /* contained in the string.                         */
   /*==================================================*/
   
   /*=======================================*/
   /* Change the locale so that numbers are */
   /* converted using the localized format. */
   /*=======================================*/
   
   oldLocale = EnvAddSymbol(theEnv,setlocale(LC_NUMERIC,NULL));
   setlocale(LC_NUMERIC,ValueToString(IOFunctionData(theEnv)->locale));

   /*========================================*/
   /* Try to parse the number as a long. The */
   /* terminating character must either be   */
   /* white space or the string terminator.  */
   /*========================================*/

#if WIN_MVC
   theLong = _strtoi64(inputString,&charPtr,10);
#else
   theLong = strtoll(inputString,&charPtr,10);
#endif

   if ((charPtr != inputString) && 
       (isspace(*charPtr) || (*charPtr == '\0')))
     {
      theToken->type = INTEGER;
      theToken->value = EnvAddLong(theEnv,theLong);
      if (inputStringSize > 0) rm(theEnv,inputString,inputStringSize);
      setlocale(LC_NUMERIC,ValueToString(oldLocale));
      return;
     }
     
   /*==========================================*/
   /* Try to parse the number as a double. The */
   /* terminating character must either be     */
   /* white space or the string terminator.    */
   /*==========================================*/

   theDouble = strtod(inputString,&charPtr);  
   if ((charPtr != inputString) && 
       (isspace(*charPtr) || (*charPtr == '\0')))
     {
      theToken->type = FLOAT;
      theToken->value = EnvAddDouble(theEnv,theDouble);
      if (inputStringSize > 0) rm(theEnv,inputString,inputStringSize);
      setlocale(LC_NUMERIC,ValueToString(oldLocale));
      return;
     }

   /*============================================*/
   /* Restore the "C" locale so that any parsing */
   /* of numbers uses the C format.              */
   /*============================================*/
   
   setlocale(LC_NUMERIC,ValueToString(oldLocale));

   /*=========================================*/
   /* Return "*** READ ERROR ***" to indicate */
   /* a number was not successfully parsed.   */
   /*=========================================*/
         
   theToken->type = STRING;
   theToken->value = EnvAddSymbol(theEnv,"*** READ ERROR ***");
  }

#endif

