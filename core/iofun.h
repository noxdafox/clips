   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*               I/O FUNCTIONS HEADER FILE             */
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
/*            Added a+, w+, rb, ab, r+b, w+b, and a+b modes  */
/*            for the open function.                         */
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
/*      6.40: Removed LOCALE definition.                     */
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
/*            Added print and println functions.             */
/*                                                           */
/*************************************************************/

#ifndef _H_iofun

#pragma once

#define _H_iofun

   void                           IOFunctionDefinitions(Environment *);
#if IO_FUNCTIONS
   bool                           SetFullCRLF(Environment *,bool);
   void                           PrintoutFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           PrintFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           PrintlnFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           ReadFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           OpenFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           CloseFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           GetCharFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           PutCharFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           ReadlineFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           FormatFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           RemoveFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           RenameFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SetLocaleFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           ReadNumberFunction(Environment *,UDFContext *,CLIPSValue *);
#endif

#endif /* _H_iofun */






