   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_iofun

#pragma once

#define _H_iofun

   void                           IOFunctionDefinitions(void *);
#if IO_FUNCTIONS
   intBool                        SetFullCRLF(void *,intBool);
   void                           PrintoutFunction(void *);
   void                           ReadFunction(void *,DATA_OBJECT_PTR);
   int                            OpenFunction(void *);
   int                            CloseFunction(void *);
   int                            GetCharFunction(void *);
   void                           PutCharFunction(void *);
   void                           ReadlineFunction(void *,DATA_OBJECT_PTR);
   void                          *FormatFunction(void *);
   int                            RemoveFunction(void *);
   int                            RenameFunction(void *);
   void                           SetLocaleFunction(void *,DATA_OBJECT_PTR);
   void                           ReadNumberFunction(void *,DATA_OBJECT_PTR);
#endif

#endif /* _H_iofun */






