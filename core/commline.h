   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/04/16            */
   /*                                                     */
   /*              COMMAND LINE HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of routines for processing        */
/*   commands entered at the top level prompt.               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Refactored several functions and added         */
/*            additional functions for use by an interface   */
/*            layered on top of CLIPS.                       */
/*                                                           */
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Metrowerks CodeWarrior (MAC_MCW, IBM_MCW) is   */
/*            no longer supported.                           */
/*                                                           */
/*            UTF-8 support.                                 */
/*                                                           */
/*            Command history and editing support            */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
/*                                                           */             
/*            Added before command execution callback        */
/*            function.                                      */
/*                                                           */  
/*            Fixed RouteCommand return value.               */           
/*                                                           */             
/*            Added AwaitingInput flag.                      */
/*                                                           */             
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_commline

#pragma once

#define _H_commline

#define COMMANDLINE_DATA 40

struct commandLineData
  { 
   bool EvaluatingTopLevelCommand;
   bool HaltCommandLoopBatch;
#if ! RUN_TIME
   struct expr *CurrentCommand;
   char *CommandString;
   size_t MaximumCharacters;
   bool ParsingTopLevelCommand;
   const char *BannerString;
   int (*EventFunction)(void *);
   int (*AfterPromptFunction)(void *);
   int (*BeforeCommandExecutionFunction)(void *);
#endif
  };

#define CommandLineData(theEnv) ((struct commandLineData *) GetEnvironmentData(theEnv,COMMANDLINE_DATA))

   void                           InitializeCommandLineData(void *);
   bool                           ExpandCommandString(void *,int);
   void                           FlushCommandString(void *);
   void                           SetCommandString(void *,const char *);
   void                           AppendCommandString(void *,const char *);
   void                           InsertCommandString(void *,const char *,unsigned);
   char                          *GetCommandString(void *);
   int                            CompleteCommand(const char *);
   void                           CommandLoop(void *);
   void                           CommandLoopBatch(void *);
   void                           CommandLoopBatchDriver(void *);
   void                           PrintPrompt(void *);
   void                           PrintBanner(void *);
   void                           SetAfterPromptFunction(void *,int (*)(void *));
   void                           SetBeforeCommandExecutionFunction(void *,int (*)(void *));
   bool                           RouteCommand(void *,const char *,bool);
   int                          (*SetEventFunction(void *,int (*)(void *)))(void *);
   bool                           TopLevelCommand(void *);
   void                           AppendNCommandString(void *,const char *,unsigned);
   void                           SetNCommandString(void *,const char *,unsigned);
   const char                    *GetCommandCompletionString(void *,const char *,size_t);
   bool                           ExecuteIfCommandComplete(void *);
   void                           CommandLoopOnceThenBatch(void *);
   bool                           CommandCompleteAndNotEmpty(void *);
   void                           SetHaltCommandLoopBatch(void *,bool);
   bool                           GetHaltCommandLoopBatch(void *);
   void                           RerouteStdin(void *,int,char *[]);

#endif





