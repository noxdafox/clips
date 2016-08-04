   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_commline

#pragma once

#define _H_commline

#define COMMANDLINE_DATA 40

typedef void AfterPromptFunction(Environment *);
typedef bool BeforeCommandExecutionFunction(Environment *);
typedef void EventFunction(Environment *);

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
   EventFunction *EventCallback;
   AfterPromptFunction *AfterPromptCallback;
   BeforeCommandExecutionFunction *BeforeCommandExecutionCallback;
#endif
  };

#define CommandLineData(theEnv) ((struct commandLineData *) GetEnvironmentData(theEnv,COMMANDLINE_DATA))

   void                           InitializeCommandLineData(Environment *);
   bool                           ExpandCommandString(Environment *,int);
   void                           FlushCommandString(Environment *);
   void                           SetCommandString(Environment *,const char *);
   void                           AppendCommandString(Environment *,const char *);
   void                           InsertCommandString(Environment *,const char *,unsigned);
   char                          *GetCommandString(Environment *);
   int                            CompleteCommand(const char *);
   void                           CommandLoop(Environment *);
   void                           CommandLoopBatch(Environment *);
   void                           CommandLoopBatchDriver(Environment *);
   void                           PrintPrompt(Environment *);
   void                           PrintBanner(Environment *);
   void                           SetAfterPromptFunction(Environment *,AfterPromptFunction *);
   void                           SetBeforeCommandExecutionFunction(Environment *,BeforeCommandExecutionFunction *);
   bool                           RouteCommand(Environment *,const char *,bool);
   EventFunction                 *SetEventFunction(Environment *,EventFunction *);
   bool                           TopLevelCommand(Environment *);
   void                           AppendNCommandString(Environment *,const char *,unsigned);
   void                           SetNCommandString(Environment *,const char *,unsigned);
   const char                    *GetCommandCompletionString(Environment *,const char *,size_t);
   bool                           ExecuteIfCommandComplete(Environment *);
   void                           CommandLoopOnceThenBatch(Environment *);
   bool                           CommandCompleteAndNotEmpty(Environment *);
   void                           SetHaltCommandLoopBatch(Environment *,bool);
   bool                           GetHaltCommandLoopBatch(Environment *);
   void                           RerouteStdin(Environment *,int,char *[]);

#endif





