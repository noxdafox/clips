/*
 *  CLIPSTerminalGlue.h
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */
 
#import <Cocoa/Cocoa.h>
#import <CLIPS/clips.h>

   bool                    QueryInterfaceRouter(Environment *,const char *);
   void                    PrintInterfaceRouter(Environment *,const char *,const char *);
   int                     GetcInterfaceRouter(Environment *,const char *);
   void                    ExitInterfaceRouter(Environment *,int);
   void                    MacPeriodicFunction(Environment *);
   void                    ClearEnvironmentWindowCommand(Environment *,UDFContext *,CLIPSValue *);
   int                     MacBeforeOpenFunction(Environment *);
   int                     MacAfterOpenFunction(Environment *);
