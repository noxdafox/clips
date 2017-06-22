/*
 *  CLIPSTerminalGlue.h
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */
 
#import <Cocoa/Cocoa.h>
#import <CLIPS/clips.h>

   bool                    QueryInterfaceRouter(Environment *,const char *,void *);
   void                    PrintInterfaceRouter(Environment *,const char *,const char *,void *);
   int                     GetcInterfaceRouter(Environment *,const char *,void *);
   void                    ExitInterfaceRouter(Environment *,int,void *);
   void                    MacPeriodicFunction(Environment *,void *);
   void                    ClearEnvironmentWindowCommand(Environment *,UDFContext *,UDFValue *);
   int                     MacBeforeOpenFunction(Environment *);
   int                     MacAfterOpenFunction(Environment *);
