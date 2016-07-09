/*
 *  CLIPSTerminalGlue.h
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */
 
#import <Cocoa/Cocoa.h>
#import <CLIPS/clips.h>

   bool                    QueryInterfaceRouter(void *,const char *);
   void                    PrintInterfaceRouter(void *,const char *,const char *);
   int                     GetcInterfaceRouter(void *,const char *);
   void                    ExitInterfaceRouter(void *,int);
   void                    MacPeriodicFunction(void *);
   void                    ClearEnvironmentWindowCommand(void *);
   int                     MacBeforeOpenFunction(void *);   
   int                     MacAfterOpenFunction(void *);
