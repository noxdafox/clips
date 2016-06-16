/*
 *  CLIPSTerminalGlue.h
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */
 
#import <Cocoa/Cocoa.h>
#import <CLIPS/clips.h>

   intBool                 QueryInterfaceRouter(void *,const char *);
   int                     PrintInterfaceRouter(void *,const char *,const char *);
   int                     GetcInterfaceRouter(void *,const char *);
   int                     ExitInterfaceRouter(void *,int);
   void                    MacPeriodicFunction(void *);
   void                    ClearEnvironmentWindowCommand(void *);
   int                     MacBeforeOpenFunction(void *);   
   int                     MacAfterOpenFunction(void *);
