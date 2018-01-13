/*
 *  CLIPSTerminalGlue.h
 *  CLIPS
 *
 *  Created by Gary Riley on 3/25/06.
 *
 */
 
#import <Cocoa/Cocoa.h>
#import <CLIPS/clips.h>

   bool                    QueryInterfaceCallback(Environment *,const char *,void *);
   void                    WriteInterfaceCallback(Environment *,const char *,const char *,void *);
   int                     ReadInterfaceCallback(Environment *,const char *,void *);
   int                     UnreadInterfaceCallback(Environment *,const char *,int,void *);
   void                    ExitInterfaceCallback(Environment *,int,void *);
   void                    MacPeriodicFunction(Environment *,void *);
   void                    ClearEnvironmentWindowCommand(Environment *,UDFContext *,UDFValue *);
   int                     MacBeforeOpenFunction(Environment *);
   int                     MacAfterOpenFunction(Environment *);
