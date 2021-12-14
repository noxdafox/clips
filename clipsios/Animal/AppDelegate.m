//
//  AppDelegate.m
//  Animal
//
//  Created by Gary Riley on 5/3/21.
//

#import "AppDelegate.h"
#import "ViewController.h"

@interface AppDelegate ()

@end

@implementation AppDelegate

/**********************************************/
/* application:didFinishLaunchingWithOptions: */
/**********************************************/
- (BOOL) application: (UIApplication *) application
         didFinishLaunchingWithOptions: (NSDictionary *) launchOptions
  {
   return YES;
  }

/************/
/* saveData */
/************/
- (void) saveData: (UIApplication *) application
  {
   ViewController *theController;

   theController = (ViewController *)  [self findKeyWindow].rootViewController;
   
   if ([theController respondsToSelector: @selector(saveData)])
     { [theController saveData]; }
  }

/*****************/
/* findKeyWindow */
/*****************/
- (UIWindow*) findKeyWindow
  {
   UIWindow *foundWindow = nil;
   NSArray *windows = [[UIApplication sharedApplication]windows];
   for (UIWindow *window in windows)
     {
      if (window.isKeyWindow)
        {
         foundWindow = window;
         break;
        }
     }
   
   return foundWindow;
  }

/********************************/
/* applicationWillResignActive: */
/********************************/
- (void) applicationWillResignActive: (UIApplication *) application
  {
  }

/**********************************/
/* applicationDidEnterBackground: */
/**********************************/
- (void) applicationDidEnterBackground: (UIApplication *) application
  {
   [self saveData: application];
  }

/***********************************/
/* applicationWillEnterForeground: */
/***********************************/
- (void) applicationWillEnterForeground: (UIApplication *) application
  {
  }

/*******************************/
/* applicationDidBecomeActive: */
/*******************************/
- (void) applicationDidBecomeActive: (UIApplication *) application
  {
  }

/*****************************/
/* applicationWillTerminate: */
/*****************************/
- (void) applicationWillTerminate: (UIApplication *) application
  {
   [self saveData: application];
  }

#pragma mark - UISceneSession lifecycle


- (UISceneConfiguration *)application:(UIApplication *)application configurationForConnectingSceneSession:(UISceneSession *)connectingSceneSession options:(UISceneConnectionOptions *)options {
    // Called when a new scene session is being created.
    // Use this method to select a configuration to create the new scene with.
    return [[UISceneConfiguration alloc] initWithName:@"Default Configuration" sessionRole:connectingSceneSession.role];
}

- (void)application:(UIApplication *)application didDiscardSceneSessions:(NSSet<UISceneSession *> *)sceneSessions {
    // Called when the user discards a scene session.
    // If any sessions were discarded while the application was not running, this will be called shortly after application:didFinishLaunchingWithOptions.
    // Use this method to release any resources that were specific to the discarded scenes, as they will not return.
}

@end
