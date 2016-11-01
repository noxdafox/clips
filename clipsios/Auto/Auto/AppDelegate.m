//
//  AppDelegate.m
//  Auto
//
//  Created by Gary Riley
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

   theController = (ViewController *) application.keyWindow.rootViewController;
   
   if ([theController respondsToSelector: @selector(saveData)])
     { [theController saveData]; }
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

@end
