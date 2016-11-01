//
//  AppDelegate.m
//  Wine
//
//  Created by Programmer X on 4/30/16.
//  Copyright © 2016 CLIPS. All rights reserved.
//

#import "AppDelegate.h"
#import "CriteriaTableViewController.h"

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
   UINavigationController *theNavController =
      (UINavigationController *) application.keyWindow.rootViewController;
      
   CriteriaTableViewController *theController =
      (CriteriaTableViewController *) theNavController.topViewController;

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
