#include <windows.h>

typedef int (*EnvLoadPtr)(void *,char *);
typedef int (*EnvBuildPtr)(void *,char *);
typedef long long (*EnvRunPtr)(void *,long long);
typedef void (*EnvResetPtr)(void *);
typedef void * (*CreateEnvironmentPtr)(void);
typedef void (*DestroyEnvironmentPtr)(void *);

/********/
/* main */
/********/
int main()
  {
   void *theEnv;
   CreateEnvironmentPtr __CreateEnvironment;
   EnvLoadPtr __EnvLoad;
   EnvBuildPtr __EnvBuild;
   EnvResetPtr __EnvReset;
   EnvRunPtr __EnvRun;
   DestroyEnvironmentPtr __DestroyEnvironment;
   HMODULE dll_handle;

   /*===============*/
   /* Load the DLL. */
   /*===============*/

#ifdef _WIN64
   dll_handle = LoadLibrary("CLIPSDynamic64.dll");
#else
   dll_handle = LoadLibrary("CLIPSDynamic32.dll");
#endif

   if (dll_handle == NULL)
     { return 0; }

   /*=====================================*/
   /* Retrieve pointers to the CLIPS API. */
   /*=====================================*/

   __CreateEnvironment = (CreateEnvironmentPtr)
      GetProcAddress(dll_handle,"__CreateEnvironment");
   
   __EnvLoad = (EnvLoadPtr)
      GetProcAddress(dll_handle,"__EnvLoad");
   
   __EnvBuild = (EnvBuildPtr)
      GetProcAddress(dll_handle,"__EnvBuild");

   __EnvReset = (EnvResetPtr)
      GetProcAddress(dll_handle,"__EnvReset");
   
   __EnvRun = (EnvRunPtr)
      GetProcAddress(dll_handle,"__EnvRun");
   
   __DestroyEnvironment = (DestroyEnvironmentPtr)
      GetProcAddress(dll_handle,"__DestroyEnvironment");
   
   if ((__CreateEnvironment == NULL) ||
       (__EnvLoad == NULL) ||
       (__EnvBuild == NULL) ||
       (__EnvReset == NULL) ||
       (__EnvRun == NULL) ||
       (__DestroyEnvironment == NULL))
      { return 0; }

   /*===========================*/
   /* Load and run the example. */
   /*===========================*/

   theEnv = __CreateEnvironment();
   
   // For load to work, the CLIPS file must be in the
   // the same directory as the executable, otherwise
   // the full path should be specified.
   //__EnvLoad(theEnv,"hello.clp");

   __EnvBuild(theEnv,"(defrule hello"
                     "   =>"
                     "  (printout t \"Hello World.\" crlf)"
                     "  (printout t \"Hit return to end.\" crlf)"
                     "  (readline))");

   __EnvReset(theEnv);
   __EnvRun(theEnv,-1);
   __DestroyEnvironment(theEnv);

   /*=================*/
   /* Unload the DLL. */
   /*=================*/

   FreeLibrary(dll_handle);

   return 1;
  }

