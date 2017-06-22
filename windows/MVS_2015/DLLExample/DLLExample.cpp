#include <windows.h>

#include "CLIPSDLL.h"

/********/
/* main */
/********/
int main()
  {
   Environment *theEnv;

   /*===========================*/
   /* Load and run the example. */
   /*===========================*/

   theEnv = __CreateEnvironment();

   // For load to work, the CLIPS file must be in the
   // the same directory as the executable, otherwise
   // the full path should be specified.
   //__Load(theEnv,"hello.clp");

   __Build(theEnv,"(defrule hello"
                     "   =>"
                     "  (printout t \"Hello World.\" crlf)"
                     "  (printout t \"DLL Example.\" crlf)"
                     "  (printout t \"Hit return to end.\" crlf)"
                     "  (readline))");
   __Reset(theEnv);
   __Run(theEnv,-1);
   __DestroyEnvironment(theEnv);
   
   return 1;
  }


