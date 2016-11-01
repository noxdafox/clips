#include "clipscpp.h"

int main()
  {
   CLIPS::CLIPSCPPEnv theEnv;

   // For load to work, the CLIPS file must be in the
   // the same directory as the executable, otherwise
   // the full path should be specified.
   // theEnv.Load("hello.clp");

   theEnv.Build("(defrule hello"
                "   =>"
                "  (printout t \"Hello World.\" crlf)"
                "  (printout t \"Hit return to end.\" crlf)"
                "  (readline))");

   theEnv.Reset();
   theEnv.Run(-1);

   return 0;
  }