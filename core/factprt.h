   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*         FACT RETE PRINT FUNCTIONS HEADER FILE       */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Updates to support new struct members.         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_factprt

#define _H_factprt

   void                           PrintFactJNCompVars1(void *,const char *,void *);
   void                           PrintFactJNCompVars2(void *,const char *,void *);
   void                           PrintFactPNCompVars1(void *,const char *,void *);
   void                           PrintFactJNGetVar1(void *,const char *,void *);
   void                           PrintFactJNGetVar2(void *,const char *,void *);
   void                           PrintFactJNGetVar3(void *,const char *,void *);
   void                           PrintFactPNGetVar1(void *,const char *,void *);
   void                           PrintFactPNGetVar2(void *,const char *,void *);
   void                           PrintFactPNGetVar3(void *,const char *,void *);
   void                           PrintFactSlotLength(void *,const char *,void *);
   void                           PrintFactPNConstant1(void *,const char *,void *);
   void                           PrintFactPNConstant2(void *,const char *,void *);

#endif /* _H_factprt */


