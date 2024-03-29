(set-dynamic-constraint-checking FALSE)
(set-sequence-operator-recognition FALSE)
(set-reset-globals TRUE)
(set-fact-duplication FALSE)
(set-salience-evaluation when-defined)
(set-strategy depth)
(open "Results//testall.rsl" testall "w")
(printout testall "*** FEATURE TESTS BEGIN ***" crlf)
(printout testall ">>> Start time: " (time) crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "attchtst.tst")
(printout testall "Completed attchtst.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "attchtst2.tst")
(printout testall "Completed attchtst2.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "attchtst3.tst")
(printout testall "Completed attchtst3.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "attchtst4.tst")
(printout testall "Completed attchtst4.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "attchtst5.tst")
(printout testall "Completed attchtst5.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "basicfnx.tst")
(printout testall "Completed basicfnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "bpgf3err.tst")
(printout testall "Completed bpgf3err.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "bpgf8err.tst")
(printout testall "Completed bpgf8err.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "bigbug.tst")
(printout testall "Completed bigbug.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "ceerr.tst")
(printout testall "Completed ceerr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "chksyntx.tst")
(printout testall "Completed chksyntx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "class.tst")
(printout testall "Completed class.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "classerr.tst")
(printout testall "Completed classerr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "cnstrsn.tst")
(printout testall "Completed cnstrsn.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "conres.tst")
(printout testall "Completed conres.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "coolcmd.tst")
(printout testall "Completed coolcmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "coolfnx.tst")
(printout testall "Completed coolfnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dffctcmd.tst")
(printout testall "Completed dffctcmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dfgblcmd.tst")
(printout testall "Completed dfgblcmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dfnxcmd.tst")
(printout testall "Completed dfnxcmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dfnxerr.tst")
(printout testall "Completed dfnxerr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dfnxexe.tst")
(printout testall "Completed dfnxexe.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dfrulcmd.tst")
(printout testall "Completed dfrulcmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dftmpcmd.tst")
(printout testall "Completed dftmpcmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dftmpfun.tst")
(printout testall "Completed dftmpfun.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest01.tst")
(printout testall "Completed drtest01.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest02.tst")
(printout testall "Completed drtest02.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest03.tst")
(printout testall "Completed drtest03.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest04.tst")
(printout testall "Completed drtest04.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest05.tst")
(printout testall "Completed drtest05.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest06.tst")
(printout testall "Completed drtest06.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest07.tst")
(printout testall "Completed drtest07.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest08.tst")
(printout testall "Completed drtest08.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest09.tst")
(printout testall "Completed drtest09.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest10.tst")
(printout testall "Completed drtest10.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "drtest11.tst")
(printout testall "Completed drtest11.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dynsal.tst")
(printout testall "Completed dynsal.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "dyobjcst.tst")
(printout testall "Completed dyobjcst.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "example.tst")
(printout testall "Completed example.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "exists.tst")
(printout testall "Completed exists.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "factsav.tst")
(printout testall "Completed factsav.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "factscmd.tst")
(printout testall "Completed factscmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "factsfun.tst")
(printout testall "Completed factsfun.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "fctpcstr.tst")
(printout testall "Completed fctpcstr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "fctquery.tst")
(printout testall "Completed fctquery.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "firstjoin.tst")
(printout testall "Completed firstjoin.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "fldval50.tst")
(printout testall "Completed fldval.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "focuscmd.tst")
(printout testall "Completed focuscmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "focusexe.tst")
(printout testall "Completed focusexe.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "globlerr.tst")
(printout testall "Completed globlerr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "globltst.tst")
(printout testall "Completed globltst.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "gnrccmd.tst")
(printout testall "Completed gnrccmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "gnrcerr.tst")
(printout testall "Completed gnrcerr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "gnrcexe.tst")
(printout testall "Completed gnrcexe.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "gnrcovl.tst")
(printout testall "Completed gnrcovl.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "gnrcprc.tst")
(printout testall "Completed gnrcprc.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "incrrset.tst")
(printout testall "Completed incrrset.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "inserr.tst")
(printout testall "Completed inserr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "insmodul.tst")
(printout testall "Completed insmodul.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "insquery.tst")
(printout testall "Completed insquery.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "inssav.tst")
(printout testall "Completed inssav.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "instance.tst")
(printout testall "Completed instance.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "iofnx.tst")
(printout testall "Completed iofnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "jnftrght.tst")
(printout testall "Completed jnftrght.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "joinshre.tst")
(printout testall "Completed joinshre.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "joinview.tst")
(printout testall "Completed joinview.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "lgclexe.tst")
(printout testall "Completed lgclexe.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "mathfnx.tst")
(printout testall "Completed mathfnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "memrycmd.tst")
(printout testall "Completed memrycmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "memtest.tst")
(printout testall "Completed memtest.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "mfvmatch.tst")
(printout testall "Completed mfvmatch.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "miscfnx.tst")
(printout testall "Completed miscfnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "misclns1.tst")
(printout testall "Completed misclns1.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "misclns2.tst")
(printout testall "Completed misclns2.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "misclns3.tst")
(printout testall "Completed misclns3.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "misclns4.tst")
(printout testall "Completed misclns4.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "modlmisc.tst")
(printout testall "Completed modlmisc.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "modulcmd.tst")
(printout testall "Completed modulcmd.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "modulprt.tst")
(printout testall "Completed modulprt.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "msgdisp.tst")
(printout testall "Completed msgdisp.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "msgerr.tst")
(printout testall "Completed msgerr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "msghand.tst")
(printout testall "Completed msghand.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "multifnx.tst")
(printout testall "Completed multifnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "multinh.tst")
(printout testall "Completed multinh.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "pataddtn.tst")
(printout testall "Completed pataddtn.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "predcfnx.tst")
(printout testall "Completed predcfnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "rfrshagn.tst")
(printout testall "Completed rfrshagn.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "rulemisc.tst")
(printout testall "Completed rulemisc.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "seqop.tst")
(printout testall "Completed seqop.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "sfmfmix.tst")
(printout testall "Completed sfmfmix.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "stobjcst.tst")
(printout testall "Completed stobjcst.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "strngfnx.tst")
(printout testall "Completed strngfnx.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "tceplace.tst")
(printout testall "Completed tceplace.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "templerr.tst")
(printout testall "Completed templerr.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "textpro.tst")
(printout testall "Completed textpro.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "tmpldflt.tst")
(printout testall "Completed tmpldflt.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "tmplmslt.tst")
(printout testall "Completed tmplmslt.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "utf8.tst")
(printout testall "Completed utf8.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "visible.tst")
(printout testall "Completed visible.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "v610fun.tst")
(printout testall "Completed v610fun.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(batch "v640fun.tst")
(printout testall "Completed v640fun.tst test" crlf)
(clear)
(release-mem)
(printout testall "Memory use: " (mem-used) crlf)
(printout testall "*** FEATURE TESTS COMPLETED ***" crlf)
(printout testall "<<< End time: " (time) crlf)
(close testall)
(exit 0)
