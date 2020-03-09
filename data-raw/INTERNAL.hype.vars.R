## code to prepare `INTERNAL.hype.vars` dataset goes here


INTERNAL.hype.vars <- toupper(c("temp","ctmp","snow","sdep","rswe","rsnw","soim","som2","sml1","sml2","sml3","smrz","sm13","stsw","srff",
                       "smfd","srfd","smfp","srfp","smdf","gwat","sfst","stmp","stm1","stm2","stm3","resf","regw","pfN1", "pfN2",
                       "pfN3","phN1", "phN2", "phN3","pIN1", "pIN2", "pIN3","pfP1", "pfP2", "pfP3","phP1", "phP2", "phP3","ppP1",
                       "ppP2", "ppP3","pSP1", "pSP2", "pSP3","phC1", "phC2", "phC3", "pfC1", "pfC2", "pfC3","pON1", "pON2", "pON3",
                       "cfsc","rfsc","smax","rfse","rfsm","rfme","wcom","wcav","wstr","cout","rout","colv","cilv","clbv","coum",
                       "coub","cgwl","cloc","cinf","rinf","clrv","cmrv","qerr","cobc","wtmp","wtm0","werr","cwbc","coli","cili",
                       "colb","cilb","cols","cils","roli","rili","rolb","rilb","rols","rils","cmri","clri","cmrb","clrb","cmrs",
                       "clrs","rmri","rlri","rmrb","rlrb","rmrs","rlrs","olst","olut","ollt","olwt","ilst","ilwt","lrst","lrwt",
                       "mrst","mrwt","rolt","rilt","rmrt","mrto","lrto","ilto","olto","coic","ciic","cmic","clic","glcv","glca",
                       "lrdp","mrdp","aqwl","cgmb","rgmb","cgma","rgma","rgmp","S105","S106","S108","S111","S114","S205","S206",
                       "S208","S211","S214","C106","C108","C111","C114","C206","C208","C211","C214","coT1","coT2","coIN","coON",
                       "coTN","coSP","coPP","coTP","reT1","reT2","reIN","reON","reSP","rePP","reTN","reTP","cpT1","ceT1","csT1",
                       "csT2","csIN","ccT1","ccT2","ccIN", "ccON", "ccTN", "ccSP", "ccPP", "ccTP","coOC","csOC","ccOC","reOC",
                       "clCO","clIN", "clON", "clTN", "clSP", "clPP", "clTP","prec","cprc","cpSF","cpRF","evap","epot","repo",
                       "eobs","icpe","evsn","levp","evpt","psim","cpIN","cpSP","crun","rrun","cro1", "cro2", "cro3","crod","cros",
                       "ros1","ros2","acdf","cINl", "cONl", "cTNl", "cSPl", "cPPl", "cTPl", "cOCl","deni","crut","faIN","atmd", 
                       "atmp","rtoN", "rtoP","irra","irld","irlr","irrg","irrs","irel","rlIN", "rlON", "rlSP", "rlPP", "rlTN", 
                       "rlTP", "rlOC","aqin","aqut","speq","clwc","clws","sl01","sl02","sl03","sl04","sl05","sl06","sl07","sl08",
                       "sl09","sl10","sl11","sl12","sl13","sl14","sl15","sl16","sl17","sl18","sl19","sl20","sl21","sl22","sl23",
                       "sl24","sl25","sl26","sl27","sl28","sl29","sl30","sl31","sl32","sl33","sl34","sl35","sl36","den3","denz",
                       "cIN1","cIN2","cIN3","sml9","mrfp","olfp","mrfg","olfg","sden","melt","roum","roub","aT11","aT12","aT13",
                       "sT11","sT12","sT13","Tsmr","Tslr","T1sf","clT1","Tcr1","Tcr2","Tcr3","Tcrd","Tcrs","coSS","ccSS","reSS",
                       "ccAE","ccTS", paste0("xom", 0:9), paste0("xos", 0:9),"dwtr","rpwl","lpwl","gmlt","loff","lrfa","mrfa",
                       "lred","mred","cSSl", "cTSl","infi","reTS","clTS","clSS","nlTS","nlSS","wilk","isps","ispp","Psmr","Pslr",
                       "Ssmr","Sslr","wilk","craw","wcIN", "wcON", "wcSP", "wcPP","wcSS","wcAE","wcOC","wcT2","wiIN", "wiON", 
                       "wiSP", "wiPP","wiSS","wiAE","wiOC","wiT2","iwin","iwut","ciwv","wiwt","cowv","wowt","hged","hgeu","upcolv",
                       "upcilv","upclbv","upclrv","upcmrv","upglcv","upglca","upirra","upirld","upirlr","upirrg","upirrs","upirel"))

usethis::use_data(INTERNAL.hype.vars, internal = T)
