//ANDREWJA JOB  CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//ASM      EXEC PGM=ASMA90,PARM='OBJECT,NODECK,XREF(FULL),RENT,FLAG(NOC-
//             ONT)'
//SYSLIB   DD   DISP=SHR,DSN=ANDREWJ.SOURCE.MAC#
//         DD   DISP=SHR,DSN=SYS1.MACLIB
//         DD   DISP=SHR,DSN=SYS1.MODGEN
//SYSUT1   DD   UNIT=SYSDA,SPACE=(CYL,(10,5)),DSN=&SYSUT1
//SYSIN    DD   *
*                                                                       00010000
*  Module name       = IEF176I                                          00020000
*                                                                       00030000
*  Descrpition       = Issuing STOP WTR when the external writer        00040000
*                      waits for further instructions                   00050000
*                                                                       00060000
*  Function          = Automatically issues P WTR                       00070000
*                      when the IEF176I appears                         00080000
*                                                                       00090000
*  Operation         = R1  points to the addr of the CTXT               00100000
*                      R13 points to the addr of the standard save area 00110000
*                      R14 return point                                 00120000
*                      R15 entry  point                                 00130000
*                                                                       00140000
*  Register usage    = R5  - addr of the CTXT                           00150000
*                      R10 - module data register                       00160000
*                      R11 - potential 2nd base                         00170000
*                      R12 - module base register                       00180000
*                      R13 - pointer of a standard save area            00190000
*                      R14 - return point                               00200000
*                      R15 - entry  point                               00210000
*                                                                       00220000
*  CONTROL  BLOCK    = R5  - pointer to the address of the CTXT         00230000
*    name     mapping macro    reason used                  usage       00240000
*   ------    -------------    ---------------------------  -----       00250000
*    CTXT       IEZVX100       WTO USER EXIT PARAMETER LIST  R,W        00260000
*    MGCR       IEZMGCR        SVC 34 PARAMETER LIST         C,D        00270000
*                                                                       00280000
*    KEY = R-READ, W-WRITE, C-CREATE, D-DELETE                          00290000
*                                                                       00300000
*    macros          =  GETMAIN, FREEMAIN, MGCR                         00310000
*                                                                       00320000
* Author : Andrew Jan   20100408                                        00330000
*                                                                       00340000
                                                                        00350000
         PRINT OFF               bypass inline macro expansion          00360000
         LCLA  &REG                                                     00370000
.LOOP    ANOP                    inline macro to generate registers     00380000
R&REG    EQU   &REG              generate the equates                   00390000
&REG     SETA  &REG+1            next                                   00400000
         AIF   (&REG LE 15).LOOP if not yet finished, loop it           00410000
         PRINT ON                trigger printing                       00420000
         PRINT GEN               not allow macro expansion              00430000
                                                                        00440000
*   Work area                                                           00450000
DATAAREA DSECT                                                          00460000
         DS    0F                                                       00470000
SAVEAREA DS    18F               standard save area                     00480000
         DS    0F                                                       00490000
MGCR     IEZMGCR DSECT=NO        for issuing MVS command                00500000
         ORG   MGCRTEXT                                                 00510000
             DS  CL2             P                                      00520000
MGCR_JOBN    DS  CL8             job name of the WTR                    00530000
         ORG                                                            00540000
DATALEN  EQU   *-DATAAREA                                               00550000
                                                                        00560000
                                                                        00570000
*   Mapping of the message text                                         00580000
MSGTEXT  DSECT                                                          00590000
MSGID    DS    CL8               console message id                     00600000
COMPNAME DS    CL3               'WTR'                                  00610000
         DS    CL1               blank                                  00620000
MSG_JOBN DS    CL8               jobname                                00630000
                                                                        00640000
         IEZVX100                DSECT for CTXT                         00650000
                                                                        00660000
IEF176I  CSECT                                                          00670000
IEF176I  AMODE 31                                                       00680000
IEF176I  RMODE ANY                                                      00690000
                                                                        00700000
         USING *,R15              setup addressibility                  00710000
         STM   R14,R12,12(R13)    save parent's register                00720000
         B     CMNTTAIL           skip over the remarks                 00730000
*                                                                       00740000
CMNTHEAD EQU   *                                                        00750000
         PRINT GEN                print out remarks                     00760000
         DC    CL8'&SYSDATE'      compiling date                        00770000
         DC    C' '                                                     00780000
         DC    CL5'&SYSTIME'      compiling time                        00790000
         DC    C'ANDREW JAN'      author                                00800000
         CNOP  2,4                ensure half word boundary             00810000
         PRINT NOGEN              disable macro expansion               00820000
CMNTTAIL EQU   *                                                        00830000
                                                                        00840000
         BALR  R12,0              module base                           00850000
         DROP  R15                avoid compiling warning               00860000
         USING *,R12              addressibility                        00870000
                                                                        00880000
         L     R5,0(,R1)          establish addressability              00890000
         USING CTXT,R5            to the CTXT                           00900000
                                                                        00910000
         GETMAIN RU,LV=DATALEN                                          00920000
         LR    R10,R1              address return in R1                 00930000
         USING DATAAREA,R10        addressability to dynmaic           X00940000
                                   storage                              00950000
         ST    R13,SAVEAREA+4      set backward ptr                     00960000
         LA    R15,SAVEAREA        get address of out own savearea      00970000
         ST    R15,8(,R13)         save ours to caller's                00980000
         LR    R13,R15             R13 points to our own savearea       00990000
                                                                        01000000
*  Determine which message is to be processed.  IEF176I                 01010000
                                                                        01020000
         L     R2,CTXTTXPJ         text of major                        01030000
         USING CTXTATTR,R2         comm task exit message               01040000
         LA    R4,CTXTTMSG         text of message (126 bytes)          01050000
         USING MSGTEXT,R4         addressibility for console messages   01060000
                                                                        01070000
         #SPM PRINT=GEN            generate smp macros                  01080000
                                                                        01090000
         #PERF R14,PROCESS_PWTR                                         01100000
         #PERF R14,ISSUE_MGCR                                           01110000
                                                                        01120000
FINISH   EQU   *                                                        01130000
         L     R13,4(R13)                                               01140000
         FREEMAIN RU,LV=DATALEN,A=(R10) free the storage                01150000
         LM    R14,R12,12(R13)        restore caller's register values  01160000
         BR    R14                    go back to caller                 01170000
                                                                        01180000
                                                                        01190000
                                                                        01200000
*  PROCEDURE  -  issue 'S PROC,.... via MGCR                            01210000
        #SUBR  ISSUE_MGCR,R14                                           01220000
                 STC   R1,MGCRLGTH         save length in the MGCRPL    01230000
                 SR    R0,R0                                            01240000
                 MGCR  MGCRPL              issue the command            01250000
        #ESUB                                                           01260000
                                                                        01270000
        #SUBR  PROCESS_PWTR,R14                                         01280000
                 XC    MGCRPL(MGCRLTH),MGCRPL  clear parm list          01290000
                 MVC   MGCRTEXT(L'STOPWTR),STOPWTR  move skeleton       01300000
                 MVC   MGCRTEXT+2(L'MSG_JOBN),MSG_JOBN real name        01310000
                 LA    R7,L'MSG_JOBN           max.length possible      01320000
                 LA    R8,MGCRTEXT+2           check the length         01330000
                 LA    R9,2                    accumulate               01340000
NAME_LOOP        EQU   *                                                01350000
                 CLI   0(R8),C' '              space?                   01360000
                 BE    NAME_CHKED              yes, branch              01370000
                 LA    R9,1(,R9)               increase the length      01380000
                 LA    R8,1(,R8)               next byte                01390000
                 BCT   R7,NAME_LOOP            go on                    01400000
NAME_CHKED       EQU   *                                                01410000
                 LA    R1,(MGCRTEXT-MGCRPL)    get length               01420000
                 AR    R1,R9                   total length             01430000
        #ESUB                                                           01440000
                                                                        01450000
*  constants                                                            01460000
                                                                        01470000
STOPWTR  DC    C'P XXXXXXXX'                                            01480000
                                                                        01490000
         END   IEF176I                                                  01500000
/*
//SYSPRINT DD   SYSOUT=*
//SYSLIN   DD   DSN=&OBJ,SPACE=(3040,(40,40),,,ROUND),UNIT=VIO,
//         DISP=(NEW,PASS),
//         DCB=(BLKSIZE=3040,LRECL=80,RECFM=FBS,BUFNO=1)
//
//LKED     EXEC PGM=IEWL,PARM='MAP,LET,LIST,NCAL,RENT',REGION=4096K,
//         COND=(4,LT,ASM)
//SYSLIN   DD   DSN=&OBJ,DISP=(OLD,DELETE)
//SYSLMOD  DD   DSN=SYS1.USER.LINKLIB(IEF176I),DISP=SHR
//SYSPRINT DD   SYSOUT=*
//