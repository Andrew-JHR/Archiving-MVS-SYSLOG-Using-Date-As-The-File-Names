//ANDREWJA JOB  CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEP1    EXEC ASMACLG
//SYSIN    DD   *
*--------------------------------------------------------------*C191104
* Allocate (640,640) instead of the old value: (64,64)          C191104
* 20191104                                         Andrew Jan   C191104
*--------------------------------------------------------------*C190821
* Keep The Dynamic Allocation Return code as the program's      C190821
* final return code                                             C190821
* 20190821                                         Andrew Jan   C190821
*-------------------------------------------------------------- C190326
* Changing the output data set names from CPAC.CONSLOG to       C190326
* SYS1.CONSLOG                                                  C190326
*    andrew jan                                                 C190326
*-------------------------------------------------------------- C190326
         PRINT NOGEN
*
*------------------------------------------------*
*
         PRINT OFF
&REG     SETA  0
.LOOP    ANOP                              GENERATE REGS.
R&REG    EQU   &REG
&REG     SETA  &REG+1
         AIF   (&REG LE 15).LOOP
         PRINT ON
*
*------------------------------------------------*
*
PARSCNLG CSECT
         USING *,R15              setup addressibility                  
         STM   R14,R12,12(R13)    save parent's register                
         LR    R2,R13             parent's save area pointer
         B     CMNTTAIL           skip over the remarks                 
*                                                                       
CMNTHEAD EQU   *                                                        
         PRINT GEN                print out remarks                     
         DC    CL8'&SYSDATE'      compiling date                        
         DC    C' '                                                     
         DC    CL5'&SYSTIME'      compiling time                        
         DC    C'ANDREW JAN'      author                                
         CNOP  2,4                ensure half word boundary             
         PRINT NOGEN              disable macro expansion               
CMNTTAIL EQU   *                                                        
                                                                        
         BALR  R12,0
         BAL   R13,76(R12)
                                                                        
         DROP  R15                avoid compiling warning               
                                                                        
SAVREG   DS    18F
         USING SAVREG,R13
         ST    R2,4(,R13)
         ST    R13,8(,R2)
*
*
*---MAINSTREAM------------------------------------*
*
*
        BAL    R6,RDJFCB            read jfcb
        BAL    R6,OPEN_FILES        open files
        B      GO_PROCESS           do the job
FINISH  EQU    *
        BAL    R6,CLOSE_FILES       close files
        B      RETURN               return to system
*
*-------------------------------------------------------*
*
RDJFCB   EQU   *
         RDJFCB SEQIN              read in the JFCB
         LTR    R15,R15            invoked OK ?
         BZR    R6                 yes, go on
         ABEND  901                no, go ABEND
*
*-------------------------------------------------------*
*
OPEN_FILES EQU  *
         OPEN  (SEQIN,INPUT,PRINT,OUTPUT)
         BR    R6
*
*-------------------------------------------------------*
*
*
*-------------------------------------------------------*
*
GO_PROCESS  EQU   *
J        USING INFMJFCB,JFCB       addressibility fot input JFCB
*        MVC   DSNTAG,J.JFCBDSNM   get the name from the input file
*
GO_LOOP     EQU   *
         GET   SEQIN              read a record
         LR    R4,R1              save the reg
         LH    R3,0(,R4)          rec size

         CH    R3,=H'29'          less than 29 bytes
         BNE   GO_LOOP1           no, go compare the date

         TM    NFIRST,L'NFIRST    any output file opened ?
         BO    GO_WRITE           yes, output file opened

         B     GO_LOOP            omit the rec

GO_LOOP1    EQU   *
         CLC   24(5,R4),=C'     '  blanks ?
         BNE   GO_LOOP2           no, go compare the date

         TM    NFIRST,L'NFIRST    any output file opened ?
         BO    GO_WRITE           yes, output file opened

         B     GO_LOOP            omit the rec

GO_LOOP2    EQU   *
         CLC   24(5,R4),YYDDD     different dates ?
         BE    GO_WRITE           no, the same date

         MVC   YYDDD,24(R4)       new yyddd

         PACK  W_JULIAN,24(5,R4)  pack yyddd from the dsn
         MVI   W_CENTURY,X'01'    20xx not 19xx

         CONVTOD CONVVAL=WORK16,TODVAL=W_TOD,DATETYPE=YYDDD
         STCKCONV STCKVAL=W_TOD,CONVVAL=WORK16,DATETYPE=YYYYMMDD

         UNPK  W_9,W_5             unpack to be in char

         MVC   DYNNAME,C_Y4MMDD+2 filename (in yymmdd)
         BAL   R6,CREATE_OUTFILE

GO_WRITE EQU   *
         PUT   SEQOUT,0(R4)       copy this rec

         B     GO_LOOP            loop it
*
*--------------------------------------------------------*
*
CREATE_OUTFILE EQU *
*                                                             
        TM    NFIRST,L'NFIRST       not the first time ?      
        BO    CLOSE_OUTPUT          no,  go close the file    
*                                   for the 1st time          
        OI    NFIRST,L'NFIRST       no longer the first       
        B     OPEN_OUTPUT           go open file directly     
*                                                             
CLOSE_OUTPUT  EQU  *                                          
        BAL   R7,CLOSE_SEQ          close this file           
*                                                             
OPEN_OUTPUT   EQU  *                                          
        BAL   R7,OPEN_SEQ           open this file            
*                                                             
        BR    R6                    go back                   
*                                                             
*-------------------------------------------------------*     
*                                                             
*                                                             
*-------------------------------------------------------*     
*                                                             
OPEN_SEQ  EQU  *                                              
        USING S99RBP,RBPTR          req blk ptr dsect         
        USING S99RB,REQBLK          req blk dsect             

** set up SVC99 request-blk                                   
        XC      REQBLK,REQBLK       clear RB                  
        MVI     S99RBLN,REQBLKLN    set RB len                
        MVI     S99VERB,S99VRBAL    set as allocation         
        LA      R2,TUPTR001        ld adr of bg of text-unit-ptr#lst
        ST      R2,S99TXTPP        sotre that adr into RB     
*                                                             
** set up SVC99 request-blk-ptr                               
        LA      R2,REQBLK          load addr of RB            
        ST      R2,S99RBPTR        store RB adr into RB ptr   
        OI      S99RBPTR,S99RBPND   turn on high order bit    
*                                                             
** issue SVC99                                                
        LA     R1,RBPTR             ld adr of RB ptr for svc99
        DYNALLOC                    svc 99                    
*                                                             
        LTR    R15,R15             test if ok                 
        BNZ    Alloc_error         error                      
*                                                             
        OPEN   (SEQOUT,OUTPUT)     open it                    
        BR     R7                                             
*                                                             
Alloc_error    EQU   *                                        
        ST     R15,EXITCODE        save the dynalloc error code C190821
        B      FINISH              halt                       
*                                                             
*--------------------------------------------------------*    
CLOSE_SEQ  EQU   *                                            
*                                                             
        CLOSE  SEQOUT             close this file             
        BR     R7                                             
*                                                             
*--------------------------------------------------------*    
*                                                             
*
*--------------------------------------------------------*
*
CLOSE_FILES EQU  *
         CLOSE (SEQIN,,PRINT)      CLOSE OUTPUT
         BR    R6
*
*--------------------------------------------------------*
*
RETURN   EQU   *
         L     R15,EXITCODE        keep the return code         C190821
         L     R13,4(,R13)         caller's save area           C190821
         ST    R15,16(,R13)        place the return code        C190821
*C190821 RETURN (14,12),RC=0        BACK TO CALLER
         LM    R14,R12,12(R13)     restore caller's regs        C190821
         BR    R14                 return                       C190821
*
*--------------------------------------------------------*
*
         LTORG

EXITCODE  DC   F'0'              return code initiated as 0     C190821
WORK16    DS   0F                work to convert time format
          DS    XL8              HHMMSSTHMIJU0000
W_5       DS   0CL5              5-byte field for unpk source
W_Y4MMDD  DS   0CL4              4-byte yyyymmdd after stckconv
W_CENTURY DS    CL1              x'01' means 20xx
W_JULIAN  DS    CL3              packed yyddd
          DS    F                reserved
*
W_TOD    DS    2F               tod
*
W_9      DS    0CL9             9-byte field for unpk outcome
C_Y4MMDD DS    CL8              8-byte yyyymmdd in character
         DS    CL1              working byte

YYDDD    DC    C'00000'         field to save the julian day

FLAG     DC   X'00'             initiate as low value
         ORG  FLAG              redefine
NFIRST   DS   0XL(B'10000000')  ind. not any out file is opened yet
         ORG
*
*--------------------------------------------------------*
*
*
*--------------------------------------------------------*
*
SEQIN  DCB DSORG=PS,DDNAME=SEQIN,MACRF=GL,EODAD=FINISH,EXLST=JFCBPTR
PRINT  DCB DSORG=PS,DDNAME=PRINT,MACRF=PM,LRECL=80
SEQOUT DCB DSORG=PS,DDNAME=SEQOUT,MACRF=PM
*
JFCB     DS    44F
JFCBPTR  DC    X'87'
         DC    AL3(JFCB)
*
*--------------------------------------------------------*
*
RBPTR     DS        F                                         
REQBLK    DS        CL(S99RBEND-S99RB)                        
REQBLKLN  EQU       L'REQBLK                                  
*                                                             
TUPTR001  DS        0F                                        
          DC        A(TUDDNA1)   adr of tu for ddname         
          DC        A(TUDSNA1)   adr of tu for dsname         
          DC        A(TUDSSA1)   adr of tu for ds status      
          DC        A(TUDDSP1)   adr of tu for norm. disp     
          DC        A(TUSTRK1)   adr of tu for space type     
          DC        A(TUSPRM1)   adr of tu for primary no.    
          DC        A(TUSSEC1)   adr of tu for secondary no.  
          DC        A(TUSRLS1)   adr of tu for rlse           
          DC        A(TUUNIT1)   adr of tu for unit           
          DC        A(TUDBLK1)   adr of tu for block size     
          DC        A(TUDLRC1)   adr of tu for lrecl          
          DC        A(TUDRFM1)   adr of tu for recfm          
*         DC        A(TUVOL1)    adr of tu for volser         
          DC        X'80'        ind. the last tu adr         
          DC        AL3(TUCLSA1)   adr of tu to request unallocation   X
                                   when close file            
*                                                             
TUDDNA1 DC AL2(DALDDNAM),AL2(1),AL2(8),CL8'SEQOUT'            
TUDSNA1 DC AL2(DALDSNAM),AL2(1),AL2(DSNL)                     
DSNTAG  DC C'SYS1.CONSLOG.D'                                    C190326
DYNNAME DC C'000000'                                          
DSNL    EQU *-DSNTAG                                          
TUDSSA1 DC AL2(DALSTATS),AL2(1),AL2(1),X'02'   mod            
TUDDSP1 DC AL2(DALNDISP),AL2(1),AL2(1),X'02'   catlg          
TUSTRK1 DC AL2(DALTRK),AL2(0)       in track                  
TUSPRM1 DC AL2(DALPRIME),AL2(1),AL2(3),X'000280'   (640,   )    D191104
TUSSEC1 DC AL2(DALSECND),AL2(1),AL2(3),X'000280'   (   ,640)    D191104
TUSRLS1 DC AL2(DALRLSE),AL2(0)    space=(trk,(640,640),rlse)    D191104
TUUNIT1 DC AL2(DALUNIT),AL2(1),AL2(5),X'E2E8E2C4C1' sysda    
TUDBLK1 DC AL2(DALBLKSZ),AL2(1),AL2(2),X'6D5E'     blksize=27998 
TUDLRC1 DC AL2(DALLRECL),AL2(1),AL2(2),AL2(137)    lrecl=137     
TUDRFM1 DC AL2(DALRECFM),AL2(1),AL2(1),X'54'       recfm=vba     
TUVOL1  DC AL2(DALVLSER),AL2(1),AL2(6),C'ZOSWRK'   vol=ser=    
TUCLSA1 DC AL2(DALCLOSE),AL2(0)                               
*
        IEFZB4D0   gen. dect for req. blk, text unit et.      
        IEFZB4D2   gen. table of quates for tu keys.          
        IEFJFCBN   gen. jfcb                                  
*
*--------------------------------------------------------*
        END
/*
//*.SYSLMOD DD  DISP=SHR,DSN=ANDREWJ.SOURCE.LMD(PARSCNLG)
//G.SEQIN DD  DISP=SHR,DSN=SYS1.CONSLOG
//G.PRINT DD  SYSOUT=*
//
