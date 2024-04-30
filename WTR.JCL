//WTR      JOB  CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID  
//IEFPROC  EXEC PGM=IASXWR00,PARM='PY,CERTIFY=Y'                    
//IEFRDER  DD DISP=SHR,DSN=SYS1.CONSLOG  
//*SYS1.CONSLOG: RECFM=VBA, BLKSIZE=27998,LRECL=137               
//*                                                       
//PARSCNLG EXEC PGM=PARSCNLG                              
//SEQIN    DD DISP=SHR,DSN=SYS1.CONSLOG                    
//PRINT    DD SYSOUT=*                                    
//                                                        