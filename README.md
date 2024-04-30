# Archiving The MVS SYSLOG to Data Sets Named According to the SYSLOG Records' Date Stamps 

This repository demonstrates how to write a program in Assembler to invoke MVS system's API: DYNALLOC (Dynamic Allocation) or SVC99 to create console log files with a DSNAME as SYS1.CONSLOG.D*yymmdd* , where *yy* stands for 2-digit Year, *mm* for 2-digit Month and *dd* for 2-digit Day.

1. **PARSCNLG.asm** is the program to discern the date format of a record in the MVS SYSLOG and creates new data sets separated the records to different data sets based on the different dates. One data set for each date. 

2. The naming: 'SYS1.CONSLOG' of the output DSNAME can be anything else, just search it in **PARSCNLG.asm** and replace it with any other naming.

3. The volume where the output data sets reside can be fixed to a dedicated VOLSER (volume serial). Unmark ** DC A(TUVOL1) ** and replace **ZOSWRK** with a valid VOLSER in **PARSCNLG.asm**.

4. **WTR.JCL** is the JCL to run PARSCNLG. The first step of **WTR.JCL** uses the IBM-supplied JES2 External Writer: IASXWR00 to save the SYSLOG output to **SYS1.CONSLOG**. *Y* in the parameter: *PY* indicates that the SYSLOG is in JES2 Output Class: **Y**. It can be any other letter depending on whether you had previously issued 'WY' or 'WL' or whatever class to write the SYSLOG records to the JES2 Output Class: Y or L or others.

5. You have to manually issue MVS command: 'P WTR' to let WTR.JCL go on to run the second step: PARSCNLG, IASXWR00 needs a 'P WTR' to stop running when it finished its work.

6. Or you can install the MPF List Exit: **IEF176I.asm** to automatically issue 'P WTR' for you when the console message: IEF176I appears. **IEF176I** message occurs when **IASXWR00** has done the job and waits for 'P WTR' to cease running.

7. The program: **IEF176I.asm** must be compiled and link-edited into an LMD in one of the LNKLST data sets -- **SYS1.USER.LINKLIB** in this sample. Use *** D PROG,LNKLST *** to display the Link List concatenation.  

8. Once the LMD of **IEF176I.asm** generated, and the MPFLST00 -- see **MPFLST00.txt** for example -- is updated, issue **'F LLA,REFRESH'** then **'T MPF=00'** (or any other version).

9. Upload in binary mode ANDREWJ.SOURCE.MAC#.xmit to an LRECL=80 sequential file then receive it to a PDS for macros needed in **IEF176I.asm**.

10. The daily archived SYSLOG will be like the following:
    SYS1.CONSLOG.D240429
    SYS1.CONSLOG.D240430
    SYS1.CONSLOG.D240501
    SYS1.CONSLOG.D240502
    ... 