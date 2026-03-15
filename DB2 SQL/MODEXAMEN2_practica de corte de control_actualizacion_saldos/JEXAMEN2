//KC03C52J JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//             TIME=(,3) ,RESTART=STEP3
//JOBLIB  DD  DSN=DSND10.SDSNLOAD,DISP=SHR                              00020002
//********************************
//*          EJEMPLO             *
//*   IDCAMS DELETE              *
//********************************
//STEP00   EXEC PGM=IDCAMS,COND=(8,LT)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
     DELETE   KC03C52.ARCHIVO.NOVCTA.NICOLE.SORT
     DELETE   KC03C52.ARCHIVO.NOVCTA.NICOLE.VACIO
     DELETE   KC03C52.LISTADO.EXAMEN2
     SET MAXCC=0
//*
//***************************************************************
//* DEFINICION ARCHIVO ORDENADO DE ENTRADA  VACIO               *
//***************************************************************
//STEP01   EXEC PGM=IEFBR14
//DD1      DD DSN=KC03C52.ARCHIVO.NOVCTA.NICOLE.VACIO,UNIT=SYSDA,
//            DCB=(LRECL=13,BLKSIZE=0,RECFM=FB),
//            SPACE=(TRK,(1,1),RLSE),DISP=(,CATLG)
//*
//***************************************************************
//* DEFINICION ARCHIVO ORDENADO DE ENTRADA                      *
//***************************************************************
//STEP02   EXEC PGM=IEFBR14
//DD1      DD DSN=KC03C52.ARCHIVO.NOVCTA.NICOLE.SORT,UNIT=SYSDA,
//            DCB=(LRECL=13,BLKSIZE=0,RECFM=FB),
//            SPACE=(TRK,(1,1),RLSE),DISP=(,CATLG)
//*
//***************************************************************
//* DEFINICION DE ARCHIVO DE SALIDA LISTADO                     *
//***************************************************************
//STEP03   EXEC PGM=IEFBR14
//DD1      DD DSN=KC03C52.LISTADO.EXAMEN2,UNIT=SYSDA,
//            DCB=(LRECL=133,BLKSIZE=0,RECFM=FBA),
//            SPACE=(TRK,(1,1),RLSE),DISP=(,CATLG)
//*
//***************************************************************
//*     SORT POR TIPO DE CUENTA Y NRO DE CUENTA                 *
//***************************************************************
//*STEP04    EXEC PGM=SORT,COND=EVEN
//*SYSOUT    DD SYSOUT=*
//*SORTIN    DD DSN=KC03C52.ARCHIVO.NOVCTA.NICOLE,DISP=SHR
//*SORTOUT   DD DSN=KC03C52.ARCHIVO.NOVCTA.NICOLE.SORT,DISP=OLD,
//*          UNIT=SYSDA,
//*          DCB=(LRECL=13,BLKSIZE=0,RECFM=FB),
//*          SPACE=(TRK,(1,1),RLSE)
//*SYSIN     DD *
//*SORT     FORMAT=BI,FIELDS=(1,2,A,3,3,A)
//*  SORT       FIELDS=(1,2,CH,A,3,3,PD,A,12,2,CH)
//*
//***************************************************************
//*      EJECUTAR PROGRAMA COBOL CON SQL EMBEBIDO               *
//***************************************************************
//STEP05   EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)                    00030002
//SYSTSPRT DD SYSOUT=*                                                  00040002
//DDENTRA  DD DSN=KC03C52.ARCHIVO.NOVCTA.NICOLE,DISP=SHR
//*DDENTRA  DD DUMMY,DISP=SHR
//DDSALE   DD DSN=KC03C52.LISTADO.EXAMEN2,DISP=OLD,
//          UNIT=SYSDA,
//          DCB=(LRECL=133,BLKSIZE=0,RECFM=FBA),
//          SPACE=(TRK,(1,1),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSTSIN  DD *                                                         00050002
  DSN SYSTEM(DBDG)                                                      00060002
  RUN  PROGRAM(EXAMEN2) PLAN(CURSOC52) +                                00554000
      LIB('KC03C52.CURSOS.PGMLIB')                                      00560000
  END                                                                   00085007
//SYSPRINT DD SYSOUT=*                                                  00090002
//SYSUDUMP DD SYSOUT=*                                                  00100002
//SYSIN    DD *                                                         00110002
//*
//
