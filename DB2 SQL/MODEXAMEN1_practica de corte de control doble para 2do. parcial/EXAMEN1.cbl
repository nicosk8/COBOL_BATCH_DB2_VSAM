       IDENTIFICATION DIVISION.                                         00002000
      *                                                        *        00003000
       PROGRAM-ID. EXAMEN1.                                             00004010
      **********************************************************        00005000
      *                                                        *        00006000
      *  PROGRAMA PARA SQL EMBEBIDO                            *        00007000
      *                                                        *        00016500
      **********************************************************        00016800
      *      MANTENIMIENTO DE PROGRAMA                         *        00016900
      **********************************************************        00017000
      *  FECHA   *    DETALLE        * COD *                            00018000
      **************************************                            00019000
      *          *                   *     *                            00019100
      *          *                   *     *                            00019200
      **************************************                            00019300
       ENVIRONMENT DIVISION.                                            00019400
       CONFIGURATION SECTION.                                           00019500
       SPECIAL-NAMES.                                                   00019600
           DECIMAL-POINT IS COMMA.                                      00019700
                                                                        00019800
       INPUT-OUTPUT SECTION.                                            00019900
       FILE-CONTROL.                                                    00020000
                                                                        00020600
             SELECT ENTRADA ASSIGN DDENTRA                              00021001
             FILE STATUS IS FS-ENTRADA.                                 00022301
                                                                        00022401
             SELECT SALIDA  ASSIGN DDSALE                               00022501
             FILE STATUS IS FS-SALIDA.                                  00022601
                                                                        00022701
       DATA DIVISION.                                                   00022801
       FILE SECTION.                                                    00022901
       FD ENTRADA                                                       00040101
             BLOCK CONTAINS 0 RECORDS                                   00040200
             RECORDING MODE IS F.                                       00040300
                                                                        00040400
       01 REG-ENTRADA     PIC X(26).                                    00040513
                                                                        00040600
       FD SALIDA                                                        00040701
             BLOCK CONTAINS 0 RECORDS                                   00040801
             RECORDING MODE IS F.                                       00040901
                                                                        00041001
       01 REG-SALIDA      PIC X(132).                                   00041101
                                                                        00041201
      **************************************                            00041301
       WORKING-STORAGE SECTION.                                         00041401
      **************************************                            00041501
       77  FILLER          PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.00041601
                                                                        00041701
      ****************************************                          00041801
      * FILE STATUS SALIDA LISTADO Y SQLCODE *                          00041901
      ****************************************                          00042001
       77  FS-ENTRADA               PIC XX    VALUE SPACES.             00042101
       77  FS-SALIDA                PIC XX    VALUE SPACES.             00042201
       77  FS-ACTUAL                PIC XX    VALUE SPACES.             00042302
       77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          00042402
                                                                        00042502
      ****************************************                          00042602
      * CLAVES DE CORTE DE CONTROL           *                          00042707
      ****************************************                          00042802
       01  WS-CLAVE-CORTE-MAYOR.                                        00042907
           03  WS-TIPDOC-ANT       PIC X(02) VALUE SPACES.              00043008
                                                                        00043107
       01  WS-CLAVE-CORTE-MENOR.                                        00043207
           03  WS-SEXO-ANT         PIC X(01) VALUE SPACES.              00043307
                                                                        00043407
      ****************************************                          00043507
      * CONTADORES                           *                          00043607
      ****************************************                          00043707
       01  WS-CONTADORES.                                               00043807
           03  WS-TOT-GRAL             PIC 9(03) VALUE ZEROS.           00043907
           03  WS-SUB-TOT-SEXO         PIC 9(03) VALUE ZEROS.           00044007
           03  WS-CONTADORES-TIPDOC.                                    00045007
               05  WS-CANT-DU          PIC 9(03) VALUE ZEROS.           00045207
               05  WS-CANT-PA          PIC 9(03) VALUE ZEROS.           00045307
               05  WS-CANT-PE          PIC 9(03) VALUE ZEROS.           00045407
           03  WS-CONTADORES-SEXO.                                      00045507
               05  WS-CANT-F           PIC 9(03) VALUE ZEROS.           00045907
               05  WS-CANT-M           PIC 9(03) VALUE ZEROS.           00046007
               05  WS-CANT-O           PIC 9(03) VALUE ZEROS.           00046107
               05  WS-CANT-DU-F        PIC 9(03) VALUE ZEROS.           00046207
               05  WS-CANT-DU-M        PIC 9(03) VALUE ZEROS.           00046307
               05  WS-CANT-DU-O        PIC 9(03) VALUE ZEROS.           00046407
               05  WS-CANT-PA-F        PIC 9(03) VALUE ZEROS.           00046507
               05  WS-CANT-PA-M        PIC 9(03) VALUE ZEROS.           00046607
               05  WS-CANT-PA-O        PIC 9(03) VALUE ZEROS.           00046707
               05  WS-CANT-PE-F        PIC 9(03) VALUE ZEROS.           00046807
               05  WS-CANT-PE-M        PIC 9(03) VALUE ZEROS.           00046907
               05  WS-CANT-PE-O        PIC 9(03) VALUE ZEROS.           00047007
           03  WS-ENTRADA-LEIDOS       PIC 9(03) VALUE ZEROS.           00047107
           03  WS-ENTRADA-ERROR        PIC 9(03) VALUE ZEROS.           00047207
           03  WS-C1-LEIDOS            PIC 9(03) VALUE ZEROS.           00047308
           03  WS-GRABADOS             PIC 9(03) VALUE ZEROS.           00047408
                                                                        00047508
      *****************************************                         00047608
      *  BANDERAS/FLAGS                       *                         00047708
      *****************************************                         00047808
       01  WS-FLAG-INICIO           PIC X.                              00047908
           88  WS-NO-INICIO            VALUE 'F'.                       00048008
           88  WS-SI-INICIO            VALUE 'T'.                       00048108
                                                                        00048208
       01  WS-FLAG-PROCESO          PIC X VALUE 'T'.                    00048308
           88  WS-SI-PROCESO           VALUE 'T'.                       00048408
           88  WS-FIN-PROCESO          VALUE 'F'.                       00048508
                                                                        00048608
       01  WS-FLAG-CURSOR           PIC X VALUE 'T'.                    00048708
           88  WS-SI-CURSOR            VALUE 'T'.                       00048808
           88  WS-FIN-CURSOR           VALUE 'F'.                       00048908
                                                                        00049008
       01  WS-CURSOR-ABIERTO        PIC X VALUE 'T'.                    00049108
           88  WS-CURSOR-ABIERTO-SI    VALUE 'T'.                       00049208
           88  WS-CURSOR-ABIERTO-NO    VALUE 'F'.                       00049308
                                                                        00049408
       01  WS-ESTADO-ENTRADA        PIC X VALUE 'F'.                    00049508
           88  WS-FIN-ENTRADA          VALUE 'T'.                       00049608
                                                                        00049708
      *****************************************                         00049808
      *  VARIABLES AUXILARES.                *                          00049908
      *****************************************                         00050008
       01 WS-AUXILIARES.                                                00051008
          03  WS-PARRAFO            PIC X(30) VALUE SPACES.             00052802
          03  WS-ARCHIVO-DESC       PIC X(30) VALUE SPACES.             00052902
          03  WS-FECHA-AUX          PIC X(10).                          00053500
          03  WS-CUENTA-LINEA       PIC 9(02) VALUE ZEROS.              00053600
          03  WS-FECHA-DB2.                                             00053700
              05 WS-DB2-AAAA        PIC X(04).                          00053800
              05 FILLER             PIC X VALUE '-'.                    00053900
              05 WS-DB2-MM          PIC X(02).                          00054000
              05 FILLER             PIC X VALUE '-'.                    00054100
              05 WS-DB2-DD          PIC X(02).                          00054200
                                                                        00054300
      *****************************************                         00055300
      *   IMPRESION DE TITULOS LISTADO        *                         00055400
      *****************************************                         00055500
       01 WS-LINEA       PIC X(132) VALUE ALL '-'.                      00055700
                                                                        00055800
       01 WS-TITULO.                                                    00055900
          03  FILLER     PIC X(05) VALUE SPACES.                        00056000
          03  WS-TITULO-LEYENDA     PIC X(33) VALUE SPACES.             00057200
      *       TIPO DE TABLA CLI =  'CONTROL DE INTEGRIDAD DE CLIENTES'. 00058000
      *       TIPO DE TABLA CTA =  'CONTROL DE INTEGRIDAD DE CUENTAS '. 00058100
          03  FILLER                PIC X(28)    VALUE SPACES.          00058200
          03  FILLER                PIC X(10)    VALUE '   FECHA: '.    00058300
          03  WS-FECHA.                                                 00059000
              05  WS-DD             PIC 9(02).                          00060000
              05  FILLER            PIC X       VALUE '/'.              00061000
              05  WS-MM             PIC 9(02).                          00062000
              05  FILLER            PIC X       VALUE '/'.              00063000
              05  WS-AAAA           PIC 9(04).                          00064000
          03  FILLER                PIC X(20)    VALUE SPACES.          00065000
          03  FILLER                PIC X(13)    VALUE 'NRO. PAGINA: '. 00065100
          03  WS-NRO-PAGINA         PIC 9(02)    VALUE ZEROES.          00065200
          03  FILLER                PIC X(08)    VALUE SPACES.          00065300
                                                                        00065900
      ***************************************************               00071800
      *  SUBTITULOS DE LISTADO TABLA DE CLIENTES        *               00071900
      ***************************************************               00072000
                                                                        00072100
       01 WS-SUBTITULO-TBCURCLI.                                        00072200
          03 FILLER                 PIC X     VALUE '|'.                00072300
          03 FILLER                 PIC X(16) VALUE                     00072400
                         ' TIPO DOCUMENTO '.                            00072500
          03 FILLER                 PIC X     VALUE '|'.                00072600
          03 FILLER                 PIC X(15) VALUE                     00072700
                         ' NRO DOCUMENTO '.                             00072800
          03 FILLER                 PIC X     VALUE '|'.                00073200
          03 FILLER                 PIC X(16) VALUE                     00073300
                         ' NRO DE CLIENTE '.                            00073400
          03 FILLER                 PIC X     VALUE '|'.                00073500
          03 FILLER                 PIC X(30) VALUE                     00073600
                         '   NOMBRE Y APELLIDO         '.               00073700
          03 FILLER                 PIC X     VALUE '|'.                00073800
          03 FILLER                 PIC X(23) VALUE                     00073900
                         '  FECHA DE NACIMIENTO  '.                     00074000
          03 FILLER                 PIC X     VALUE '|'.                00074100
          03 FILLER                 PIC X(10) VALUE                     00074200
                         '   SEXO   '.                                  00074300
                                                                        00074400
      ****************************************************              00074500
      *         LAYOUT TABLA DE CLIENTES                 *              00074600
      ****************************************************              00074700
       01 WS-REG-SALIDA-CLI.                                            00074800
          03  FILLER              PIC X         VALUE '|'.              00074900
          03  FILLER              PIC X(04)     VALUE SPACES.           00075000
          03  REG-CLI-TIPDOC      PIC X(02)     VALUE SPACES.           00075100
          03  FILLER              PIC X(10)     VALUE SPACES.           00075200
          03  FILLER              PIC X         VALUE '|'.              00075300
          03  REG-CLI-NRODOC      PIC Z(11)     VALUE ZEROS.            00075400
          03  FILLER              PIC X(04)     VALUE SPACES.           00075500
          03  FILLER              PIC X         VALUE '|'.              00075600
          03  REG-CLI-NROCLI      PIC Z(03)     VALUE ZEROS.            00075700
          03  FILLER              PIC X(13)     VALUE SPACES.           00075800
          03  FILLER              PIC X         VALUE '|'.              00075900
          03  REG-CLI-NOMAPE      PIC X(30)     VALUE ZEROS.            00076000
          03  FILLER              PIC X         VALUE '|'.              00076100
          03  FILLER              PIC X(07)     VALUE SPACES.           00076200
          03  REG-CLI-FECNAC.                                           00076300
              05 REG-FECNAC-AAAA  PIC 9(04)     VALUE ZEROES.           00076400
              05 FILLER           PIC X         VALUE '/'.              00076500
              05 REG-FECNAC-MM    PIC 9(02)     VALUE ZEROES.           00076600
              05 FILLER           PIC X         VALUE '/'.              00076700
              05 REG-FECNAC-DD    PIC 9(02)     VALUE ZEROES.           00076800
          03  FILLER              PIC X(06)     VALUE SPACES.           00076900
          03  FILLER              PIC X         VALUE '|'.              00077000
          03  FILLER              PIC X(05)     VALUE SPACES.           00077100
          03  REG-CLI-SEXO        PIC X         VALUE SPACES.           00077200
                                                                        00077300
      ****************************************************              00077400
      *  SUBTITULOS DE LISTADO TABLA DE CUENTAS          *              00077500
      ****************************************************              00077600
       01 WS-SUBTITULO-TBCURCTA.                                        00077700
          03 FILLER                 PIC X     VALUE '|'.                00077800
          03 FILLER                 PIC X(16) VALUE                     00077900
                         ' TIPO DE CUENTA '.                            00078000
          03 FILLER                 PIC X     VALUE '|'.                00078100
          03 FILLER                 PIC X(15) VALUE                     00078200
                         ' NRO DE CUENTA '.                             00078300
          03 FILLER                 PIC X     VALUE '|'.                00078400
          03 FILLER                 PIC X(17) VALUE                     00078500
                         ' SUCURSAL CUENTA '.                           00078600
          03 FILLER                 PIC X     VALUE '|'.                00078700
          03 FILLER                 PIC X(16) VALUE                     00078800
                         ' NRO DE CLIENTE '.                            00078900
          03 FILLER                 PIC X     VALUE '|'.                00079300
          03 FILLER                 PIC X(10) VALUE                     00079400
                         '  SALDO  '.                                   00079500
          03 FILLER                 PIC X     VALUE '|'.                00079600
          03 FILLER                 PIC X(21) VALUE                     00079700
                         '  FECHA ACTUALIZACION'.                       00079800
                                                                        00079900
      *****************************************                         00080000
      *         LAYOUT TABLA DE CUENTAS       *                         00080100
      *****************************************                         00080200
       01 WS-REG-SALIDA-CTA.                                            00080300
          03  FILLER              PIC X         VALUE '|'.              00080400
          03  FILLER              PIC X(04)     VALUE SPACES.           00080500
          03  REG-CTA-TIPCUEN     PIC X(02)     VALUE SPACES.           00080600
          03  FILLER              PIC X(10)     VALUE SPACES.           00080700
          03  FILLER              PIC X         VALUE '|'.              00080800
          03  REG-CTA-NROCUEN     PIC Z(05)     VALUE ZEROS.            00080900
          03  FILLER              PIC X(10)     VALUE SPACES.           00081000
          03  FILLER              PIC X         VALUE '|'.              00081100
          03  REG-CTA-SUCUEN      PIC Z(02)     VALUE ZEROS.            00081200
          03  FILLER              PIC X(15)     VALUE SPACES.           00081300
          03  FILLER              PIC X         VALUE '|'.              00081400
          03  REG-CTA-NROCLI      PIC Z(03)     VALUE ZEROS.            00081500
          03  FILLER              PIC X(13)     VALUE SPACES.           00081600
          03  FILLER              PIC X         VALUE '|'.              00081700
          03  REG-CTA-SALDO       PIC $Z.ZZ9,99- VALUE ZEROS.           00082000
          03  FILLER              PIC X         VALUE '|'.              00082100
          03  FILLER              PIC X(03)     VALUE SPACES.           00082200
          03  REG-CTA-FECSAL.                                           00082300
              05 REG-FECSAL-AAAA  PIC 9(04)     VALUE ZEROES.           00082400
              05 FILLER           PIC X         VALUE '/'.              00082500
              05 REG-FECSAL-MM    PIC 9(02)     VALUE ZEROES.           00082600
              05 FILLER           PIC X         VALUE '/'.              00082700
              05 REG-FECSAL-DD    PIC 9(02)     VALUE ZEROES.           00082800
                                                                        00082900
      *****************************************                         00084607
      *   FIN IMPRESION DE TITULOS LISTADO    *                         00084707
      *****************************************                         00084807
       77  FILLER        PIC X(26) VALUE '* VARIABLES SQL          *'.  00084907
                                                                        00085007
      ********************************************************          00085107
      *  AREA DE COPYS                                       *          00085207
      ********************************************************          00085307
                                                                        00085407
      **   AGREGAR ACA LA COPY DE ARCHIVO DE ENTRADA        **          00085507
      *     COPY NOVECLIE.  <= EJEMPLO                                  00085607
                                                                        00086007
      ********************************************************          00090400
      *  AREA DE COMUNICACION Y DEFINICION SQL DB2           *          00090500
      ********************************************************          00090600
            EXEC SQL                                                    00090700
              INCLUDE SQLCA                                             00090800
            END-EXEC.                                                   00090900
                                                                        00091000
            EXEC SQL                                                    00091100
              INCLUDE TBCURCTA                                          00091200
            END-EXEC.                                                   00091300
                                                                        00091400
            EXEC SQL                                                    00091500
              INCLUDE TBCURCLI                                          00091600
            END-EXEC.                                                   00091700
                                                                        00091800
      ***************************************************               00091900
      * CURSOR C1 QUE METRAE REGISTROS DE CLIENTES      *               00092012
      ***************************************************               00092100
            EXEC SQL                                                    00092200
              DECLARE C1 CURSOR FOR                                     00092300
              SELECT *                                                  00092400
                FROM KC02803.TBCURCLI                                   00092500
              ORDER BY TIPDOC, SEXO ASC                                 00092612
            END-EXEC.                                                   00092700
                                                                        00092800
      ***************************************************               00096300
      * CURSOR C4 QUE METRAE REGISTROS DE CUENTAS       *               00096412
      ***************************************************               00096500
            EXEC SQL                                                    00096600
              DECLARE C4 CURSOR FOR                                     00096700
              SELECT *                                                  00096800
                FROM KC02803.TBCURCTA                                   00096900
            END-EXEC.                                                   00097000
                                                                        00102100
       77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.  00102200
                                                                        00102300
      ***************************************************************.  00102400
       PROCEDURE DIVISION.                                              00102500
      **************************************                            00102600
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            00102700
      **************************************                            00102800
       MAIN-PROGRAM.                                                    00102900
                                                                        00103000
           PERFORM 1000-I-INICIO   THRU                                 00103100
                   1000-F-INICIO.                                       00103200
                                                                        00103300
           PERFORM 2000-I-PROCESO                                       00103400
              THRU 2000-F-PROCESO                                       00103500
             UNTIL WS-FIN-PROCESO OR WS-FIN-CURSOR.                     00103707
                                                                        00103820
           PERFORM 2300-I-CORTE-MENOR                                   00103926
              THRU 2300-F-CORTE-MENOR                                   00104026
                                                                        00104126
           PERFORM 2200-I-CORTE-MAYOR                                   00104226
              THRU 2200-F-CORTE-MAYOR                                   00104326
                                                                        00104400
           PERFORM 9999-I-FINAL    THRU                                 00104500
                   9999-F-FINAL.                                        00104600
                                                                        00104700
       F-MAIN-PROGRAM. GOBACK.                                          00104800
                                                                        00104900
      *************************************                             00105025
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            00105100
      **************************************                            00105200
       1000-I-INICIO.                                                   00105300
                                                                        00105402
           MOVE FUNCTION CURRENT-DATE TO WS-FECHA-AUX.                  00105500
           MOVE WS-FECHA-AUX(1:4)     TO WS-AAAA.                       00105600
           MOVE WS-FECHA-AUX(5:2)     TO WS-MM.                         00105700
           MOVE WS-FECHA-AUX(7:2)     TO WS-DD.                         00105800
           MOVE 16 TO WS-CUENTA-LINEA                                   00105900
                                                                        00106000
           SET WS-SI-INICIO  TO TRUE.                                   00106103
           SET WS-SI-PROCESO TO TRUE.                                   00107003
                                                                        00109800
           PERFORM 8000-I-APERTURA-ARCHIVOS                             00111002
              THRU 8000-F-APERTURA-ARCHIVOS                             00111102
                                                                        00111202
           IF FS-ENTRADA = '00' AND  FS-SALIDA  = '00'                  00111314
              PERFORM 8200-I-ABRIR-CURSOR                               00111414
                 THRU 8200-F-ABRIR-CURSOR                               00111514
           ELSE                                                         00111614
              DISPLAY '*********************************'               00111714
              DISPLAY '* ERROR EN APERTURA DE ARCHIVOS *'               00111814
              DISPLAY '* APERTURA DE CURSOR CANCELADA  *'               00111914
              DISPLAY '*********************************'               00112014
           END-IF                                                       00112114
                                                                        00113303
      *    PERFORM 8005-I-LECTURA-INICIAL                               00113407
      *       THRU 8005-F-LECTURA-INICIAL.                              00113507
                                                                        00113607
      *    LECTURA INICIAL DE CURSOR.                                   00113707
           PERFORM 8400-I-LEER-CURSOR                                   00113807
              THRU 8400-F-LEER-CURSOR                                   00113907
                                                                        00114007
      *    CARGO LAS VARIABLES DE CORTE MAYOR Y MENOR.                  00114107
      *    PARA QUE ENTRE A PROCESAR.                                   00114207
           MOVE WT-TIPDOC TO WS-TIPDOC-ANT                              00114307
           MOVE WT-SEXO   TO WS-SEXO-ANT.                               00114407
                                                                        00114507
       1000-F-INICIO.   EXIT.                                           00114607
                                                                        00115007
      **************************************                            00120602
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            00120702
      **************************************                            00120802
       2000-I-PROCESO.                                                  00120902
                                                                        00121622
            IF WT-TIPDOC  = 'DU' OR                                     00121723
               WT-TIPDOC  = 'PA' OR                                     00121823
               WT-TIPDOC  = 'PE'                                        00121923
                                                                        00122322
                  IF WT-TIPDOC = WS-TIPDOC-ANT                          00122422
                                                                        00122522
                        ADD 1 TO WS-TOT-GRAL                            00122622
                                                                        00122722
                        IF WT-SEXO = WS-SEXO-ANT                        00122822
                            ADD 1 TO WS-SUB-TOT-SEXO                    00122922
                        ELSE                                            00123022
      *                |----------------- CORTE DE CLAVE MENOR ---------00123122
                            PERFORM 2300-I-CORTE-MENOR                  00123222
                               THRU 2300-F-CORTE-MENOR                  00124022
                        END-IF                                          00128022
                  END-IF                                                00128122
                                                                        00128222
      *          |----------------------- CORTE DE CLAVE MAYOR ---------00128322
                  IF WT-TIPDOC NOT = WS-TIPDOC-ANT                      00128422
                       PERFORM 2300-I-CORTE-MENOR                       00128622
                          THRU 2300-F-CORTE-MENOR                       00128722
                       PERFORM 2200-I-CORTE-MAYOR                       00128822
                          THRU 2200-F-CORTE-MAYOR                       00128922
                  END-IF                                                00130222
                                                                        00130322
                  PERFORM 2900-I-ARMAR-REG-SALIDA-CLI                   00130422
                     THRU 2900-F-ARMAR-REG-SALIDA-CLI                   00130522
                                                                        00130622
                  PERFORM 3000-I-GRABAR-LISTADO                         00130722
                     THRU 3000-F-GRABAR-LISTADO                         00130822
            ELSE                                                        00130923
                  ADD 1 TO WS-ENTRADA-ERROR                             00131023
            END-IF.                                                     00131122
                                                                        00132022
      *    VOY A LEER EL SIGUIENTE REGISTRO QUE ACTUALIZA CLAVES        00137222
      *    MAYOR Y MENOR ACTUALES Y VUELVE A PROCESAR.                  00137322
      *     PERFORM 8050-I-LEER-ENTRADA                                 00137422
      *        THRU 8050-F-LEER-ENTRADA                                 00137522
            PERFORM 8400-I-LEER-CURSOR                                  00137622
               THRU 8400-F-LEER-CURSOR                                  00137722
            .                                                           00139017
       2000-F-PROCESO. EXIT.                                            00140007
                                                                        00140115
      ******************************************************************00141015
      * CORTE DE CLAVE MAYOR                                            00142015
      ******************************************************************00143015
       2200-I-CORTE-MAYOR.                                              00150015
      *     DISPLAY '+++++ CORTE MAYOR +++++'                           00150127
      *     ME GUARDO EL TOTAL GENERAL DE LA CLAVE MAYOR                00150215
            EVALUATE WS-TIPDOC-ANT                                      00150315
            WHEN 'DU'                                                   00150415
                    MOVE  WS-TOT-GRAL TO WS-CANT-DU                     00150515
      *             DISPLAY '*** CANTIDAD DU = ' WS-CANT-DU             00150627
            WHEN 'PA'                                                   00150715
                    MOVE  WS-TOT-GRAL TO WS-CANT-PA                     00150815
      *             DISPLAY '*** CANTIDAD PA = ' WS-CANT-PA             00150927
            WHEN 'PE'                                                   00151015
                    MOVE  WS-TOT-GRAL TO WS-CANT-PE                     00151115
      *             DISPLAY '*** CANTIDAD PE = ' WS-CANT-PE             00151227
            END-EVALUATE                                                00151315
                                                                        00151415
      *     RESETEO EN 1 EL CONTADOR GENERAL                            00151515
      *     RESETEO EN 0 EL CONTADOR PARCIAL DE LA CLAVE MENOR          00151615
      *     ACTUALIZO LA CLAVE MAYOR.                                   00151715
            MOVE 1         TO WS-TOT-GRAL                               00151815
            MOVE 1         TO WS-SUB-TOT-SEXO                           00151924
            MOVE WT-TIPDOC TO WS-TIPDOC-ANT                             00152015
            .                                                           00152115
       2200-F-CORTE-MAYOR. EXIT.                                        00152216
                                                                        00152315
      ******************************************************************00152415
      * CORTE DE CLAVE MENOR                                            00152515
      ******************************************************************00152615
       2300-I-CORTE-MENOR.                                              00152715
      *     DISPLAY '----- CORTE MENOR -----'                           00152829
      *    GUARDO EL TOTAL PARCIAL DE LA CLAVE MENOR                    00152915
           EVALUATE WS-TIPDOC-ANT                                       00153015
           WHEN 'DU'                                                    00153115
                     EVALUATE WS-SEXO-ANT                               00153215
                     WHEN 'F'                                           00153315
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-DU-F       00153419
      *                      DISPLAY '>>> CANTIDAD DU-F = ' WS-CANT-DU-F00153527
                     WHEN 'M'                                           00153615
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-DU-M       00153719
      *                      DISPLAY '>>> CANTIDAD DU-M = ' WS-CANT-DU-M00153827
                     WHEN 'O'                                           00153915
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-DU-O       00154019
      *                      DISPLAY '>>> CANTIDAD DU-O = ' WS-CANT-DU-O00154127
                     END-EVALUATE                                       00154215
           WHEN 'PA'                                                    00154315
                     EVALUATE WS-SEXO-ANT                               00154415
                     WHEN 'F'                                           00154515
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-PA-F       00154619
      *                      DISPLAY '>>> CANTIDAD PA-F = ' WS-CANT-PA-F00154727
                     WHEN 'M'                                           00154815
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-PA-M       00154919
      *                      DISPLAY '>>> CANTIDAD PA-M = ' WS-CANT-PA-M00155027
                     WHEN 'O'                                           00155115
                              MOVE WS-SUB-TOT-SEXO TO WS-CANT-PA-O      00155215
      *                      DISPLAY '>>> CANTIDAD PA-O = ' WS-CANT-PA-O00155327
                     END-EVALUATE                                       00155415
           WHEN 'PE'                                                    00155515
                     EVALUATE WS-SEXO-ANT                               00155615
                     WHEN 'F'                                           00155715
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-PE-F       00155819
      *                      DISPLAY '>>> CANTIDAD PE-F = ' WS-CANT-PE-F00155927
                     WHEN 'M'                                           00156015
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-PE-M       00156119
      *                      DISPLAY '>>> CANTIDAD PE-M = ' WS-CANT-PE-M00156227
                     WHEN 'O'                                           00156315
                             MOVE WS-SUB-TOT-SEXO TO WS-CANT-PE-O       00156419
      *                      DISPLAY '>>> CANTIDAD PE-O = ' WS-CANT-PE-O00156527
                     END-EVALUATE                                       00156615
           END-EVALUATE                                                 00156715
      *    Y RESETEO EN 1 EL CONTADOR PARCIAL                           00156915
      *    ACTUALIZO LA CLAVE MENOR                                     00157024
      *    LA CLAVE DE CORTE MAYOR QUEDA INTACTA.                       00157115
           MOVE 1         TO WS-SUB-TOT-SEXO                            00157218
           MOVE WT-SEXO   TO WS-SEXO-ANT                                00157315
           .                                                            00157415
       2300-F-CORTE-MENOR. EXIT.                                        00157515
                                                                        00158015
      ***********************************************************       00166302
      *  ARMARDO DE SALIDA REGISTRO DE CLIENTE                  *       00166402
      ***********************************************************       00166502
       2900-I-ARMAR-REG-SALIDA-CLI.                                     00166602
           MOVE  WT-TIPDOC       TO REG-CLI-TIPDOC                      00166702
           MOVE  WT-NRODOC       TO REG-CLI-NRODOC                      00166802
           MOVE  WT-NROCLI       TO REG-CLI-NROCLI                      00166902
           MOVE  WT-NOMAPE       TO REG-CLI-NOMAPE                      00167002
           MOVE  WT-FECNAC(1:4)  TO REG-FECNAC-AAAA                     00167102
           MOVE  WT-FECNAC(6:2)  TO REG-FECNAC-MM                       00167202
           MOVE  WT-FECNAC(9:2)  TO REG-FECNAC-DD                       00167302
           MOVE  WT-SEXO         TO REG-CLI-SEXO.                       00167402
       2900-F-ARMAR-REG-SALIDA-CLI. EXIT.                               00167502
                                                                        00167602
      ***********************************************************       00167702
      *  ARMARDO DE SALIDA REGISTRO DE CUENTA                   *       00167802
      ***********************************************************       00167902
       2950-I-ARMAR-REG-SALIDA-CTA.                                     00168002
           MOVE  WS-TIPCUEN      TO REG-CTA-TIPCUEN                     00168102
           MOVE  WS-NROCUEN      TO REG-CTA-NROCUEN                     00168202
           MOVE  WS-SUCUEN       TO REG-CTA-SUCUEN                      00168302
           MOVE  WS-NROCLI       TO REG-CTA-NROCLI                      00168402
           MOVE  WS-SALDO        TO REG-CTA-SALDO                       00168502
           MOVE  WS-FECSAL(1:4)  TO REG-FECSAL-AAAA                     00168602
           MOVE  WS-FECSAL(6:2)  TO REG-FECSAL-MM                       00168702
           MOVE  WS-FECSAL(9:2)  TO REG-FECSAL-DD.                      00168802
       2950-F-ARMAR-REG-SALIDA-CTA. EXIT.                               00168902
                                                                        00169002
      **************************************                            00169102
      *  GRABAR LISTADO ERRORES            *                            00169202
      **************************************                            00169302
       3000-I-GRABAR-LISTADO.                                           00169403
                                                                        00169502
           MOVE '3000-I-GRABAR-LISTADO' TO WS-PARRAFO.                  00169603
                                                                        00169702
           IF WS-CUENTA-LINEA > 15                                      00169802
                PERFORM 9000-I-GRABAR-TITULOS                           00169902
                   THRU 9000-F-GRABAR-TITULOS                           00170002
           END-IF                                                       00170102
                                                                        00170219
      *    DISPLAY '>>>REG A GRABAR = ' WS-REG-SALIDA-CLI               00170328
           WRITE REG-SALIDA FROM WS-REG-SALIDA-CLI  AFTER 1             00170412
                                                                        00170600
      *    WRITE REG-SALIDA FROM WS-REG-SALIDA-CTA  AFTER 1             00170812
                                                                        00171000
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00171102
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00171202
                                                                        00171300
           ADD  1   TO  WS-GRABADOS                                     00171400
           ADD  1   TO  WS-CUENTA-LINEA.                                00171500
                                                                        00171600
       3000-F-GRABAR-LISTADO. EXIT.                                     00171708
                                                                        00183300
      **************************************                            00183402
      * APERTURA DE ARCHIVOS               *                            00183502
      **************************************                            00183602
       8000-I-APERTURA-ARCHIVOS.                                        00183702
                                                                        00183802
           MOVE '8000-I-APERTURA-ARCHIVOS' TO WS-PARRAFO                00183902
                                                                        00184002
           OPEN INPUT  ENTRADA.                                         00184102
           MOVE FS-ENTRADA TO FS-ACTUAL                                 00184202
           MOVE 'ARCHIVO ENTRADA' TO WS-ARCHIVO-DESC                    00184302
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00184402
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00184502
                                                                        00184602
           OPEN OUTPUT SALIDA.                                          00184702
           MOVE FS-SALIDA  TO FS-ACTUAL                                 00184802
           MOVE 'ARCHIVO LISTADO' TO WS-ARCHIVO-DESC                    00184902
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00185002
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00185102
                                                                        00185202
           .                                                            00186214
       8000-F-APERTURA-ARCHIVOS. EXIT.                                  00186302
                                                                        00186403
      **************************************                            00186503
      * LECTURA INICIAL DE ARCHIVO ENTRADA *                            00186603
      **************************************                            00186703
       8005-I-LECTURA-INICIAL.                                          00186803
                                                                        00186903
           MOVE '8005-I-LECTURA-INICIAL'  TO WS-PARRAFO                 00187003
                                                                        00187103
      *    READ  ENTRADA INTO WS-REG-ENTRADA.                           00187204
           MOVE FS-ENTRADA TO FS-ACTUAL                                 00187303
           MOVE 'LECTURA ENTRADA' TO WS-ARCHIVO-DESC                    00187403
           IF FS-ACTUAL  IS NOT EQUAL '00'                              00187503
              DISPLAY '*****************************************'       00187603
              DISPLAY '*     *   FILE STATUS ERROR   *         *'       00187703
              DISPLAY '*****************************************'       00187803
              DISPLAY '*   EL ARCHIVO ENTRADA ESTA VACIO       *'       00187903
              DISPLAY '*****************************************'       00188003
              DISPLAY '* ERROR EN   : ' WS-PARRAFO                      00188103
              DISPLAY '* ARCHIVO    : ' WS-ARCHIVO-DESC                 00188203
              DISPLAY '* ERROR CODE : ' FS-ACTUAL                       00188303
              DISPLAY '*****************************************'       00188403
              MOVE 9999 TO RETURN-CODE                                  00188503
              SET  WS-FIN-PROCESO TO TRUE                               00188603
           ELSE                                                         00188703
              CONTINUE                                                  00188803
           END-IF.                                                      00188903
                                                                        00189303
       8005-F-LECTURA-INICIAL. EXIT.                                    00189403
                                                                        00189503
                                                                        00189603
      **************************************                            00189703
      * LECTURA  DE ARCHIVO ENTRADA        *                            00189803
      **************************************                            00189903
       8050-I-LEER-ENTRADA.                                             00190003
                                                                        00190103
           MOVE '8000-I-APERTURA-ARCHIVOS' TO WS-PARRAFO                00190203
                                                                        00190303
      *    READ  ENTRADA INTO WS-REG-ENTRADA.                           00190406
           MOVE FS-ENTRADA TO FS-ACTUAL                                 00190503
           MOVE 'LECTURA ENTRADA' TO WS-ARCHIVO-DESC                    00190603
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00190703
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00190803
           .                                                            00190903
                                                                        00191003
       8050-F-LEER-ENTRADA. EXIT.                                       00191103
                                                                        00191203
      **************************************                            00191303
      *  CIERRE DE ARCHIVOS                *                            00191403
      **************************************                            00191503
       8100-I-CIERRE-ARCHIVOS.                                          00191603
                                                                        00191703
           MOVE '8100-I-CIERRE-ARCHIVOS' TO WS-PARRAFO                  00191803
                                                                        00191903
           CLOSE ENTRADA.                                               00192003
           MOVE FS-ENTRADA TO FS-ACTUAL                                 00192103
           MOVE 'ARCHIVO ENTRADA' TO WS-ARCHIVO-DESC                    00192203
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00192303
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00192403
                                                                        00192503
           CLOSE SALIDA.                                                00192603
           MOVE FS-SALIDA  TO FS-ACTUAL                                 00192703
           MOVE 'ARCHIVO LISTADO' TO WS-ARCHIVO-DESC                    00192803
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00192903
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00193003
           .                                                            00193103
                                                                        00193203
       8100-F-CIERRE-ARCHIVOS. EXIT.                                    00193303
                                                                        00193403
      ***********************************************************       00193503
      *  APERTURA DE CURSOR                                     *       00193603
      ***********************************************************       00193703
       8200-I-ABRIR-CURSOR.                                             00193803
                                                                        00193903
           MOVE '8200-I-ABRIR-CURSOR' TO WS-PARRAFO.                    00194003
                                                                        00194103
           EXEC SQL  OPEN C1  END-EXEC                                  00194203
                                                                        00194303
           IF  SQLCODE EQUAL ZEROS                                      00194403
                SET WS-CURSOR-ABIERTO-SI   TO TRUE                      00194503
                SET WS-SI-CURSOR           TO TRUE                      00194603
           ELSE                                                         00194703
                MOVE '8200-I-ABRIR-CURSOR' TO WS-PARRAFO                00194803
                MOVE SQLCODE   TO WS-SQLCODE                            00194903
                DISPLAY '************************************'          00195003
                DISPLAY '* ERROR DB2   : ' WS-PARRAFO                   00195103
                DISPLAY '* SQLCODE     : ' WS-SQLCODE                   00195203
                DISPLAY '************************************'          00195303
                DISPLAY '* TABLA       : CLIENTE / CUENTAS  *'          00195403
                DISPLAY '* DESCRIPCION : APERTURA DE CURSOR *'          00195503
                DISPLAY '************************************'          00195603
                MOVE 9999 TO RETURN-CODE                                00195703
                SET  WS-FIN-CURSOR  TO TRUE                             00195803
                SET  WS-FIN-PROCESO TO TRUE                             00195903
           END-IF.                                                      00196003
                                                                        00196103
       8200-F-ABRIR-CURSOR. EXIT.                                       00196203
                                                                        00196303
      ***********************************************************       00196403
      *  CIERRE DE CURSOR                                       *       00196503
      ***********************************************************       00196603
       8300-I-CERRAR-CURSOR.                                            00196703
                                                                        00196803
           MOVE '8300-I-CERRAR-CURSOR' TO WS-PARRAFO.                   00196903
                                                                        00197003
           EXEC SQL CLOSE C1  END-EXEC                                  00197103
                                                                        00197203
           IF  SQLCODE EQUAL ZEROS                                      00197303
                SET WS-CURSOR-ABIERTO-NO TO TRUE                        00197403
                CONTINUE                                                00197503
           ELSE                                                         00197603
                MOVE '8300-I-CERRAR-CURSOR' TO WS-PARRAFO               00197703
                MOVE SQLCODE   TO WS-SQLCODE                            00197803
                DISPLAY '************************************'          00197903
                DISPLAY '* ERROR DB2   : ' WS-PARRAFO                   00198003
                DISPLAY '* SQLCODE     : ' WS-SQLCODE                   00198103
                DISPLAY '************************************'          00198203
                DISPLAY '* TABLA       : CLIENTE / CUENTAS  *'          00198303
                DISPLAY '* DESCRIPCION : CIERRE DE CURSOR   *'          00198403
                DISPLAY '************************************'          00198503
                MOVE 9999 TO RETURN-CODE                                00198603
                SET  WS-FIN-CURSOR  TO TRUE                             00198703
                SET  WS-FIN-PROCESO TO TRUE                             00198803
           END-IF.                                                      00198903
                                                                        00199003
       8300-F-CERRAR-CURSOR. EXIT.                                      00199103
                                                                        00199203
      ***********************************************************       00199303
      *  LECTURA DE CURSOR                                       *      00199403
      ***********************************************************       00199503
       8400-I-LEER-CURSOR.                                              00199603
                                                                        00199703
           MOVE '8400-I-LEER-CURSOR' TO WS-PARRAFO.                     00199803
                                                                        00199903
      *             LEE TABLA DE CLIENTES                               00200003
                EXEC SQL                                                00200103
                   FETCH C1                                             00200203
                   INTO  :DCLTBCURCLI.WT-TIPDOC,                        00200303
                         :DCLTBCURCLI.WT-NRODOC,                        00200403
                         :DCLTBCURCLI.WT-NROCLI,                        00200503
                         :DCLTBCURCLI.WT-NOMAPE,                        00200603
                         :DCLTBCURCLI.WT-FECNAC,                        00200703
                         :DCLTBCURCLI.WT-SEXO                           00200803
                END-EXEC                                                00200903
                                                                        00201003
      *             LEE TABLA DE CUENTAS                                00201103
      *         EXEC SQL                                                00201207
      *            FETCH C4                                             00201307
      *            INTO  :DCLTBCURCTA.WS-TIPCUEN,                       00201407
      *                  :DCLTBCURCTA.WS-NROCUEN,                       00201507
      *                  :DCLTBCURCTA.WS-SUCUEN,                        00201607
      *                  :DCLTBCURCTA.WS-NROCLI,                        00201707
      *                  :DCLTBCURCTA.WS-SALDO,                         00201807
      *                  :DCLTBCURCTA.WS-FECSAL                         00201907
      *         END-EXEC                                                00202007
                                                                        00202103
           EVALUATE TRUE                                                00202203
           WHEN SQLCODE EQUAL ZEROS                                     00202303
                                                                        00202403
                ADD 1 TO WS-C1-LEIDOS                                   00202503
                                                                        00202603
           WHEN SQLCODE EQUAL +100                                      00202703
                SET  WS-FIN-CURSOR  TO TRUE                             00202803
           WHEN OTHER                                                   00202903
                MOVE '8400-I-LEER-CURSOR' TO WS-PARRAFO                 00203003
                MOVE SQLCODE   TO WS-SQLCODE                            00203103
                DISPLAY '************************************'          00203203
                DISPLAY '* ERROR DB2   : ' WS-PARRAFO                   00203303
                DISPLAY '* SQLCODE     : ' WS-SQLCODE                   00203403
                DISPLAY '************************************'          00203503
                DISPLAY '* TABLA       : CLIENTE / CUENTAS  *'          00203603
                DISPLAY '* DESCRIPCION : LECTURA DE CURSOR  *'          00203703
                DISPLAY '************************************'          00203803
                MOVE 9999 TO RETURN-CODE                                00203903
                SET  WS-FIN-CURSOR  TO TRUE                             00204003
                SET  WS-FIN-PROCESO TO TRUE                             00204103
             END-EVALUATE.                                              00204203
                                                                        00204303
       8400-F-LEER-CURSOR. EXIT.                                        00204403
                                                                        00204503
      **************************************                            00204603
      *  GRABAR TITULOS                    *                            00204703
      **************************************                            00204803
       9000-I-GRABAR-TITULOS.                                           00204903
                                                                        00205003
           MOVE '9000-I-GRABAR-TITULOS' TO WS-PARRAFO.                  00205103
                                                                        00205203
           ADD 1 TO WS-NRO-PAGINA                                       00205303
                                                                        00205403
           MOVE 'REGISTRO  DE CLIENTES PROCESADOS '                     00205617
                                   TO WS-TITULO-LEYENDA                 00205712
           WRITE REG-SALIDA FROM WS-LINEA  AFTER PAGE                   00205812
           WRITE REG-SALIDA FROM WS-TITULO                              00205912
           WRITE REG-SALIDA FROM WS-LINEA                               00206012
                                                                        00206203
      *    MOVE 'CONTROL DE INTEGRIDAD DE CUENTAS '                     00206412
      *                            TO WS-TITULO-LEYENDA                 00206512
      *    WRITE REG-SALIDA FROM WS-LINEA  AFTER PAGE                   00206612
      *    WRITE REG-SALIDA FROM WS-TITULO                              00206712
      *    WRITE REG-SALIDA FROM WS-LINEA                               00206812
                                                                        00207003
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00207103
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00207203
                                                                        00207303
           PERFORM 9050-I-GRABAR-SUBTITULOS                             00207403
              THRU 9050-F-GRABAR-SUBTITULOS                             00207503
                                                                        00207603
           MOVE 5   TO  WS-CUENTA-LINEA.                                00207703
                                                                        00207803
       9000-F-GRABAR-TITULOS.   EXIT.                                   00207903
                                                                        00208003
      *********************************************************         00208103
      *  GRABAR SUBTITULOS PARA REG SALIDA CLIENTE Y CUENTA   *         00208203
      *********************************************************         00208303
       9050-I-GRABAR-SUBTITULOS.                                        00208403
                                                                        00208503
           MOVE '9050-I-GRABAR-SUBTITULOS' TO WS-PARRAFO.               00208603
                                                                        00208703
           WRITE REG-SALIDA FROM WS-SUBTITULO-TBCURCLI  AFTER 1         00208912
           WRITE REG-SALIDA FROM WS-LINEA                               00209012
                                                                        00209203
      *    WRITE REG-SALIDA FROM WS-SUBTITULO-TBCURCTA  AFTER 1         00209412
      *    WRITE REG-SALIDA FROM WS-LINEA                               00209512
                                                                        00209703
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00209803
              THRU 9996-F-VALIDAR-FS-ACTUAL.                            00209903
                                                                        00210003
       9050-F-GRABAR-SUBTITULOS. EXIT.                                  00210103
                                                                        00210203
      *********************************************************         00210303
      *  VALIDACION FILE STATUS ARCHIVO SALIDA LISTADO        *         00210403
      *********************************************************         00210503
       9996-I-VALIDAR-FS-ACTUAL.                                        00210603
           IF FS-ACTUAL  IS NOT EQUAL '00'                              00210703
              DISPLAY '*****************************************'       00210803
              DISPLAY '*     *   FILE STATUS ERROR   *         *'       00210903
              DISPLAY '*****************************************'       00211003
              DISPLAY '* ERROR EN   : ' WS-PARRAFO                      00211103
              DISPLAY '* ARCHIVO    : ' WS-ARCHIVO-DESC                 00211203
              DISPLAY '* ERROR CODE : ' FS-ACTUAL                       00211303
              DISPLAY '*****************************************'       00211403
              MOVE 9999 TO RETURN-CODE                                  00211503
              SET  WS-FIN-PROCESO TO TRUE                               00211603
           ELSE                                                         00211703
              CONTINUE                                                  00211803
           END-IF.                                                      00211903
       9996-F-VALIDAR-FS-ACTUAL. EXIT.                                  00212003
                                                                        00212103
      **************************************                            00212203
      *  CUERPO FINAL CIERRE DE FILES      *                            00212303
      **************************************                            00212403
       9999-I-FINAL.                                                    00212503
                                                                        00212603
           PERFORM 8100-I-CIERRE-ARCHIVOS                               00212703
              THRU 8100-F-CIERRE-ARCHIVOS                               00212803
                                                                        00212903
           DISPLAY '**********************************************'.    00213003
           DISPLAY '* TOTALES POR TIPO DE DOCUMENTO Y SUBTOTALES *'.    00213107
           DISPLAY '*  POR GENERO, POR CADA TIPO DE DOCUMENTO.   *'.    00213207
           DISPLAY '**********************************************'.    00213307
           DISPLAY 'TOTAL DOCUMENTOS DU : ' WS-CANT-DU.                 00213407
           DISPLAY '       * TOTAL FEMENINOS : ' WS-CANT-DU-F.          00213507
           DISPLAY '       * TOTAL MASCULINOS: ' WS-CANT-DU-M.          00213607
           DISPLAY '       * TOTAL OTRO      : ' WS-CANT-DU-O.          00213707
           DISPLAY 'TOTAL DOCUMENTOS PA : ' WS-CANT-PA.                 00213807
           DISPLAY '       * TOTAL FEMENINOS : ' WS-CANT-PA-F.          00213907
           DISPLAY '       * TOTAL MASCULINOS: ' WS-CANT-PA-M.          00214007
           DISPLAY '       * TOTAL OTRO      : ' WS-CANT-PA-O.          00214107
           DISPLAY 'TOTAL DOCUMENTOS PE : ' WS-CANT-PE.                 00214207
           DISPLAY '       * TOTAL FEMENINOS : ' WS-CANT-PE-F.          00214307
           DISPLAY '       * TOTAL MASCULINOS: ' WS-CANT-PE-M.          00214407
           DISPLAY '       * TOTAL OTRO      : ' WS-CANT-PE-O.          00214507
           DISPLAY '**********************************************'.    00214607
           DISPLAY ' '.                                                 00214707
           DISPLAY '**********************************************'.    00214807
           DISPLAY '*  TOTALES DE CONTROL DE PROCESO             *'.    00214907
           DISPLAY '**********************************************'.    00215007
           DISPLAY 'TOTAL REG. ENTRADA LEIDOS: ' WS-ENTRADA-LEIDOS.     00215107
           DISPLAY 'TOTAL REG. ENTRADA ERROR : ' WS-ENTRADA-ERROR.      00215207
           DISPLAY 'TOTAL REG. CURSOR LEIDOS : ' WS-C1-LEIDOS.          00215307
           DISPLAY '----------------------------------------------'     00215407
           DISPLAY 'TOTAL REGISTROS GRABADOS   : '  WS-GRABADOS.        00215507
           DISPLAY '**********************************************'.    00215607
                                                                        00215707
       9999-F-FINAL.                                                    00215807
           EXIT.                                                        00216003
      *                                                                 00220000
