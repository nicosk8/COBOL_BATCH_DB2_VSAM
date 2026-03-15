       IDENTIFICATION DIVISION.                                         00002000
      *                                                        *        00003000
       PROGRAM-ID. EXAMEN2.                                             00004000
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
             SELECT ENTRADA ASSIGN DDENTRA                              00021000
             FILE STATUS IS FS-ENTRADA.                                 00022300
                                                                        00022400
             SELECT SALIDA  ASSIGN DDSALE                               00022500
             FILE STATUS IS FS-SALIDA.                                  00022600
                                                                        00022700
       DATA DIVISION.                                                   00022800
       FILE SECTION.                                                    00022900
       FD ENTRADA                                                       00040100
             BLOCK CONTAINS 0 RECORDS                                   00040200
             RECORDING MODE IS F.                                       00040300
                                                                        00040400
       01 REG-ENTRADA     PIC X(13).                                    00040501
                                                                        00040600
       FD SALIDA                                                        00040700
             BLOCK CONTAINS 0 RECORDS                                   00040800
             RECORDING MODE IS F.                                       00040900
                                                                        00041000
       01 REG-SALIDA      PIC X(132).                                   00041100
                                                                        00041200
      **************************************                            00041300
       WORKING-STORAGE SECTION.                                         00041400
      **************************************                            00041500
       77  FILLER          PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.00041600
                                                                        00041700
      ****************************************                          00041800
      * FILE STATUS SALIDA LISTADO Y SQLCODE *                          00041900
      ****************************************                          00042000
       77  FS-ENTRADA               PIC XX    VALUE SPACES.             00042100
       77  FS-SALIDA                PIC XX    VALUE SPACES.             00042200
       77  FS-ACTUAL                PIC XX    VALUE SPACES.             00042300
       77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          00042400
                                                                        00042500
      ****************************************                          00042600
      * CLAVES DE CORTE DE CONTROL           *                          00042700
      ****************************************                          00042800
       01  WS-CLAVE-ACTUAL-ENTRADA.                                     00042901
           03  WS-TIPCUEN-ACT      PIC X(02)  VALUE SPACES.             00043019
           03  WS-NROCUEN-ACT      PIC S9(5)V VALUE ZEROES.             00043119
                                                                        00043201
       01  WS-CLAVE-CORTE-MAYOR.                                        00043301
           03  WS-TIPCUEN-ANT      PIC X(02)  VALUE SPACES.             00043419
           03  WS-NROCUEN-ANT      PIC S9(5)V VALUE ZEROES.             00043519
                                                                        00043601
       01  WS-CLAVE-CORTE-MENOR.                                        00043701
           03  WS-TIPMOV-ANT       PIC X(02) VALUE SPACES.              00043801
                                                                        00043901
      ****************************************                          00044001
      * CONTADORES                           *                          00044101
      ****************************************                          00044201
       01  WS-CONTADORES.                                               00044301
           03  WS-TOT-GRAL             PIC 9(03) VALUE ZEROS.           00044401
           03  WS-SUB-TOT              PIC 9(03) VALUE ZEROS.           00044501
           03  WS-SALDO-ACUMULADO      PIC S9(05) VALUE ZEROS.          00044601
                                                                        00044701
           03  WS-CONTADORES-CLAVE-MAYOR.                               00045001
               05  WS-CANT-MOV         PIC 9(03) VALUE ZEROS.           00045201
                                                                        00045301
           03  WS-CONTADORES-MENOR.                                     00045501
               05  WS-CANT-MOV-DB      PIC 9(03) VALUE ZEROS.           00045901
               05  WS-CANT-MOV-CR      PIC 9(03) VALUE ZEROS.           00046001
                                                                        00047001
           03  WS-ENTRADA-LEIDOS       PIC 9(03) VALUE ZEROS.           00047100
           03  WS-ENTRADA-ERROR        PIC 9(03) VALUE ZEROS.           00047200
           03  WS-C4-LEIDOS            PIC 9(03) VALUE ZEROS.           00047301
           03  WS-GRABADOS             PIC 9(03) VALUE ZEROS.           00047400
           03  WS-EDIT                 PIC Z(03) VALUE ZEROS.           00047701
                                                                        00047801
      *****************************************                         00047901
      *  BANDERAS/FLAGS                       *                         00048001
      *****************************************                         00048101
                                                                        00048501
       01  WS-FLAG-PROCESO          PIC X VALUE 'T'.                    00048601
           88  WS-SI-PROCESO           VALUE 'T'.                       00048701
           88  WS-FIN-PROCESO          VALUE 'F'.                       00048801
                                                                        00048901
       01  WS-FLAG-CURSOR           PIC X VALUE 'T'.                    00049001
           88  WS-SI-CURSOR            VALUE 'T'.                       00049101
           88  WS-FIN-CURSOR           VALUE 'F'.                       00049201
                                                                        00049301
       01  WS-CURSOR-ABIERTO        PIC X VALUE 'T'.                    00049401
           88  WS-CURSOR-ABIERTO-SI    VALUE 'T'.                       00049501
           88  WS-CURSOR-ABIERTO-NO    VALUE 'F'.                       00049601
                                                                        00049701
      *****************************************                         00050101
      *  VARIABLES AUXILARES.                *                          00050201
      *****************************************                         00050301
       01 WS-AUXILIARES.                                                00051000
          03  WS-PARRAFO            PIC X(30) VALUE SPACES.             00052800
          03  WS-ARCHIVO-DESC       PIC X(30) VALUE SPACES.             00052900
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
      *****************************************                         00084600
      *   FIN IMPRESION DE TITULOS LISTADO    *                         00084700
      *****************************************                         00084800
       77  FILLER        PIC X(26) VALUE '* VARIABLES SQL          *'.  00084900
                                                                        00085000
      ********************************************************          00085100
      *  AREA DE COPYS                                       *          00085200
      ********************************************************          00085300
                                                                        00085400
      **   AGREGAR ACA LA COPY DE ARCHIVO DE ENTRADA        **          00085500
      **    COPY CPNOVCTA.                                              00085603
       01  NOVEDADES-CUENTAS.                                           00085703
           03 NOV-CTA-TIPCUEN          PIC X(2).                        00085803
           03 NOV-CTA-NROCUEN          PIC S9(5)V USAGE COMP-3.         00085903
           03 NOV-CTA-NROCLI           PIC S9(3)V USAGE COMP-3.         00086003
           03 NOV-CTA-SALDO            PIC S9(5)V9(2) USAGE COMP-3.     00086103
                                                                        00086203
      * TIPO DE MOVIMIENTO VALIDO 'CR' - 'DB'                           00086303
           03 NOV-CTA-TIPMOV           PIC X(2).                        00086403
                                                                        00087000
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
      ***************************************************               00096300
      * CURSOR C4 QUE METRAE REGISTROS DE CUENTAS       *               00096400
      ***************************************************               00096500
            EXEC SQL                                                    00096600
              DECLARE C4 CURSOR FOR                                     00096700
              SELECT *                                                  00096800
                FROM KC02803.TBCURCTA                                   00096900
              ORDER BY TIPCUEN, NROCUEN ASC                             00097001
            END-EXEC.                                                   00098000
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
             UNTIL WS-FIN-PROCESO.                                      00103701
                                                                        00103800
           PERFORM 5000-I-ULTIMO-CORTE                                  00103909
              THRU 5000-F-ULTIMO-CORTE                                  00104009
                                                                        00104100
           PERFORM 9999-I-FINAL    THRU                                 00104500
                   9999-F-FINAL.                                        00104600
                                                                        00104700
       F-MAIN-PROGRAM. GOBACK.                                          00104800
                                                                        00104900
      *************************************                             00105000
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            00105100
      **************************************                            00105200
       1000-I-INICIO.                                                   00105300
                                                                        00105400
           MOVE FUNCTION CURRENT-DATE TO WS-FECHA-AUX                   00105511
           MOVE WS-FECHA-AUX(1:4)     TO WS-AAAA                        00105611
           MOVE WS-FECHA-AUX(5:2)     TO WS-MM                          00105711
           MOVE WS-FECHA-AUX(7:2)     TO WS-DD                          00105811
           MOVE 16 TO WS-CUENTA-LINEA                                   00105900
                                                                        00106000
           SET WS-SI-PROCESO TO TRUE                                    00107011
                                                                        00109800
           PERFORM 8000-I-APERTURA-ARCHIVOS                             00111000
              THRU 8000-F-APERTURA-ARCHIVOS                             00111100
                                                                        00111200
           IF FS-ENTRADA = '00' AND  FS-SALIDA  = '00'                  00111300
              PERFORM 8005-I-LECTURA-INICIAL                            00111401
                 THRU 8005-F-LECTURA-INICIAL                            00111502
              PERFORM 8200-I-ABRIR-CURSOR                               00111600
                 THRU 8200-F-ABRIR-CURSOR                               00111700
              PERFORM 8400-I-LEER-CURSOR                                00111801
                 THRU 8400-F-LEER-CURSOR                                00111901
              DISPLAY '***********************************************' 00112018
              DISPLAY '* TOTAL MOVIMIENTOS POR TIPO Y NRO. DE CUENTA *' 00112118
              DISPLAY '*  CANTIDAD DE DEBITOS Y CREDITOS POR CUENTA  *' 00112218
              DISPLAY '***********************************************' 00112318
           ELSE                                                         00112500
              DISPLAY '*********************************'               00112600
              DISPLAY '* ERROR EN APERTURA DE ARCHIVOS *'               00112700
              DISPLAY '* APERTURA DE CURSOR CANCELADA  *'               00112800
              DISPLAY '*********************************'               00112900
           END-IF                                                       00113000
           .                                                            00115704
       1000-F-INICIO.   EXIT.                                           00115801
                                                                        00116000
      **************************************                            00120600
      *  CUERPO PRINCIPAL DEL PROGRAMA     *                            00120700
      **************************************                            00120800
       2000-I-PROCESO.                                                  00120900
                                                                        00121600
            IF NOV-CTA-TIPCUEN = '01' OR                                00121701
               NOV-CTA-TIPCUEN = '02'                                   00121801
                                                                        00122300
                  IF WS-CLAVE-ACTUAL-ENTRADA = WS-CLAVE-CORTE-MAYOR     00122401
                                                                        00122500
                        ADD NOV-CTA-SALDO TO WS-SALDO-ACUMULADO         00122601
                        ADD 1 TO WS-TOT-GRAL                            00122701
                                                                        00122800
                        IF NOV-CTA-TIPMOV = WS-CLAVE-CORTE-MENOR        00122901
                            ADD 1 TO WS-SUB-TOT                         00123001
                        ELSE                                            00123100
      *                |----------------- CORTE DE CLAVE MENOR ---------00123200
                            PERFORM 2300-I-CORTE-MENOR                  00123300
                               THRU 2300-F-CORTE-MENOR                  00124000
                        END-IF                                          00128000
                  END-IF                                                00128100
                                                                        00128200
      *          |----------------------- CORTE DE CLAVE MAYOR ---------00128300
                  IF WS-CLAVE-ACTUAL-ENTRADA NOT = WS-CLAVE-CORTE-MAYOR 00128401
                       PERFORM 2300-I-CORTE-MENOR                       00128600
                          THRU 2300-F-CORTE-MENOR                       00128700
                       PERFORM 2200-I-CORTE-MAYOR                       00128800
                          THRU 2200-F-CORTE-MAYOR                       00128900
                                                                        00129001
                       PERFORM 8400-I-LEER-CURSOR                       00129101
                          THRU 8400-F-LEER-CURSOR                       00130001
                                                                        00130101
                       PERFORM 2950-I-ARMAR-REG-SALIDA-CTA              00130401
                          THRU 2950-F-ARMAR-REG-SALIDA-CTA              00130501
                                                                        00130601
                       PERFORM 3000-I-GRABAR-LISTADO                    00130701
                          THRU 3000-F-GRABAR-LISTADO                    00130801
                                                                        00130905
                       MOVE 0 TO WS-SALDO-ACUMULADO                     00131005
                  END-IF                                                00131100
            ELSE                                                        00131501
                  ADD 1 TO WS-ENTRADA-ERROR                             00131601
            END-IF.                                                     00131701
                                                                        00132000
      *    VOY A LEER EL SIGUIENTE REGISTRO QUE ACTUALIZA CLAVES        00137200
      *    MAYOR Y MENOR ACTUALES Y VUELVE A PROCESAR.                  00137300
            PERFORM 8050-I-LEER-ENTRADA                                 00137401
               THRU 8050-F-LEER-ENTRADA                                 00137501
            .                                                           00137610
       2000-F-PROCESO. EXIT.                                            00140000
                                                                        00140100
      ******************************************************************00141000
      * CORTE DE CLAVE MAYOR                                            00142000
      ******************************************************************00143000
       2200-I-CORTE-MAYOR.                                              00150000
      *     DISPLAY '+++++ CORTE MAYOR +++++'                           00150100
      *     ME GUARDO EL TOTAL GENERAL DE LA CLAVE MAYOR                00150200
            MOVE  WS-TOT-GRAL TO WS-CANT-MOV                            00150501
                                                                        00150614
            PERFORM 4000-I-DISPLAY-CORTE-CONTROL                        00150712
               THRU 4000-F-DISPLAY-CORTE-CONTROL                        00150812
                                                                        00151400
      *     RESETEO EN 1 EL CONTADOR GENERAL                            00151500
      *     RESETEO EN 0 EL CONTADOR PARCIAL DE LA CLAVE MENOR          00151600
      *     ACTUALIZO LA CLAVE MAYOR.                                   00151700
            MOVE 0   TO WS-CANT-MOV-DB                                  00151806
            MOVE 0   TO WS-CANT-MOV-CR                                  00151906
            MOVE 1   TO WS-TOT-GRAL                                     00152006
            MOVE 1   TO WS-SUB-TOT                                      00152106
            MOVE WS-CLAVE-ACTUAL-ENTRADA TO WS-CLAVE-CORTE-MAYOR        00152201
            .                                                           00152300
       2200-F-CORTE-MAYOR. EXIT.                                        00152400
                                                                        00152500
      ******************************************************************00152600
      * CORTE DE CLAVE MENOR                                            00152700
      ******************************************************************00152800
       2300-I-CORTE-MENOR.                                              00152900
      *     DISPLAY '----- CORTE MENOR -----'                           00153000
      *    GUARDO EL TOTAL PARCIAL DE LA CLAVE MENOR                    00153100
           EVALUATE WS-TIPMOV-ANT                                       00153201
           WHEN 'DB'                                                    00153301
                    MOVE WS-SUB-TOT TO WS-CANT-MOV-DB                   00153401
           WHEN 'CR'                                                    00154301
                    MOVE WS-SUB-TOT TO WS-CANT-MOV-CR                   00154401
           END-EVALUATE                                                 00156700
      *    Y RESETEO EN 1 EL CONTADOR PARCIAL                           00156900
      *    ACTUALIZO LA CLAVE MENOR                                     00157000
      *    LA CLAVE DE CORTE MAYOR QUEDA INTACTA.                       00157100
           MOVE 1   TO WS-SUB-TOT                                       00157405
           MOVE NOV-CTA-TIPMOV   TO WS-TIPMOV-ANT                       00157505
           .                                                            00157605
       2300-F-CORTE-MENOR. EXIT.                                        00157705
                                                                        00167600
      ***********************************************************       00167700
      *  ARMARDO DE SALIDA REGISTRO DE CUENTA                   *       00167800
      ***********************************************************       00167900
       2950-I-ARMAR-REG-SALIDA-CTA.                                     00168000
           MOVE  WS-TIPCUEN      TO REG-CTA-TIPCUEN                     00168100
           MOVE  WS-NROCUEN      TO REG-CTA-NROCUEN                     00168200
           MOVE  WS-SUCUEN       TO REG-CTA-SUCUEN                      00168300
           MOVE  WS-NROCLI       TO REG-CTA-NROCLI                      00168400
                                                                        00168514
      *    MOVE  WS-SALDO           TO  REG-CTA-SALDO                   00168614
           MOVE  WS-SALDO-ACUMULADO TO  REG-CTA-SALDO                   00168701
                                                                        00168814
      *    MOVE  WS-FECSAL(1:4)  TO REG-FECSAL-AAAA                     00168901
      *    MOVE  WS-FECSAL(6:2)  TO REG-FECSAL-MM                       00169001
      *    MOVE  WS-FECSAL(9:2)  TO REG-FECSAL-DD.                      00169101
           MOVE  WS-AAAA         TO  REG-FECSAL-AAAA                    00169201
           MOVE  WS-MM           TO  REG-FECSAL-MM                      00169301
           MOVE  WS-DD           TO  REG-FECSAL-DD                      00169401
           .                                                            00169501
       2950-F-ARMAR-REG-SALIDA-CTA. EXIT.                               00169601
                                                                        00169701
      **************************************                            00169801
      *  GRABAR LISTADO ERRORES            *                            00169901
      **************************************                            00170001
       3000-I-GRABAR-LISTADO.                                           00170101
                                                                        00170201
           MOVE '3000-I-GRABAR-LISTADO' TO WS-PARRAFO.                  00170301
                                                                        00170401
           IF WS-CUENTA-LINEA > 15                                      00170501
                PERFORM 9000-I-GRABAR-TITULOS                           00170601
                   THRU 9000-F-GRABAR-TITULOS                           00170701
           END-IF                                                       00170801
                                                                        00170901
           WRITE REG-SALIDA FROM WS-REG-SALIDA-CTA  AFTER 1             00171101
                                                                        00171201
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00171301
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00171401
                                                                        00171501
           ADD  1   TO  WS-GRABADOS                                     00171601
           ADD  1   TO  WS-CUENTA-LINEA.                                00171701
                                                                        00171801
       3000-F-GRABAR-LISTADO. EXIT.                                     00171901
                                                                        00172001
      **************************************                            00172101
      *  MOSTRAR ENCABEZADO DE CORTE SPOOL *                            00172201
      **************************************                            00172301
       4000-I-DISPLAY-CORTE-CONTROL.                                    00173012
           DISPLAY 'DETALLE POR CUENTA    '.                            00173501
                                                                        00173612
           MOVE WS-TIPCUEN-ANT  TO WS-EDIT.                             00173712
           DISPLAY '       * TIPO DE CUENTA  : ' WS-EDIT.               00173812
                                                                        00173912
           MOVE WS-NROCUEN-ANT  TO WS-EDIT.                             00174012
           DISPLAY '       * NRO. DE CUENTA  : ' WS-EDIT.               00174112
           DISPLAY '       * TOTAL MOVIMIENTOS : ' WS-CANT-MOV.         00174515
           DISPLAY '                * TOTAL DB : ' WS-CANT-MOV-DB.      00174817
           DISPLAY '                * TOTAL CR : ' WS-CANT-MOV-CR.      00175114
           DISPLAY '**********************************************'.    00175201
           DISPLAY ' '.                                                 00175301
       4000-F-DISPLAY-CORTE-CONTROL. EXIT.                              00175412
                                                                        00175510
      *****************************************                         00175607
      *  ULTIMO CORTE CUANDO SALE DEL PROCESO *                         00175707
      *****************************************                         00175807
       5000-I-ULTIMO-CORTE.                                             00175909
           PERFORM 2300-I-CORTE-MENOR                                   00176007
              THRU 2300-F-CORTE-MENOR                                   00177007
                                                                        00177107
           PERFORM 2200-I-CORTE-MAYOR                                   00178007
              THRU 2200-F-CORTE-MAYOR                                   00179007
                                                                        00180007
           PERFORM 8400-I-LEER-CURSOR                                   00181007
              THRU 8400-F-LEER-CURSOR                                   00182007
                                                                        00183007
           PERFORM 2950-I-ARMAR-REG-SALIDA-CTA                          00183107
              THRU 2950-F-ARMAR-REG-SALIDA-CTA                          00183207
                                                                        00183307
           PERFORM 3000-I-GRABAR-LISTADO                                00183407
              THRU 3000-F-GRABAR-LISTADO                                00183507
                                                                        00183607
           MOVE 0 TO WS-SALDO-ACUMULADO                                 00183707
           .                                                            00183808
       5000-F-ULTIMO-CORTE. EXIT.                                       00183909
                                                                        00184000
      **************************************                            00184100
      * APERTURA DE ARCHIVOS               *                            00184200
      **************************************                            00184300
       8000-I-APERTURA-ARCHIVOS.                                        00184400
                                                                        00184500
           MOVE '8000-I-APERTURA-ARCHIVOS' TO WS-PARRAFO                00184600
                                                                        00184700
           OPEN INPUT  ENTRADA.                                         00184800
           MOVE FS-ENTRADA TO FS-ACTUAL                                 00184900
           MOVE 'ARCHIVO ENTRADA' TO WS-ARCHIVO-DESC                    00185000
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00185100
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00185200
                                                                        00185300
           OPEN OUTPUT SALIDA.                                          00185400
           MOVE FS-SALIDA  TO FS-ACTUAL                                 00185500
           MOVE 'ARCHIVO LISTADO' TO WS-ARCHIVO-DESC                    00185600
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00185700
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00185800
                                                                        00185900
           .                                                            00186200
       8000-F-APERTURA-ARCHIVOS. EXIT.                                  00186300
                                                                        00186400
      **************************************                            00186500
      * LECTURA INICIAL DE ARCHIVO ENTRADA *                            00186600
      **************************************                            00186700
       8005-I-LECTURA-INICIAL.                                          00186800
                                                                        00187100
           READ  ENTRADA INTO NOVEDADES-CUENTAS.                        00187201
                                                                        00187310
           EVALUATE FS-ENTRADA                                          00187515
           WHEN '00'                                                    00187615
      *       CARGO LA CALVE ACTUAL : WS-CLAVE-ACTUAL-ENTRADA.          00187715
      *       CARGO LAS VARIABLES DE CORTE MAYOR Y MENOR.               00187815
              MOVE NOV-CTA-TIPCUEN TO WS-TIPCUEN-ACT, WS-TIPCUEN-ANT    00187915
              MOVE NOV-CTA-NROCUEN TO WS-NROCUEN-ACT, WS-NROCUEN-ANT    00188015
              MOVE NOV-CTA-TIPMOV  TO WS-TIPMOV-ANT                     00188115
              ADD 1 TO WS-ENTRADA-LEIDOS                                00188215
           WHEN '10'                                                    00188315
              MOVE '8005-I-LECTURA-INICIAL'  TO WS-PARRAFO              00188415
              MOVE 'LECTURA ENTRADA INICIAL' TO WS-ARCHIVO-DESC         00188515
              DISPLAY '*****************************************'       00188615
              DISPLAY '*     *   FILE STATUS ERROR   *         *'       00188715
              DISPLAY '*****************************************'       00188815
              DISPLAY '*   EL ARCHIVO ENTRADA ESTA VACIO       *'       00188915
              DISPLAY '*****************************************'       00189015
              DISPLAY '* ERROR EN   : ' WS-PARRAFO                      00189115
              DISPLAY '* ARCHIVO    : ' WS-ARCHIVO-DESC                 00189215
              DISPLAY '* ERROR CODE : ' FS-ACTUAL                       00189315
              DISPLAY '*****************************************'       00189415
              SET WS-FIN-PROCESO TO TRUE                                00189518
           WHEN OTHER                                                   00189615
              MOVE '8005-I-LECTURA-INICIAL'  TO WS-PARRAFO              00189710
              MOVE 'LECTURA ENTRADA INICIAL' TO WS-ARCHIVO-DESC         00189815
              DISPLAY '*****************************************'       00189900
              DISPLAY '*     *   FILE STATUS ERROR   *         *'       00190000
              DISPLAY '*****************************************'       00190100
              DISPLAY '*   ERROR EN ARCHIVO ENTRADA NOVEDADES  *'       00190215
              DISPLAY '*****************************************'       00190300
              DISPLAY '* ERROR EN   : ' WS-PARRAFO                      00190400
              DISPLAY '* ARCHIVO    : ' WS-ARCHIVO-DESC                 00190500
              DISPLAY '* ERROR CODE : ' FS-ACTUAL                       00190600
              DISPLAY '*****************************************'       00190700
              MOVE 9999 TO RETURN-CODE                                  00190800
              SET  WS-FIN-PROCESO TO TRUE                               00190900
           END-EVALUATE                                                 00191015
           .                                                            00191115
       8005-F-LECTURA-INICIAL. EXIT.                                    00191200
                                                                        00191300
      **************************************                            00191400
      * LECTURA  DE ARCHIVO ENTRADA        *                            00191500
      **************************************                            00191600
       8050-I-LEER-ENTRADA.                                             00191700
                                                                        00191800
           READ  ENTRADA INTO NOVEDADES-CUENTAS.                        00192001
                                                                        00192101
           EVALUATE FS-ENTRADA                                          00192204
           WHEN '00'                                                    00192304
              MOVE NOV-CTA-TIPCUEN TO WS-TIPCUEN-ACT                    00192404
              MOVE NOV-CTA-NROCUEN TO WS-NROCUEN-ACT                    00192504
              ADD 1 TO WS-ENTRADA-LEIDOS                                00192604
                                                                        00192704
           WHEN '10'                                                    00192804
              SET  WS-FIN-PROCESO TO TRUE                               00192904
                                                                        00193015
           WHEN OTHER                                                   00193104
              MOVE '8050-I-LEER-ENTRADA'      TO WS-PARRAFO             00193210
              MOVE 'LECTURA ENTRADA PROCESO' TO WS-ARCHIVO-DESC         00193310
              DISPLAY '*****************************************'       00193404
              DISPLAY '*     *   FILE STATUS ERROR   *         *'       00193504
              DISPLAY '*****************************************'       00193604
              DISPLAY '* ERROR EN   : ' WS-PARRAFO                      00193704
              DISPLAY '* ARCHIVO    : ' WS-ARCHIVO-DESC                 00193804
              DISPLAY '* ERROR CODE : ' FS-ACTUAL                       00193904
              DISPLAY '*****************************************'       00194004
              MOVE 9999 TO RETURN-CODE                                  00194104
              SET  WS-FIN-PROCESO TO TRUE                               00194204
           END-EVALUATE                                                 00194311
           .                                                            00194404
                                                                        00194504
       8050-F-LEER-ENTRADA. EXIT.                                       00194604
                                                                        00194704
      **************************************                            00194804
      *  CIERRE DE ARCHIVOS                *                            00194904
      **************************************                            00195004
       8100-I-CIERRE-ARCHIVOS.                                          00195104
                                                                        00195204
           MOVE '8100-I-CIERRE-ARCHIVOS' TO WS-PARRAFO                  00195304
                                                                        00195404
           CLOSE ENTRADA.                                               00195504
           MOVE FS-ENTRADA TO FS-ACTUAL                                 00195604
           MOVE 'ARCHIVO ENTRADA' TO WS-ARCHIVO-DESC                    00195704
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00195804
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00195904
                                                                        00196004
           CLOSE SALIDA.                                                00196104
           MOVE FS-SALIDA  TO FS-ACTUAL                                 00196204
           MOVE 'ARCHIVO LISTADO' TO WS-ARCHIVO-DESC                    00196304
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00196404
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00196504
           .                                                            00196604
                                                                        00196704
       8100-F-CIERRE-ARCHIVOS. EXIT.                                    00196804
                                                                        00196904
      ***********************************************************       00197004
      *  APERTURA DE CURSOR                                     *       00197104
      ***********************************************************       00197204
       8200-I-ABRIR-CURSOR.                                             00197304
                                                                        00197404
           MOVE '8200-I-ABRIR-CURSOR' TO WS-PARRAFO.                    00197504
                                                                        00197604
           EXEC SQL  OPEN C4  END-EXEC                                  00197704
                                                                        00197804
           IF  SQLCODE EQUAL ZEROS                                      00197904
                SET WS-CURSOR-ABIERTO-SI   TO TRUE                      00198004
                SET WS-SI-CURSOR           TO TRUE                      00198104
           ELSE                                                         00198204
                MOVE '8200-I-ABRIR-CURSOR' TO WS-PARRAFO                00198304
                MOVE SQLCODE   TO WS-SQLCODE                            00198404
                DISPLAY '************************************'          00198504
                DISPLAY '* ERROR DB2   : ' WS-PARRAFO                   00198604
                DISPLAY '* SQLCODE     : ' WS-SQLCODE                   00198704
                DISPLAY '************************************'          00198804
                DISPLAY '* TABLA       : CUENTAS - TBCURCTA *'          00198910
                DISPLAY '* DESCRIPCION : APERTURA DE CURSOR *'          00199004
                DISPLAY '************************************'          00199104
                MOVE 9999 TO RETURN-CODE                                00199204
                SET  WS-FIN-CURSOR  TO TRUE                             00199304
                SET  WS-FIN-PROCESO TO TRUE                             00199404
           END-IF.                                                      00199504
                                                                        00199604
       8200-F-ABRIR-CURSOR. EXIT.                                       00199704
                                                                        00199804
      ***********************************************************       00199904
      *  CIERRE DE CURSOR                                       *       00200004
      ***********************************************************       00200104
       8300-I-CERRAR-CURSOR.                                            00200204
                                                                        00200304
           MOVE '8300-I-CERRAR-CURSOR' TO WS-PARRAFO.                   00200404
                                                                        00200504
           EXEC SQL CLOSE C4  END-EXEC                                  00200604
                                                                        00200704
           IF  SQLCODE EQUAL ZEROS                                      00200804
                SET WS-CURSOR-ABIERTO-NO TO TRUE                        00200904
                CONTINUE                                                00201004
           ELSE                                                         00201104
                MOVE '8300-I-CERRAR-CURSOR' TO WS-PARRAFO               00201204
                MOVE SQLCODE   TO WS-SQLCODE                            00201304
                DISPLAY '************************************'          00201404
                DISPLAY '* ERROR DB2   : ' WS-PARRAFO                   00201504
                DISPLAY '* SQLCODE     : ' WS-SQLCODE                   00201604
                DISPLAY '************************************'          00201704
                DISPLAY '* TABLA       : CUENTAS - TBCURCTA *'          00201810
                DISPLAY '* DESCRIPCION : CIERRE DE CURSOR   *'          00201904
                DISPLAY '************************************'          00202004
                MOVE 9999 TO RETURN-CODE                                00202104
                SET  WS-FIN-CURSOR  TO TRUE                             00202204
                SET  WS-FIN-PROCESO TO TRUE                             00202304
           END-IF.                                                      00202404
                                                                        00202504
       8300-F-CERRAR-CURSOR. EXIT.                                      00202604
                                                                        00202704
      ***********************************************************       00202804
      *  LECTURA DE CURSOR                                       *      00202904
      ***********************************************************       00203004
       8400-I-LEER-CURSOR.                                              00203104
                                                                        00204004
                EXEC SQL                                                00204104
                   FETCH C4                                             00204204
                   INTO  :DCLTBCURCTA.WS-TIPCUEN,                       00204304
                         :DCLTBCURCTA.WS-NROCUEN,                       00204404
                         :DCLTBCURCTA.WS-SUCUEN,                        00204504
                         :DCLTBCURCTA.WS-NROCLI,                        00204604
                         :DCLTBCURCTA.WS-SALDO,                         00204704
                         :DCLTBCURCTA.WS-FECSAL                         00204804
                END-EXEC                                                00204911
                                                                        00205004
           EVALUATE TRUE                                                00205104
           WHEN SQLCODE EQUAL ZEROS                                     00205204
                                                                        00205304
                ADD WS-SALDO  TO WS-SALDO-ACUMULADO                     00205404
                ADD 1 TO WS-C4-LEIDOS                                   00205504
                                                                        00205604
           WHEN SQLCODE EQUAL +100                                      00205704
                SET  WS-FIN-CURSOR  TO TRUE                             00205804
                                                                        00205914
           WHEN OTHER                                                   00206004
                MOVE '8400-I-LEER-CURSOR' TO WS-PARRAFO                 00206204
                MOVE SQLCODE   TO WS-SQLCODE                            00206304
                DISPLAY '************************************'          00206404
                DISPLAY '* ERROR DB2   : ' WS-PARRAFO                   00206504
                DISPLAY '* SQLCODE     : ' WS-SQLCODE                   00206604
                DISPLAY '************************************'          00206704
                DISPLAY '* TABLA       : CUENTAS - TBCURCTA *'          00206810
                DISPLAY '* DESCRIPCION : LECTURA DE CURSOR  *'          00206904
                DISPLAY '************************************'          00207004
                MOVE 9999 TO RETURN-CODE                                00207104
                SET  WS-FIN-CURSOR  TO TRUE                             00207204
                SET  WS-FIN-PROCESO TO TRUE                             00207304
           END-EVALUATE.                                                00207411
                                                                        00207504
       8400-F-LEER-CURSOR. EXIT.                                        00207604
                                                                        00207704
      **************************************                            00207804
      *  GRABAR TITULOS                    *                            00207904
      **************************************                            00208004
       9000-I-GRABAR-TITULOS.                                           00208104
                                                                        00208204
           MOVE '9000-I-GRABAR-TITULOS' TO WS-PARRAFO.                  00208304
                                                                        00208404
           ADD 1 TO WS-NRO-PAGINA                                       00208504
                                                                        00208604
           MOVE 'SALDOS ACTUALIZADOS DE CUENTAS   '                     00209304
                                   TO WS-TITULO-LEYENDA                 00209404
                                                                        00209510
           WRITE REG-SALIDA FROM WS-LINEA  AFTER PAGE                   00209604
           WRITE REG-SALIDA FROM WS-TITULO                              00209704
           WRITE REG-SALIDA FROM WS-LINEA                               00209804
                                                                        00209904
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00210004
              THRU 9996-F-VALIDAR-FS-ACTUAL                             00210104
                                                                        00210204
           PERFORM 9050-I-GRABAR-SUBTITULOS                             00210304
              THRU 9050-F-GRABAR-SUBTITULOS                             00210404
                                                                        00210504
           MOVE 5   TO  WS-CUENTA-LINEA.                                00210604
                                                                        00210704
       9000-F-GRABAR-TITULOS.   EXIT.                                   00210804
                                                                        00210904
      *********************************************************         00211004
      *  GRABAR SUBTITULOS PARA REG SALIDA CLIENTE Y CUENTA   *         00211104
      *********************************************************         00211204
       9050-I-GRABAR-SUBTITULOS.                                        00211304
                                                                        00211404
           MOVE '9050-I-GRABAR-SUBTITULOS' TO WS-PARRAFO.               00211504
                                                                        00211604
           WRITE REG-SALIDA FROM WS-SUBTITULO-TBCURCTA  AFTER 1         00212004
           WRITE REG-SALIDA FROM WS-LINEA                               00212104
                                                                        00212204
           PERFORM 9996-I-VALIDAR-FS-ACTUAL                             00212304
              THRU 9996-F-VALIDAR-FS-ACTUAL.                            00212404
                                                                        00212504
       9050-F-GRABAR-SUBTITULOS. EXIT.                                  00212604
                                                                        00212704
      *********************************************************         00212804
      *  VALIDACION FILE STATUS ARCHIVO SALIDA LISTADO        *         00212904
      *********************************************************         00213004
       9996-I-VALIDAR-FS-ACTUAL.                                        00213104
           IF FS-ACTUAL  IS NOT EQUAL '00'                              00213204
              DISPLAY '*****************************************'       00213304
              DISPLAY '*     *   FILE STATUS ERROR   *         *'       00213404
              DISPLAY '*****************************************'       00213504
              DISPLAY '* ERROR EN   : ' WS-PARRAFO                      00213604
              DISPLAY '* ARCHIVO    : ' WS-ARCHIVO-DESC                 00213704
              DISPLAY '* ERROR CODE : ' FS-ACTUAL                       00213804
              DISPLAY '*****************************************'       00213904
              MOVE 9999 TO RETURN-CODE                                  00214004
              SET  WS-FIN-PROCESO TO TRUE                               00214104
           ELSE                                                         00214204
              CONTINUE                                                  00214304
           END-IF.                                                      00214404
       9996-F-VALIDAR-FS-ACTUAL. EXIT.                                  00214504
                                                                        00214604
      **************************************                            00214704
      *  CUERPO FINAL CIERRE DE FILES      *                            00214804
      **************************************                            00214904
       9999-I-FINAL.                                                    00215004
                                                                        00215104
           PERFORM 8100-I-CIERRE-ARCHIVOS                               00215204
              THRU 8100-F-CIERRE-ARCHIVOS                               00215304
                                                                        00215404
           DISPLAY '**********************************************'.    00215504
           DISPLAY '*  TOTALES DE CONTROL DE PROCESO             *'.    00215604
           DISPLAY '**********************************************'.    00215704
           DISPLAY 'TOTAL REG. ENTRADA LEIDOS: ' WS-ENTRADA-LEIDOS.     00215916
           DISPLAY 'TOTAL REG. ENTRADA ERROR : ' WS-ENTRADA-ERROR.      00216216
           DISPLAY 'TOTAL REG. CURSOR LEIDOS : ' WS-C4-LEIDOS.          00216516
           DISPLAY '----------------------------------------------'     00216704
           DISPLAY 'TOTAL REGISTROS GRABADOS   : '  WS-GRABADOS.        00216916
           DISPLAY '**********************************************'.    00217004
                                                                        00217104
       9999-F-FINAL.                                                    00217204
           EXIT.                                                        00218000
      *                                                                 00220000
