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
       01 REG-ENTRADA     PIC X(80).                                    00040501
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
      * CURSOR C1 VALIDAR CLIENTES - DATOS              *               00092000
      ***************************************************               00092100
            EXEC SQL                                                    00092200
              DECLARE C1 CURSOR FOR                                     00092300
              SELECT *                                                  00092400
                FROM KC02803.TBCURCLI                                   00092500
              ORDER BY NROCLI ASC                                       00092600
            END-EXEC.                                                   00092700
                                                                        00092800
      *******************************************************           00092900
      * CURSOR C2 PARA VALIDAR CLIENTES DUPLICADOS          *           00093000
      *******************************************************           00093100
            EXEC SQL                                                    00093200
              DECLARE C2 CURSOR FOR                                     00093300
              SELECT *                                                  00093400
              FROM   KC02803.TBCURCLI                                   00093500
              WHERE NROCLI IN                                           00093600
                    (SELECT NROCLI                                      00093700
                       FROM KC02803.TBCURCLI                            00093800
                     GROUP BY NROCLI                                    00093900
                     HAVING COUNT(*) > 1)                               00094000
                     ORDER BY NROCLI                                    00094100
            END-EXEC.                                                   00094200
                                                                        00094300
      *********************************************************         00094400
      * CURSOR C3 SELECCIONAR CLIENTES QUE NO TIENEN CUENTAS  *         00094500
      *********************************************************         00094600
            EXEC SQL                                                    00094700
              DECLARE C3 CURSOR FOR                                     00094800
              SELECT A.TIPDOC,                                          00094900
                     A.NRODOC,                                          00095000
                     A.NROCLI,                                          00095100
                     A.NOMAPE,                                          00095200
                     A.FECNAC,                                          00095300
                     A.SEXO                                             00095400
              FROM KC02803.TBCURCLI A                                   00095500
              WHERE NOT EXISTS                                          00095600
                    (SELECT 1                                           00095700
                       FROM KC02803.TBCURCTA B                          00095800
                       WHERE B.NROCLI = A.NROCLI)                       00095900
                       ORDER BY NROCLI                                  00096000
            END-EXEC.                                                   00096100
                                                                        00096200
      ***************************************************               00096300
      * CURSOR C4 VALIDACION CUENTAS - DATOS            *               00096400
      ***************************************************               00096500
            EXEC SQL                                                    00096600
              DECLARE C4 CURSOR FOR                                     00096700
              SELECT *                                                  00096800
                FROM KC02803.TBCURCTA                                   00096900
            END-EXEC.                                                   00097000
                                                                        00097100
      *******************************************************           00097200
      * CURSOR C5 PARA VALIDAR DUPLICADOS EN TBCURCTA       *           00097300
      *******************************************************           00097400
            EXEC SQL                                                    00097500
              DECLARE C5 CURSOR FOR                                     00097600
              SELECT TIPCUEN,                                           00097700
                     NROCUEN,                                           00097800
                     SUCUEN,                                            00097900
                     NROCLI,                                            00098000
                     SALDO,                                             00098100
                     FECSAL,                                            00098200
                     COUNT(*)                                           00098300
              FROM   KC02803.TBCURCTA                                   00098400
              GROUP BY TIPCUEN,                                         00098500
                       NROCUEN,                                         00098600
                       SUCUEN,                                          00098700
                       NROCLI,                                          00098800
                       SALDO,                                           00098900
                       FECSAL                                           00099000
              HAVING COUNT(*) > 1                                       00099100
            END-EXEC.                                                   00099200
                                                                        00099300
      *******************************************************           00099400
      * CURSOR C6 SELECCIONAR CUENTAS QUE NO TIENEN CLIENTE *           00099500
      *******************************************************           00099600
            EXEC SQL                                                    00099700
              DECLARE C6 CURSOR FOR                                     00099800
              SELECT A.TIPCUEN,                                         00099900
                     A.NROCUEN,                                         00100000
                     A.SUCUEN,                                          00100100
                     A.NROCLI,                                          00100200
                     A.SALDO,                                           00100300
                     A.FECSAL                                           00100400
              FROM KC02803.TBCURCTA A                                   00100500
              LEFT JOIN KC02803.TBCURCLI B                              00100600
                     ON A.NROCLI = B.NROCLI                             00100700
              WHERE B.NROCLI IS NULL                                    00100800
              ORDER BY NROCLI ASC                                       00100900
            END-EXEC.                                                   00101000
                                                                        00101100
      ***************************************************               00101200
      * CURSOR C7 PARA TRAER LOS SALDOS NEGATIVOS       *               00101300
      ***************************************************               00101400
            EXEC SQL                                                    00101500
              DECLARE C7 CURSOR FOR                                     00101600
              SELECT *                                                  00101700
                FROM KC02803.TBCURCTA                                   00101800
               WHERE SALDO < 0                                          00101900
            END-EXEC.                                                   00102000
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
                                                                        00103900
           PERFORM 9999-I-FINAL    THRU                                 00104000
                   9999-F-FINAL.                                        00104100
                                                                        00104200
       F-MAIN-PROGRAM. GOBACK.                                          00104300
                                                                        00104400
      **************************************                            00104500
      *  CUERPO INICIO APERTURA ARCHIVOS   *                            00104600
      **************************************                            00104700
       1000-I-INICIO.                                                   00104800
                                                                        00104902
           MOVE FUNCTION CURRENT-DATE TO WS-FECHA-AUX.                  00105200
           MOVE WS-FECHA-AUX(1:4)     TO WS-AAAA.                       00105300
           MOVE WS-FECHA-AUX(5:2)     TO WS-MM.                         00105400
           MOVE WS-FECHA-AUX(7:2)     TO WS-DD.                         00105500
           MOVE 16 TO WS-CUENTA-LINEA                                   00105600
                                                                        00105700
           SET WS-SI-INICIO  TO TRUE.                                   00105803
           SET WS-SI-PROCESO TO TRUE.                                   00105903
                                                                        00109800
           PERFORM 8000-I-APERTURA-ARCHIVOS                             00111002
              THRU 8000-F-APERTURA-ARCHIVOS                             00111102
                                                                        00111202
           IF WS-SI-PROCESO                                             00112402
              PERFORM 8200-I-ABRIR-CURSOR                               00112503
                 THRU 8200-F-ABRIR-CURSOR                               00112603
           ELSE                                                         00112701
              DISPLAY '*********************************'               00112801
              DISPLAY '* ERROR EN APERTURA DE ARCHIVOS *'               00112901
              DISPLAY '* APERTURA DE CURSOR CANCELADA  *'               00113001
              DISPLAY '*********************************'               00113101
           END-IF                                                       00113203
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
                                                                        00121002
            IF WT-TIPDOC = WS-TIPDOC-ANT                                00121107
                                                                        00121207
                  ADD 1 TO WS-TOT-GRAL                                  00122107
                                                                        00122207
                  IF WT-SEXO = WS-SEXO-ANT                              00122307
                      ADD 1 TO WS-SUB-TOT-SEXO                          00122407
                  ELSE                                                  00122607
      *               --------------- CORTE DE CLAVE MENOR -----------  00124007
      *               GUARDO EL TOTAL PARCIAL DE LA CLAVE MENOR         00124107
                      EVALUATE WS-TIPDOC-ANT                            00124208
                      WHEN 'DU'                                         00124307
                          EVALUATE WS-SEXO-ANT                          00124409
                          WHEN 'F'                                      00124507
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-DU-F 00124607
                          WHEN 'M'                                      00124707
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-DU-M 00124807
                          WHEN 'O'                                      00124907
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-DU-O 00125007
                          END-EVALUATE                                  00125107
                      WHEN 'PA'                                         00125207
                          EVALUATE WS-SEXO-ANT                          00125408
                          WHEN 'F'                                      00125507
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-PA-F 00125607
                          WHEN 'M'                                      00125707
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-PA-M 00125807
                          WHEN 'O'                                      00125907
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-PA-O 00126007
                          END-EVALUATE                                  00126107
                      WHEN 'PE'                                         00126207
                          EVALUATE WS-SEXO-ANT                          00126408
                          WHEN 'F'                                      00126507
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-PE-F 00126607
                          WHEN 'M'                                      00126707
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-PE-M 00126807
                          WHEN 'O'                                      00126907
                                    ADD WS-SUB-TOT-SEXO TO WS-CANT-PE-O 00127007
                          END-EVALUATE                                  00127107
                      END-EVALUATE                                      00127207
      *               ACTUALIZO LA CLAVE MENOR                          00127407
      *               Y RESETEO EN 1 EL CONTADOR PARCIAL                00127507
      *               LA CLAVE DE CORTE MAYOR QUEDA INTACTA.            00127607
                      MOVE WT-SEXO   TO WS-SEXO-ANT                     00127707
                      MOVE 1         TO WS-SUB-TOT-SEXO                 00127807
                  END-IF                                                00127907
                                                                        00128007
            ELSE
      *               --------------- CORTE DE CLAVE MAYOR ------------ 00128107
      *             ME GUARDO EL TOTAL GENERAL DE LA CLAVE MAYOR        00128207
                    EVALUATE WS-TIPDOC-ANT                              00128608
                    WHEN 'DU'                                           00128707
                              ADD WS-TOT-GRAL TO WS-CANT-DU             00128807
                    WHEN 'PA'                                           00128907
                              ADD WS-TOT-GRAL TO WS-CANT-PA             00129007
                    WHEN 'PE'                                           00129107
                              ADD WS-TOT-GRAL TO WS-CANT-PE             00129207
                    END-EVALUATE                                        00129307
                                                                        00129407
      *             RESETEO EN 1 EL CONTADOR GENERAL                    00129507
      *             RESETEO EN 0 EL CONTADOR PARCIAL DE LA CLAVE MENOR  00129607
      *             ACTUALIZO LA CLAVE MAYOR.                           00129707
                    MOVE 1         TO WS-TOT-GRAL                       00129807
                    MOVE ZEROES    TO WS-SUB-TOT-SEXO                   00129907
                    MOVE WT-TIPDOC TO WS-TIPDOC-ANT                     00130007
            END-IF.                                                     00130107
                                                                        00130207
      *     PERFORM 8050-I-LEER-ENTRADA                                 00130307
      *        THRU 8050-F-LEER-ENTRADA                                 00130407
                                                                        00130507
      *    VOY A LEER EL SIGUIENTE REGISTRO QUE ACTUALIZA CLAVES        00130607
      *    MAYOR Y MENOR ACTUALES Y VUELVE A PROCESAR.                  00130707
           PERFORM 8400-I-LEER-CURSOR                                   00130807
              THRU 8400-F-LEER-CURSOR                                   00130907
            .                                                           00131007
       2000-F-PROCESO. EXIT.                                            00140007
                                                                        00151702
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
                                                                        00170202
      *    IF WS-TIPO-TABLA(ID-CURSOR) = 'CLIENTE'                      00170304
                WRITE REG-SALIDA FROM WS-REG-SALIDA-CLI  AFTER 1        00170400
      *    END-IF                                                       00170504
                                                                        00170600
      *    IF WS-TIPO-TABLA(ID-CURSOR) = 'CUENTAS'                      00170704
                WRITE REG-SALIDA FROM WS-REG-SALIDA-CTA  AFTER 1        00170800
      *    END-IF                                                       00170904
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
           IF FS-ENTRADA = '00' AND  FS-SALIDA  = '00'                  00185302
              PERFORM 8200-I-ABRIR-CURSOR                               00185404
                 THRU 8200-F-ABRIR-CURSOR                               00185504
           ELSE                                                         00185602
              DISPLAY '*********************************'               00185702
              DISPLAY '* ERROR EN APERTURA DE ARCHIVOS *'               00185802
              DISPLAY '* APERTURA DE CURSOR CANCELADA  *'               00185902
              DISPLAY '*********************************'               00186002
           END-IF.                                                      00186102
                                                                        00186202
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
      *    IF WS-TIPO-TABLA(ID-CURSOR) = 'CLIENTE'                      00205505
                MOVE 'CONTROL DE INTEGRIDAD DE CLIENTES'                00205603
                                        TO WS-TITULO-LEYENDA            00205703
                WRITE REG-SALIDA FROM WS-LINEA  AFTER PAGE              00205803
                WRITE REG-SALIDA FROM WS-TITULO                         00205903
                WRITE REG-SALIDA FROM WS-LINEA                          00206003
      *    END-IF                                                       00206105
                                                                        00206203
      *    IF WS-TIPO-TABLA(ID-CURSOR) = 'CUENTAS'                      00206305
                MOVE 'CONTROL DE INTEGRIDAD DE CUENTAS '                00206403
                                        TO WS-TITULO-LEYENDA            00206503
                WRITE REG-SALIDA FROM WS-LINEA  AFTER PAGE              00206603
                WRITE REG-SALIDA FROM WS-TITULO                         00206703
                WRITE REG-SALIDA FROM WS-LINEA                          00206803
      *    END-IF                                                       00206905
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
      *    IF WS-TIPO-TABLA(ID-CURSOR) = 'CLIENTE'                      00208805
                WRITE REG-SALIDA FROM WS-SUBTITULO-TBCURCLI  AFTER 1    00208903
                WRITE REG-SALIDA FROM WS-LINEA                          00209003
      *    END-IF                                                       00209105
                                                                        00209203
      *    IF WS-TIPO-TABLA(ID-CURSOR) = 'CUENTAS'                      00209305
                WRITE REG-SALIDA FROM WS-SUBTITULO-TBCURCTA  AFTER 1    00209403
                WRITE REG-SALIDA FROM WS-LINEA                          00209503
      *    END-IF                                                       00209605
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
