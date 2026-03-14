       01 WS-LINEA       PIC X(132) VALUE ALL '-'.                      02720001
                                                                        02730001
       01 WS-TITULO.                                                    02740001
          03  FILLER     PIC X(05) VALUE SPACES.                        02750001
          03  FILLER     PIC X(56) VALUE 'CONTROL DE INTEGRIDAD DE '    02760001
      -                            'CLIENTES/CUENTAS - FECHA: '.        02770001
          03  WS-FECHA.                                                 02780001
              05  WS-DD             PIC 9(02).                          02790001
              05  FILLER            PIC X       VALUE '/'.              02800001
              05  WS-MM             PIC 9(02).                          02810001
              05  FILLER            PIC X       VALUE '/'.              02820001
              05  WS-AAAA           PIC 9(04).                          02830001
          03  FILLER                PIC X(61)    VALUE SPACES.          02840001
                                                                        02850001
      ************************************************                  02860001
      *   IMPRESION DE TITULO DATOS DE TABLA CLIENTE *                  02870001
      ************************************************                  02880001
       01 WS-TITULO-TBCURCLI.                                           02890001
          03  FILLER     PIC X(05) VALUE SPACES.                        02900001
          03  FILLER     PIC X(56) VALUE 'REGISTROS DE TABLA DE '       02910001
      -                            'CLIENTES                  '.        02920001
          03  WS-FECHA.                                                 02930001
              05  WS-DD             PIC 9(02).                          02940001
              05  FILLER            PIC X       VALUE '/'.              02950001
              05  WS-MM             PIC 9(02).                          02960001
              05  FILLER            PIC X       VALUE '/'.              02970001
              05  WS-AAAA           PIC 9(04).                          02980001
          03  FILLER                PIC X(61)    VALUE SPACES.          02990001
                                                                        03000001
      ************************************************                  03010001
      *   IMPRESION DE TITULO DATOS DE TABLA CUENTAS *                  03020001
      ************************************************                  03030001
       01 WS-TITULO-TBCURCTA.                                           03040001
          03  FILLER     PIC X(05) VALUE SPACES.                        03050001
          03  FILLER     PIC X(56) VALUE 'REGISTROS DE TABLA DE '       03060001
      -                            'CUENTAS                   '.        03070001
          03  WS-FECHA.                                                 03080001
              05  WS-DD             PIC 9(02).                          03090001
              05  FILLER            PIC X       VALUE '/'.              03100001
              05  WS-MM             PIC 9(02).                          03110001
              05  FILLER            PIC X       VALUE '/'.              03120001
              05  WS-AAAA           PIC 9(04).                          03130001
          03  FILLER                PIC X(61)    VALUE SPACES.          03140001
                                                                        03150001
      ***************************************************               03160001
      *  SUBTITULOS DE LISTADO TABLA DE CLIENTES        *               03170001
      ***************************************************               03180001
                                                                        03190001
       01 WS-SUBTITULO-TBCURCLI.                                        03200001
          03 FILLER                 PIC X     VALUE '|'.                03210001
          03 FILLER                 PIC X(16) VALUE                     03220001
                         ' TIPO DOCUMENTO '.                            03230001
          03 FILLER                 PIC X     VALUE '|'.                03240001
          03 FILLER                 PIC X(15) VALUE                     03250001
                         ' NRO DOCUMENTO '.                             03260001
          03 FILLER                 PIC X     VALUE '|'.                03270001
          03 FILLER                 PIC X(17) VALUE                     03280001
                         ' SUCURSAL CUENTA '.                           03290001
          03 FILLER                 PIC X     VALUE '|'.                03300001
          03 FILLER                 PIC X(16) VALUE                     03310001
                         ' NRO DE CLIENTE '.                            03320001
          03 FILLER                 PIC X     VALUE '|'.                03330001
          03 FILLER                 PIC X(30) VALUE                     03340001
                         '   NOMBRE Y APELLIDO         '.               03350001
          03 FILLER                 PIC X     VALUE '|'.                03360001
          03 FILLER                 PIC X(21) VALUE                     03370001
                         '  FECHA DE NACIMIENTO'.                       03380001
          03 FILLER                 PIC X     VALUE '|'.                03390001
          03 FILLER                 PIC X(10) VALUE                     03400001
                         '   SEXO   '.                                  03410001
                                                                        03420001
      ****************************************************              03430001
      *         LAYOUT TABLA DE CLIENTES                 *              03440001
      ****************************************************              03450001
       01 WS-REG-SALIDA-CLI.                                            03460002
          03  FILLER              PIC X         VALUE '|'.              03470001
          03  FILLER              PIC X(04)     VALUE SPACES.           03480001
          03  REG-CLI-TIPDOC      PIC 9(02)     VALUE ZEROS.            03490002
          03  FILLER              PIC X(10)     VALUE SPACES.           03500001
          03  FILLER              PIC X         VALUE '|'.              03510001
          03  REG-CLI-NRODOC      PIC Z(11)     VALUE ZEROS.            03520002
          03  FILLER              PIC X(10)     VALUE SPACES.           03530001
          03  FILLER              PIC X         VALUE '|'.              03540001
          03  REG-CLI-NRO         PIC Z(03)     VALUE ZEROS.            03550002
          03  FILLER              PIC X(10)     VALUE SPACES.           03560001
          03  FILLER              PIC X         VALUE '|'.              03570001
          03  REG-CLI-NOMAPE      PIC X(30)     VALUE ZEROS.            03610001
          03  FILLER              PIC X         VALUE '|'.              03620001
          03  REG-CLI-FECNAC.                                           03630004
              05 REG-FECNAC-AAAA  PIC 9(04)     VALUE ZEROES.           03631004
              05 FILLER           PIC X         VALUE '/'.              03632004
              05 REG-FECNAC-MM    PIC 9(02)     VALUE ZEROES.           03633004
              05 FILLER           PIC X         VALUE '/'.              03634004
              05 REG-FECNAC-DD    PIC 9(02)     VALUE ZEROES.           03635004
          03  FILLER              PIC X         VALUE '|'.              03640001
          03  FILLER              PIC X(03)     VALUE SPACES.           03650001
          03  REG-CLI-SEXO        PIC X(10)     VALUE SPACES.           03660002
                                                                        03670001
      ****************************************************              03680001
      *  SUBTITULOS DE LISTADO TABLA DE CUENTAS          *              03690001
      ****************************************************              03700001
       01 WS-SUBTITULO-TBCURCTA.                                        03710001
          03 FILLER                 PIC X     VALUE '|'.                03720001
          03 FILLER                 PIC X(16) VALUE                     03730001
                         ' TIPO DE CUENTA '.                            03740001
          03 FILLER                 PIC X     VALUE '|'.                03750001
          03 FILLER                 PIC X(15) VALUE                     03760001
                         ' NRO DE CUENTA '.                             03770001
          03 FILLER                 PIC X     VALUE '|'.                03780001
          03 FILLER                 PIC X(17) VALUE                     03790001
                         ' SUCURSAL CUENTA '.                           03800001
          03 FILLER                 PIC X     VALUE '|'.                03810001
          03 FILLER                 PIC X(16) VALUE                     03820001
                         ' NRO DE CLIENTE '.                            03830001
          03 FILLER                 PIC X     VALUE '|'.                03840001
          03 FILLER                 PIC X(30) VALUE                     03850001
                         '   NOMBRE Y APELLIDO         '.               03860001
          03 FILLER                 PIC X     VALUE '|'.                03870001
          03 FILLER                 PIC X(10) VALUE                     03880001
                         '  SALDO  '.                                   03890001
          03 FILLER                 PIC X     VALUE '|'.                03900001
          03 FILLER                 PIC X(21) VALUE                     03910001
                         '  FECHA ACTUALIZACION'.                       03920001
                                                                        03930001
      *****************************************                         03940001
      *         LAYOUT TABLA DE CUENTAS       *                         03950001
      *****************************************                         03960001
       01 WS-REG-SALIDA-CTA.                                            03970003
          03  FILLER              PIC X         VALUE '|'.              03980001
          03  FILLER              PIC X(04)     VALUE SPACES.           03990001
          03  REG-CTA-TIPCUEN     PIC 9(02)     VALUE ZEROS.            04000001
          03  FILLER              PIC X(10)     VALUE SPACES.           04010001
          03  FILLER              PIC X         VALUE '|'.              04020001
          03  REG-CTA-NROCUEN     PIC Z(05)     VALUE ZEROS.            04030001
          03  FILLER              PIC X(10)     VALUE SPACES.           04040001
          03  FILLER              PIC X         VALUE '|'.              04050001
          03  REG-CTA-SUCUEN      PIC Z(07)     VALUE ZEROS.            04060001
          03  FILLER              PIC X(10)     VALUE SPACES.           04070001
          03  FILLER              PIC X         VALUE '|'.              04080001
          03  REG-CTA-NROCLI      PIC Z(03)     VALUE ZEROS.            04090001
          03  FILLER              PIC X(13)     VALUE SPACES.           04100001
          03  FILLER              PIC X         VALUE '|'.              04110001
          03  REG-CTA-NOMAPE      PIC X(30)     VALUE ZEROS.            04120001
          03  FILLER              PIC X         VALUE '|'.              04130001
          03  REG-CTA-SALDO       PIC $Z.ZZ9,99- VALUE ZEROS.           04140001
          03  FILLER              PIC X         VALUE '|'.              04150001
          03  FILLER              PIC X(03)     VALUE SPACES.           04160001
          03  REG-CTA-FECSAL.                                           04170004
              05 REG-FECSAL-AAAA  PIC 9(04)     VALUE ZEROES.           04170104
              05 FILLER           PIC X         VALUE '/'.              04170204
              05 REG-FECSAL-MM    PIC 9(02)     VALUE ZEROES.           04170304
              05 FILLER           PIC X         VALUE '/'.              04170404
              05 REG-FECSAL-DD    PIC 9(02)     VALUE ZEROES.           04170504
                                                                        04171003
      *****************************************                         04180003
      *   SUBTITULO LEYENDA DE ERROR          *                         04190003
      *****************************************                         04200003
       01 WS-SUB-ERROR.                                                 04210003
          03 FILLER                PIC X        VALUE '|'.              04220003
          03 FILLER                PIC X(04)    VALUE SPACES.           04230003
          03 FILLER                PIC X(15)    VALUE 'TIPO DE ERROR: '.04240003
          03 WS-DESCRIPCION        PIC X(50)    VALUE SPACES.           04250003
