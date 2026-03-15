****************************************                          
* COPY CPNOVCTA - LRECL=13             *                          
****************************************
01 NOVEDADES-CUENTAS.
    03 NOV-CTA-TIPCUEN          PIC X(2).
    03 NOV-CTA-NROCUEN          PIC S9(5)V USAGE COMP-3.
    03 NOV-CTA-NROCLI           PIC S9(3)V USAGE COMP-3.
    03 NOV-CTA-SALDO            PIC S9(5)V9(2) USAGE COMP-3.
    03 NOV-CTA-TIPMOV           PIC X(2).
 
****************************************                          
* CLAVES DE CORTE DE CONTROL           *                          
****************************************                          
 01  WS-CLAVE-ACTUAL-ENTRADA.                                     
     03  WS-TIPCUEN-ACT      PIC X(02) VALUE SPACES.              
     03  WS-NROCUEN-ACT      PIC S9(5)V USAGE COMP-3 VALUE ZEROES.
                                                                                            
 01  WS-CLAVE-CORTE-MAYOR.                                        
     03  WS-TIPCUEN-ANT      PIC X(02) VALUE SPACES.              
     03  WS-NROCUEN-ANT      PIC S9(5)V USAGE COMP-3 VALUE ZEROES.
                                                                  
 01  WS-CLAVE-CORTE-MENOR.                                        
     03  WS-TIPMOV-ANT       PIC X(02) VALUE SPACES.              
                                                                  
****************************************                          
* CONTADORES                           *                          
****************************************                          
 01  WS-CONTADORES.                                               
     03  WS-TOT-GRAL             PIC 9(03) VALUE ZEROS.           
     03  WS-SUB-TOT              PIC 9(03) VALUE ZEROS.           
     03  WS-SALDO-ACUMULADO      PIC S9(05) VALUE ZEROS.          
                                                                  
     03  WS-CONTADORES-CLAVE-MAYOR.                               
         05  WS-CANT-MOV         PIC 9(03) VALUE ZEROS.           
                                                                  
     03  WS-CONTADORES-MENOR.                                     
         05  WS-CANT-MOV-DB      PIC 9(03) VALUE ZEROS.           
         05  WS-CANT-MOV-CR      PIC 9(03) VALUE ZEROS.           
                                                                  
     03  WS-ENTRADA-LEIDOS       PIC 9(03) VALUE ZEROS.           
     03  WS-ENTRADA-ERROR        PIC 9(03) VALUE ZEROS.           
     03  WS-C1-LEIDOS            PIC 9(03) VALUE ZEROS.           
     03  WS-GRABADOS             PIC 9(03) VALUE ZEROS.           
*    03  WS-EDIT-SALDO           PIC $Z(05) VALUE ZEROS.          
     03  WS-EDIT                 PIC Z(03) VALUE ZEROS.           
                                                                  