       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP2.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
        
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

        SELECT MAE-TIMES    ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE IS SEQUENTIAL
                        RECORD KEY IS TIM-CLAVE
                        FILE STATUS IS TIMES-ESTADO.
        SELECT PROFESORES ASSIGN TO DISK
                          ORGANIZATION IS INDEXED
                          ACCESS MODE IS RANDOM
                          RECORD KEY IS PROF-NUMERO
                          FILE STATUS IS PROF-ESTADO.
        SELECT SUCURSALES ASSIGN TO DISK
                          ORGANIZATION IS INDEXED
                          ACCESS MODE IS RANDOM
                          RECORD KEY IS SUC-SUCURSAL
                          FILE STATUS IS SUC-ESTADO.
        SELECT TARIFAS ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE IS RANDOM
                       RECORD KEY IS TAR-CLAVE
                       FILE STATUS IS TAR-ESTADO.
        SELECT PARAMETROS ASSIGN TO DISK
                          ORGANIZATION IS SEQUENTIAL
                          FILE STATUS IS PAR-ESTADO.
        SELECT ARCHIVO-ORDENADO ASSIGN TO DISK
                          FILE STATUS IS ARCH-ESTADO.

        SELECT LISTADOTP2 ASSIGN TO PRINTER "LISTADOTP2.DAT".
        
        DATA DIVISION.
        FILE SECTION.
 
        FD MAE-TIMES    LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS "Times.dat".
        01 REG-TIMES.
            02 TIM-CLAVE.
                03 TIM-NUMERO PIC X(5).
                03 TIM-FECHA.
                   05 TIM-FECHA-ANIO PIC 9(4).
                   05 TIM-FECHA-MES PIC 99.
                   05 TIM-FECHA-DIA PIC 99.
                03 TIM-CUIT PIC 9(11).
                03 TIM-SEC  PIC 9(4).
            02 TIM-TIP-CLASE PIC X(4).
            02 TIM-HORAS PIC 9(2)V99.

        FD PROFESORES LABEL RECORD IS STANDARD
                      VALUE OF FILE-ID IS "Profesores.dat".
        01 REG-PROFESORES.
            02 PROF-NUMERO PIC X(5).
            02 PROF-DNI PIC 9(8).
            02 PROF-NOMBRE PIC X(25).
            02 PROF-DIRE PIC X(20).
            02 PROF-TEL PIC X(20).

        FD SUCURSALES LABEL RECORD IS STANDARD
                      VALUE OF FILE-ID IS "Sucursales.dat".
        01 REG-SUCURSALES.
            02 SUC-SUCURSAL PIC X(3).
            02 SUC-RAZON PIC X(25).
            02 SUC-DIRE PIC X(20).
            02 SUC-TEL PIC X(20).
            02 SUC-CUIT PIC 9(11).
        
       FD TARIFAS LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS "Tarifas.dat".
       01 REG-TARIFAS.
            02 TAR-CLAVE.
               03 TAR-TIP-CLASE PIC X(4).
               03 TAR-VIG-DES PIC 9(8).
            02 TAR-TARIFA PIC 9(5)V99.

       FD PARAMETROS LABEL RECORD IS STANDARD
                     VALUE OF FILE-ID IS "Parametros.dat".
       01 REG-PARAMETROS.
           02 PAR-CUIT-DESDE PIC 9(11).
           02 PAR-CUIT-HASTA PIC 9(11).

       SD ARCHIVO-ORDENADO DATA RECORD IS REG-ORDENADO.
       01 REG-ORDENADO.
           02 ORD-SUC-RAZON PIC X(25).
           02 ORD-SUC-CUIT PIC 9(11).
           02 ORD-TIM-FECHA.
               03 ORD-TIM-FECHA-ANIO PIC 9(4).
               03 ORD-TIM-FECHA-MES PIC 99.
               03 ORD-TIM-FECHA-DIA PIC 99.
           02 ORD-PROF-NUMERO PIC X(5).
           02 ORD-PROF-NOMBRE PIC X(25).
           02 ORD-HORAS PIC 9(2)V99.
           02 ORD-IMPORTE PIC 9(7)V99.
       FD LISTADOTP2 LABEL RECORD OMITTED.
       01 LINEA-LISTADO PIC X(60).

       WORKING-STORAGE SECTION.
       77 PAR-ESTADO PIC XX.
       77 TIMES-ESTADO PIC XX.
           88 OK-TIM VALUE '00'.
           88 NO-TIM VALUE '17'.
           88 EOF-TIM VALUE '10'.
       77 PROF-ESTADO PIC XX.
           88 OK-PROF VALUE '00'.
           88 NO-PROF VALUE '17'.
           88 EOF-PROF VALUE '10'.
       77 SUC-ESTADO PIC XX.
           88 OK-SUC VALUE '00'.
           88 NO-SUC VALUE '17'.
           88 EOF-SUC VALUE '10'.
       77 TAR-ESTADO PIC XX.
           88 OK-TAR VALUE '00'.
           88 NO-TAR VALUE '17'.
           88 EOF-TAR VALUE '10'.
       77 ARCH-ESTADO PIC XX.
           88 OK-ORD VALUE '00'.
           88 NO-ORD VALUE '17'.
           88 EOF-ORD VALUE '10'.
       77 EOF-ARCH-ORDENADO PIC XX.
           88 EOF-ARCHIVO-ORDENADO VALUE 'SI'.

       01 REG-RELEASE.
           02 REG-RELEASE-SUC-RAZON PIC X(25).
           02 REG-RELEASE-SUC-CUIT PIC 9(11).
           02 REG-RELEASE-TIM-FECHA PIC 9(8).
           02 REG-RELEASE-PROF-NUMERO PIC X(5).
           02 REG-RELEASE-PROF-NOMBRE PIC X(25).
           02 REG-RELEASE-HORAS PIC 9(2)V99.
           02 REG-RELEASE-IMPORTE PIC 9(7)V99.

        01 PROFESOR-ANTERIOR PIC X(5).
        01 SUC-ANTERIOR.
            02 ANTERIOR-SUCURSAL PIC X(3).
            02 ANTERIOR-RAZON PIC X(25).
            02 ANTERIOR-DIRE PIC X(20).
            02 ANTERIOR-TEL PIC X(20).
            02 ANTERIOR-CUIT PIC 9(11).
        01 FECHA-ANTERIOR PIC 9(8).
        01 MATCH-CUIT PIC 9(11).
        01 LINEA-A-ESCRIBIR PIC 9(2) VALUE 1.
        01 IMPORTE PIC 9(7)V99 VALUE 0.
        01 IMPORTE-FECHA PIC 9(8)V99 VALUE 0.
        01 IMPORTE-TOTAL PIC 9(10)V99 VALUE 0.
        01 AUX-TARIFA PIC 9(5)V99.
        01 WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR     PIC X(04).
               10 WS-CURRENT-MONTH    PIC X(02).
               10 WS-CURRENT-DAY     PIC X(02).
           05  WS-CURRENT-TIME.
               10 WS-CURRENT-HOUR     PIC  9(2).
               10  WS-CURRENT-MINUTE  PIC  9(2).
               10  WS-CURRENT-SECOND  PIC  9(2).
               10  WS-CURRENT-MS      PIC  9(2).
               10  WS-GMT-SIGN        PIC X(01).
               10  WS-GMT-TIME        PIC X(04).
      

       01 ENCABEZADO.
          03 FILLER PIC X VALUE SPACES.
          03 DIA PIC 99.
          03 FILLER PIC X VALUE '/'.
          03 MES PIC 99.
          03 FILLER PIC X VALUE '/'.
          03 ANIO PIC 9(4).
          03 FILLER PIC X(61) VALUE SPACES.
          03 FILLER PIC X(5) VALUE 'HOJA '.
          03 HOJA PIC 9(3) VALUE 1.
       01 LINEA-EN-BLANCO.
          03 FILLER PIC X(80) VALUE SPACES.
       01 MOSTRAR-DATOS-SUCURSAL.
          03 PRIMER-LINEA-SUCURSAL.
             05 FILLER PIC X VALUE SPACES.
             05 FILLER PIC X(10) VALUE 'Sucursal: '.
             05 FILLER PIC X(69) VALUE SPACES.
          03 SEGUNDA-LINEA-SUCURSAL.
             05 FILLER PIC X VALUE SPACES.
             05 FILLER PIC X(6) VALUE 'Cuit: '.
             05 FILLER PIC X(73) VALUE SPACES.
       01 ENCABEZADO-TABLA.
          03 FILLER PIC X(25) VALUE ' FECHA         PROFESOR  '.
          03 FILLER PIC X(31) VALUE '    NOMBRE                     '.
          03 FILLER PIC X(24) VALUE '       HORAS     IMPORTE'.
       01 LINEA-HORIZONTAL.
          03 FILLER PIC X(80) VALUE ALL "_".
       01 DATOS-TABLA.
          03 MOSTRAR-FECHA.
             05 MOSTRAR-DIA PIC 99.
             05 FILLER PIC X VALUE '/'.
             05 MOSTRAR-MES PIC 99.
             05 FILLER PIC X VALUE '/'.
             05 MOSTRAR-ANIO PIC 9(4).        
          03 FILLER PIC X(5) VALUE SPACES.
          03 MOSTRAR-PROFESOR PIC X(5).
          03 FILLER PIC X(10) VALUE SPACES.
          03 MOSTRAR-NOMBRE PIC X(25).
          03 FILLER PIC X(1) VALUE SPACES.
          03 MOSTRAR-HORAS PIC Z9,99.
          03 FILLER PIC X(1) VALUE SPACES.
          03 MOSTRAR-IMPORTE PIC ZZZZZZ9,99.
       01 LINEA-TOTAL-FECHA.
          03 FILLER PIC X(18) VALUE 'TOTALES POR FECHA:'.
          03 FILLER PIC X(44) VALUE SPACES.
          03 MOSTRAR-TOTAL-HORAS-FECHA PIC ZZ9,99.
          03 FILLER PIC X(1) VALUE SPACES.
          03 MOTRAR-TOTAL-IMPORTE-FECHA PIC ZZZZZZZ9,99.
       01 LINEA-TOTAL-GRAL.
          03 FILLER PIC X(14) VALUE 'TOTAL GENERAL:'.
          03 FILLER PIC X(53) VALUE SPACES.
          03 MOSTRAR-TOTAL-GENERAL PIC ZZZZZZZ9,99.   
       

       PROCEDURE DIVISION.
      *****************************************************
      *****************************************************
        SORT ARCHIVO-ORDENADO
             ON ASCENDING KEY ORD-SUC-RAZON
             ON ASCENDING KEY ORD-SUC-CUIT
             ON ASCENDING KEY ORD-TIM-FECHA
             ON ASCENDING KEY ORD-PROF-NUMERO
             INPUT PROCEDURE IS ENTRADA
             OUTPUT PROCEDURE IS SALIDA.
        STOP RUN.       



      *****************************************************
      *****************************************************  
       ENTRADA SECTION.
      *****************************************************
      *****************************************************
       PERFORM 0100-INICIO-ENTRADA.
       PERFORM 0200-LEER-PARAMETROS.
       PERFORM 0300-LEER-MAE-TIMES UNTIL EOF-TIM OR
       (PAR-CUIT-HASTA >=  TIM-CUIT AND TIM-CUIT >= PAR-CUIT-DESDE).
       PERFORM 0400-PROCESAR-TIMES UNTIL TIMES-ESTADO EQUAL '10'.
       PERFORM 0500-FIN-ENTRADA.
       
      *****************************************************
      *****************************************************
       0100-INICIO-ENTRADA.
        OPEN INPUT MAE-TIMES.
        OPEN INPUT PROFESORES.
        OPEN INPUT TARIFAS.
        OPEN INPUT PARAMETROS.

      *****************************************************
      *****************************************************
       0200-LEER-PARAMETROS.
        READ PARAMETROS.

      *****************************************************
      *****************************************************
       0300-LEER-MAE-TIMES.
        READ MAE-TIMES RECORD.

      *****************************************************
      *****************************************************
       0400-PROCESAR-TIMES.
        MOVE TIM-NUMERO TO PROFESOR-ANTERIOR.
        PERFORM 0600-BUSCAR-PROFESOR.
        PERFORM 0700-PROCESAR-PROFESOR UNTIL TIMES-ESTADO EQUAL'10'
                OR (PROFESOR-ANTERIOR NOT EQUAL TIM-NUMERO).
        
      *****************************************************
      *****************************************************
       0500-FIN-ENTRADA.
        CLOSE MAE-TIMES.
        CLOSE PROFESORES.
        CLOSE TARIFAS.
        CLOSE PARAMETROS.
      *****************************************************
      *****************************************************
       0600-BUSCAR-PROFESOR.
        MOVE PROFESOR-ANTERIOR TO PROF-NUMERO.
        READ PROFESORES RECORD.
        IF OK-PROF THEN
            MOVE PROF-NOMBRE TO REG-RELEASE-PROF-NOMBRE      
        ELSE
            DISPLAY "NO SE ENCONTRARON LOS DATOS DEL PROFESOR".

      *****************************************************
      *****************************************************
       0700-PROCESAR-PROFESOR.
        PERFORM 0800-BUSCAR-SUCURSAL.
        MOVE TIM-HORAS TO REG-RELEASE-HORAS.
        MOVE TIM-CUIT TO REG-RELEASE-SUC-CUIT.
        MOVE TIM-NUMERO TO REG-RELEASE-PROF-NUMERO.
        MOVE TIM-FECHA TO REG-RELEASE-TIM-FECHA.
        MOVE ANTERIOR-RAZON TO REG-RELEASE-SUC-RAZON
        PERFORM 0900-BUSCAR-TARIFAS.
        COMPUTE REG-RELEASE-IMPORTE = TIM-HORAS * AUX-TARIFA.
        RELEASE REG-ORDENADO FROM REG-RELEASE.
        MOVE 0 TO TIM-CUIT.
        PERFORM 0300-LEER-MAE-TIMES UNTIL EOF-TIM OR 
       (PAR-CUIT-HASTA >= TIM-CUIT AND TIM-CUIT >= PAR-CUIT-DESDE).
      
      *****************************************************
      *****************************************************
       0800-BUSCAR-SUCURSAL.
        OPEN INPUT SUCURSALES
        PERFORM 5000-LEER-SUCURSALES UNTIL SUC-CUIT EQUAL MATCH-CUIT.
        PERFORM UNTIL SUC-CUIT <>MATCH-CUIT OR EOF-SUC
                MOVE REG-SUCURSALES TO SUC-ANTERIOR
        PERFORM 5000-LEER-SUCURSALES
        END-PERFORM.
        DISPLAY ANTERIOR-RAZON.
        CLOSE SUCURSALES.
      *****************************************************
      *****************************************************
       5000-LEER-SUCURSALES.
        READ SUCURSALES RECORD.
      *****************************************************
      *****************************************************
       0900-BUSCAR-TARIFAS.
        MOVE TIM-TIP-CLASE TO TAR-TIP-CLASE.
        MOVE TIM-FECHA TO TAR-VIG-DES.
        READ TARIFAS RECORD KEY IS TAR-CLAVE.
        IF OK-TAR THEN
           MOVE TAR-TARIFA TO AUX-TARIFA
        ELSE
           DISPLAY "NO SE ENCONTRARON TARIFAS".

      *****************************************************
      *****************************************************
       SALIDA SECTION.
      *****************************************************
      *****************************************************
       PERFORM 1000-INICIO-SALIDA.
       MOVE 0 TO IMPORTE-TOTAL.
       PERFORM 1100-LEER-ORDENADO.
       PERFORM 1200-MOSTRAR-ENCABEZADO.
       PERFORM 1300-FIN-SALIDA.
       
      *****************************************************
      *****************************************************
       1000-INICIO-SALIDA.
        OPEN OUTPUT LISTADOTP2.
      *****************************************************
      *****************************************************
       1100-LEER-ORDENADO.
        RETURN ARCHIVO-ORDENADO AT END MOVE "SI" TO EOF-ARCH-ORDENADO.

      *****************************************************
      *****************************************************
       1200-MOSTRAR-ENCABEZADO.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR TO ANIO.
           MOVE WS-CURRENT-MONTH TO MES.
           MOVE WS-CURRENT-DAY TO DIA.
           MOVE 1 TO LINEA-A-ESCRIBIR.
           WRITE LINEA-LISTADO FROM LINEA-HORIZONTAL.
           WRITE LINEA-LISTADO FROM ENCABEZADO.
           WRITE LINEA-LISTADO FROM LINEA-EN-BLANCO.
           ADD 3 TO LINEA-A-ESCRIBIR.   
      *****************************************************
      *****************************************************
       1300-FIN-SALIDA.
           CLOSE LISTADOTP2.

