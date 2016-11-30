       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP2.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
        
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

        SELECT TIMES    ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE IS SEQUENTIAL
                        RECORD KEY IS TIM-CLAVE.
        SELECT PROFESORES ASSIGN TO DISK
                          ORGANIZATION IS INDEXED
                          ACCESS MODE IS RANDOM
                          RECORD KEY IS PROF-NUMERO.
        SELECT SUCURSALES ASSIGN TO DISK
                          ORGANIZATION IS INDEXED
                          ACCESS MODE IS RANDOM
                          RECORD KEY IS SUC-SUCURSAL.
        SELECT TARIFAS ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE IS RANDOM
                       RECORD KEY IS TAR-CLAVE.
        SELECT PARAMETROS ASSIGN TO DISK
                          ORGANIZATION IS SEQUENTIAL.
        SELECT ARCHIVO-ORDENADO ASSIGN TO DISK.

        SELECT LISTADO ASSIGN TO PRINTER "LISTADOTP2.DAT".
        
        DATA DIVISION.
        FILE SECTION.
 
        FD TIMES    LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS "Times.dat".
        01 REG-TIMES
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


                
                                
