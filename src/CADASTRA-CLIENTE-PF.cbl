       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADASTRA-CLIENTE-PF.
       AUTHOR. JOAOPSSX.
       DATE-WRITTEN. 2026-07-31.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENT-FILE
               ASSIGN TO WS-CLIENT-FILE-PATH
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLI-CPF
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLIENT-FILE.
       COPY "CLIENTE-PF.cpy".

       WORKING-STORAGE SECTION.
       COPY "VALIDACAO.cpy".

       01  WS-CLIENT-FILE-PATH            PIC X(64)
           VALUE "data/CLIENTES-PF.dat".

       01  WS-FILE-STATUS                 PIC X(02).
           88  WS-FILE-OK                            VALUE "00".
           88  WS-FILE-NOT-FOUND                     VALUE "35".

       01  WS-RUN-CONTROL                 PIC X(01).
           88  WS-RUN-ABORTED                        VALUE "S".

       01  WS-ERROR-COUNT                 PIC 9(02) VALUE 0.
       01  WS-PROMPT                      PIC X(50).
       01  WS-INPUT-FIELD                 PIC X(60).

       01  CLIENT-INPUT.
           05  IN-CPF                     PIC X(60).
           05  IN-FULL-NAME               PIC X(60).
           05  IN-BIRTH-DATE              PIC X(60).
           05  IN-PHONE-AREA-CODE         PIC X(60).
           05  IN-PHONE-NUMBER            PIC X(60).
           05  IN-EMAIL                   PIC X(60).
           05  IN-STREET                  PIC X(60).
           05  IN-STREET-NUMBER           PIC X(60).
           05  IN-DISTRICT                PIC X(60).
           05  IN-CITY                    PIC X(60).
           05  IN-STATE                   PIC X(60).
           05  IN-ZIP-CODE                PIC X(60).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM SHOW-HEADER
           PERFORM OPEN-CLIENT-FILE
           IF NOT WS-RUN-ABORTED
               PERFORM ASK-AND-CHECK-CPF
               IF WS-ERROR-COUNT = 0
                   PERFORM ASK-PERSONAL-DATA
                   PERFORM ASK-ADDRESS-DATA
                   PERFORM VALIDATE-PERSONAL-DATA
                   PERFORM VALIDATE-ADDRESS-DATA
               END-IF
               IF WS-ERROR-COUNT = 0
                   PERFORM STORE-CLIENT
               ELSE
                   PERFORM REPORT-REGISTRATION-REFUSED
               END-IF
               CLOSE CLIENT-FILE
           END-IF
           STOP RUN.

       SHOW-HEADER.
           DISPLAY " "
           DISPLAY "brazament - cadastro de cliente pessoa fisica"
           DISPLAY "--------------------------------------------"
           DISPLAY " ".

       OPEN-CLIENT-FILE.
           MOVE "N" TO WS-RUN-CONTROL
           OPEN I-O CLIENT-FILE
           IF WS-FILE-NOT-FOUND
               OPEN OUTPUT CLIENT-FILE
               IF WS-FILE-OK
                   CLOSE CLIENT-FILE
                   OPEN I-O CLIENT-FILE
               END-IF
           END-IF
           IF NOT WS-FILE-OK
               DISPLAY "erro: nao foi possivel abrir o arquivo de "
                       "clientes (status " WS-FILE-STATUS ")"
               DISPLAY "verifique se a pasta data/ existe"
               SET WS-RUN-ABORTED TO TRUE
           END-IF.

       ASK-AND-CHECK-CPF.
           MOVE "digite o cpf (somente numeros):" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-CPF

           MOVE "cpf" TO VAL-FIELD-LABEL
           MOVE IN-CPF TO VAL-FIELD-VALUE
           MOVE 11 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-DIGITS TO TRUE
           PERFORM RUN-CHECK

           IF WS-ERROR-COUNT = 0
               PERFORM LOOK-FOR-EXISTING-CLIENT
           END-IF.

       LOOK-FOR-EXISTING-CLIENT.
           MOVE IN-CPF(1:11) TO CLI-CPF
           READ CLIENT-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY "erro: cpf ja cadastrado no sistema"
           END-READ.

       ASK-PERSONAL-DATA.
           MOVE "digite o nome completo:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-FULL-NAME

           MOVE "digite a data de nascimento (aaaammdd):"
               TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-BIRTH-DATE

           MOVE "digite o ddd:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-PHONE-AREA-CODE

           MOVE "digite o telefone (somente numeros):" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-PHONE-NUMBER

           MOVE "digite o email:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-EMAIL.

       ASK-ADDRESS-DATA.
           MOVE "digite o logradouro:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-STREET

           MOVE "digite o numero:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-STREET-NUMBER

           MOVE "digite o bairro:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-DISTRICT

           MOVE "digite a cidade:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-CITY

           MOVE "digite a uf:" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-STATE

           MOVE "digite o cep (somente numeros):" TO WS-PROMPT
           PERFORM ASK-FOR-VALUE
           MOVE WS-INPUT-FIELD TO IN-ZIP-CODE.

       ASK-FOR-VALUE.
           DISPLAY FUNCTION TRIM(WS-PROMPT) " " WITH NO ADVANCING
           MOVE SPACES TO WS-INPUT-FIELD
           ACCEPT WS-INPUT-FIELD.

       VALIDATE-PERSONAL-DATA.
           MOVE "nome completo" TO VAL-FIELD-LABEL
           MOVE IN-FULL-NAME TO VAL-FIELD-VALUE
           MOVE 0 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-REQUIRED TO TRUE
           PERFORM RUN-CHECK

           MOVE "data de nascimento" TO VAL-FIELD-LABEL
           MOVE IN-BIRTH-DATE TO VAL-FIELD-VALUE
           MOVE 8 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-DATE TO TRUE
           PERFORM RUN-CHECK

           MOVE "ddd" TO VAL-FIELD-LABEL
           MOVE IN-PHONE-AREA-CODE TO VAL-FIELD-VALUE
           MOVE 2 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-DIGITS TO TRUE
           PERFORM RUN-CHECK

           MOVE "telefone" TO VAL-FIELD-LABEL
           MOVE IN-PHONE-NUMBER TO VAL-FIELD-VALUE
           MOVE 9 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-DIGITS-MAX TO TRUE
           PERFORM RUN-CHECK

           MOVE "email" TO VAL-FIELD-LABEL
           MOVE IN-EMAIL TO VAL-FIELD-VALUE
           MOVE 0 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-EMAIL TO TRUE
           PERFORM RUN-CHECK.

       VALIDATE-ADDRESS-DATA.
           MOVE "logradouro" TO VAL-FIELD-LABEL
           MOVE IN-STREET TO VAL-FIELD-VALUE
           MOVE 0 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-REQUIRED TO TRUE
           PERFORM RUN-CHECK

           MOVE "numero" TO VAL-FIELD-LABEL
           MOVE IN-STREET-NUMBER TO VAL-FIELD-VALUE
           MOVE 0 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-REQUIRED TO TRUE
           PERFORM RUN-CHECK

           MOVE "bairro" TO VAL-FIELD-LABEL
           MOVE IN-DISTRICT TO VAL-FIELD-VALUE
           MOVE 0 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-REQUIRED TO TRUE
           PERFORM RUN-CHECK

           MOVE "cidade" TO VAL-FIELD-LABEL
           MOVE IN-CITY TO VAL-FIELD-VALUE
           MOVE 0 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-REQUIRED TO TRUE
           PERFORM RUN-CHECK

           MOVE "uf" TO VAL-FIELD-LABEL
           MOVE IN-STATE TO VAL-FIELD-VALUE
           MOVE 0 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-STATE TO TRUE
           PERFORM RUN-CHECK

           MOVE "cep" TO VAL-FIELD-LABEL
           MOVE IN-ZIP-CODE TO VAL-FIELD-VALUE
           MOVE 8 TO VAL-EXPECTED-LENGTH
           SET VAL-CHECK-DIGITS TO TRUE
           PERFORM RUN-CHECK.

       RUN-CHECK.
           CALL "VALIDA-CAMPO" USING VALIDATION-AREA
           IF VAL-HAS-ERROR
               ADD 1 TO WS-ERROR-COUNT
               DISPLAY "erro: " FUNCTION TRIM(VAL-MESSAGE)
           END-IF.

       STORE-CLIENT.
           MOVE SPACES TO CLIENT-PF-RECORD
           MOVE IN-CPF(1:11) TO CLI-CPF
           MOVE IN-FULL-NAME TO CLI-FULL-NAME
           MOVE IN-BIRTH-DATE(1:8) TO CLI-BIRTH-DATE
           MOVE IN-PHONE-AREA-CODE(1:2) TO CLI-PHONE-AREA-CODE
           MOVE IN-PHONE-NUMBER TO CLI-PHONE-NUMBER
           MOVE IN-EMAIL TO CLI-EMAIL
           MOVE IN-STREET TO CLI-STREET
           MOVE IN-STREET-NUMBER TO CLI-STREET-NUMBER
           MOVE IN-DISTRICT TO CLI-DISTRICT
           MOVE IN-CITY TO CLI-CITY
           MOVE FUNCTION UPPER-CASE(IN-STATE(1:2)) TO CLI-STATE
           MOVE IN-ZIP-CODE(1:8) TO CLI-ZIP-CODE
           MOVE FUNCTION CURRENT-DATE(1:8) TO CLI-REGISTRATION-DATE
           SET CLI-IS-ACTIVE TO TRUE

           WRITE CLIENT-PF-RECORD
               INVALID KEY
                   ADD 1 TO WS-ERROR-COUNT
                   DISPLAY "erro: cpf ja cadastrado no sistema"
           END-WRITE

           IF WS-FILE-OK
               DISPLAY " "
               DISPLAY "cliente cadastrado com sucesso"
               DISPLAY "cpf: " CLI-CPF
               DISPLAY "nome: " FUNCTION TRIM(CLI-FULL-NAME)
           ELSE
               PERFORM REPORT-REGISTRATION-REFUSED
           END-IF.

       REPORT-REGISTRATION-REFUSED.
           DISPLAY " "
           DISPLAY "cadastro nao realizado".

       END PROGRAM CADASTRA-CLIENTE-PF.
