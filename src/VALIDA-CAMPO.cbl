       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA-CAMPO.
       AUTHOR. JOAOPSSX.
       DATE-WRITTEN. 2026-07-31.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VALUE-LENGTH                PIC 9(02).
       01  WS-TRAILING-SPACES             PIC 9(02).
       01  WS-LENGTH-TEXT                 PIC Z9.
       01  WS-AT-SIGN-COUNT               PIC 9(02).
       01  WS-DOT-COUNT                   PIC 9(02).
       01  WS-DAYS-IN-MONTH               PIC 9(02).
       01  WS-DATE-PARTS.
           05  WS-YEAR                    PIC 9(04).
           05  WS-MONTH                   PIC 9(02).
           05  WS-DAY                     PIC 9(02).
       01  WS-STATE-CANDIDATE             PIC X(02).
       01  WS-STATE-INDEX                 PIC 9(02).
       01  WS-STATE-SEARCH                PIC X(01).
           88  WS-STATE-WAS-FOUND                    VALUE "S".
       01  WS-STATE-LIST.
           05  FILLER                     PIC X(18)
               VALUE "ACALAPAMBACEDFESGO".
           05  FILLER                     PIC X(18)
               VALUE "MAMGMSMTPAPBPRPEPI".
           05  FILLER                     PIC X(18)
               VALUE "RJRNRSRORRSCSPSETO".
       01  WS-STATE-ENTRIES REDEFINES WS-STATE-LIST.
           05  WS-STATE-CODE              PIC X(02) OCCURS 27 TIMES.

       LINKAGE SECTION.
       COPY "VALIDACAO.cpy".

       PROCEDURE DIVISION USING VALIDATION-AREA.
       MAIN-VALIDATION.
           SET VAL-IS-OK TO TRUE
           MOVE SPACES TO VAL-MESSAGE
           PERFORM MEASURE-VALUE-LENGTH
           IF WS-VALUE-LENGTH = 0
               PERFORM REJECT-EMPTY-FIELD
           ELSE
               PERFORM APPLY-REQUESTED-CHECK
           END-IF
           GOBACK.

       APPLY-REQUESTED-CHECK.
           EVALUATE TRUE
               WHEN VAL-CHECK-REQUIRED
                   CONTINUE
               WHEN VAL-CHECK-DIGITS
                   PERFORM CHECK-EXACT-DIGITS
               WHEN VAL-CHECK-DIGITS-MAX
                   PERFORM CHECK-DIGITS-WITHIN-LIMIT
               WHEN VAL-CHECK-DATE
                   PERFORM CHECK-DATE
               WHEN VAL-CHECK-EMAIL
                   PERFORM CHECK-EMAIL
               WHEN VAL-CHECK-STATE
                   PERFORM CHECK-STATE
               WHEN OTHER
                   PERFORM REJECT-UNKNOWN-CHECK
           END-EVALUATE.

       MEASURE-VALUE-LENGTH.
           MOVE 0 TO WS-TRAILING-SPACES
           INSPECT FUNCTION REVERSE(VAL-FIELD-VALUE)
               TALLYING WS-TRAILING-SPACES FOR LEADING SPACE
           COMPUTE WS-VALUE-LENGTH =
               LENGTH OF VAL-FIELD-VALUE - WS-TRAILING-SPACES.

       CHECK-EXACT-DIGITS.
           IF WS-VALUE-LENGTH NOT = VAL-EXPECTED-LENGTH
           OR VAL-FIELD-VALUE(1:WS-VALUE-LENGTH) IS NOT NUMERIC
               SET VAL-HAS-ERROR TO TRUE
               MOVE VAL-EXPECTED-LENGTH TO WS-LENGTH-TEXT
               STRING "o campo " FUNCTION TRIM(VAL-FIELD-LABEL)
                      " deve ter " FUNCTION TRIM(WS-LENGTH-TEXT)
                      " digitos numericos"
                      DELIMITED BY SIZE INTO VAL-MESSAGE
           END-IF.

       CHECK-DIGITS-WITHIN-LIMIT.
           IF WS-VALUE-LENGTH > VAL-EXPECTED-LENGTH
           OR VAL-FIELD-VALUE(1:WS-VALUE-LENGTH) IS NOT NUMERIC
               SET VAL-HAS-ERROR TO TRUE
               MOVE VAL-EXPECTED-LENGTH TO WS-LENGTH-TEXT
               STRING "o campo " FUNCTION TRIM(VAL-FIELD-LABEL)
                      " aceita ate " FUNCTION TRIM(WS-LENGTH-TEXT)
                      " digitos numericos"
                      DELIMITED BY SIZE INTO VAL-MESSAGE
           END-IF.

       CHECK-DATE.
           IF WS-VALUE-LENGTH NOT = 8
           OR VAL-FIELD-VALUE(1:8) IS NOT NUMERIC
               PERFORM REJECT-INVALID-DATE
           ELSE
               MOVE VAL-FIELD-VALUE(1:8) TO WS-DATE-PARTS
               IF WS-MONTH < 1 OR WS-MONTH > 12
                   PERFORM REJECT-INVALID-DATE
               ELSE
                   PERFORM RESOLVE-DAYS-IN-MONTH
                   IF WS-YEAR < 1900
                   OR WS-DAY < 1
                   OR WS-DAY > WS-DAYS-IN-MONTH
                       PERFORM REJECT-INVALID-DATE
                   END-IF
               END-IF
           END-IF.

       RESOLVE-DAYS-IN-MONTH.
           EVALUATE WS-MONTH
               WHEN 2
                   IF FUNCTION MOD(WS-YEAR, 4) = 0
                   AND (FUNCTION MOD(WS-YEAR, 100) NOT = 0
                   OR FUNCTION MOD(WS-YEAR, 400) = 0)
                       MOVE 29 TO WS-DAYS-IN-MONTH
                   ELSE
                       MOVE 28 TO WS-DAYS-IN-MONTH
                   END-IF
               WHEN 4
               WHEN 6
               WHEN 9
               WHEN 11
                   MOVE 30 TO WS-DAYS-IN-MONTH
               WHEN OTHER
                   MOVE 31 TO WS-DAYS-IN-MONTH
           END-EVALUATE.

       CHECK-EMAIL.
           MOVE 0 TO WS-AT-SIGN-COUNT
           MOVE 0 TO WS-DOT-COUNT
           INSPECT VAL-FIELD-VALUE(1:WS-VALUE-LENGTH)
               TALLYING WS-AT-SIGN-COUNT FOR ALL "@"
           INSPECT VAL-FIELD-VALUE(1:WS-VALUE-LENGTH)
               TALLYING WS-DOT-COUNT FOR ALL "."
           IF WS-AT-SIGN-COUNT NOT = 1 OR WS-DOT-COUNT = 0
               SET VAL-HAS-ERROR TO TRUE
               STRING "o campo " FUNCTION TRIM(VAL-FIELD-LABEL)
                      " nao contem um email valido"
                      DELIMITED BY SIZE INTO VAL-MESSAGE
           END-IF.

       CHECK-STATE.
           IF WS-VALUE-LENGTH NOT = 2
               PERFORM REJECT-INVALID-STATE
           ELSE
               MOVE FUNCTION UPPER-CASE(VAL-FIELD-VALUE(1:2))
                   TO WS-STATE-CANDIDATE
               MOVE "N" TO WS-STATE-SEARCH
               PERFORM VARYING WS-STATE-INDEX FROM 1 BY 1
                   UNTIL WS-STATE-INDEX > 27
                      OR WS-STATE-WAS-FOUND
                   IF WS-STATE-CODE(WS-STATE-INDEX)
                      = WS-STATE-CANDIDATE
                       SET WS-STATE-WAS-FOUND TO TRUE
                   END-IF
               END-PERFORM
               IF NOT WS-STATE-WAS-FOUND
                   PERFORM REJECT-INVALID-STATE
               END-IF
           END-IF.

       REJECT-EMPTY-FIELD.
           SET VAL-HAS-ERROR TO TRUE
           STRING "campo obrigatorio nao preenchido: "
                  FUNCTION TRIM(VAL-FIELD-LABEL)
                  DELIMITED BY SIZE INTO VAL-MESSAGE.

       REJECT-INVALID-DATE.
           SET VAL-HAS-ERROR TO TRUE
           STRING "data invalida em " FUNCTION TRIM(VAL-FIELD-LABEL)
                  ", use o formato aaaammdd"
                  DELIMITED BY SIZE INTO VAL-MESSAGE.

       REJECT-INVALID-STATE.
           SET VAL-HAS-ERROR TO TRUE
           STRING "uf invalida em " FUNCTION TRIM(VAL-FIELD-LABEL)
                  DELIMITED BY SIZE INTO VAL-MESSAGE.

       REJECT-UNKNOWN-CHECK.
           SET VAL-HAS-ERROR TO TRUE
           MOVE "tipo de validacao nao reconhecido" TO VAL-MESSAGE.

       END PROGRAM VALIDA-CAMPO.
