       01  VALIDATION-AREA.
           05  VAL-CHECK-TYPE             PIC X(01).
               88  VAL-CHECK-REQUIRED                VALUE "R".
               88  VAL-CHECK-DIGITS                  VALUE "D".
               88  VAL-CHECK-DIGITS-MAX              VALUE "N".
               88  VAL-CHECK-DATE                    VALUE "T".
               88  VAL-CHECK-EMAIL                   VALUE "M".
               88  VAL-CHECK-STATE                   VALUE "U".
           05  VAL-FIELD-LABEL            PIC X(20).
           05  VAL-FIELD-VALUE            PIC X(60).
           05  VAL-EXPECTED-LENGTH        PIC 9(02).
           05  VAL-RESULT                 PIC X(01).
               88  VAL-IS-OK                         VALUE "S".
               88  VAL-HAS-ERROR                     VALUE "N".
           05  VAL-MESSAGE                PIC X(70).
