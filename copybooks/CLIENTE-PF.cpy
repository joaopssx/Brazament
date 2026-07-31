       01  CLIENT-PF-RECORD.
           05  CLI-CPF                    PIC X(11).
           05  CLI-FULL-NAME              PIC X(60).
           05  CLI-BIRTH-DATE.
               10  CLI-BIRTH-YEAR         PIC 9(04).
               10  CLI-BIRTH-MONTH        PIC 9(02).
               10  CLI-BIRTH-DAY          PIC 9(02).
           05  CLI-PHONE.
               10  CLI-PHONE-AREA-CODE    PIC X(02).
               10  CLI-PHONE-NUMBER       PIC X(09).
           05  CLI-EMAIL                  PIC X(60).
           05  CLI-ADDRESS.
               10  CLI-STREET             PIC X(40).
               10  CLI-STREET-NUMBER      PIC X(06).
               10  CLI-DISTRICT           PIC X(30).
               10  CLI-CITY               PIC X(30).
               10  CLI-STATE              PIC X(02).
               10  CLI-ZIP-CODE           PIC X(08).
           05  CLI-REGISTRATION-DATE.
               10  CLI-REG-YEAR           PIC 9(04).
               10  CLI-REG-MONTH          PIC 9(02).
               10  CLI-REG-DAY            PIC 9(02).
           05  CLI-STATUS                 PIC X(01).
               88  CLI-IS-ACTIVE                     VALUE "A".
               88  CLI-IS-INACTIVE                   VALUE "I".
           05  FILLER                     PIC X(25).
