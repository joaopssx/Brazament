       01  CLIENT-PJ-RECORD.
           05  PJC-CNPJ                   PIC X(14).
           05  PJC-LEGAL-NAME             PIC X(60).
           05  PJC-TRADE-NAME             PIC X(60).
           05  PJC-OPENING-DATE.
               10  PJC-OPENING-YEAR       PIC 9(04).
               10  PJC-OPENING-MONTH      PIC 9(02).
               10  PJC-OPENING-DAY        PIC 9(02).
           05  PJC-PHONE.
               10  PJC-PHONE-AREA-CODE    PIC X(02).
               10  PJC-PHONE-NUMBER       PIC X(09).
           05  PJC-EMAIL                  PIC X(60).
           05  PJC-ADDRESS.
               10  PJC-STREET             PIC X(40).
               10  PJC-STREET-NUMBER      PIC X(06).
               10  PJC-DISTRICT           PIC X(30).
               10  PJC-CITY               PIC X(30).
               10  PJC-STATE              PIC X(02).
               10  PJC-ZIP-CODE           PIC X(08).
           05  PJC-REGISTRATION-DATE.
               10  PJC-REG-YEAR           PIC 9(04).
               10  PJC-REG-MONTH          PIC 9(02).
               10  PJC-REG-DAY            PIC 9(02).
           05  PJC-STATUS                 PIC X(01).
               88  PJC-IS-ACTIVE                     VALUE "A".
               88  PJC-IS-INACTIVE                   VALUE "I".
           05  FILLER                     PIC X(12).
