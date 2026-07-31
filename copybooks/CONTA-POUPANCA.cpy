       01  SAVINGS-ACCOUNT-RECORD.
           05  SAV-ACCOUNT-NUMBER         PIC X(10).
           05  SAV-BRANCH                 PIC X(04).
           05  SAV-HOLDER.
               10  SAV-HOLDER-TYPE        PIC X(01).
                   88  SAV-HOLDER-IS-PF              VALUE "F".
                   88  SAV-HOLDER-IS-PJ              VALUE "J".
               10  SAV-HOLDER-DOCUMENT    PIC X(14).
           05  SAV-BALANCE                PIC S9(13)V99    COMP-3.
           05  SAV-MONTHLY-YIELD-RATE     PIC S9(01)V9(06) COMP-3.
           05  SAV-OPENING-DATE.
               10  SAV-OPENING-YEAR       PIC 9(04).
               10  SAV-OPENING-MONTH      PIC 9(02).
               10  SAV-OPENING-DAY        PIC 9(02).
           05  SAV-STATUS                 PIC X(01).
               88  SAV-IS-ACTIVE                     VALUE "A".
               88  SAV-IS-BLOCKED                    VALUE "B".
               88  SAV-IS-CLOSED                     VALUE "E".
           05  FILLER                     PIC X(30).
