       01  CHECKING-ACCOUNT-RECORD.
           05  CHK-ACCOUNT-NUMBER         PIC X(10).
           05  CHK-BRANCH                 PIC X(04).
           05  CHK-HOLDER.
               10  CHK-HOLDER-TYPE        PIC X(01).
                   88  CHK-HOLDER-IS-PF              VALUE "F".
                   88  CHK-HOLDER-IS-PJ              VALUE "J".
               10  CHK-HOLDER-DOCUMENT    PIC X(14).
           05  CHK-BALANCE                PIC S9(13)V99    COMP-3.
           05  CHK-OVERDRAFT-LIMIT        PIC S9(13)V99    COMP-3.
           05  CHK-OPENING-DATE.
               10  CHK-OPENING-YEAR       PIC 9(04).
               10  CHK-OPENING-MONTH      PIC 9(02).
               10  CHK-OPENING-DAY        PIC 9(02).
           05  CHK-STATUS                 PIC X(01).
               88  CHK-IS-ACTIVE                     VALUE "A".
               88  CHK-IS-BLOCKED                    VALUE "B".
               88  CHK-IS-CLOSED                     VALUE "E".
           05  FILLER                     PIC X(26).
