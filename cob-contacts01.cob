000000* MIT License
      * Copyright (c) 2018 Christer Stig Åke Landstedt
      * 
      * Permission is hereby granted, free of charge, to any person obtaining a copy
      * of this software and associated documentation files (the "Software"), to deal
      * in the Software without restriction, including without limitation the rights
      * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      * copies of the Software, and to permit persons to whom the Software is
      * furnished to do so, subject to the following conditions:
      * 
      * The above copyright notice and this permission notice shall be included in all
      * copies or substantial portions of the Software.
      * 
      * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      * SOFTWARE.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob-contacts01.
       AUTHOR.  "Christer Stig Åke Landstedt".

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT DATAFILE ASSIGN TO "cob-contacts01.dat"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS CKEY.
       
       DATA DIVISION.
         FILE SECTION.
         FD DATAFILE
           RECORD CONTAINS 200 CHARACTERS.
         01 DATAFILEFD.
           05 CKEY PIC 9(4).
           05 CNAME PIC X(25).
           05 CTEL PIC X(25).
           05 CADR-S PIC X(20).
           05 CADR-P PIC X(10).
           05 CADR-C PIC X(15).
           05 CADR-N PIC X(15).
           05 CNOTE PIC X(80).
         WORKING-STORAGE SECTION.
         01 WS-ENDOFFILE PIC 9 VALUE ZERO. 
         01 WS-DATAFILEFD.
           05 WS-CKEY PIC 9(4).
           05 WS-CNAME PIC X(25).
           05 WS-CTEL PIC X(25).
           05 WS-CADR-S PIC X(20).
           05 WS-CADR-P PIC X(10).
           05 WS-CADR-C PIC X(15).
           05 WS-CADR-N PIC X(15).
           05 WS-CNOTE PIC X(100).
         01 DATEANDTIME.
           05 CURRENTDATE.
             10 YY PIC 99.
             10 MM PIC 99.
             10 DD PIC 99.
           05 CURRENTTIME.
             10 TIMEHH PIC 99.
             10 TIMEMM PIC 99.
             10 TIMESS PIC 99.
             10 TIMEMS PIC 99.
         01 CURRENTDATE2.
           05 YYYY PIC 9999.
           05 MM2 PIC 99.
           05 DD2 PIC 99.

         LOCAL-STORAGE SECTION.
         01 USER-SELECTION PIC 9 VALUE ZERO.
         01 CID-SELECTION PIC 9(4) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
       0000SELECTIONSTART.
       MOVE 0 TO USER-SELECTION.
       ACCEPT CURRENTDATE2 FROM DATE yyyymmdd.
       DISPLAY "-----------------------------".
       DISPLAY "Cobol Contacts 0.1 "YYYY"-"MM2"-"DD2.
       DISPLAY "-----------------------------".
       PERFORM UNTIL USER-SELECTION>0
         DISPLAY "MENU"
         DISPLAY "------------------------"
         DISPLAY "    1 : Contacts"
         DISPLAY "    2 : Information"
         DISPLAY "    3 : Exit application"
         DISPLAY "Select number and press Enter: "ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 GO TO 0000SELECTIONCONTACTS
           WHEN 2 GO TO 0000SELECTIONINFO
           WHEN 3 GO TO 0000SELECTIONQUIT
           WHEN OTHER PERFORM 0000SELECTIONSTARTERROR
         END-EVALUATE
       END-PERFORM.

       0000SELECTIONSTARTERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONSTART.
       
       0000SELECTIONCONTACTS.
       ACCEPT CURRENTDATE2 FROM DATE yyyymmdd.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "-------------------".
       DISPLAY "Contacts "YYYY"-"MM2"-"DD2.
       DISPLAY "-------------------".
       DISPLAY "Contact ID | Contact                   | Telephone".
       DISPLAY "-------------------------------------------------------".

       OPEN I-O DATAFILE.
       PERFORM UNTIL WS-ENDOFFILE = 1
         READ DATAFILE INTO WS-DATAFILEFD
           AT END MOVE 1 TO WS-ENDOFFILE
           NOT AT END
           DISPLAY CKEY "       | " CNAME " | " CTEL
         END-READ    
       END-PERFORM.
       CLOSE DATAFILE.
       MOVE 0 TO WS-ENDOFFILE.

       PERFORM UNTIL USER-SELECTION>0
         DISPLAY " "
         DISPLAY "----------------------------------------"
         DISPLAY "MENU"
         DISPLAY "----------------------------------------"
         DISPLAY "    1 : Add Contact"
         DISPLAY "    2 : Edit Contact"
         DISPLAY "    3 : Delete Contact"
         DISPLAY "    4 : Detailed Contact Information"
         DISPLAY "    5 : Delete ALL Contacts"
         DISPLAY "    6 : Go To Main Menu"
         DISPLAY "    7 : Exit Application"
         DISPLAY "Select number and press Enter: "
         ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 PERFORM 0000SELECTIONADDCONTACT
           WHEN 2 PERFORM 0000SELECTIONEDIT
           WHEN 3 PERFORM 0000SELECTIONDELETE
           WHEN 4 PERFORM 0000SELECTIONMORE
           WHEN 5 PERFORM 0000SELECTIONDELETEALL
           WHEN 6 PERFORM 0000SELECTIONSTART
           WHEN 7 GO TO 0000SELECTIONQUIT
           WHEN OTHER PERFORM 0000SELECTIONCONTACTSERROR
         END-EVALUATE
       END-PERFORM.

       0000SELECTIONCONTACTSERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONCONTACTS.

       0000SELECTIONADDCONTACT.
       MOVE 0 TO USER-SELECTION.

       DISPLAY " ".
       DISPLAY "Enter Contact ID:".
       ACCEPT WS-CKEY.
       IF WS-CKEY IS NUMERIC
         DISPLAY "Enter Contact Name (max 25 characters):"
         ACCEPT WS-CNAME
         ELSE
           DISPLAY "!!!ERROR WRONG INPUT"
           GO TO 0000SELECTIONADDCONTACT
       END-IF.

       MOVE " " TO WS-CTEL.
       MOVE " " TO WS-CADR-S.
       MOVE " " TO WS-CADR-P.
       MOVE " " TO WS-CADR-C.
       MOVE " " TO WS-CADR-N.
       MOVE " " TO WS-CNOTE.

       MOVE WS-CKEY TO CKEY.
       MOVE WS-CNAME TO CNAME.
       MOVE WS-CTEL TO CKEY.
       MOVE WS-CADR-S TO CNAME.
       MOVE WS-CADR-P TO CKEY.
       MOVE WS-CADR-C TO CNAME.
       MOVE WS-CADR-N TO CKEY.
       MOVE WS-CNOTE TO CNAME.

       MOVE WS-DATAFILEFD TO DATAFILEFD.

       OPEN I-O DATAFILE.
       WRITE DATAFILEFD
         INVALID KEY DISPLAY"!ERROR RECORD ALREADY EXIST!"
         NOT INVALID KEY DISPLAY "Contact Added."
       END-WRITE.
       CLOSE DATAFILE.

       GO TO 0000SELECTIONCONTACTS.

       0000SELECTIONEDIT.
         MOVE 0 TO USER-SELECTION.

         DISPLAY " ".
         DISPLAY "Enter Contact ID:".
         ACCEPT WS-CKEY.
         IF WS-CKEY IS NUMERIC
           DISPLAY "New Contact Name:"
           ACCEPT WS-CNAME
           ELSE
             DISPLAY "!!!ERROR WRONG INPUT"
             GO TO 0000SELECTIONEDIT
         END-IF.
         DISPLAY "New Contact Tel:"
         ACCEPT WS-CTEL.

         OPEN I-O DATAFILE.
         MOVE WS-CKEY TO CKEY.
         MOVE WS-CNAME TO CNAME.
         MOVE WS-CTEL TO CTEL.
           REWRITE DATAFILEFD
             INVALID KEY DISPLAY"!ERROR CONTACT DOSE NOT EXIST!"
             NOT INVALID KEY DISPLAY "Contact Edited."
           END-REWRITE.
         CLOSE DATAFILE.

       GO TO 0000SELECTIONCONTACTS.

       0000SELECTIONDELETE.
       MOVE 0 TO USER-SELECTION.
       DISPLAY " ".
       DISPLAY "-------------------------"
       DISPLAY "Enter ID Of Contact To Be Deleted:".
       ACCEPT WS-CKEY.
       IF WS-CKEY IS NUMERIC
         MOVE WS-CKEY TO CKEY
         ELSE
           DISPLAY "!ERROR WRONG INPUT!"
           GO TO 0000SELECTIONDELETE
       END-IF.

       PERFORM UNTIL USER-SELECTION>0
         DISPLAY "Are you sure that you want to delete this contact?"
         DISPLAY "    1 : Yes I want to delete this contact"
         DISPLAY "    2 : No!"
         DISPLAY "Select number and press Enter: "ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 PERFORM 0000CONTINUEDELETE
           WHEN 2 PERFORM 0000SELECTIONCONTACTS
           WHEN OTHER PERFORM 0000SELECTIONDELETEERROR
         END-EVALUATE
       END-PERFORM.

       0000CONTINUEDELETE.

       OPEN I-O DATAFILE.
       DELETE DATAFILE
         INVALID KEY DISPLAY "!ERROR CONTACT DOSE NOT EXIST!"
         NOT INVALID KEY DISPLAY "Contact Deleted."
       END-DELETE.
       CLOSE DATAFILE.

       GO TO 0000SELECTIONCONTACTS.

       0000SELECTIONDELETEERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONDELETE.

       0000SELECTIONMORE.
         MOVE 0 TO USER-SELECTION.
         DISPLAY " ".
         DISPLAY "Enter Contact ID:".
         ACCEPT CKEY.
         PERFORM 0000SELECTIONBACKTOMORE.
       0000SELECTIONBACKTOMORE.
         MOVE 0 TO USER-SELECTION.
         OPEN I-O DATAFILE.
           READ DATAFILE INTO WS-DATAFILEFD
             KEY IS CKEY
             INVALID KEY
               DISPLAY "!ERROR"
             NOT INVALID KEY
               DISPLAY "-------------------------------------------"
               DISPLAY "Detailed Contact Information"
               DISPLAY "-------------------------------------------"
               DISPLAY "Contact ID : "WS-CKEY
               DISPLAY "Contact : "WS-CNAME
               DISPLAY "Telephone: "WS-CTEL
               DISPLAY "Street: "WS-CADR-S
               DISPLAY "Post Code: "WS-CADR-P
               DISPLAY "City: "WS-CADR-C
               DISPLAY "Nation: "WS-CADR-N
               DISPLAY "Notes: "WS-CNOTE
           END-READ.
         CLOSE DATAFILE.
         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "-------------------------------------------"
           DISPLAY "MENU"
           DISPLAY "-------------------------------------------"
           DISPLAY "    1 : Edit"
           DISPLAY "    2 : Contacts"
           DISPLAY "    3 : Main Menu"
           DISPLAY "    4 : Exit application"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 GO TO 0000SELECTIONEDITMORE
             WHEN 2 GO TO 0000SELECTIONCONTACTS
             WHEN 3 GO TO 0000SELECTIONSTART
             WHEN 4 GO TO 0000SELECTIONQUIT
             WHEN OTHER PERFORM 0000SELECTIONMOREERROR
           END-EVALUATE
         END-PERFORM.

       0000SELECTIONMOREERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONMORE.

       0000SELECTIONEDITMORE.
         MOVE 0 TO USER-SELECTION.

       PERFORM UNTIL USER-SELECTION>0
         DISPLAY " "
         DISPLAY "----------------------------------------"
         DISPLAY "MENU"
         DISPLAY "----------------------------------------"
         DISPLAY "    1 : Edit Name"
         DISPLAY "    2 : Edit Telephone"
         DISPLAY "    3 : Edit Street"
         DISPLAY "    4 : Edit Post Code"
         DISPLAY "    5 : Edit City"
         DISPLAY "    6 : Edit Nation"
         DISPLAY "    7 : Edit Notes"
         DISPLAY "    8 : Go Back To Contacts"
         DISPLAY "Select number and press Enter: "
         ACCEPT USER-SELECTION

         EVALUATE USER-SELECTION
           WHEN 1 PERFORM 0000SELECTIONEDITNAME
           WHEN 2 PERFORM 0000SELECTIONEDITTEL
           WHEN 3 PERFORM 0000SELECTIONEDITSTREET
           WHEN 4 PERFORM 0000SELECTIONEDITPOST
           WHEN 5 PERFORM 0000SELECTIONEDITCITY
           WHEN 6 PERFORM 0000SELECTIONEDITNATION
           WHEN 7 PERFORM 0000SELECTIONEDITNOTES
           WHEN 8 GO TO 0000SELECTIONCONTACTS
           WHEN OTHER PERFORM 0000SELECTIONCONTACTSERROR
         END-EVALUATE
       END-PERFORM.

         0000SELECTIONEDITNAME.
           DISPLAY " ".
           DISPLAY "New Name:"
           ACCEPT WS-CNAME.
           GO TO 0000CONTINUEEDIT.
         0000SELECTIONEDITTEL.
           DISPLAY " ".
           DISPLAY "New Tel:"
           ACCEPT WS-CTEL.
           GO TO 0000CONTINUEEDIT.
         0000SELECTIONEDITSTREET.
           DISPLAY " ".
           DISPLAY "New Street:"
           ACCEPT WS-CADR-S.
           GO TO 0000CONTINUEEDIT.
         0000SELECTIONEDITPOST.
           DISPLAY " ".
           DISPLAY "New Post number:"
           ACCEPT WS-CADR-P.
           GO TO 0000CONTINUEEDIT.
         0000SELECTIONEDITCITY.
           DISPLAY " ".
           DISPLAY "New City:"
           ACCEPT WS-CADR-C.
           GO TO 0000CONTINUEEDIT.
         0000SELECTIONEDITNATION.
           DISPLAY " ".
           DISPLAY "New Nation:"
           ACCEPT WS-CADR-N.
           GO TO 0000CONTINUEEDIT.
         0000SELECTIONEDITNOTES.
           DISPLAY " ".
           DISPLAY "New Note:"
           ACCEPT WS-CTEL.
           GO TO 0000CONTINUEEDIT.

         0000CONTINUEEDIT.

         OPEN I-O DATAFILE.
         MOVE WS-CKEY TO CKEY.
         MOVE WS-CNAME TO CNAME.
         MOVE WS-CTEL TO CTEL.
         MOVE WS-CADR-S TO CADR-S
         MOVE WS-CADR-P TO CADR-P
         MOVE WS-CADR-C TO CADR-C
         MOVE WS-CADR-N TO CADR-N
         MOVE WS-CNOTE TO CNOTE
           REWRITE DATAFILEFD
             INVALID KEY DISPLAY"!ERROR CONTACT DOSE NOT EXIST!"
             NOT INVALID KEY DISPLAY "Contact Edited."
           END-REWRITE.
         CLOSE DATAFILE.

       GO TO 0000SELECTIONBACKTOMORE.

       0000SELECTIONEDITMOREERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONEDITMORE.

       0000SELECTIONDELETEALL.
         MOVE 0 TO USER-SELECTION.
         DISPLAY " ".
         DISPLAY "----------------------------------------".
         PERFORM UNTIL USER-SELECTION>0
           DISPLAY "Are you sure that you want to DELETE ALL contacts?"
           DISPLAY "    1 : Yes I want to DELETE ALL contacts."
           DISPLAY "    2 : No!"
           DISPLAY "Select number and press Enter: "
           ACCEPT USER-SELECTION

           EVALUATE USER-SELECTION
             WHEN 1 PERFORM 0000CONTINUEDELETEALL
             WHEN 2 PERFORM 0000SELECTIONCONTACTS
             WHEN OTHER PERFORM 0000SELECTIONDELETEALLERROR
           END-EVALUATE
         END-PERFORM.

       0000CONTINUEDELETEALL.

         DELETE FILE
           DATAFILE
         END-DELETE.

         OPEN OUTPUT DATAFILE.
           MOVE "0000" TO CKEY.
           MOVE "Example Contact" TO CNAME.
           MOVE "+46 771 793 336 " TO CTEL
           MOVE "Riksgatan 1" TO CADR-S
           MOVE "100 12" TO CADR-P
           MOVE "Stockholm" TO CADR-C
           MOVE "Sweden" TO CADR-N
           MOVE "Sweden is a country in Northern Europe." TO CNOTE
           WRITE DATAFILEFD
           END-WRITE.
         CLOSE DATAFILE.

         GO TO 0000SELECTIONCONTACTS.

       0000SELECTIONDELETEALLERROR.

       DISPLAY " ".
       DISPLAY "!ERROR WRONG INPUT!".
       GO TO 0000SELECTIONDELETEALL.

       0000SELECTIONINFO.
         MOVE 0 TO USER-SELECTION.

       DISPLAY " ".
       DISPLAY "-----------------------".
       DISPLAY "Application information".
       DISPLAY "-----------------------".
       DISPLAY "Application: Cobol Contacts 0.1".
       DISPLAY "Made with: Ubuntu 16.04 and GnuCobol(OpenCobol) 2.2".
       DISPLAY "---------------------------------------------------".
       DISPLAY "MIT License".
       DISPLAY "Copyright (c) 2018 Christer Stig Åke Landstedt".
       DISPLAY " ".
       DISPLAY 
        "Permission is hereby granted, free of charge, to any "
        "person obtaining a copy of this software and "
        "associated documentation files (the ""Software""), "
        "to deal in the Software without restriction, "
        "including without limitation the rights "
        "to use, copy, modify, merge, publish, distribute, "
        "sublicense, and/or sell copies of the Software,"
        "and to permit persons to whom the Software is "
        "furnished to do so, subject to the following "
        "conditions:".
       DISPLAY " ".
       DISPLAY 
        "The above copyright notice and this permission notice "
        "shall be included in all copies or substantial "
        "portions of the Software.".
       DISPLAY " ".
       DISPLAY 
        "THE SOFTWARE IS PROVIDED ""AS IS"", WITHOUT WARRANTY "
        "OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT "
        "LIMITED TO THE WARRANTIES OF MERCHANTABILITY, "
        "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. "
        "IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS "
        "BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER "
        "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR "
        "OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION "
        "WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE "
        "SOFTWARE.".
       GO TO 0000SELECTIONSTART.
       

       0000SELECTIONQUIT.
       STOP-RUN.
