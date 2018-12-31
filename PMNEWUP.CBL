      $set ans85 mf noosvs defaultbyte"00" callfh"extfh"
      *--------------------------------------------------------------*
      * PMNEWUP.CBL.
      *
      * Copyright 1989, Micro Focus Ltd.
      * Author B J Edwards
      *--------------------------------------------------------------*

       environment division.
       special-names.
            call-convention 3 is OS2API.

      *---------------------------------------------------------------*
       input-output section.
       file-control.
           select PmFile assign "pmfile.ism"
           organization is indexed
           file status is file-status
           record key is file-record-key
           access is dynamic.

      ******************* DATA DIVISION *******************************
       data division.

      *---------------------------------------------------------------*
       file section.
       fd  PmFile.
       01  PmFileRecord.
            03 file-record-key     pic x(5).
            03 file-numeric-value  pic 9(4).
            03 file-another-1      pic x(20).
            03 file-another-2      pic x(20).

        working-storage section.

        copy "fcf.78".
        copy "en.78".
        copy "em.78".
        copy "es.78".
        copy "cs.78".
        copy "wc.78".
        copy "mb.78".
        copy "wm.78".
        copy "ws.78".
        copy "vk.78".
        copy "qw.78".
        copy "swp.78".
        copy "fid.78".
        copy "mbid.78".
        copy "hwnd.78".
        copy "sptr.78".
        copy "cursor.78".

      ****************************************************************
        copy "pmnewup.cpy".
      ****************************************************************
        78  object-id-1         value 999.
        78  object-id-2         value 998.
        78  object-id-3         value 997.
        78  object-id-4         value 996.

        77  MY-MB               pic 9(4) comp-5.
        77  object-flag         pic 99 comp-5 value 0.
        77  msg-box-answer      pic 9(4) comp-5.

        01  file-status         pic xx.

        01  entry-field-contents.

          78 field-1-start value NEXT.
            03 entry-field-1         pic x(5).
          78 size-of-field-1 value NEXT - field-1-start.

            03 filler                pic x value x"00".

          78 field-2-start value NEXT.
            03 entry-field-2         pic 9(4).
          78 size-of-field-2 value NEXT - field-2-start.
            03 entry-field-2-x redefines entry-field-2 pic x(4).

            03 filler                pic x value x"00".

          78 field-3-start value NEXT.
            03 entry-field-3         pic x(20).
          78 size-of-field-3 value NEXT - field-3-start.

            03 filler                pic x value x"00".

          78 field-4-start value NEXT.
            03 entry-field-4         pic x(20).
          78 size-of-field-4 value NEXT - field-4-start.
            03 filler                pic x value x"00".

        78 valid-new-msg         value "Record written..........".
        78 valid-load-msg        value "Record read.............".
        78 valid-delete-msg      value "Record deleted..........".
        78 valid-overwrite-msg   value "Record rewritten........".
        78 not-deleted-msg       value "Record not deleted......".

        78 invalid-new-msg       value "ERROR: Record exists..........".
        78 invalid-load-msg      value "ERROR: Record not found.......".
        78 invalid-delete-msg    value "ERROR: Record not present.....".
        78 invalid-overwrite-msg value "ERROR: Record not present.....".
        78 invalid-key-msg       value "ERROR: Record key empty.......".

        78 delete-msg-confirm    value "Delete. Are you sure?".

        01 No-help-yet-message.
           03   pic x(42)
              value "This program is written using Micro Focus ".
           03   pic x(40)
              value "COBOL/2. The source for this program is ".
           03   pic x(42)
              value "available in the program PMNEWUP.CBL. The ".
           03   pic x(40)
              value "program was written by B J Edwards.".
           03   pic x     value x"00".

        01  end-message.
           03   pic x(40) value "Do you really want to end?".
           03   pic x     value x"00".

        78 no-numerics-msg       value "Numeric Characters not allowed".
        78 numerics-only-msg     value "Numeric Characters only".

        01  work-data.
            03 hab                 pic 9(9) comp-5.
            03 hmq                 pic 9(9) comp-5.
            03 hwndClient          pic 9(9) comp-5.
            03 hwndFrame           pic 9(9) comp-5.
            03 hwndParent          pic 9(9) comp-5.
            03 hwndMenu            pic 9(9) comp-5.
            03 hwndEntryField      pic xxxx comp-5.
            03 hwndEntryField-1    pic xxxx comp-5.
            03 hwndEntryField-2    pic xxxx comp-5.
            03 hwndEntryField-3    pic xxxx comp-5.
            03 hwndEntryField-4    pic xxxx comp-5.
            03 nullText            pic x  value x"00".
            03 ClientWndProc       procedure-pointer.
            03 temp-long           pic 9(9) comp-5.
            03 EntryFieldWinProc  redefines temp-long procedure-pointer.
            03 DefEntryFieldWinProc
                         REDEFINES temp-long procedure-pointer.
            03 qmsg.
               05  qmsghwnd        pic 9(9) comp-5.
               05  qmsgmsg         pic 9(4) comp-5.
               05  qmsgmp1         pic 9(9) comp-5.
               05  qmsgmp2         pic 9(9) comp-5.
               05  qmsgtime        pic 9(9) comp-5.
               05  qmsgptl.
                   07  qmsgptlx    pic 9(9) comp-5.
                   07  qmsgptly    pic 9(9) comp-5.
            03 loop-flag           pic x value 'C'.
               88  loop-end            value 'E'.
            03 bool                pic 9(4) comp-5.
               88  boolTRUE            value 1.
               88  boolFALSE           value 0.

            03  flFrameFlags        pic 9(9) comp-5.
            03  winStyle            pic 9(9) comp-5.
            03  szClientClass       pic x(10) value 'FileUpdate'.
            03  filler              pic x    value x"00".
            03  sFlag               pic 9(4)  comp-5.
            03  temp-num1           pic 9(4)  comp-5.
            03  temp-num2           pic 9(4)  comp-5.
          78  screen-message-start value NEXT.
            03  screen-message       pic x(32).
          78  size-of-message-line value NEXT - screen-message-start.

       01  workarea.
            03  temp-word       pic xx   comp-5.
            03  REDEFINES temp-word.
                05 temp-ls      pic x   comp-5.
                05 temp-ms      pic x   comp-5.

       01  field-coords.
           03  x           pic s9(4) comp-5.
           03  y           pic s9(4) comp-5.

       01  short-vars.
           03  cxChar      pic s9(4) comp-5.
           03  cxCaps      pic s9(4) comp-5.
           03  cyChar      pic s9(4) comp-5.
           03  cyDesc      pic s9(4) comp-5.
           03  cxClient    pic s9(4) comp-5.
           03  cyClient    pic s9(4) comp-5.

       01  mp3                 pic xxxx comp-5.
       01  redefines mp3.
           03  mp3w1           pic xx   comp-5.
           03  mp3w2           pic xx   comp-5.

       01  hdr1-line.
         78 hdr1-line-start value NEXT.
           03                     pic x(65) value
                       "Simple Presentation Manager, COBOL Indexed File,
      -                " update program".
         78 size-of-hdr1-line value NEXT - hdr1-line-start.
           03                     pic x value x"00".

       01 Character-bits.
            03  ACTUAL-KC-INVALIDCHAR    pic 9.
            03  ACTUAL-KC-TOGGLE         pic 9.
            03  ACTUAL-KC-INVALIDCOMP    pic 9.
            03  ACTUAL-KC-COMPOSITE      pic 9.
            03  ACTUAL-KC-DEADKEY        pic 9.
            03  ACTUAL-KC-LONEKEY        pic 9.
            03  ACTUAL-KC-PREVDOWN       pic 9.
            03  ACTUAL-KC-KEYUP          pic 9.
            03  ACTUAL-KC-ALT            pic 9.
            03  ACTUAL-KC-CTRL           pic 9.
            03  ACTUAL-KC-SHIFT          pic 9.
            03  ACTUAL-KC-SCANCODE       pic 9.
            03  ACTUAL-KC-VIRTUALKEY     pic 9.
            03  ACTUAL-KC-CHAR           pic 9.


      *---------------------------------------------------------*
        local-storage section.

        01  hps                 pic x(4) comp-5.

        01  swp.
            03                  PIC 9(4) COMP-5.
            03 win-size.
               05  sxLeft          pic x(2) comp-5.
               05  syBottom        pic x(2) comp-5.
               05  sxRight         pic x(2) comp-5.
               05  syTop           pic x(2) comp-5.
            03                     PIC 9(9) COMP-5.
            03                     PIC 9(9) COMP-5.

        01 ptl.
            03  x       pic s9(9) comp-5.
            03  y       pic s9(9) comp-5.

            copy "RECTL.CPY".

        01  mresult             pic x(4) comp-5.

      *---------------------------------------------------------*
       linkage section.
       01  hwnd                pic xxxx comp-5.
       01  msg                 pic xx   comp-5.

       01  mp1                 pic xxxx comp-5.
       01  redefines mp1.
           03  mp1w1           pic xx   comp-5.
           03  mp1w2           pic xx   comp-5.
       01  redefines mp1.
           03 fs               pic 9(4)  comp-5.
           03 cRepeat          pic 99   comp-5.
           03 scancode         pic 99   comp-5.
           03 scancode-x   redefines scancode pic x.

       01  mp2                 pic xxxx comp-5.
       01  redefines mp2.
           03  mp2w1           pic xx   comp-5.
           03  mp2w2           pic xx   comp-5.
       01  redefines mp2.
           03 chr              pic 9(4)  comp-5.
           03 chr-x        redefines chr
                               pic xx.
           03 vKey             pic 9(4)  comp-5.

      *---------------------------------------------------------*
        procedure division OS2API.
        main section.
           perform start-up
           perform register-classes
           if boolTRUE
               perform open-file
               perform create-client-window
               perform set-data-entry-first-field
               if hwndFrame not = 0
                    perform message-loop until loop-end
               end-if
               close PmFile
           end-if
           perform shut-down
           stop run.

      *---------------------------------------------------------*
       start-up section.
           perform set-procedure-entry-point
           call OS2API 'WinInitialize'
                       using   by value 0 size 2
                       returning hab

           call OS2API 'WinCreateMsgQueue'
                       using by value hab
                             by value 0 size 2
                       returning hmq.

      *---------------------------------------------------------*
       set-procedure-entry-point section.
           set ClientWndProc to ENTRY 'ClientWndProc'.

      *---------------------------------------------------------*
       register-classes section.
           call OS2API 'WinRegisterClass'
                       using by value     hab
                             by reference szClientClass
                             by value     ClientWndProc
                             by value     CS-SIZEREDRAW size 4
                             by value     0        size 2
                       returning bool.

      *---------------------------------------------------------*
       message-loop section.
           call OS2API 'WinGetMsg'
                         using   by value hab
                                 by reference qmsg
                                 by value 0            size 4
                                 by value 0            size 2
                                 by value 0            size 2
                         returning bool

           if boolFALSE
                add MB-YESNOCANCEL MB-ICONQUESTION giving MY-MB
                call OS2API 'WinMessageBox'
                      using by value HWND-DESKTOP size 4
                            by value hwndClient
                            by reference end-message
                            by reference szClientClass
                            by value 0 size 2
                            by value MY-MB
                      returning msg-box-answer
                if msg-box-answer = MBID-YES
                    set loop-end to true
                else
                    call OS2API 'WinCancelShutdown'
                         using by value hmq
                               by value 0 size 2
                end-if
           else
                call OS2API 'WinDispatchMsg'
                         using by value hab
                               by reference qmsg
           end-if.
      *---------------------------------------------------------*
       shut-down section.
           call OS2API 'WinDestroyWindow'   using by value hwndFrame
           call OS2API 'WinDestroyMsgQueue' using by value hmq
           call OS2API 'WinTerminate'       using by value hab.
      *---------------------------------------------------------*
       create-client-window section.
           compute flFrameFlags = FCF-TITLEBAR   + FCF-SYSMENU
                                + FCF-SIZEBORDER + FCF-MINBUTTON
                                + FCF-MAXBUTTON  + FCF-SHELLPOSITION
                                + FCF-TASKLIST   + FCF-MENU
                                + FCF-ACCELTABLE + FCF-ICON

           call OS2API 'WinCreateStdWindow'
                            using by value     HWND-DESKTOP size 4
                                  by value     WS-VISIBLE size 4
                                  by reference flFrameFlags
                                  by reference szClientClass
                                  by reference nulltext
                                  by value     0        size 4
                                  by value     0        size 2
                                  by value     ID-RESOURCE  size 2
                                  by reference hwndClient
                            returning hwndFrame

           call OS2API 'WinQueryWindowPos'
                          using   by value hwndFrame
                                  by reference swp
                          returning bool

           call OS2API 'WinSetWindowPos'
                                using by value hwndFrame
                                      by value HWND-TOP size 4
                                      by value 0        size 2
                                      by value 0        size 2
                                      by value 0        size 2
                                      by value 0        size 2
                                      by value SWP-ACTIVATE size 2.

      *---------------------------------------------------------------*
       set-data-entry-first-field section.
           call OS2API 'WinSetFocus'
                          using by value HWND-DESKTOP size 4
                                by value hwndEntryField-1.
      *---------------------------------------------------------------
        MyWndProc-S section.
        entry 'ClientWndProc' using by value hwnd
                              by value msg
                              by value mp1
                              by value mp2.

            move 0 to mresult
            evaluate msg

               when WM-CREATE

                   perform WM-CREATE-routine

               when WM-PAINT

                   perform WM-PAINT-routine

               when WM-SIZE

                   perform WM-SIZE-routine

               when WM-CONTROL

                   perform WM-CONTROL-routine

               when WM-COMMAND

                   perform WM-COMMAND-routine

               when WM-HELP
                   perform WM-HELP-routine

               when OTHER
                   PERFORM Call-Default-WinProc

            end-evaluate
            exit program returning mresult.

      *-----------------------------------------------------------------
       WM-CREATE-routine section.
           call OS2API 'WinQueryWindow'
                          using by value hwnd
                                by value QW-PARENT size 2
                                by value 0 size 2
                          returning hwndParent

           call OS2API 'WinWindowFromID'
                          using by value hwndParent
                                by value FID-MENU size 2
                          returning hwndMenu

           move low-values to entry-field-contents
      *    move 0 to entry-field-2
           move spaces to screen-message
           move 0 to mResult.
      *-----------------------------------------------------------------
       WM-PAINT-routine section.
           call OS2API 'WinBeginPaint'
                                using by value hwnd
                                      by value 0 size 4
                                      by value 0 size 4
                                returning hps

           call OS2API 'GpiErase'
                                using by value hps

           move 0   to x of ptl
           compute y of ptl = cyClient - 15

           call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value size-of-hdr1-line
                                      by reference hdr1-line

           compute x of ptl = cxClient / 5
           compute y of ptl = cyClient / 2 + 20

           call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value 10 size 4
                                      by reference "Record Key"

           compute x of ptl = (cxClient / 5) * 3
           compute y of ptl = cyClient / 2 + 20

           call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value 14 size 4
                                      by reference "Data Field 1"

           compute x of ptl = cxClient / 5
           compute y of ptl = cyClient / 4 + 20

           call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value 14 size 4
                                      by reference "Data Field 2"

           compute x of ptl = (cxClient / 5) * 3
           compute y of ptl = cyClient / 4 + 20

           call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value 14 size 4
                                      by reference "Data Field 3"

           move 1  to x of ptl
           move 20 to y of ptl

           call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value size-of-message-line
                                      by reference screen-message

           call OS2API 'WinEndPaint' using by value hps
           move 0 to mResult.
      *-----------------------------------------------------------------
       WM-SIZE-routine section.
           move mp2w1 to cxClient
           move mp2w2 to cyClient
           if hwndEntryField-1 not = 0
               PERFORM get-screen-contents
               PERFORM Destroy-Entry-Fields
           end-if
           PERFORM Create-Entry-Fields
           move 0 to mResult.
      *-----------------------------------------------------------------
       WM-CONTROL-routine section.
           IF  mp2 = hwndClient OR hwndFrame
               PERFORM Call-Default-WinProc
           ELSE
               EVALUATE mp1w2
                  WHEN EN-KILLFOCUS
                      perform kill-focus
                  WHEN EN-SETFOCUS
                      set EntryFieldWinProc to ENTRY 'EWndProc'
                      EVALUATE mp1w1
                         WHEN object-id-1
                            perform set-focus-1
                         WHEN object-id-2
                            perform set-focus-2
                         WHEN object-id-3
                            perform set-focus-3
                         WHEN object-id-4
                            perform set-focus-4
                      END-EVALUATE
                      PERFORM Call-Default-WinProc
               END-EVALUATE
           END-IF.
      *-----------------------------------------------------------------
       WM-COMMAND-routine section.
           evaluate mp1w1
               when IDM-READ
                   perform load-record
               when IDM-DELETE
                   perform delete-record
               when IDM-WRITE
                   perform save-new-record
               when IDM-REWRITE
                   perform overwrite-record
               when IDM-PREVIOUS
                   perform read-previous
               when IDM-NEXT
                   perform read-next
               when IDM-CLEAR
                   perform clear-record
               when IDM-EXIT
                   call OS2API 'WinSendMsg'
                      using by value hwnd
                            by value WM-CLOSE size 2
                            by value 0        size 4
                            by value 0        size 4

           end-evaluate
           move 0 to mresult.
      *-----------------------------------------------------------------
       WM-HELP-routine section.
           add MB-OK MB-ICONEXCLAMATION giving MY-MB
           call OS2API 'WinMessageBox'using
                 by value HWND-DESKTOP size 4
                 by value hwnd
                 by reference No-help-yet-message
                 by reference szClientClass
                 by value 0 size 2
                 by value MY-MB
           move 0 to mresult.
      *---------------------------------------------------------------*
       Confirm-delete-routine section.
           add MB-YESNO MB-ICONEXCLAMATION giving MY-MB
           call OS2API 'WinMessageBox' using
                 by value HWND-DESKTOP size 4
                 by value hwnd
                 by reference delete-msg-confirm
                 by reference szClientClass
                 by value 0 size 2
                 by value MY-MB
             returning msg-box-answer.
      *---------------------------------------------------------------*
       process-virtual-keys section.
           evaluate vKey
               when VK-TAB
                  perform skip-next-field
               when VK-BACKTAB
                  perform skip-previous-field
               when other
                  PERFORM Call-Default-EntryFieldWinProc
           end-evaluate.
      *---------------------------------------------------------------*
       skip-next-field section.
           if object-flag not = 0
               evaluate object-flag
                   when 1
                       move hwndEntryField-2 to hwndEntryField
                   when 2
                       move hwndEntryField-3 to hwndEntryField
                   when 3
                       move hwndEntryField-4 to hwndEntryField
                   when 4
                       move hwndEntryField-1 to hwndEntryField
               end-evaluate
               call OS2API 'WinSetFocus'
                      using by value HWND-DESKTOP size 4
                            by value hwndEntryField
           end-if.
      *---------------------------------------------------------------*
       skip-previous-field section.
           if object-flag not = 0
               evaluate object-flag
                   when 1
                       move hwndEntryField-4 to hwndEntryField
                   when 2
                       move hwndEntryField-1 to hwndEntryField
                   when 3
                       move hwndEntryField-2 to hwndEntryField
                   when 4
                       move hwndEntryField-3 to hwndEntryField
               end-evaluate
               call OS2API 'WinSetFocus'
                      using by value HWND-DESKTOP size 4
                            by value hwndEntryField
           end-if.
      *---------------------------------------------------------------*
       EntryFieldWinProc-E SECTION.
       ENTRY 'EWndProc' USING BY VALUE hwnd
                              BY VALUE msg
                              BY VALUE mp1
                              BY VALUE mp2.

           MOVE ZERO TO mresult
           EVALUATE msg
              WHEN WM-CHAR
                 perform WM-CHAR-routine

              WHEN OTHER
                 PERFORM Call-Default-EntryFieldWinProc

           END-EVALUATE
           EXIT PROGRAM RETURNING mresult.

      *-----------------------------------------------------------------
       WM-CHAR-routine section.
           perform strip-sFlag-bits
           if ACTUAL-KC-VIRTUALKEY = 1
               if ACTUAL-KC-KEYUP not = 1
                   perform process-virtual-keys
               else
                   PERFORM Call-Default-EntryFieldWinProc
               end-if
           else
               MOVE mp1w1 TO Temp-Word
               MULTIPLY 128 BY Temp-LS
               IF  Temp-LS > ZERO
      *----------------------------------------*
      * Field  1 does not allow numerics       *
      * Field  2 is numeric                    *
      * Fields 3 & 4 can be any character      *
      *----------------------------------------*
                  EVALUATE hwnd
                     WHEN hwndEntryField-1
                        IF  mp2w1 > 47 AND < 58
                           move no-numerics-msg to screen-message
                           perform display-screen-message
                           PERFORM sound-beep
                        ELSE
                           perform test-for-message-suppression
                           PERFORM Call-Default-EntryFieldWinProc
                        END-IF
                     WHEN hwndEntryField-2
                        IF  (mp2w1 > 47 AND < 58) OR mp2w1 < 32
                           perform test-for-message-suppression
                           PERFORM Call-Default-EntryFieldWinProc
                        ELSE
                           move numerics-only-msg to screen-message
                           perform display-screen-message
                           PERFORM sound-beep
                        END-IF
                     WHEN hwndEntryField-3
                     WHEN hwndEntryField-4
                        perform test-for-message-suppression
                        PERFORM Call-Default-EntryFieldWinProc
                     WHEN OTHER
                        PERFORM Call-Default-EntryFieldWinProc
                  END-EVALUATE
               ELSE
                  PERFORM Call-Default-EntryFieldWinProc
               END-IF
           END-IF.
      *-----------------------------------------------------------------
       get-screen-contents section.
      * For reasons which escape me, it seems that the size of the
      * field must be set to 1 greater than it really is. This is
      * not a bug, it is described as a feature!

            call OS2API 'WinQueryWindowText'
                             using by value hwndEntryField-1
                                   by value 6 size 2
                                   by reference entry-field-1

            call OS2API 'WinQueryWindowText'
                           using by value hwndEntryField-2
                                 by value 5 size 2
                                 by reference entry-field-2-x

            call OS2API 'WinQueryWindowText'
                           using by value hwndEntryField-3
                                 by value 21 size 2
                                 by reference entry-field-3

            call OS2API 'WinQueryWindowText'
                           using by value hwndEntryField-4
                                 by value 21 size 2
                                 by reference entry-field-4.
      *-----------------------------------------------------------------
       Create-Entry-Fields SECTION.
           compute winstyle = WS-VISIBLE + ES-LEFT +
                              ES-MARGIN
           compute x of field-coords = cxClient / 5
           compute y of field-coords = cyClient / 2
           call OS2API 'WinCreateWindow'
                       using by value hwndClient
                             by value WC-ENTRYFIELD size 4
                             by reference entry-field-1
                             by value winstyle
                             by value x of field-coords
                             by value y of field-coords
                             by value 60            size 2
                             by value 14            size 2
                             by value hwndClient
                             by value HWND-TOP      size 4
                             by value object-id-1   size 2
                             by value 0             size 4
                             by value 0             size 4
                       returning hwndEntryField-1

           move size-of-field-1 to mp3w1
           move 0 to mp3w2
           call OS2API 'WinSendMsg'
                          using   by value hwndEntryField-1
                                  by value EM-SETTEXTLIMIT   size 2
                                  by value mp3
                                  by value 0            size 4

           compute winstyle = WS-VISIBLE + ES-RIGHT +
                              ES-MARGIN
           compute x of field-coords = (cxClient / 5) * 3
           compute y of field-coords = cyClient / 2
           call OS2API 'WinCreateWindow'
                       using by value hwndClient
                             by value WC-ENTRYFIELD size 4
                             by reference entry-field-2-x
                             by value winstyle
                             by value x of field-coords
                             by value y of field-coords
                             by value 50            size 2
                             by value 14            size 2
                             by value hwndClient
                             by value HWND-TOP      size 4
                             by value object-id-2   size 2
                             by value 0             size 4
                             by value 0             size 4
                       returning hwndEntryField-2

           move size-of-field-2 to mp3w1
           move 0 to mp3w2
           call OS2API 'WinSendMsg'
                          using   by value hwndEntryField-2
                                  by value EM-SETTEXTLIMIT   size 2
                                  by value mp3
                                  by value 0        size 4


           compute winstyle = WS-VISIBLE + ES-AUTOSCROLL +
                              ES-MARGIN
           compute x of field-coords = cxClient / 5
           compute y of field-coords = cyClient / 4
           call OS2API 'WinCreateWindow'
                       using by value hwndClient
                             by value WC-ENTRYFIELD size 4
                             by reference entry-field-3
                             by value winstyle
                             by value x of field-coords
                             by value y of field-coords
                             by value 90            size 2
                             by value 14            size 2
                             by value hwndClient
                             by value HWND-TOP      size 4
                             by value object-id-3   size 2
                             by value 0             size 4
                             by value 0             size 4
                       returning hwndEntryField-3

           move size-of-field-3 to mp3w1
           move 0 to mp3w2
           call OS2API 'WinSendMsg'
                          using   by value hwndEntryField-3
                                  by value EM-SETTEXTLIMIT   size 2
                                  by value mp3
                                  by value 0            size 4

           compute winstyle = WS-VISIBLE + ES-AUTOSCROLL +
                              ES-MARGIN
           compute x of field-coords = (cxClient / 5) * 3
           compute y of field-coords = cyClient / 4
           call OS2API 'WinCreateWindow'
                       using by value hwndClient
                             by value WC-ENTRYFIELD size 4
                             by reference entry-field-4
                             by value winstyle
                             by value x of field-coords
                             by value y of field-coords
                             by value 90            size 2
                             by value 14            size 2
                             by value hwndClient
                             by value HWND-TOP      size 4
                             by value object-id-4   size 2
                             by value 0             size 4
                             by value 0             size 4
                       returning hwndEntryField-4

           move size-of-field-4 to mp3w1
           move 0 to mp3w2
           call OS2API 'WinSendMsg'
                          using   by value hwndEntryField-4
                                  by value EM-SETTEXTLIMIT   size 2
                                  by value mp3
                                  by value 0            size 4.
      *-----------------------------------------------------------------
       Destroy-Entry-Fields section.
          call OS2API 'WinDestroyWindow'
                         using by value hwndEntryField-1
          call OS2API 'WinDestroyWindow'
                         using by value hwndEntryField-2
          call OS2API 'WinDestroyWindow'
                         using by value hwndEntryField-3
          call OS2API 'WinDestroyWindow'
                         using by value hwndEntryField-4.
      *-----------------------------------------------------------------
       Call-Default-EntryFieldWinProc SECTION.
            CALL OS2API DefEntryFieldWinProc
                        using by value hwnd
                              by value msg
                              by value mp1
                              by value mp2
                        returning mresult.

      *-----------------------------------------------------------------
       Call-Default-WinProc SECTION.
            CALL OS2API 'WinDefWindowProc'
                        using by value hwnd
                              by value msg
                              by value mp1
                              by value mp2
                        returning mresult.
      *-----------------------------------------------------------------
       test-for-message-suppression section.
           if screen-message not = spaces
               move spaces to screen-message
               perform display-screen-message
           end-if.
      *-----------------------------------------------------------------
       sound-beep SECTION.
           CALL OS2API 'DOSBEEP'
                        USING BY VALUE 512 SIZE 2
                                 VALUE 50  SIZE 2.
      *-----------------------------------------------------------------
       open-file section.
           open i-o PmFile
      *-----------------------------------------------------------*
      * Create Header and Trailer records, if they are not already
      * present. These make read next and read previous simpler to
      * implement. Particularily for wrapping round the begining and
      * the end of the file.
      *-----------------------------------------------------------*
           move low-values to file-record-key
           read PmFile
               invalid key
                   move 0 to file-numeric-value
                   move all "*" to file-another-1
                   move all "*" to file-another-2
                   write PmFileRecord
           end-read
           move high-values to file-record-key
           read PmFile
               invalid key
                   move 0 to file-numeric-value
                   move all "*" to file-another-1
                   move all "*" to file-another-2
                   write PmFileRecord
           end-read.
      *-----------------------------------------------------------------
       delete-record section.
           perform get-screen-contents
           move entry-field-1 to file-record-key
           if file-record-key = spaces or low-values
               move invalid-key-msg to screen-message
               perform display-screen-message
           else
               perform confirm-delete-routine
               if msg-box-answer = MBID-YES
                   delete PmFile
                       invalid key
                           move invalid-delete-msg to screen-message
                       not invalid key
                           move valid-delete-msg to screen-message
                           move low-values to entry-field-contents
      *                    move 0 to entry-field-2
                           perform refresh-windows
                   end-delete
               else
                   move not-deleted-msg to screen-message
               end-if
               perform display-screen-message
           end-if.
      *-----------------------------------------------------------------
       read-next section.
           perform get-screen-contents
           initialize PmFileRecord
           move entry-field-1 to file-record-key.
           read PmFile
           read PmFile next
           if file-status not = "00" or file-record-key = high-values
               move low-values to file-record-key
               read PmFile
               read PmFile next
           end-if
           perform fill-screen-from-file-record
           move valid-load-msg to screen-message
           perform refresh-windows
           perform display-screen-message.
      *-----------------------------------------------------------------
       read-previous section.
           perform get-screen-contents
           initialize PmFileRecord
           move entry-field-1 to file-record-key
           read PmFile
           read PmFile previous
           if file-status not = "00" or file-record-key = low-values
               move high-values to file-record-key
               read PmFile
               read PmFile previous
           end-if
           perform fill-screen-from-file-record
           move valid-load-msg to screen-message
           perform refresh-windows
           perform display-screen-message.
      *-----------------------------------------------------------------
       clear-record section.
           move low-values to entry-field-contents
      *    move 0 to entry-field-2
           move spaces to screen-message
           perform refresh-windows
           perform display-screen-message.
      *-----------------------------------------------------------------
       load-record section.
           perform get-screen-contents
           initialize PmFileRecord
           move entry-field-1 to file-record-key.
           if file-record-key = spaces or low-values or high-values
               move invalid-key-msg to screen-message
               perform display-screen-message
           else
               read PmFile
                   invalid key
                       move low-values to entry-field-contents
      *                move 0 to entry-field-2
                       move file-record-key to entry-field-1
                       move invalid-load-msg to screen-message
                   not invalid key
                       perform fill-screen-from-file-record
                       move valid-load-msg to screen-message
               end-read
               perform refresh-windows
               perform display-screen-message
           end-if.
      *---------------------------------------------------------------*
       save-new-record section.
           perform get-screen-contents
           perform fill-file-record-from-screen
           if file-record-key = spaces or low-values or high-values
               move invalid-key-msg to screen-message
               perform display-screen-message
           else
               write PmFileRecord
                   invalid key
                       move invalid-new-msg to screen-message
                   not invalid key
                       move valid-new-msg to screen-message
               end-write
               perform display-screen-message
          end-if.
      *---------------------------------------------------------------*
       overwrite-record section.
           perform get-screen-contents
           perform fill-file-record-from-screen.
           if file-record-key = spaces or low-values or high-values
               move invalid-key-msg to screen-message
               perform display-screen-message
           else
               rewrite PmFileRecord
                   invalid key
                       move invalid-overwrite-msg to screen-message
                   not invalid key
                       move valid-overwrite-msg to screen-message
               end-rewrite
               perform display-screen-message
           end-if.
      *---------------------------------------------------------------*
       display-screen-message section.
           if screen-message(1:5) = "ERROR"
              call OS2API 'WinReleasePS'
                      using by value hps

              call OS2API 'WinMessageBox'
                      using by value HWND-DESKTOP size 4
                            by value HWND-DESKTOP size 4
                            by reference screen-message
                            by reference szClientClass
                            by value 0 size 2
                            by value MB-HELP size 2
              move spaces to screen-message
           end-if
           move 1   to RECTL-xleft
           move 15  to RECTL-yBottom
           move 300 to RECTL-xRight
           move 35  to RECTL-yTop
           call OS2API 'WinInvalidateRect'
                           using by value hwndClient
                                 by reference rectl
                                 by value 0 size 2.
      *---------------------------------------------------------------*
       fill-file-record-from-screen section.
           move entry-field-1 to file-record-key
           move entry-field-2 to file-numeric-value
           move entry-field-3 to file-another-1
           move entry-field-4 to file-another-2.
      *---------------------------------------------------------------*
       fill-screen-from-file-record section.
           move file-record-key    to entry-field-1
           move file-numeric-value to entry-field-2
           move file-another-1     to entry-field-3
           move file-another-2     to entry-field-4.
      *---------------------------------------------------------------*
       refresh-windows section.

           call OS2API 'WinSetWindowText'
                       using by value hwndEntryField-1
                             by reference entry-field-1

           call OS2API 'WinSetWindowText'
                       using by value hwndEntryField-2
                             by reference entry-field-2-x

           call OS2API 'WinSetWindowText'
                       using by value hwndEntryField-3
                             by reference entry-field-3

           call OS2API 'WinSetWindowText'
                       using by value hwndEntryField-4
                             by reference entry-field-4.
      *---------------------------------------------------------------*
       strip-sFlag-bits section.
           move fs to sFlag
           divide sFlag     by 2 giving temp-num1
                              remainder ACTUAL-KC-CHAR
           divide temp-num1 by 2 giving temp-num2
                              remainder ACTUAL-KC-VIRTUALKEY
           divide temp-num2 by 2 giving temp-num1
                              remainder ACTUAL-KC-SCANCODE
           divide temp-num1 by 2 giving temp-num2
                              remainder ACTUAL-KC-SHIFT
           divide temp-num2 by 2 giving temp-num1
                              remainder ACTUAL-KC-CTRL
           divide temp-num1 by 2 giving temp-num2
                              remainder ACTUAL-KC-ALT
           divide temp-num2 by 2 giving temp-num1
                              remainder ACTUAL-KC-KEYUP
           divide temp-num1 by 2 giving temp-num2
                              remainder ACTUAL-KC-PREVDOWN
           divide temp-num2 by 2 giving temp-num1
                              remainder ACTUAL-KC-LONEKEY
           divide temp-num1 by 2 giving temp-num2
                              remainder ACTUAL-KC-DEADKEY
           divide temp-num2 by 2 giving temp-num1
                              remainder ACTUAL-KC-COMPOSITE
           divide temp-num1 by 2 giving temp-num2
                              remainder ACTUAL-KC-INVALIDCOMP
           divide temp-num2 by 2 giving temp-num1
                              remainder ACTUAL-KC-TOGGLE
           divide temp-num1 by 2 giving temp-num2
                              remainder ACTUAL-KC-INVALIDCHAR.
      *---------------------------------------------------------------*
       kill-focus section.
           call OS2API 'WinSubClassWindow'
                           using by value mp2
                                 by value DefEntryFieldWinProc
                           returning DefEntryFieldWinProc.
      *---------------------------------------------------------------*
       set-focus-1 section.
           move 1 to object-flag
           call OS2API 'WinSubClassWindow'
                             using by value hwndEntryField-1
                                   by value EntryFieldWinproc
                             returning DefEntryFieldWinProc.
      *---------------------------------------------------------------*
       set-focus-2 section.
           move 2 to object-flag
           call OS2API 'WinSubClassWindow'
                             using by value hwndEntryField-2
                                   by value EntryFieldWinproc
                             returning DefEntryFieldWinProc.
      *---------------------------------------------------------------*
       set-focus-3 section.
           move 3 to object-flag
           call OS2API 'WinSubClassWindow'
                             using by value hwndEntryField-3
                                   by value EntryFieldWinproc
                             returning DefEntryFieldWinProc.
      *---------------------------------------------------------------*
       set-focus-4 section.
           move 4 to object-flag
           call OS2API 'WinSubClassWindow'
                             using by value hwndEntryField-4
                                   by value EntryFieldWinproc
                             returning DefEntryFieldWinProc.
