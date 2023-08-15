* Scroll 80 column text screen function for BASIC programs
* V 1.00 by Glen Hewlett

* Sample BASIC program:
*
* 10 'STOP SPACE & UNDERSCORE BEING DRAWN AFTER A PRINT
* 20 POKE&HF812,&H12:POKE&HF813,&H12
* 30 WIDTH80
* 40 CLEAR 255,&H7DE2
* 50 LOADM"SCROLL80"
* 60 L=&HFA7A
* 70 DEFUSR0=L+&H15
* 80 '0 = DOWN, 1 = RIGHT, 2 = LEFT, 3 = UP
* 90 POKEL-7,2' DIRECTION
* 100 POKEL-6,22' X POSITION
* 110 POKEL-5,5' Y POSITION
* 120 POKEL-4,25' WIDTH
* 130 POKEL-3,10' HEIGHT
* 140 E=USR0(L)'SETUP SCROLL
* 150 IF E=0 THEN GOTO 230
* 160 CLS
* 170 PRINT"ERROR SETTING UP SCROLL WINDOW"
* 180 IF E AND 1 THEN PRINT"USED NEGATIVE NUMBERS"
* 190 IF E AND 2 THEN PRINT"WIDTH + STARTING X IS TOO WIDE"
* 200 IF E AND 4 THEN PRINT"HEIGHT + STARTING Y IS TOO HIGH"
* 210 IF E AND 8 THEN PRINT"DIRECTION MUST BE 0 TO 3 MAX"
* 220 END
* 230 FOR Y=5 TO 14
* 240 LOCATE 21,Y:PRINTCHR$(124);:LOCATE 47,Y:PRINTCHR$(124);
* 250 NEXT Y
* 260 FOR X=22 TO 22+24
* 270 LOCATE X,4:PRINTCHR$(45);
* 280 LOCATE X,15:PRINTCHR$(45);
* 290 NEXT X
* 300 LOCATE 21,4:PRINTCHR$(43);
* 310 LOCATE 21,15:PRINTCHR$(43);
* 320 LOCATE 47,4:PRINTCHR$(43);
* 330 LOCATE 47,15:PRINTCHR$(43);
* 340 LOCATE46,RND(10)+4:PRINTCHR$(RND(64)+64);
* 350 EXEC L 'DO THE SCROLL
* 360 GOTO 340
* 

* 
* ROM Calls for BASIC ROM version 1.2 & CoCo 3
INTCNV          EQU     $B3ED           * Convert FPA0 to a signed 2-byte integer; return the value in ACCD.
GIVABF          EQU     $B4F4           * Convert the value in ACCD into a floating point number in FPA0.
UnPackXToFPA0   EQU     $BC14           * COPY A PACKED FP NUMBER FROM (X) TO FPA0
PackFPA0AtX     EQU     $BC35           * PACK FPA0 AND SAVE IT IN ADDRESS POINTED TO BY X
MaxWidth        EQU     80              * 80/40 columns
MaxHeight       EQU     24              * 24 rows

        ORG     $FA73
* User variables
Direction       RMB     1               * 0 = Down, 1 = Right, 2 = Left, 3 = Up Same as Print@ (range of 0 to 511)
StartX          RMB     1               * X starting location
StartY          RMB     1               * Y starting location
Width           RMB     1               * Width of scroll window (Max 32)
Height          RMB     1               * Height of scroll window (Max 16)

* Program variable space
StackPointer    RMB     2               * Backup S stack before stack blasting

;        ORG     $FA0C           * Start of program (let the user decide where to load it with LOADM"TXTSCRLL",&H7000)
CoCo_START:
***********************************************************
* BASIC's scroll command CPU cycles: 3 + (21 * 240) + 21 + (6 + (13 * 32) + 5) = 5,491 total cycles 
* BASIC's 40 and 80 column screen is in BLOCK $36 (BASIC uses this at $2000 to $2F00 for 80 column and $2780 for 40 columns)

* 80 columns * 2 = 160 byes per row
* 24 rows        = 160 * 24 = 3840 = $F00 bytes

DoScroll:
        PSHS    CC,D,DP,X,Y,U   * Save the registers
        ORCC    #$50            * Disable interrupts
        LDB     #$36
        STB     $FFA1           * Move Text screen to $2000-$3FFF
PrepB:
        LDB     #$FF            * Self Mod value for B (Height)
PrepX:
        LDX     #$FFFF          * Self mod value for X (starting location)
PrepY:
        LDY     #$FFFF          * Self mod value for Y (2nd Jmp location)
PrepScrollJump:
        JMP     $FFFF           * Self mod to jump to the preset scroll code
SetupScroll:
        PSHS    CC,D,DP,X,Y,U   * Save the registers
        ORCC    #$50            * Disable interrupts
* Error codes:
NegValue        EQU     1       * (set bit 0) one of the setup values is a negative
TooWide         EQU     2       * (set bit 1) Settings are too wide goes past 0 to 31 columns
TooTall         EQU     4       * (set bit 2) Settings are too high, goes past 0 to 15 rows
DirectError     EQU     8       * (set bit 3) Direction value is not between 0 and 3
* Test values are safe
        CLRB
        LDA     StartX          * Get the x start location
        ORA     Width           * OR with Width
        ORA     StartY          * OR the y start location
        ORA     Height          * OR the height
        BPL     >               * If all are positive then good so far
        ORB     #NegValue       * ERROR: at least one setting is a negative value
!       LDA     StartX          * Get the x start location
        ADDA    Width           * Add the width
        BCC     >
        ORB     #TooWide        * ERROR: If we've gone over 255 then too big, return with ERROR
!       CMPA    #MaxWidth       * 1 to MaxWidth is the text screen width
        BLS     >               * if lower or the same as MaxHeight-1 then good
        ORB     #TooWide        * ERROR: too wide
!       LDA     StartY          * Get the y start location
        ADDA    Height          * Add the height
        BCC     >
        ORB     #TooTall        * ERROR: If we've gone over 255 then too big, return with ERROR
!       CMPA    #MaxHeight      * 1 to MaxHeight is the text screen height
        BLS     >               * If less or the same then good skip ahead
        ORB     #TooTall        * ERROR: If we've gone over 255 then too big, return with ERROR
!       LDA     Direction       * Get the direction the user wants to scroll, 0 = Down, 1 = Right, 2 = Left, 3 = Up
        CMPA    #4              * Is it lower then 4?
        BLO     >               * Skip ahead if so, good direction #
        ORB     #DirectError    * Value of the direction is not between 0 and 3
!       TSTB                    * Did we find any errors?
        BEQ     >               * If no erros skip ahead
SendErrorOut:        
        CLRA                    * D now has the Error code
        JSR     GIVABF          * Convert the value in ACCD into a floating point number in FPA0.
        PULS    CC,D,DP,X,Y,U,PC  * Restore & Return to BASIC
!       LDA     #MaxWidth*2     * Two bytes per character (character+attribute byte)
        LDB     StartY
        MUL                     * D has the row to start address
        TFR     D,X
        LDB     StartX
        LSLB                    * StartX * 2
        ABX                     * X = The users starting location
        LDA     Direction       * Get the direction the user wants to scroll, 0 = Down, 1 = Right, 2 = Left, 3 = Up
        BEQ     ScrollDown 
        DECA
        LBEQ    ScrollRight
        DECA
        LBEQ    ScrollLeft
* If we get here then we Scroll Up
* Enter with X = users screen starting location
SetupUp:
        LDB     Width           * B is the width
        LBSR    SetJumps        * Setup the jumps to blast a row on screen
        LEAX    $2000+MaxWidth*2,X  * Now points to the top left corner of the users requested window + down a row
        LDB     Height          * B = Height
        DECB                    * Decrement the number of rows to copy
        LEAU    DoUp,PCR        * Get address for doing the Scrolling Up routine
SavePresets:
        STB     PrepB+1,PCR     * Self mod B's value for doing the actual Scroll
        STX     PrepX+1,PCR     * Self mod B's value for doing the actual Scroll
        STY     PrepY+2,PCR     * Self mod B's value for doing the actual Scroll
        STU     PrepScrollJump+1,PCR     * Self mod B's value for doing the actual Scroll
        LDD     #$0000          * Return with a value of zero to signify no errors occurred setting up the scroll area
        JSR     GIVABF          * Convert the value in ACCD into a floating point number in FPA0.
        PULS    CC,D,DP,X,Y,U,PC  * All Preset - Restore & Return to BASIC
 
DoUp:
!       LEAU    -MaxWidth*2,X           * U is now the row below X on screen
        BSR     Blast           * Blast (copy) a row of RAM from X to U
        LEAX    MaxWidth*2,X            * Move X down a row
        DECB                    * Decrement our counter
        BNE     <               * If not done, go do another row
        LDB     #$39
        STB     $FFA1           * Restore block $2000-$3FFF to normal BASIC block
        PULS    CC,D,DP,X,Y,U,PC  * Restore & Return to BASIC

* Scroll Down
* Enter with X = users screen starting location
ScrollDown:
        LDB     Width           * B is the width
        BSR     SetJumps        * Setup the jumps to blast a row on screen
        LDA     #MaxWidth*2     * Number of bytes per row
        LDB     Height          * B = Height
        MUL
        ADDD    #$2000-MaxWidth*2*2       * Offset it to the start of the Text Screen in RAM
        LEAX    D,X
        LDB     Height          * B = Height
        DECB                    * Decrement the number of rows to copy
        LEAU    DoDown,PCR      * Get address for doing the Scrolling Down routine
        BRA     SavePresets     * Save values for doing the sctual scrolling and return
DoDown:
!       LEAU    MaxWidth*2,X            * U is the row below X on screen
        BSR     Blast           * Blast (copy) a row of RAM from X to U
        LEAX    -MaxWidth*2,X           * Move X up a row
        DECB                    * Decrement our counter
        BNE     <               * If not done, go do another row
        LDB     #$39
        STB     $FFA1           * Restore block $2000-$3FFF to normal BASIC block
        PULS    CC,D,DP,X,Y,U,PC  * Restore & Return to BASIC

* X = Source address
* U = Destination address
Blast:
        PSHS    B,X,Y,U         * Save Row counter B & starting points of X & U
        STS     StackPointer    * Save S
FirstJump:
        JMP     $FFFF           * Go do First routine then 2nd routine (This address get's self modded)

* Enter with X = users screen starting location
ScrollRight:
        LEAX    $2000-4,X        * Offset it to the start of the Text Screen in RAM
        LDB     Width           * B is the width
        DECB
        ABX                     * Move X starting position to the right (we work right to left)
        BSR     SetJumpsR       * Setup the jumps to blast a row on screen to the right
        LDB     Height          * B = Height
        LEAU    DoRight,PCR     * Get address for doing the Scrolling Right routine
        BRA     SavePresets     * Save values for doing the sctual scrolling and return
DoRight:
!       LEAU    2,X             * U is the byte to the right
        BSR     Blast           * Blast (copy) a row of RAM from X to U
        LEAX    MaxWidth*2,X    * Move X down a row
        DECB                    * Decrement our counter
        BNE     <               * If not done, go do another row
        LDB     #$39
        STB     $FFA1           * Restore block $2000-$3FFF to normal BASIC block
        PULS    CC,D,DP,X,Y,U,PC  * Restore & Return to BASIC

* Enter with X = users screen starting location
ScrollLeft:
        LEAX    $2002,X         * Offset it to the start of the Text Screen in RAM
        LDB     Width           * B is the width
        DECB
        BSR     SetJumps        * Setup the jumps to blast a row on screen
        LDB     Height          * B = Height
        LEAU    DoLeft,PCR      * Get address for doing the Scrolling Left routine
        LBRA    SavePresets     * Save values for doing the sctual scrolling and return
DoLeft:
!       LEAU    -2,X            * U is the byte to the left
        BSR     Blast           * Blast (copy) a row of RAM from X to U
        LEAX    MaxWidth*2,X    * Move X down a row
        DECB                    * Decrement our counter
        BNE     <               * If not done, go do another row
        LDB     #$39
        STB     $FFA1           * Restore block $2000-$3FFF to normal BASIC block
        PULS    CC,D,DP,X,Y,U,PC  * Restore & Return to BASIC

* Set two required jumps to do a row
* Enter with B = the width of window (Range of 0 to 79)
SetJumps:
        PSHS    D,X,U           * Save registers
        CLRA
        LSLB
        LSLB
        ROLA                    * D = B * 2 New range is 0 to 320
        LEAX    BlastTable,PCR  * X = the current BlastTable entry lcoation
        LEAX    D,X             * Move X to the correct entry in the table
        LDD     ,X              * D = the correct jump address for the routine
        LEAU    D,X             * Add the offset to the jump location
        STU     FirstJump+1,PCR * Save the First Jump value to be used later (Self Mod)
        LDD     2,X             * D = the correct jump address for the routine
        LEAY    D,X             * Make Y the 2nd jump location
        PULS    D,X,U,PC

SetJumpsR:
        PSHS    D,X,U           * Save registers
        CLRA
        LSLB
        LSLB
        ROLA                    * D = B * 2 New range is 0 to 320
        LEAX    BlastTableR,PCR  * X = the current BlastTable entry lcoation
        LEAX    D,X             * Move X to the correct entry in the table
        LDD     ,X              * D = the correct jump address for the routine
        LEAU    D,X             * Add the offset to the jump location
        STU     FirstJump+1,PCR * Save the First Jump value to be used later (Self Mod)
        LDD     2,X             * D = the correct jump address for the routine
        LEAY    D,X             * Make Y the 2nd jump location
        PULS    D,X,U,PC

* Jump Table for 80 entries 0 to 79 or 1 to 80
BlastTable:
        FDB     DoNothing-*,Return-*     * Do width of zero
        FDB     Do2-*,Return-*    * Do width of 2
        FDB     Do4-*,Return-*    * Do width of 4
        FDB     Do6-*,Return-*    * Do width of 6
        FDB     Do1-*,Do7-*    * Do width of 8
        FDB     Do3-*,Do7-*    * Do width of 10
        FDB     Do5-*,Do7-*    * Do width of 12
        FDB     Do0-*,Do14-*    * Do width of 14
        FDB     Do2-*,Do14-*    * Do width of 16
        FDB     Do4-*,Do14-*    * Do width of 18
        FDB     Do6-*,Do14-*    * Do width of 20
        FDB     Do1-*,Do21-*    * Do width of 22
        FDB     Do3-*,Do21-*    * Do width of 24
        FDB     Do5-*,Do21-*    * Do width of 26
        FDB     Do0-*,Do28-*    * Do width of 28
        FDB     Do2-*,Do28-*    * Do width of 30
        FDB     Do4-*,Do28-*    * Do width of 32
        FDB     Do6-*,Do28-*    * Do width of 34
        FDB     Do1-*,Do35-*    * Do width of 36
        FDB     Do3-*,Do35-*    * Do width of 38
        FDB     Do5-*,Do35-*    * Do width of 40
        FDB     Do0-*,Do42-*    * Do width of 42
        FDB     Do2-*,Do42-*    * Do width of 44
        FDB     Do4-*,Do42-*    * Do width of 46
        FDB     Do6-*,Do42-*    * Do width of 48
        FDB     Do1-*,Do49-*    * Do width of 50
        FDB     Do3-*,Do49-*    * Do width of 52
        FDB     Do5-*,Do49-*    * Do width of 54
        FDB     Do0-*,Do56-*    * Do width of 56
        FDB     Do2-*,Do56-*    * Do width of 58
        FDB     Do4-*,Do56-*    * Do width of 60
        FDB     Do6-*,Do56-*    * Do width of 62
        FDB     Do1-*,Do63-*    * Do width of 64
        FDB     Do3-*,Do63-*    * Do width of 66
        FDB     Do5-*,Do63-*    * Do width of 68
        FDB     Do0-*,Do70-*    * Do width of 70
        FDB     Do2-*,Do70-*    * Do width of 72
        FDB     Do4-*,Do70-*    * Do width of 74
        FDB     Do6-*,Do70-*    * Do width of 76
        FDB     Do1-*,Do77-*    * Do width of 78
        FDB     Do3-*,Do77-*    * Do width of 80
        FDB     Do5-*,Do77-*    * Do width of 82
        FDB     Do0-*,Do84-*    * Do width of 84
        FDB     Do2-*,Do84-*    * Do width of 86
        FDB     Do4-*,Do84-*    * Do width of 88
        FDB     Do6-*,Do84-*    * Do width of 90
        FDB     Do1-*,Do91-*    * Do width of 92
        FDB     Do3-*,Do91-*    * Do width of 94
        FDB     Do5-*,Do91-*    * Do width of 96
        FDB     Do0-*,Do98-*    * Do width of 98
        FDB     Do2-*,Do98-*    * Do width of 100
        FDB     Do4-*,Do98-*    * Do width of 102
        FDB     Do6-*,Do98-*    * Do width of 104
        FDB     Do1-*,Do105-*    * Do width of 106
        FDB     Do3-*,Do105-*    * Do width of 108
        FDB     Do5-*,Do105-*    * Do width of 110
        FDB     Do0-*,Do112-*    * Do width of 112
        FDB     Do2-*,Do112-*    * Do width of 114
        FDB     Do4-*,Do112-*    * Do width of 116
        FDB     Do6-*,Do112-*    * Do width of 118
        FDB     Do1-*,Do119-*    * Do width of 120
        FDB     Do3-*,Do119-*    * Do width of 122
        FDB     Do5-*,Do119-*    * Do width of 124
        FDB     Do0-*,Do126-*    * Do width of 126
        FDB     Do2-*,Do126-*    * Do width of 128
        FDB     Do4-*,Do126-*    * Do width of 130
        FDB     Do6-*,Do126-*    * Do width of 132
        FDB     Do1-*,Do133-*    * Do width of 134
        FDB     Do3-*,Do133-*    * Do width of 136
        FDB     Do5-*,Do133-*    * Do width of 138
        FDB     Do0-*,Do140-*    * Do width of 140
        FDB     Do2-*,Do140-*    * Do width of 142
        FDB     Do4-*,Do140-*    * Do width of 144
        FDB     Do6-*,Do140-*    * Do width of 146
        FDB     Do1-*,Do147-*    * Do width of 148
        FDB     Do3-*,Do147-*    * Do width of 150
        FDB     Do5-*,Do147-*    * Do width of 152
        FDB     Do0-*,Do154-*    * Do width of 154
        FDB     Do2-*,Do154-*    * Do width of 156
        FDB     Do4-*,Do154-*    * Do width of 158
        FDB     Do6-*,Do154-*    * Do width of 160

Do6:    LEAS    ,X
        LEAU    6,U
        PULS    D,X,Y
        PSHU    D,X,Y
        LEAU    7+6,U       
        JMP     [PrepY+2,PCR]    * Clobber Y above so have to do this for the second jump, restore registers and Return
Do5:    LEAS    ,X
        LEAU    5,U
        PULS    D,DP,X
        PSHU    D,DP,X
        LEAU    7+5,U       
        JMP     ,Y              * Do second jump, restore registers and Return
Do4:    LEAS    ,X
        LEAU    4,U
        PULS    D,X
        PSHU    D,X
        LEAU    7+4,U       
        JMP     ,Y              * Do second jump, restore registers and Return
Do3:    LDD     ,X
        STD     ,U
        LDA     2,X
        STA     2,U
        LEAS    3,X             * S = Source address
        LEAU    7+3,U           * Prepare U for a stack blast
        JMP     ,Y              * Do second jump, restore registers and Return
Do2:    LDD     ,X
        STD     ,U
        LEAS    2,X             * S = Source address
        LEAU    7+2,U           * Prepare U for a stack blast
        JMP     ,Y              * Do second jump, restore registers and Return
Do1:    LDA     ,X
        STA     ,U
        LEAS    1,X             * S = Source address
        LEAU    7+1,U           * Prepare U for a stack blast
        JMP     ,Y              * Do second jump, restore registers and Return
Do0:    LEAS    ,X              * S = Source address
        LEAU    7,U             * Prepare U for a stack blast
        JMP     ,Y              * Do second jump, restore registers and Return

Do154:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 154 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do147:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 147 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do140:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 140 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do133:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 133 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do126:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 126 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do119:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 119 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do112:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 112 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do105:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 105 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do98:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 98 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do91:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 91 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do84:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 84 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do77:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 77 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do70:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 70 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do63:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 63 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do56:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 56 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do49:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 49 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do42:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 42 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do35:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 35 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do28:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 28 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do21:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 21 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do14:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 14 Total
        LEAU    14,U            * Position U for the next 7 Bytes
Do7:    PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 7 Total
        LDS     StackPointer    * Restore S
        PULS    B,X,Y,U,PC      * Done Return

        ORG     $7000+$DE2
* Jump Table for entries 1 to 32 scroll right
BlastTableR:
        FDB     DoNothing-*,Return-*    * Do width of zero
        FDB     Do2R-*,Return-* * Do width of 2
        FDB     Do4R-*,Return-* * Do width of 4
        FDB     Do6R-*,Return-* * Do width of 6
        FDB     Do1R-*,Do7R-*  * Do width of 8
        FDB     Do3R-*,Do7R-*  * Do width of 10
        FDB     Do5R-*,Do7R-*  * Do width of 12
        FDB     Do0R-*,Do14R-*  * Do width of 14
        FDB     Do2R-*,Do14R-*  * Do width of 16
        FDB     Do4R-*,Do14R-*  * Do width of 18
        FDB     Do6R-*,Do14R-*  * Do width of 20
        FDB     Do1R-*,Do21R-*  * Do width of 22
        FDB     Do3R-*,Do21R-*  * Do width of 24
        FDB     Do5R-*,Do21R-*  * Do width of 26
        FDB     Do0R-*,Do28R-*  * Do width of 28
        FDB     Do2R-*,Do28R-*  * Do width of 30
        FDB     Do4R-*,Do28R-*  * Do width of 32
        FDB     Do6R-*,Do28R-*  * Do width of 34
        FDB     Do1R-*,Do35R-*  * Do width of 36
        FDB     Do3R-*,Do35R-*  * Do width of 38
        FDB     Do5R-*,Do35R-*  * Do width of 40
        FDB     Do0R-*,Do42R-*  * Do width of 42
        FDB     Do2R-*,Do42R-*  * Do width of 44
        FDB     Do4R-*,Do42R-*  * Do width of 46
        FDB     Do6R-*,Do42R-*  * Do width of 48
        FDB     Do1R-*,Do49R-*  * Do width of 50
        FDB     Do3R-*,Do49R-*  * Do width of 52
        FDB     Do5R-*,Do49R-*  * Do width of 54
        FDB     Do0R-*,Do56R-*  * Do width of 56
        FDB     Do2R-*,Do56R-*  * Do width of 58
        FDB     Do4R-*,Do56R-*  * Do width of 60
        FDB     Do6R-*,Do56R-*  * Do width of 62
        FDB     Do1R-*,Do63R-*  * Do width of 64
        FDB     Do3R-*,Do63R-*  * Do width of 66
        FDB     Do5R-*,Do63R-*  * Do width of 68
        FDB     Do0R-*,Do70R-*  * Do width of 70
        FDB     Do2R-*,Do70R-*  * Do width of 72
        FDB     Do4R-*,Do70R-*  * Do width of 74
        FDB     Do6R-*,Do70R-*  * Do width of 76
        FDB     Do1R-*,Do77R-*  * Do width of 78
        FDB     Do3R-*,Do77R-*  * Do width of 80
        FDB     Do5R-*,Do77R-*  * Do width of 82
        FDB     Do0R-*,Do84R-*  * Do width of 84
        FDB     Do2R-*,Do84R-*  * Do width of 86
        FDB     Do4R-*,Do84R-*  * Do width of 88
        FDB     Do6R-*,Do84R-*  * Do width of 90
        FDB     Do1R-*,Do91R-*  * Do width of 92
        FDB     Do3R-*,Do91R-*  * Do width of 94
        FDB     Do5R-*,Do91R-*  * Do width of 96
        FDB     Do0R-*,Do98R-*  * Do width of 98
        FDB     Do2R-*,Do98R-*  * Do width of 100
        FDB     Do4R-*,Do98R-*  * Do width of 102
        FDB     Do6R-*,Do98R-*  * Do width of 104
        FDB     Do1R-*,Do105R-* * Do width of 106
        FDB     Do3R-*,Do105R-* * Do width of 108
        FDB     Do5R-*,Do105R-* * Do width of 110
        FDB     Do0R-*,Do112R-* * Do width of 112
        FDB     Do2R-*,Do112R-* * Do width of 114
        FDB     Do4R-*,Do112R-* * Do width of 116
        FDB     Do6R-*,Do112R-* * Do width of 118
        FDB     Do1R-*,Do119R-* * Do width of 120
        FDB     Do3R-*,Do119R-* * Do width of 122
        FDB     Do5R-*,Do119R-* * Do width of 124
        FDB     Do0R-*,Do126R-* * Do width of 126
        FDB     Do2R-*,Do126R-* * Do width of 128
        FDB     Do4R-*,Do126R-* * Do width of 130
        FDB     Do6R-*,Do126R-* * Do width of 132
        FDB     Do1R-*,Do133R-* * Do width of 134
        FDB     Do3R-*,Do133R-* * Do width of 136
        FDB     Do5R-*,Do133R-* * Do width of 138
        FDB     Do0R-*,Do140R-* * Do width of 140
        FDB     Do2R-*,Do140R-* * Do width of 142
        FDB     Do4R-*,Do140R-* * Do width of 144
        FDB     Do6R-*,Do140R-* * Do width of 146
        FDB     Do1R-*,Do147R-* * Do width of 148
        FDB     Do3R-*,Do147R-* * Do width of 150
        FDB     Do5R-*,Do147R-* * Do width of 152
        FDB     Do0R-*,Do154R-* * Do width of 154
        FDB     Do2R-*,Do154R-* * Do width of 156
        FDB     Do4R-*,Do154R-* * Do width of 158
        FDB     Do6R-*,Do154R-* * Do width of 160

Do6R:   LEAS    -4,X
        LEAU    2,U
        PULS    D,X,Y           * Read 6 Bytes
        PSHU    D,X,Y           * Write 6 Bytes
        LEAS    -7-6,S          * Position S for the next 7 Bytes
        JMP     [PrepY+2,PCR]   * Clobber Y above so have to do this for the second jump, restore registers and Return
Do5R:   LEAS    -3,X
        LEAU    2,U
        PULS    D,DP,X        * Read 7 Bytes
        PSHU    D,DP,X        * Write 7 Bytes - 28 Total
        LEAS    -7-5,S          * Position S for the next 7 Bytes
        JMP     ,Y              * Do second jump, restore registers and Return
Do4R:   LEAS    -2,X            * X=417,U=418
        LEAU    2,U
        PULS    D,X        * Read 7 Bytes
        PSHU    D,X        * Write 7 Bytes - 28 Total
        LEAS    -7-4,S          * Position S for the next 7 Bytes 
        JMP     ,Y              * Do second jump, restore registers and Return  S=40D,U=415  should be 40E & 416
Do3R:   LDD     ,X              * X=416,U=417
        STD     ,U
        LDA     -1,X
        STA     -1,U
        LEAS    -7-1,X          * S = Source address ; S= 40E
        LEAU    -1,U            * U=416
        JMP     ,Y              * Do second jump, restore registers and Return
Do2R:   LDD     ,X
        STD     ,U
        LEAS    -7,X            * S = Source address
        JMP     ,Y              * Do second jump, restore registers and Return
Do1R:   LDA     1,X
        STA     1,U
        LEAS    -7+1,X          * S = Source address
        LEAU    1,U
        JMP     ,Y              * Do second jump, restore registers and Return
Do0R:   LEAS    -7+2,X          * S = Source address
        LEAU    2,U
        JMP     ,Y              * Do second jump, restore registers and Return
DoNothing:
Return:
        LDS     StackPointer    * Restore S
        PULS    B,X,Y,U,PC      * Done Return

* U is the byte to the right
Do154R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 154 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do147R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 147 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do140R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 140 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do133R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 133 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do126R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 126 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do119R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 119 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do112R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 112 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do105R: PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 105 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do98R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 98 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do91R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 91 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do84R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 84 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do77R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 77 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do70R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 70 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do63R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 63 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do56R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 56 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do49R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 49 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do42R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 42 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do35R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 35 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do28R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 28 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do21R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 21 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do14R:  PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 14 Total
        LEAS    -14,S           * Position S for the next 7 Bytes
Do7R:   PULS    D,DP,X,Y        * Read 7 Bytes
        PSHU    D,DP,X,Y        * Write 7 Bytes - 7 Total
        LDS     StackPointer    * Restore S
        PULS    B,X,Y,U,PC      * Done Return

LAST:
_4kCLEAR        EQU     $1000-LAST
_16kCLEAR       EQU     $4000-LAST
_32kCLEAR       EQU     $8000-LAST
        END     CoCo_START
