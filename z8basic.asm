;	Z8 NASCOM BASIC
;	Converted source code from 8080/Z80 to Z8
;	Assembler: Macro Assembler 1.42
;	Converted by Satoshi Okue
;	2023/10/26
;
;	EMUBASIC based on GRANT's BASIC
;	TARGET: EMUZ80
;	ASSEMBLER: ARCPIT XZ80.EXE
;
;	START UP ROUTINE
;	VERSION 1.0, 2022/02/15
;	WRITTEN by TETSUYA SUZUKI
;
;;; Functions
lo	function	x,x & 255
hi	function	x,(x >> 8)&255

	CPU	Z8601

; Internal processor memory & SFR
P0	EQU	0	; Port 0
P1	EQU	1	; Port 1
P2	EQU	2	; Port 2
P3	EQU	3	; Port 3

SIO	EQU	0F0H	; Serial I/O
TMR	EQU	0F1H	; Timer	mode
T1	EQU	0F2H	; Timer/counter	1
PRE1	EQU	0F3H	; t1 prescaler
T0	EQU	0F4H	; Timer/counter	0
PRE0	EQU	0F5H	; t0 prescaler
P2M	EQU	0F6H	; Port 2 mode
P3M	EQU	0F7H	; Port 3 mode
P01M	EQU	0F8H	; Ports	0-1 mode
IPR	EQU	0F9H	; Interrupt priority register
IRQ	EQU	0FAH	; Interrupt request register
IMR	EQU	0FBH	; Interrupt mask register
FLAGS	EQU	0FCH	; Program control flags
RP	EQU	0FDH	; Register pointer
SPH	EQU	0FEH	; Stack pointer high
SPL	EQU	0FFH	; Stack pointer low
;
; Internal UART bps = CLK / SDIV0 / SDIV1 / 16 / 8
SDIV0:	EQU	1	; T0	1-256 (0 means 256)
SDIV1:	EQU	1	; PRE0	1-64 (0 means 64)

REGPTR	EQU	70H

;	MEMORY ASIGN
ROMTOP	EQU	0000H
RAMTOP	EQU	8000H
TSTACK	EQU	80EDH
;
	ORG	ROMTOP

INT0:	DW	0
INT1:	DW	0
INT2:	DW	0
INT3:	DW	0
INT4:	DW	0
INT5:	DW	0

	DI
	LD	RP,#REGPTR
	LD	P01M,#0B2H
	NOP
	NOP
	NOP
	LD	SPH,#hi(TSTACK)
	LD	SPL,#lo(TSTACK)
	CALL	CONINIT
	JP	SINIT

;	Z8 Internal UART
;
;	UART Initialize
CONINIT:
	LD	P2M,#00H
	LD	P3M,#41H	; NoParity
	LD	T0,#SDIV0
	LD	PRE0,#(SDIV1 << 2) + 1
	LD	TMR,#73H
	CLR	IMR
	EI
	DI
	LD	SIO,#0AH
	AND	IRQ,#0EFH
	RET
;
;	UART -> R0;
CONIN:
	LD	R0,IRQ
	AND	R0,#08H
	JR	Z,CONIN
	LD	R0,SIO
	AND	IRQ,#0F7H
	RET
;
;	CHECK RECEIVE STATUS
CONST:
	LD	R0,IRQ
	AND	R0,#08H
	RET
;
;	R0 -> UART
CONOUT:
	LD	R1,IRQ
	AND	R1,#10H
	JR	Z,CONOUT
	LD	SIO,R0
	AND	IRQ,#0EFH
	RET

;
; Memory Mapped UART REGISTER ADDRESS
UARTDR	EQU	0E000H	; UART DATA REGISTOR
UARTCR	EQU	0E001H	; UART CONTROL REGISTOR

;;	Memory mapped  UART
;;
;;	UART Initialize
;CONINIT:
;	RET
;;
;;	UART -> R0
;CONIN:
;	LD	R9,#lo(UARTCR)
;	LD	R8,#hi(UARTCR)
;CONIN1:
;	LDC	R0,@RR8
;	AND	R0,#01H
;	JR	Z,CONIN1
;	LD	R9,#lo(UARTDR)
;	LDC	R0,@RR8
;	RET
;;
;;	CHECK RECEIVE STATUS
;CONST:
;	LD	R9,#lo(UARTCR)
;	LD	R8,#hi(UARTCR)
;	LDC	R0,@RR8
;	AND	R0,#01H
;	RET
;;
;;	R0 -> UART
;CONOUT:
;	LD	R9,#lo(UARTCR)
;	LD	R8,#hi(UARTCR)
;CONOUT1:
;	LDC	R1,@RR8
;	AND	R1,#02H
;	JR	Z,CONOUT1
;	LD	R9,#lo(UARTDR)
;	LDC	@RR8,R0
;	RET

;
;	SYSTEM INITIALIZE
SINIT:
	JP	COLD
;
;==================================================================================
; The updates to the original BASIC within this file are copyright Grant Searle
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
;==================================================================================
;
; NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft
; Scanned from source published in 80-BUS NEWS from Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; Adapted for the freeware Zilog Macro Assembler 2.10 to produce
; the original ROM code (checksum A934H). PA
;
; GENERAL EQUATES
;
CTRLC	EQU	03H		; Control "C"
CTRLG	EQU	07H		; Control "G"
BKSP	EQU	08H		; Back space
LF	EQU	0AH		; Line feed
CS	EQU	0CH		; Clear screen
CR	EQU	0DH		; Carriage return
CTRLO	EQU	0FH		; Control "O"
CTRLQ	EQU	11H		; Control "Q"
CTRLR	EQU	12H		; Control "R"
CTRLS	EQU	13H		; Control "S"
CTRLU	EQU	15H		; Control "U"
ESC	EQU	1BH		; Escape
DEL	EQU	7FH		; Delete
;
; BASIC WORK SPACE LOCATIONS
;
WRKSPC	EQU	RAMTOP+45H	; BASIC Work space
USR	EQU	WRKSPC+3H	; "USR (x)" jump
OUTSUB	EQU	WRKSPC+6H	; "OUT p,n"
OTPORT	EQU	WRKSPC+7H	; Port (p)
DIVSUP	EQU	WRKSPC+9H	; Division support routine
DIV1	EQU	WRKSPC+0BH	; <- Values
DIV2	EQU	WRKSPC+12H	; <- to
DIV3	EQU	WRKSPC+19H	; <- be
DIV4	EQU	WRKSPC+1DH	; <- inserted
SEED	EQU	WRKSPC+16H+8	; Random number seed
LSTRND	EQU	WRKSPC+3AH+8	; Last random number
INPSUB	EQU	WRKSPC+3EH+8	; #INP (x)" Routine
INPORT	EQU	WRKSPC+3FH+8	; PORT (x)
NULLS	EQU	WRKSPC+41H+8	; Number of nulls
LWIDTH	EQU	WRKSPC+42H+8	; Terminal width
COMMAN	EQU	WRKSPC+43H+8	; Width for commas
NULFLG	EQU	WRKSPC+44H+8	; Null after input byte flag
CTLOFG	EQU	WRKSPC+45H+8	; Control "O" flag
LINESC	EQU	WRKSPC+46H+8	; Lines counter
LINESN	EQU	WRKSPC+48H+8	; Lines number
CHKSUM	EQU	WRKSPC+4AH+8	; Array load/save check sum
NMIFLG	EQU	WRKSPC+4CH+8	; Flag for NMI break routine
BRKFLG	EQU	WRKSPC+4DH+8	; Break flag
RINPUT	EQU	WRKSPC+4EH+8	; Input reflection
POINT	EQU	WRKSPC+51H+8	; "POINT" reflection (unused)
PSET	EQU	WRKSPC+54H+8	; "SET"	reflection
RESET	EQU	WRKSPC+57H+8	; "RESET" reflection
STRSPC	EQU	WRKSPC+5AH+8	; Bottom of string space
LINEAT	EQU	WRKSPC+5CH+8	; Current line number
BASTXT	EQU	WRKSPC+5EH+8	; Pointer to start of program
BUFFER	EQU	WRKSPC+61H+8	; Input buffer
STACK	EQU	WRKSPC+66H+8	; Initial stack
CURPOS	EQU	WRKSPC+0ABH+8	; Character position on line
LCRFLG	EQU	WRKSPC+0ACH+8	; Locate/Create flag
TYPE	EQU	WRKSPC+0ADH+8	; Data type flag
DATFLG	EQU	WRKSPC+0AEH+8	; Literal statement flag
LSTRAM	EQU	WRKSPC+0AFH+8	; Last available RAM
TMSTPT	EQU	WRKSPC+0B1H+8	; Temporary string pointer
TMSTPL	EQU	WRKSPC+0B3H+8	; Temporary string pool
TMPSTR	EQU	WRKSPC+0BFH+8	; Temporary string
STRBOT	EQU	WRKSPC+0C3H+8	; Bottom of string space
CUROPR	EQU	WRKSPC+0C5H+8	; Current operator in EVAL
LOOPST	EQU	WRKSPC+0C7H+8	; First statement of loop
DATLIN	EQU	WRKSPC+0C9H+8	; Line of current DATA item
FORFLG	EQU	WRKSPC+0CBH+8	; "FOR" loop flag
LSTBIN	EQU	WRKSPC+0CCH+8	; Last byte entered
READFG	EQU	WRKSPC+0CDH+8	; Read/Input flag
BRKLIN	EQU	WRKSPC+0CEH+8	; Line of break
NXTOPR	EQU	WRKSPC+0D0H+8	; Next operator in EVAL
ERRLIN	EQU	WRKSPC+0D2H+8	; Line of error
CONTAD	EQU	WRKSPC+0D4H+8	; Where to CONTinue
PROGND	EQU	WRKSPC+0D6H+8	; End of program
VAREND	EQU	WRKSPC+0D8H+8	; End of variables
ARREND	EQU	WRKSPC+0DAH+8	; End of arrays
NXTDAT	EQU	WRKSPC+0DCH+8	; Next data item
FNRGNM	EQU	WRKSPC+0DEH+8	; Name of FN argument
FNARG	EQU	WRKSPC+0E0H+8	; FN argument value
FPREG	EQU	WRKSPC+0E4H+8	; Floating point register
FPEXP	EQU	FPREG+3		; Floating point exponent
SGNRES	EQU	WRKSPC+0E8H+8	; Sign of result
PBUFF	EQU	WRKSPC+0E9H+8	; Number print buffer
MULVAL	EQU	WRKSPC+0F6H+8	; Multiplier
PROGST	EQU	WRKSPC+0F9H+8	; Start of program text area
STLOOK	EQU	WRKSPC+15DH+8	; Start of memory test
;
; BASIC ERROR CODE VALUES
;
NF	EQU	00H		; NEXT without FOR
SN	EQU	02H		; Syntax error
RG	EQU	04H		; RETURN without GOSUB
OD	EQU	06H		; Out of DATA
FC	EQU	08H		; Function call error
OV	EQU	0AH		; Overflow
OM	EQU	0CH		; Out of memory
UL	EQU	0EH		; Undefined line number
BS	EQU	10H		; Bad subscript
RD	EQU	12H		; Re-DIMensioned array
DZ	EQU	14H		; Division by zero (/0)
ID	EQU	16H		; Illegal direct
TM	EQU	18H		; Type miss-match
OS	EQU	1AH		; Out of string space
LS	EQU	1CH		; String too long
ST	EQU	1EH		; String formula too complex
CN	EQU	20H		; Can't CONTinue
UF	EQU	22H		; UnDEFined FN function
MO	EQU	24H		; Missing operand
HX	EQU	26H		; HEX error
BN	EQU	28H		; BIN error
;
COLD:
	JP	STARTB		; Jump for cold start
WARM:
	JP	WARMST		; Jump for warm start
STARTB:
	JP	CSTART		; Jump to initialise
;
	DW	DEINT		; Get integer -32768 to 32767
	DW	ABPASS		; Return integer in AB
;
CSTART:
	LD	R2,#hi(WRKSPC)	; Start of workspace RAM
	LD	R3,#lo(WRKSPC)
	LD	SPH,R2		; Set up a temporary stack
	LD	SPL,R3

	JP	INITST		; Go to initialise
;
INIT:
	LD	R6,#hi(INITAB)	; Initialise workspace
	LD	R7,#lo(INITAB)
	LD	R4,#INITBE-INITAB+3; Bytes to copy
	LD	R2,#hi(WRKSPC)	; Into workspace RAM
	LD	R3,#lo(WRKSPC)
COPY:
	LDC	R0,@RR6		; Get source
	LDC	@RR2,R0		; To destination
	INCW	RR2		; Next destination
	INCW	RR6		; Next source
	DEC	R4		; Count bytes
	JP	NZ,COPY		; More to move
	LD	SPH,R2		; Temporary stack
	LD	SPL,R3
	CALL	CLREG		; Clear registers and stack
	CALL	PRCRLF		; Output CRLF
	LD	R8,#hi(BUFFER+72+1)
	LD	R9,#lo(BUFFER+72+1)
	LDC	@RR8,R0		; Mark end of buffer
	LD	R8,#hi(PROGST)
	LD	R9,#lo(PROGST)
	LDC	@RR8,R0		; Initialise program area
MSIZE:
	LD	R2,#hi(STLOOK)	; Point to start of RAM
	LD	R3,#lo(STLOOK)
MLOOP:
	LD	R1,FLAGS
	INCW	RR2		; Next byte
	LD	FLAGS,R1
	LD	R0,R2		; Above address FFFF ?
	OR	R0,R3
	RCF
	JP	Z,SETTOP	; Yes - 64K RAM
	LDC	R0,@RR2		; Get contents
	LD	R4,R0		; Save it
	COM	R0		; Flip all bits
	LDC	@RR2,R0		; Put it back
	LDC	R8,@RR2
	CP	R0,R8		; RAM there if same
	LDC	@RR2,R4		; Restore old contents
	JP	Z,MLOOP		; If RAM - test next byte
SETTOP:
	LD	R1,FLAGS
	DECW	RR2
	LD	FLAGS,R1	; Back one byte
	LD	R6,#hi(STLOOK-1); See if enough RAM
	LD	R7,#lo(STLOOK-1)
	CALL	CPDEHL		; Compare DE with HL
	JP	C,NEMEM		; If not enough RAM
	LD	R6,#hi(0-50)	; 50 Bytes string space
	LD	R7,#lo(0-50)
	LD	R8,#hi(LSTRAM)
	LD	R9,#lo(LSTRAM)
	LDC	@RR8,R2		; Save last available RAM
	INCW	RR8
	LDC	@RR8,R3
	ADD	R3,R7		; Allocate string space
	ADC	R2,R6
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	@RR8,R2		; Save string space
	INCW	RR8
	LDC	@RR8,R3
	CALL	CLRPTR		; Clear program area
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	R2,@RR8		; Get end of memory
	INCW	RR8
	LDC	R3,@RR8
	LD	R6,#hi(0-17)	; Offset for free bytes
	LD	R7,#lo(0-17)
	ADD	R3,R7		; Adjust HL
	ADC	R2,R6
	LD	R6,#hi(PROGST)	; Start of program text
	LD	R7,#lo(PROGST)
	LD	R0,R3		; Get LSB
	SUB	R0,R7		; Adjust it
	LD	R3,R0		; Re-save
	LD	R0,R2		; Get MSB
	SBC	R0,R6		; Adjust it
	LD	R2,R0		; Re-save
	PUSH	R3		; Save bytes free
	PUSH	R2
	LD	R2,#hi(SIGNON)	; Sign-on message
	LD	R3,#lo(SIGNON)
	CALL	PRS		; Output string
	POP	R2		; Get bytes free back
	POP	R3
	CALL	PRNTHL		; Output amount of free memory
	LD	R2,#hi(BFREE)	; " Bytes free" message
	LD	R3,#lo(BFREE)
	CALL	PRS		; Output string
;
WARMST:
	LD	SPH,#hi(STACK)	; Temporary stack
	LD	SPL,#lo(STACK)
BRKRET:
	CALL	CLREG		; Clear registers and stack
	JP	PRNTOK		; Go to get command line
;
NEMEM:
	LD	R2,#hi(MEMMSG)	; Memory size not enough
	LD	R3,#lo(MEMMSG)
	CALL	PRS		; Print it
XXXXX:
	JP	XXXXX		; Stop
;
BFREE:
	DB	" Bytes free",CR,LF,0,0
;
SIGNON:
	DB	"Z80 Based Z8 BASIC Ver 4.7b",CR,LF
	DB	"Copyright ",40,"C",41
	DB	" 1978 by Microsoft",CR,LF,0,0
;
MEMMSG:
	DB	"Memory size not enough",CR,LF
	DB	"The system is stopped.",CR,LF,0,0
;
; FUNCTION ADDRESS TABLE
;
FNCTAB:
	DW	SGN
	DW	INT
	DW	ABS
	DW	USR
	DW	FRE
	DW	INP
	DW	POS
	DW	SQR
	DW	RND
	DW	LOG
	DW	EXP
	DW	COS
	DW	SIN
	DW	TAN
	DW	ATN
	DW	PEEK
	DW	DEEK
	DW	POINT
	DW	LEN
	DW	STR
	DW	VAL
	DW	ASC
	DW	CHR
	DW	HEX
	DW	BIN
	DW	LEFT
	DW	RIGHT
	DW	MID
;
; RESERVED WORD LIST
;
WORDS:
	DB	0C5H,"ND"	; END
	DB	0C6H,"OR"	; FOR
	DB	0CEH,"EXT"	; NEXT
	DB	0C4H,"ATA"	; DATA
	DB	0C9H,"NPUT"	; INPUT
	DB	0C4H,"IM"	; DIM
	DB	0D2H,"EAD"	; READ
	DB	0CCH,"ET"	; LET
	DB	0C7H,"OTO"	; GOTO
	DB	0D2H,"UN"	; RUN
	DB	0C9H,"F"	; IF
	DB	0D2H,"ESTORE"	; RESTORE
	DB	0C7H,"OSUB"	; GOSUB
	DB	0D2H,"ETURN"	; RETURN
	DB	0D2H,"EM"	; REM
	DB	0D3H,"TOP"	; STOP
	DB	0CFH,"UT"	; OUT
	DB	0CFH,"N"	; ON
	DB	0CEH,"ULL"	; NULL
	DB	0D7H,"AIT"	; WAIT
	DB	0C4H,"EF"	; DEF
	DB	0D0H,"OKE"	; POKE
	DB	0C4H,"OKE"	; DOKE
	DB	0D3H,"CREEN"	; SCREEN
	DB	0CCH,"INES"	; LINES
	DB	0C3H,"LS"	; CLS
	DB	0D7H,"IDTH"	; WIDTH
	DB	0CDH,"ONITOR"	; MONITOR
	DB	0D3H,"ET"	; SET
	DB	0D2H,"ESET"	; RESET
	DB	0D0H,"RINT"	; PRINT
	DB	0C3H,"ONT"	; CONT
	DB	0CCH,"IST"	; LIST
	DB	0C3H,"LEAR"	; CLEAR
	DB	0C3H,"LOAD"	; CLOAD
	DB	0C3H,"SAVE"	; CSAVE
	DB	0CEH,"EW"	; NEW
;
	DB	0D4H,"AB("	; TAB(
	DB	0D4H,"O"	; TO
	DB	0C6H,"N"	; FN
	DB	0D3H,"PC("	; SPC(
	DB	0D4H,"HEN"	; THEN
	DB	0CEH,"OT"	; NOT
	DB	0D3H,"TEP"	; STEP
;
	DB	0ABH
	DB	0ADH
	DB	0AAH
	DB	0AFH
	DB	0DEH
	DB	0C1H,"ND"	; AND
	DB	0CFH,"R"	; OR
	DB	0BEH
	DB	0BDH
	DB	0BCH
;
	DB	0D3H,"GN"	; SGN
	DB	0C9H,"NT"	; INT
	DB	0C1H,"BS"	; ABS
	DB	0D5H,"SR"	; USR
	DB	0C6H,"RE"	; FRE
	DB	0C9H,"NP"	; INP
	DB	0D0H,"OS"	; POS
	DB	0D3H,"QR"	; SQR
	DB	0D2H,"ND"	; RND
	DB	0CCH,"OG"	; LOG
	DB	0C5H,"XP"	; EXP
	DB	0C3H,"OS"	; COS
	DB	0D3H,"IN"	; SIN
	DB	0D4H,"AN"	; TAN
	DB	0C1H,"TN"	; ATN
	DB	0D0H,"EEK"	; PEEK
	DB	0C4H,"EEK"	; DEEK
	DB	0D0H,"OINT"	; POINT
	DB	0CCH,"EN"	; LEN
	DB	0D3H,"TR$"	; STR
	DB	0D6H,"AL"	; VAL
	DB	0C1H,"SC"	; ASC
	DB	0C3H,"HR$"	; CHR$
	DB	0C8H,"EX$"	; HEX$
	DB	0C2H,"IN$"	; BIN$
	DB	0CCH,"EFT$"	; LEFT$
	DB	0D2H,"IGHT$"	; RIGHT$
	DB	0CDH,"ID$"	; MID$
	DB	80H		; End of list marker
;
; KEYWORD ADDRESS TABLE
;
WORDTB:
	DW	PEND
	DW	FOR
	DW	NEXT
	DW	DATA
	DW	INPUT
	DW	DIM
	DW	READ
	DW	LET
	DW	GOTO
	DW	RUN
	DW	IF
	DW	RESTOR
	DW	GOSUB
	DW	RETURN
	DW	REM
	DW	STOP
	DW	POUT
	DW	ON
	DW	NULL
	DW	WAIT
	DW	DEF
	DW	POKE
	DW	DOKE
	DW	REM
	DW	LINES
	DW	CLS
	DW	WIDTH
	DW	MONITR
	DW	PSET
	DW	RESET
	DW	PRINT
	DW	CONT
	DW	LIST
	DW	CLEAR
	DW	REM
	DW	REM
	DW	NEW
;
; RESERVED WORD TOKEN VALUES
;
ZEND	EQU	080H		; END
ZFOR	EQU	081H		; FOR
ZDATA	EQU	083H		; DATA
ZGOTO	EQU	088H		; GOTO
ZGOSUB	EQU	08CH		; GOSUB
ZREM	EQU	08EH		; REM
ZPRINT	EQU	09EH		; PRINT
ZNEW	EQU	0A4H		; NEW
;
ZTAB	EQU	0A5H		; TAB
ZTO	EQU	0A6H		; TO
ZFN	EQU	0A7H		; FN
ZSPC	EQU	0A8H		; SPC
ZTHEN	EQU	0A9H		; THEN
ZNOT	EQU	0AAH		; NOT
ZSTEP	EQU	0ABH		; STEP
;
ZPLUS	EQU	0ACH		; +
ZMINUS	EQU	0ADH		; -
ZTIMES	EQU	0AEH		; *
ZDIV	EQU	0AFH		; /
ZOR	EQU	0B2H		; OR
ZGTR	EQU	0B3H		; >
ZEQUAL	EQU	0B4H		; M
ZLTH	EQU	0B5H		; <
ZSGN	EQU	0B6H		; SGN
ZPOINT	EQU	0C7H		; POINT
ZLEFT	EQU	0CDH +2		; LEFT$
;
; ARITHMETIC PRECEDENCE TABLE
;
PRITAB:
	DB	79H		; Precedence value
	DW	PADD		; FPREG = <last> + FPREG
;
	DB	79H		; Precedence value
	DW	PSUB		; FPREG = <last> - FPREG
;
	DB	7CH		; Precedence value
	DW	MULT		; PPREG = <last> * FPREG
;
	DB	7CH		; Precedence value
	DW	DIV		; FPREG = <last> / FPREG
;
	DB	7FH		; Precedence value
	DW	POWER		; FPREG = <last> ^ FPREG
;
	DB	50H		; Precedence value
	DW	PAND		; FPREG = <last> AND FPREG
;
	DB	46H		; Precedence value
	DW	POR		; FPREG = <last> OR FPREG
;
; BASIC ERROR CODE LIST
;
ERRORS:
	DB	"NF"		; NEXT without FOR
	DB	"SN"		; Syntax error
	DB	"RG"		; RETURN without GOSUB
	DB	"OD"		; Out of DATA
	DB	"FC"		; Illegal function call
	DB	"OV"		; Overflow error
	DB	"OM"		; Out of memory
	DB	"UL"		; Undefined line
	DB	"BS"		; Bad subscript
	DB	"DD"		; Re-DIMensioned array
	DB	"/0"		; Division by zero
	DB	"ID"		; Illegal direct
	DB	"TM"		; Type mis-match
	DB	"OS"		; Out of string space
	DB	"LS"		; String too long
	DB	"ST"		; String formula too complex
	DB	"CN"		; Can't CONTinue
	DB	"UF"		; Undefined FN function
	DB	"MO"		; Missing operand
	DB	"HX"		; HEX error
	DB	"BN"		; BIN error
;
; INITIALISATION TABLE -------------------------------------------------------
;
INITAB:
	JP	WARMST		; Warm start jump
	JP	FCERR		; "USR (X)" jump (Set to Error)
;  OUT	(0),A		; "OUT p,n" skeleton
	NOP
	NOP
	RET
	SUB	R0,#0		; Division support routine
	LD	R3,R0
	LD	R0,R2
	SBC	R0,#0
	LD	R2,R0
	LD	R0,R4
	SBC	R0,#0
	LD	R4,R0
	LD	R0,#0
	RET
	DB	0,0,0			; Random number seed table used by RND
	DB	035H,04AH,0CAH,099H	;-2.65145E+07
	DB	039H,01CH,076H,098H	; 1.61291E+07
	DB	022H,095H,0B3H,098H	;-1.17691E+07
	DB	00AH,0DDH,047H,098H	; 1.30983E+07
	DB	053H,0D1H,099H,099H	;-2-01612E+07
	DB	00AH,01AH,09FH,098H	;-1.04269E+07
	DB	065H,0BCH,0CDH,098H	;-1.34831E+07
	DB	0D6H,077H,03EH,098H	; 1.24825E+07
	DB	052H,0C7H,04FH,080H	; Last random number
;  IN	A,(0)		; INP (x) skeleton
	NOP
	NOP
	RET
	DB	1		; POS (x) number (1)
	DB	255		; Terminal width (255 = no auto CRLF)
	DB	28		; Width for commas (3 columns)
	DB	0		; No nulls after input bytes
	DB	0		; Output enabled (^O off)
	DW	20		; Initial lines counter
	DW	20		; Initial lines number
	DW	0		; Array load/save check sum
	DB	0		; Break not by NMI
	DB	0		; Break flag
	JP	TTYLIN		; Input reflection (set to TTY)
	JP	0000H		; POINT reflection unused
	JP	0000H		; SET reflection
	JP	0000H			; RESET reflection
	DW	STLOOK		; Temp string space
	DW	-2		; Current line number (cold)
	DW	PROGST+1	; Start of program text
INITBE:
;
; END OF INITIALISATION TABLE ---------------------------------------------------
;
ERRMSG:
	DB	" Error",0
INMSG:
	DB	" in ",0
ZERBYT	EQU	$-1		; A zero byte
OKMSG:
	DB	"Ok",CR,LF,0,0
BRKMSG:
	DB	"Break",0
;
BAKSTK:
	LD	R2,#hi(4)	; Look for "FOR" block with
	LD	R3,#lo(4)
	ADD	R3,SPL
	ADC	R2,SPH		; same index as specified
LOKFOR:
	LDC	R0,@RR2		; Get block ID
	LD	R1,FLAGS
	INCW	RR2		; Point to index address
	LD	FLAGS,R1
	CP	R0,#ZFOR	; Is it a "FOR" token
	JR	Z,$+3
	RET			; No - exit
	LDC	R5,@RR2		; BC = Address of "FOR" index
	INCW	RR2
	LDC	R4,@RR2
	INCW	RR2		; Point to sign of STEP
	PUSH	R3
	PUSH	R2		; Save pointer to sign
	LD	R3,R5		; HL = address of "FOR" index
	LD	R2,R4
	LD	R0,R6		; See if an index was specified
	OR	R0,R7		; DE = 0 if no index specified
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8		; Specified index into HL
	LD	R3,R9
	JP	Z,INDFND	; Skip if no index given
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2		; Index back into DE
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	CALL	CPDEHL		; Compare index with one given
INDFND:
	LD	R4,#hi(16-3)	; Offset to next block
	LD	R5,#lo(16-3)
	POP	R2
	POP	R3		; Restore pointer to sign
	JR	NZ,$+3
	RET			; Return if block found
	ADD	R3,R5
	ADC	R2,R4		; Point to next block
	JP	LOKFOR		; Keep on looking
;
MOVUP:
	CALL	ENFMEM		; See if enough memory
MOVSTR:
	PUSH	R5
	PUSH	R4		; Save end of source
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8		; Swap source and dest" end
	LD	R3,R9
	POP	R4
	POP	R5		; Get end of destination
MOVLP:
	CALL	CPDEHL		; See if list moved
	LDC	R0,@RR2		; Get byte
	LDC	@RR4,R0		; Move it
	JR	NZ,$+3
	RET			; Exit if all done
	DECW	RR4		; Next byte to move to
	DECW	RR2		; Next byte to move
	JP	MOVLP		; Loop until all bytes moved
;
CHKSTK:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Lowest free memory
	LD	R4,#0		; BC = Number of levels to test
	ADD	R3,R5
	ADC	R2,R4		; 2 Bytes for each level
	ADD	R3,R5
	ADC	R2,R4
	JR	ENFMEM1		; Skip "PUSH HL"
ENFMEM:
	PUSH	R3
	PUSH	R2		; Save code string address
ENFMEM1:
	LD	R0,#0D0H	; LOW -48; 48 Bytes minimum RAM
	SUB	R0,R3
	LD	R3,R0
	LD	R0,#0FFH	; HIGH (-48); 48 Bytes minimum RAM
	SBC	R0,R2
	JP	C,OMERR		; Not enough - ?OM Error
	LD	R2,R0
	ADD	R3,SPL
	ADC	R2,SPH		; Test if stack is overflowed
	POP	R2
	POP	R3		; Restore code string address
	JR	NC,$+3
	RET			; Return if enough mmory
OMERR:
	LD	R7,#OM		; ?OM Error
	JP	ERROR
;
DATSNR:
	LD	R8,#hi(DATLIN)
	LD	R9,#lo(DATLIN)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get line of current DATA item
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save as current line
SNERR:
	LD	R7,#SN		; ?SN Error
	JR	ERROR		; Skip "LD E,DZ"
DZERR:
	LD	R7,#DZ		; ?/0 Error
	JR	ERROR		; Skip "LD E,NF"
NFERR:
	LD	R7,#NF		; ?NF Error
	JR	ERROR		; Skip "LD E,RD"
DDERR:
	LD	R7,#RD		; ?DD Error
	JR	ERROR		; Skip "LD E,UF"
UFERR:
	LD	R7,#UF		; ?UF Error
	JR	ERROR		; Skip "LD E,OV
OVERR:
	LD	R7,#OV		; ?OV Error
	JR	ERROR		; Skip "LD E,TM"
TMERR:
	LD	R7,#TM		; ?TM Error
;
ERROR:
	CALL	CLREG		; Clear registers and stack
	LD	R8,#hi(CTLOFG)
	LD	R9,#lo(CTLOFG)
	LDC	@RR8,R0		; Enable output (A is 0)
	CALL	STTLIN		; Start new line
	LD	R2,#hi(ERRORS)	; Point to error codes
	LD	R3,#lo(ERRORS)
	LD	R6,R0		; D = 0 (A is 0)
	LD	R0,#'?'
	CALL	OUTC		; Output '?'
	ADD	R3,R7
	ADC	R2,R6		; Offset to correct error code
	LDC	R0,@RR2		; First character
	CALL	OUTC		; Output it
	CALL	GETCHR		; Get next character
	CALL	OUTC		; Output it
	LD	R2,#hi(ERRMSG)	; "Error" message
	LD	R3,#lo(ERRMSG)
ERRIN:
	CALL	PRS		; Output message
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get line of error
	LD	R6,#hi(-2)
	LD	R7,#lo(-2)	; Cold start error if -2
	CALL	CPDEHL		; See if cold start error
	JP	Z,CSTART	; Cold start error - Restart
	LD	R0,R2		; Was it a direct error?
	AND	R0,R3		; Line = -1 if direct error
	INC	R0
	JR	Z,$+5
	CALL	LINEIN		; No - output line of error
	JR	PRNTOK		; Skip "POP BC"
POPNOK:
	POP	R4
	POP	R5		; Drop address in input buffer
;
PRNTOK:
	XOR	R0,R0		; Output "Ok" and get command
	LD	R8,#hi(CTLOFG)
	LD	R9,#lo(CTLOFG)
	LDC	@RR8,R0		; Enable output
	CALL	STTLIN		; Start new line
	LD	R2,#hi(OKMSG)
	LD	R3,#lo(OKMSG)	; "Ok" message
	CALL	PRS		; Output "Ok"
GETCMD:
	LD	R2,#hi(-1)	; Flag direct mode
	LD	R3,#lo(-1)
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save as current line
	CALL	GETLIN		; Get an input line
	JP	C,GETCMD	; Get line again if break
	CALL	GETCHR		; Get first character
	INC	R0		; Test if end of line
	DEC	R0		; Without affecting Carry
	JP	Z,GETCMD	; Nothing entered - Get another
	LD	R1,FLAGS
	PUSH	R1		; Save Carry status
	PUSH	R0
	CALL	ATOH		; Get line number into DE
	PUSH	R7
	PUSH	R6		; Save line number
	CALL	CRUNCH		; Tokenise rest of line
	LD	R4,R0		; Length of tokenised line
	POP	R6		; Restore line number
	POP	R7
	POP	R0
	POP	R1		; Restore Carry
	LD	FLAGS,R1
	JP	NC,EXCUTE	; No line number - Direct mode
	PUSH	R7
	PUSH	R6		; Save line number
	PUSH	R5
	PUSH	R4		; Save length of tokenised line
	XOR	R0,R0
	LD	R8,#hi(LSTBIN)
	LD	R9,#lo(LSTBIN)
	LDC	@RR8,R0		; Clear last byte input
	CALL	GETCHR		; Get next character
	OR	R0,R0		; Set flags
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; And save them
	CALL	SRCHLN		; Search for line number in DE
	JP	C,LINFND	; Jump if line found
	POP	R0
	POP	R1		; Get status
	LD	FLAGS,R1
	PUSH	R1
	PUSH	R0		; And re-save
	JP	Z,ULERR		; Nothing after number - Error
	RCF			; Clear Carry
LINFND:
	PUSH	R5
	PUSH	R4		; Save address of line in prog
	JP	NC,INEWLN	; Line not found - Insert new
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Next line address in DE
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; End of program
SFTPRG:
	LDC	R0,@RR6		; Shift rest of program down
	LDC	@RR4,R0
	INCW	RR4		; Next destination
	INCW	RR6		; Next source
	CALL	CPDEHL		; All done?
	JP	NZ,SFTPRG	; More to do
	LD	R2,R4		; HL - New end of program
	LD	R3,R5
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Update end of program
;
INEWLN:
	POP	R6		; Get address of line,
	POP	R7
	POP	R0
	POP	R1		; Get status
	LD	FLAGS,R1
	JP	Z,SETPTR	; No text - Set up pointers
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get end of program
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Get length of input line
	POP	R4
	POP	R5		; End of program to BC
	ADD	R3,R5
	ADC	R2,R4		; Find new end
	PUSH	R3
	PUSH	R2		; Save new end
	CALL	MOVUP		; Make space for line
	POP	R2
	POP	R3		; Restore new end
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Update end of program pointer
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Get line to move up in HL
	LDC	@RR2,R2		; Save MSB
	POP	R6
	POP	R7		; Get new line number
	INCW	RR2		; Skip pointer
	INCW	RR2
	LDC	@RR2,R7		; Save LSB of line number
	INCW	RR2
	LDC	@RR2,R6		; Save MSB of line number
	INCW	RR2		; To first byte in line
	LD	R6,#hi(BUFFER)
	LD	R7,#lo(BUFFER)	; Copy buffer to program
MOVBUF:
	LDC	R0,@RR6		; Get source
	LDC	@RR2,R0		; Save destinations
	INCW	RR2		; Next source
	INCW	RR6		; Next destination
	OR	R0,R0		; Done?
	JP	NZ,MOVBUF	; No - Repeat
SETPTR:
	CALL	RUNFST		; Set line pointers
	INCW	RR2		; To LSB of pointer
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Address to DE
PTRLP:
	LD	R2,R6		; Address to HL
	LD	R3,R7
	LDC	R0,@RR2		; Get LSB of pointer
	INCW	RR2		; To MSB of pointer
	LDC	R1,@RR2
	OR	R0,R1		; Compare with MSB pointer
	JP	Z,GETCMD	; Get command line if end
	INCW	RR2		; To LSB of line number
	INCW	RR2		; Skip line number
	INCW	RR2		; Point to first byte in line
	XOR	R0,R0		; Looking for 00 byte
FNDEND:
	LDC	R8,@RR2
	CP	R0,R8		; Found end of line?
	LD	R1,FLAGS
	INCW	RR2		; Move to next byte
	LD	FLAGS,R1
	JP	NZ,FNDEND	; No - Keep looking
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Next line address to HL
	LDC	@RR2,R7		; Save LSB of pointer
	INCW	RR2
	LDC	@RR2,R6		; Save MSB of pointer
	JP	PTRLP		; Do next line
;
SRCHLN:
	LD	R8,#hi(BASTXT)
	LD	R9,#lo(BASTXT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Start of program text
SRCHLP:
	LD	R4,R2		; BC = Address to look at
	LD	R5,R3
	LDC	R0,@RR2		; Get address of next line
	INCW	RR2
	LDC	R1,@RR2
	OR	R0,R1		; End of program found?
	LD	R1,FLAGS
	DECW	RR2
	LD	FLAGS,R1
	JR	NZ,$+3
	RET			; Yes - Line not found
	INCW	RR2
	INCW	RR2
	LDC	R0,@RR2		; Get LSB of line number
	INCW	RR2
	LDC	R2,@RR2		; Get MSB of line number
	LD	R3,R0
	CALL	CPDEHL		; Compare with line in DE
	LD	R2,R4		; HL = Start of this line
	LD	R3,R5
	LDC	R0,@RR2		; Get LSB of next line address
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	R2,@RR2		; Get MSB of next line address
	LD	R3,R0		; Next line to HL
	CCF
	JR	NZ,$+3
	RET			; Lines found - Exit
	CCF
	JR	C,$+3
	RET			; Line not found,at line after
	JP	SRCHLP		; Keep looking
;
NEW:
	JR	Z,$+3
	RET			; Return if any more on line
CLRPTR:
	LD	R8,#hi(BASTXT)
	LD	R9,#lo(BASTXT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Point to start of program
	XOR	R0,R0		; Set program area to empty
	LDC	@RR2,R0		; Save LSB = 00
	INCW	RR2
	LDC	@RR2,R0		; Save MSB = 00
	INCW	RR2
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set program end
;
RUNFST:
	LD	R8,#hi(BASTXT)
	LD	R9,#lo(BASTXT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Clear all variables
	DECW	RR2
;
INTVAR:
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Initialise RUN variables
	LD	R8,#hi(LSTRAM)
	LD	R9,#lo(LSTRAM)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get end of RAM
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Clear string space
	XOR	R0,R0
	CALL	RESTOR		; Reset DATA pointers
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get end of program
	LD	R8,#hi(VAREND)
	LD	R9,#lo(VAREND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Clear variables
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Clear arrays
;
CLREG:
	POP	R4
	POP	R5		; Save return address
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get end of working RAN
	LD	SPH,R2		; Set stack
	LD	SPL,R3
	LD	R2,#hi(TMSTPL)
	LD	R3,#lo(TMSTPL)	; Temporary string pool
	LD	R8,#hi(TMSTPT)
	LD	R9,#lo(TMSTPT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Reset temporary string ptr
	XOR	R0,R0		; A = 00
	LD	R3,R0		; HL = 0000
	LD	R2,R0
	LD	R8,#hi(CONTAD)
	LD	R9,#lo(CONTAD)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; No CONTinue
	LD	R8,#hi(FORFLG)
	LD	R9,#lo(FORFLG)
	LDC	@RR8,R0		; Clear FOR flag
	LD	R8,#hi(FNRGNM)
	LD	R9,#lo(FNRGNM)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Clear FN argument
	PUSH	R3
	PUSH	R2		; HL = 0000
	PUSH	R5
	PUSH	R4		; Put back return
DOAGN:
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	R2,@RR8
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	R3,@RR8		; Get address of code to RUN
	RET			; Return to execution driver
;
PROMPT:
	LD	R0,#'?'		; '?'
	CALL	OUTC		; Output character
	LD	R0,#' '		; Space
	CALL	OUTC		; Output character
	JP	RINPUT		; Get input line
;
CRUNCH:
	XOR	R0,R0		; Tokenise line @ HL to BUFFER
	LD	R8,#hi(DATFLG)
	LD	R9,#lo(DATFLG)
	LDC	@RR8,R0		; Reset literal flag
	LD	R5,#2+3		; 2 byte number and 3 nulls
	LD	R6,#hi(BUFFER)
	LD	R7,#lo(BUFFER)	; Start of input buffer
CRNCLP:
	LDC	R0,@RR2		; Get byte
	CP	R0,#' '		; Is it a space?
	JP	Z,MOVDIR	; Yes - Copy direct
	LD	R4,R0		; Save character
	CP	R0,#'"'		; Is it a quote?
	JP	Z,CPYLIT	; Yes - Copy literal string
	OR	R0,R0		; Is it end of buffer?
	JP	Z,ENDBUF	; Yes - End buffer
	LD	R8,#hi(DATFLG)
	LD	R9,#lo(DATFLG)
	LDC	R0,@RR8		; Get data type
	OR	R0,R0		; Literal?
	LDC	R0,@RR2		; Get byte to copy
	JP	NZ,MOVDIR	; Literal - Copy direct
	CP	R0,#'?'		; Is it '?' short for PRINT
	LD	R0,#ZPRINT	; "PRINT" token
	JP	Z,MOVDIR	; Yes - replace it
	LDC	R0,@RR2		; Get byte again
	CP	R0,#'0'		; Is it less than '0'
	JP	C,FNDWRD	; Yes - Look for reserved words
	CP	R0,#60		; ";"+1; Is it "0123456789:;" ?
	JP	C,MOVDIR	; Yes - copy it direct
FNDWRD:
	PUSH	R7
	PUSH	R6		; Look for reserved words
	LD	R6,#hi(WORDS-1)
	LD	R7,#lo(WORDS-1)	; Point to table
	PUSH	R5
	PUSH	R4		; Save count
	LD	R4,#hi(RETNAD)
	LD	R5,#lo(RETNAD)	; Where to return to
	PUSH	R5
	PUSH	R4		; Save return address
	LD	R4,#ZEND-1	; First token value -1
	LDC	R0,@RR2		; Get byte
	CP	R0,#'a'		; Less than 'a' ?
	JP	C,SEARCH	; Yes - search for words
	CP	R0,#'z'+1	; Greater than 'z' ?
	JP	NC,SEARCH	; Yes - search for words
	AND	R0,#01011111B	; Force upper case
	LDC	@RR2,R0		; Replace byte
SEARCH:
	LDC	R5,@RR2		; Search for a word
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
GETNXT:
	INCW	RR2		; Get next reserved word
	LDC	R1,@RR2
	OR	R0,R1		; Start of word?
	JP	PL,GETNXT	; No - move on
	INC	R4		; Increment token value
	LDC	R0,@RR2		; Get byte from table
	AND	R0,#01111111B	; Strip bit 7
	JR	NZ,$+3
	RET			; Return if end of list
	CP	R0,R5		; Same character as in buffer?
	JP	NZ,GETNXT	; No - get next word
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	PUSH	R3
	PUSH	R2		; Save start of word
;
NXTBYT:
	INCW	RR6		; Look through rest of word
	LDC	R0,@RR6		; Get byte from table
	OR	R0,R0		; End of word ?
	JP	MI,MATCH	; Yes - Match found
	LD	R5,R0		; Save it
	LD	R0,R4		; Get token value
	CP	R0,#ZGOTO	; Is it "GOTO" token ?
	JP	NZ,NOSPC	; No - Don't allow spaces
	CALL	GETCHR		; Get next character
	DECW	RR2		; Cancel increment from GETCHR
NOSPC:
	INCW	RR2		; Next byte
	LDC	R0,@RR2		; Get byte
	CP	R0,#'a'		; Less than 'a' ?
	JP	C,NOCHNG	; Yes - don't change
	AND	R0,#01011111B	; Make upper case
NOCHNG:
	CP	R0,R5		; Same as in buffer ?
	JP	Z,NXTBYT	; Yes - keep testing
	POP	R2		; Get back start of word
	POP	R3
	JP	SEARCH		; Look at next word
;
MATCH:
	LD	R5,R4		; Word found - Save token value
	POP	R0		; Throw away return
	POP	R1
	LD	FLAGS,R1
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	RET			; Return to "RETNAD"
RETNAD:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Get address in string
	LD	R0,R5		; Get token value
	POP	R4
	POP	R5		; Restore buffer length
	POP	R6
	POP	R7		; Get destination address
MOVDIR:
	INCW	RR2		; Next source in buffer
	LDC	@RR6,R0		; Put byte in buffer
	INCW	RR6		; Move up buffer
	INC	R5		; Increment length of buffer
	SUB	R0,#':'		; End of statement?
	JP	Z,SETLIT	; Jump if multi-statement line
	CP	R0,#ZDATA-3AH	; Is it DATA statement ?
	JP	NZ,TSTREM	; No - see if REM
SETLIT:
	LD	R8,#hi(DATFLG)
	LD	R9,#lo(DATFLG)
	LDC	@RR8,R0		; Set literal flag
TSTREM:
	SUB	R0,#ZREM-3AH	; Is it REM?
	JP	NZ,CRNCLP	; No - Leave flag
	LD	R4,R0		; Copy rest of buffer
NXTCHR:
	LDC	R0,@RR2		; Get byte
	OR	R0,R0
	RCF			; End of line ?
	JP	Z,ENDBUF	; Yes - Terminate buffer
	CP	R0,R4		; End of statement ?
	JP	Z,MOVDIR	; Yes - Get next one
CPYLIT:
	INCW	RR2		; Move up source string
	LDC	@RR6,R0		; Save in destination
	INC	R5		; Increment length
	INCW	RR6		; Move up destination
	JP	NXTCHR		; Repeat
;
ENDBUF:
	LD	R2,#hi(BUFFER-1)
	LD	R3,#lo(BUFFER-1); Point to start of buffer
	LDC	@RR6,R0		; Mark end of buffer (A = 00)
;	LD	R1,FLAGS
	INCW	RR6
;	LD	FLAGS,R1
	LDC	@RR6,R0		; A = 00
;	LD	R1,FLAGS
	INCW	RR6
;	LD	FLAGS,R1
	LDC	@RR6,R0		; A = 00
	RET
;
DODEL:
	LD	R8,#hi(NULFLG)
	LD	R9,#lo(NULFLG)
	LDC	R0,@RR8		; Get null flag status
	OR	R0,R0
	RCF			; Is it zero?
	LD	R0,#0		; Zero A - Leave flags
	LD	R8,#hi(NULFLG)
	LD	R9,#lo(NULFLG)
	LDC	@RR8,R0		; Zero null flag
	JP	NZ,ECHDEL	; Set - Echo it
	DEC	R4		; Decrement length
	JP	Z,GETLIN	; Get line again if empty
	CALL	OUTC		; Output null character
	JR	ECHDEL1		; Skip "DEC B"
ECHDEL:
	DEC	R4		; Count bytes in buffer
ECHDEL1:
	LD	R1,FLAGS
	DECW	RR2		; Back space buffer
	LD	FLAGS,R1
	JP	Z,OTKLN		; No buffer - Try again
	LDC	R0,@RR2		; Get deleted byte
	CALL	OUTC		; Echo it
	JP	MORINP		; Get more input
;
DELCHR:
	DEC	R4		; Count bytes in buffer
	LD	R1,FLAGS
	DECW	RR2		; Back space buffer
	LD	FLAGS,R1
	CALL	OUTC		; Output character in A
	JP	NZ,MORINP	; Not end - Get more
OTKLN:
	CALL	OUTC		; Output character in A
KILIN:
	CALL	PRCRLF		; Output CRLF
	JP	TTYLIN		; Get line again
;
GETLIN:
TTYLIN:
	LD	R2,#hi(BUFFER)	; Get a line by character
	LD	R3,#lo(BUFFER)
	LD	R4,#1		; Set buffer as empty
	XOR	R0,R0
	LD	R8,#hi(NULFLG)
	LD	R9,#lo(NULFLG)
	LDC	@RR8,R0		; Clear null flag
MORINP:
	CALL	CLOTST		; Get character and test ^O
	LD	R5,R0		; Save character in C
	CP	R0,#DEL		; Delete character?
	JP	Z,DODEL		; Yes - Process it
	LD	R8,#hi(NULFLG)
	LD	R9,#lo(NULFLG)
	LDC	R0,@RR8		; Get null flag
	OR	R0,R0		; Test null flag status
	JP	Z,PROCES	; Reset - Process character
	LD	R0,#0		; Set a null
	CALL	OUTC		; Output null
	XOR	R0,R0		; Clear A
	RCF
	LD	R8,#hi(NULFLG)
	LD	R9,#lo(NULFLG)
	LDC	@RR8,R0		; Reset null flag
PROCES:
	LD	R0,R5		; Get character
	CP	R0,#CTRLG	; Bell?
	JP	Z,PUTCTL	; Yes - Save it
	CP	R0,#CTRLC	; Is it control "C"?
	JR	NZ,$+5
	CALL	PRCRLF		; Yes - Output CRLF
	SCF			; Flag break
	JR	NZ,$+3
	RET			; Return if control "C"
	CP	R0,#CR		; Is it enter?
	JP	Z,ENDINP	; Yes - Terminate input
	CP	R0,#CTRLU	; Is it control "U"?
	JP	Z,KILIN		; Yes - Get another line
	CP	R0,#'@'		; Is it "kill line"?
	JP	Z,OTKLN		; Yes - Kill line
	CP	R0,#'_'		; Is it delete?
	JP	Z,DELCHR	; Yes - Delete character
	CP	R0,#BKSP	; Is it backspace?
	JP	Z,DELCHR	; Yes - Delete character
	CP	R0,#CTRLR	; Is it control "R"?
	JP	NZ,PUTBUF	; No - Put in buffer
	PUSH	R5
	PUSH	R4		; Save buffer length
	PUSH	R7
	PUSH	R6		; Save DE
	PUSH	R3
	PUSH	R2		; Save buffer address
	LD	R1,#0
	LDC	@RR2,R1		; Mark end of buffer
	CALL	OUTNCR		; Output and do CRLF
	LD	R2,#hi(BUFFER)	; Point to buffer start
	LD	R3,#lo(BUFFER)
	CALL	PRS		; Output buffer
	POP	R2
	POP	R3		; Restore buffer address
	POP	R6
	POP	R7		; Restore DE
	POP	R4
	POP	R5		; Restore buffer length
	JP	MORINP		; Get another character
;
PUTBUF:
	CP	R0,#' '		; Is it a control code?
	JP	C,MORINP	; Yes - Ignore
PUTCTL:
	LD	R0,R4		; Get number of bytes in buffer
	CP	R0,#72+1	; Test for line overflow
	LD	R0,#CTRLG	; Set a bell
	JP	NC,OUTNBS	; Ring bell if buffer full
	LD	R0,R5		; Get character
	LDC	@RR2,R5		; Save in buffer
	LD	R8,#hi(LSTBIN)
	LD	R9,#lo(LSTBIN)
	LDC	@RR8,R0		; Save last input byte
	INCW	RR2		; Move up buffer
	INC	R4		; Increment length
OUTIT:
	CALL	OUTC		; Output the character entered
	JP	MORINP		; Get another character
;
OUTNBS:
	CALL	OUTC		; Output bell and back over it
	LD	R0,#BKSP	; Set back space
	JP	OUTIT		; Output it and get more
;
CPDEHL:
	LD	R0,R2		; Get H
	SUB	R0,R6		; Compare with D
	JR	Z,$+3
	RET			; Different - Exit
	LD	R0,R3		; Get L
	SUB	R0,R7		; Compare with E
	RET			; Return status
;
CHKSYN:
	LDC	R0,@RR2		; Check syntax of character
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Address of test byte
	LDC	R8,@RR2
	INCW	RR2		; Return address
	CP	R0,R8		; Same as in code string?
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Put it back
	JP	Z,GETCHR	; Yes - Get next character
	JP	SNERR		; Different - ?SN Error
;
OUTC:
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save character
	LD	R8,#hi(CTLOFG)
	LD	R9,#lo(CTLOFG)
	LDC	R0,@RR8		; Get control "O" flag
	OR	R0,R0
	RCF			; Is it set?
	JP	NZ,POPAF	; Yes - don't output
	POP	R0		; Restore character
	POP	R1
	LD	FLAGS,R1
	PUSH	R5
	PUSH	R4		; Save buffer length
;	LD	R1,FLAGS
;	PUSH	R1
	PUSH	R0		; Save character
	CP	R0,#' '		; Is it a control code?
	JP	C,DINPOS	; Yes - Don't INC POS(X)
	LD	R8,#hi(LWIDTH)
	LD	R9,#lo(LWIDTH)
	LDC	R0,@RR8		; Get line width
	LD	R4,R0		; To B
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	R0,@RR8		; Get cursor position
	INC	R4		; Width 255?
	JP	Z,INCLEN	; Yes - No width limit
	DEC	R4		; Restore width
	CP	R0,R4		; At end of line?
	JR	NZ,$+5
	CALL	PRCRLF		; Yes - output CRLF
INCLEN:
	INC	R0		; Move on one character
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	@RR8,R0		; Save new position
DINPOS:
	POP	R0		; Restore character
;	POP	R1
;	LD	FLAGS,R1
	POP	R4
	POP	R5		; Restore buffer length
	CALL	MONOUT		; Send it
	RET
;
CLOTST:
	CALL	GETINP		; Get input character
	AND	R0,#01111111B	; Strip bit 7
	CP	R0,#CTRLO	; Is it control "O"?
	JR	Z,$+3
	RET			; No don't flip flag
	LD	R8,#hi(CTLOFG)
	LD	R9,#lo(CTLOFG)
	LDC	R0,@RR8		; Get flag
	COM	R0		; Flip it
	LD	R8,#hi(CTLOFG)
	LD	R9,#lo(CTLOFG)
	LDC	@RR8,R0		; Put it back
	XOR	R0,R0		; Null character
;	RCF
	RET
;
LIST:
	CALL	ATOH		; ASCII number to DE
	JR	Z,$+3
	RET			; Return if anything extra
	POP	R4
	POP	R5		; Rubbish - Not needed
	CALL	SRCHLN		; Search for line number in DE
	PUSH	R5
	PUSH	R4		; Save address of line
	CALL	SETLIN		; Set up lines counter
LISTLP:
	POP	R2
	POP	R3		; Restore address of line
	LDC	R5,@RR2		; Get LSB of next line
	INCW	RR2
	LDC	R4,@RR2		; Get MSB of next line
	INCW	RR2
	LD	R0,R4		; BC = 0 (End of program)?
	OR	R0,R5
	JP	Z,PRNTOK	; Yes - Go to command mode
	CALL	COUNT		; Count lines
	CALL	TSTBRK		; Test for break key
	PUSH	R5
	PUSH	R4		; Save address of next line
	CALL	PRCRLF		; Output CRLF
	LDC	R7,@RR2		; Get LSB of line number
	INCW	RR2
	LDC	R6,@RR2		; Get MSB of line number
	INCW	RR2
	PUSH	R3
	PUSH	R2		; Save address of line start
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Line number to HL
	CALL	PRNTHL		; Output line number in decimal
	LD	R0,#' '		; Space after line number
	POP	R2
	POP	R3		; Restore start of line address
LSTLP2:
	CALL	OUTC		; Output character in A
LSTLP3:
	LDC	R0,@RR2		; Get next byte in line
	INCW	RR2		; To next byte in line
	OR	R0,R0		; End of line?
	JP	Z,LISTLP	; Yes - get next line
	JP	PL,LSTLP2	; No token - output it
	SUB	R0,#ZEND-1	; Find and output word
	LD	R5,R0		; Token offset+1 to C
	LD	R6,#hi(WORDS)
	LD	R7,#lo(WORDS)	; Reserved word list
FNDTOK:
	LDC	R0,@RR6		; Get character in list
	INCW	RR6		; Move on to next
	OR	R0,R0		; Is it start of word?
	JP	PL,FNDTOK	; No - Keep looking for word
	DEC	R5		; Count words
	JP	NZ,FNDTOK	; Not there - keep looking
OUTWRD:
	AND	R0,#01111111B	; Strip bit 7
	CALL	OUTC		; Output first character
	LDC	R0,@RR6		; Get next character
	INCW	RR6		; Move on to next
	OR	R0,R0		; Is it end of word?
	JP	PL,OUTWRD	; No - output the rest
	JP	LSTLP3		; Next byte in line
;
SETLIN:
	PUSH	R3
	PUSH	R2		; Set up LINES counter
	LD	R8,#hi(LINESN)
	LD	R9,#lo(LINESN)
	LDC	R2,@RR8
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	R3,@RR8		; Get LINES number
	LD	R8,#hi(LINESC)
	LD	R9,#lo(LINESC)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Save in LINES counter
	POP	R2
	POP	R3
	RET
;
COUNT:
	PUSH	R3
	PUSH	R2		; Save code string address
	PUSH	R7
	PUSH	R6
	LD	R8,#hi(LINESC)
	LD	R9,#lo(LINESC)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get LINES counter
	LD	R6,#hi(-1)
	LD	R7,#lo(-1)
	ADC	R3,R7
	ADC	R2,R6		; Decrement
	LD	R8,#hi(LINESC)
	LD	R9,#lo(LINESC)
	LDC	@RR8,R2
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Put it back
	POP	R6
	POP	R7
	POP	R2
	POP	R3		; Restore code string address
	JR	MI,$+3
	RET			; Return if more lines to go
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(LINESN)
	LD	R9,#lo(LINESN)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get LINES number
	LD	R8,#hi(LINESC)
	LD	R9,#lo(LINESC)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Reset LINES counter
	CALL	GETINP		; Get input character
	CP	R0,#CTRLC	; Is it control "C"?
	JP	Z,RSLNBK	; Yes - Reset LINES and break
	POP	R2
	POP	R3		; Restore code string address
	JP	COUNT		; Keep on counting
;
RSLNBK:
	LD	R8,#hi(LINESN)
	LD	R9,#lo(LINESN)
	LDC	R2,@RR8
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	R3,@RR8		; Get LINES number
	LD	R8,#hi(LINESC)
	LD	R9,#lo(LINESC)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Reset LINES counter
	JP	BRKRET		; Go and output "Break"
;
FOR:
	LD	R0,#64H		; Flag "FOR" assignment
	LD	R8,#hi(FORFLG)
	LD	R9,#lo(FORFLG)
	LDC	@RR8,R0		; Save "FOR" flag
	CALL	LET		; Set up initial index
	POP	R4
	POP	R5		; Drop RETurn address
	PUSH	R3
	PUSH	R2		; Save code string address
	CALL	DATA		; Get next statement address
	LD	R8,#hi(LOOPST)
	LD	R9,#lo(LOOPST)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save it for start of loop
	LD	R2,#hi(2)
	LD	R3,#lo(2)	; Offset for "FOR" block
	ADD	R3,SPL
	ADC	R2,SPH		; Point to it
FORSLP:
	CALL	LOKFOR		; Look for existing "FOR" block
	POP	R6
	POP	R7		; Get code string address
	JP	NZ,FORFND	; No nesting found
	ADD	R3,R5
	ADC	R2,R4		; Move into "FOR" block
	PUSH	R7
	PUSH	R6		; Save code string address
	DECW	RR2
	LDC	R6,@RR2		; Get MSB of loop statement
	DECW	RR2
	LDC	R7,@RR2		; Get LSB of loop statement
	INCW	RR2
	INCW	RR2
	PUSH	R3
	PUSH	R2		; Save block address
	LD	R8,#hi(LOOPST)
	LD	R9,#lo(LOOPST)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get address of loop statement
	CALL	CPDEHL		; Compare the FOR loops
	POP	R2
	POP	R3		; Restore block address
	JP	NZ,FORSLP	; Different FORs - Find another
	POP	R6
	POP	R7		; Restore code string address
	LD	SPH,R2
	LD	SPL,R3		; Remove all nested loops
;
FORFND:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Code string address to HL
	LD	R5,#8
	CALL	CHKSTK		; Check for 8 levels of stack
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(LOOPST)
	LD	R9,#lo(LOOPST)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get first statement of loop
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save and restore code string
	PUSH	R3
	PUSH	R2		; Re-save code string address
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get current line number
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save and restore code string
	CALL	TSTNUM		; Make sure it's a number
	CALL	CHKSYN		; Make sure "TO" is next
	DB	ZTO		; "TO" token
	CALL	GETNUM		; Get "TO" expression value
	PUSH	R3
	PUSH	R2		; Save code string address
	CALL	BCDEFP		; Move "TO" value to BCDE
	POP	R2
	POP	R3		; Restore code string address

	PUSH	R4		; Save "TO" value in block
	PUSH	R5		; @@@ SWAP BC
	PUSH	R6		; @@@ SWAP DE
	PUSH	R7

	LD	R4,#hi(8100H)
	LD	R5,#lo(8100H)	; BCDE - 1 (default STEP)
	LD	R6,R5		; C=0
	LD	R7,R6		; D=0
	LDC	R0,@RR2		; Get next byte in code string
	CP	R0,#ZSTEP	; See if "STEP" is stated
	LD	R0,#1		; Sign of step = 1
	JP	NZ,SAVSTP	; No STEP given - Default to 1
	CALL	GETCHR		; Jump over "STEP" token
	CALL	GETNUM		; Get step value
	PUSH	R3
	PUSH	R2		; Save code string address
	CALL	BCDEFP		; Move STEP to BCDE
	CALL	TSTSGN		; Test sign of FPREG
	POP	R2
	POP	R3		; Restore code string address
SAVSTP:
	PUSH	R4		; Save the STEP value in block
	PUSH	R5		; @@@ SWAP DE
	PUSH	R6		; @@@ SWAP BC
	PUSH	R7

	LD	R1,FLAGS
;	PUSH	R1
	PUSH	R0		; Save sign of STEP
;	ADD	SPL,#1
;	ADC	SPH,#0		; Don't save flags

	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get address of index variable
	POP	R8
	POP	R9

	PUSH	R2		; @@@ SWAP HL
	PUSH	R3

	LD	R2,R8
	LD	R3,R9		; Save and restore code string
PUTFID:
	LD	R4,#ZFOR	; "FOR" block marker
;	PUSH	R5
	PUSH	R4		; Save it
;	ADD	SPL,#1
;	ADC	SPH,#0		; Don't save C
;
RUNCNT:
	CALL	TSTBRK		; Execution driver - Test break
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save code address for break
	LDC	R0,@RR2		; Get next byte in code string
	CP	R0,#':'		; Multi statement line?
	JP	Z,EXCUTE	; Yes - Execute it
	OR	R0,R0		; End of line?
	JP	NZ,SNERR	; No - Syntax error
	INCW	RR2		; Point to address of next line
	LDC	R0,@RR2		; Get LSB of line pointer
	INCW	RR2
	LDC	R1,@RR2
	OR	R0,R1		; Is it zero (End of prog)?
	JP	Z,ENDPRG	; Yes - Terminate execution
	INCW	RR2		; Point to line number
	LDC	R7,@RR2		; Get LSB of line number
	INCW	RR2
	LDC	R6,@RR2		; Get MSB of line number
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Line number to HL
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save as current line number
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Line number back to DE
EXCUTE:
	CALL	GETCHR		; Get key word
	LD	R6,#hi(RUNCNT)
	LD	R7,#lo(RUNCNT)	; Where to RETurn to
	PUSH	R7
	PUSH	R6		; Save for RETurn
IFJMP:
	JR	NZ,$+3
	RET			; Go to RUNCNT if end of STMT
ONJMP:
	SUB	R0,#ZEND	; Is it a token?
	JP	C,LET		; No - try to assign it
	CP	R0,#ZNEW+1-ZEND	; END to NEW ?
	JP	NC,SNERR	; Not a key word - ?SN Error
	RL	R0		; Double it
	LD	R5,R0		; BC = Offset into table
	LD	R4,#0
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Save code string address
	LD	R2,#hi(WORDTB)
	LD	R3,#lo(WORDTB)	; Keyword address table
	ADD	R3,R5
	ADC	R2,R4		; Point to routine address

;	LDC	R5,@RR2		; Get LSB of routine address
	LDC	R4,@RR2		; Get MSB of routine address

	INCW	RR2

;	LDC	R4,@RR2		; Get MSB of routine address
	LDC	R5,@RR2		; Get LSB of routine address

	PUSH	R5
	PUSH	R4		; Save routine address
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Restore code string address
;
GETCHR:
	INCW	RR2		; Point to next character
	LDC	R0,@RR2		; Get next code string byte
	CP	R0,#':'		; Z if ':'
	JR	C,$+3
	RET			; NC if > "9"
	CP	R0,#' '
	JP	Z,GETCHR	; Skip over spaces
	CP	R0,#'0'
	CCF			; NC if < '0'
	INC	R0		; Test for zero - Leave carry
	DEC	R0		; Z if Null
	RET
;
RESTOR:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Save code string address
	LD	R8,#hi(BASTXT)
	LD	R9,#lo(BASTXT)
	LDC	R2,@RR8
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	R3,@RR8		; Point to start of program
	JP	Z,RESTNL	; Just RESTORE - reset pointer
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Restore code string address
	CALL	ATOH		; Get line number to DE
	PUSH	R3
	PUSH	R2		; Save code string address
	CALL	SRCHLN		; Search for line number in DE
	LD	R2,R4		; HL = Address of line
	LD	R3,R5
	POP	R6
	POP	R7		; Restore code string address
	JP	NC,ULERR	; ?UL Error if not found
RESTNL:
;	LD	R1,FLAGS
	DECW	RR2		; Byte before DATA statement
;	LD	FLAGS,R1
UPDATA:
	LD	R8,#hi(NXTDAT)
	LD	R9,#lo(NXTDAT)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Update DATA pointer
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Restore code string address
	RET
;

TSTBRK:
	CALL	CONST		; Check input status
	JR	NZ,$+3
	RET			; No key, go back
	CALL	CONIN		; Get the key into A
	CP	R0,#ESC		; Escape key?
	JR	Z,BRK		; Yes, break
	CP	R0,#CTRLC		; <Ctrl-C>
	JR	Z,BRK		; Yes, break
	CP	R0,#CTRLS		; Stop scrolling?
	JR	Z,$+3
	RET			; Other key, ignore
;

STALL:
	CALL	CONIN		; Wait for key
	CP	R0,#CTRLQ	; Resume scrolling?
	JR	NZ,$+3
	RET			; Release the chokehold
	CP	R0,#CTRLC	; Second break?
	JR	Z,STOP		; Break during hold exits prog
	JR	STALL		; Loop until <Ctrl-Q> or <brk>
;
BRK:
	LD	R0,#0FFH	; Set BRKFLG
	LD	R8,#hi(BRKFLG)
	LD	R9,#lo(BRKFLG)
	LDC	@RR8,R0		; Store it
;

STOP:
	JR	Z,$+3
	RET			; Exit if anything else
	OR	R0,#0C0H	; Flag "STOP"
	JR	PEND1
PEND:
	JR	Z,$+3
	RET			; Exit if anything else
PEND1:
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	@RR8,R2
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Save point of break
	JR	INPBRK1		; Skip "OR 11111111B"
INPBRK:
	OR	R0,#11111111B	; Flag "Break" wanted
INPBRK1:
	POP	R4
	POP	R5		; Return not needed and more
ENDPRG:
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	R2,@RR8
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	R3,@RR8		; Get current line number
	LD	R1,FLAGS
	PUSH	R1		; Save STOP / END status
	PUSH	R0
	LD	R0,R3		; Is it direct break?
	AND	R0,R2
	INC	R0		; Line is -1 if direct break
	JP	Z,NOLIN		; Yes - No line number
	LD	R8,#hi(ERRLIN)
	LD	R9,#lo(ERRLIN)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save line of break
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get point of break
	LD	R8,#hi(CONTAD)
	LD	R9,#lo(CONTAD)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save point to CONTinue
NOLIN:
	XOR	R0,R0
	LD	R8,#hi(CTLOFG)
	LD	R9,#lo(CTLOFG)
	LDC	@RR8,R0		; Enable output
	CALL	STTLIN		; Start a new line
	POP	R0
	POP	R1		; Restore STOP / END status
	LD	FLAGS,R1
	LD	R2,#hi(BRKMSG)
	LD	R3,#lo(BRKMSG)	; "Break" message
	JP	NZ,ERRIN	; "in line" wanted?
	JP	PRNTOK		; Go to command mode
;
CONT:
	LD	R8,#hi(CONTAD)
	LD	R9,#lo(CONTAD)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get CONTinue address
	LD	R0,R2		; Is it zero?
	OR	R0,R3
	LD	R7,#CN		; ?CN Error
	JP	Z,ERROR		; Yes - output "?CN Error"
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Save code string address
	LD	R8,#hi(ERRLIN)
	LD	R9,#lo(ERRLIN)
	LDC	R2,@RR8
	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	R3,@RR8		; Get line of last break
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Set up current line number
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Restore code string address
	RET			; CONTinue where left off
;
NULL:
	CALL	GETINT		; Get integer 0-255
	JR	Z,$+3
	RET			; Return if bad value
	LD	R8,#hi(NULLS)
	LD	R9,#lo(NULLS)
	LDC	@RR8,R0		; Set nulls number
	RET
;

ACCSUM:
	PUSH	R3
	PUSH	R2		; Save address in array
	LD	R8,#hi(CHKSUM)
	LD	R9,#lo(CHKSUM)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get check sum
	LD	R4,#0		; BC - Value of byte
	LD	R5,R0
	ADD	R3,R5
	ADC	R2,R4		; Add byte to check sum
	LD	R8,#hi(CHKSUM)
	LD	R9,#lo(CHKSUM)
	LDC	@RR8,R2
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Re-save check sum
	POP	R2
	POP	R3		; Restore address in array
	RET
;
CHKLTR:
	LDC	R0,@RR2		; Get byte
	CP	R0,#'A'		; < 'a' ?
	JR	NC,$+3
	RET			; Carry set if not letter
	CP	R0,#'Z'+1	; > 'z' ?
	CCF
	RET			; Carry set if not letter
;
FPSINT:
	CALL	GETCHR		; Get next character
POSINT:
	CALL	GETNUM		; Get integer 0 to 32767
DEPINT:
	CALL	TSTSGN		; Test sign of FPREG
	JP	MI,FCERR	; Negative - ?FC Error
DEINT:
	LD	R8,#hi(FPEXP)
	LD	R9,#lo(FPEXP)
	LDC	R0,@RR8		; Get integer value to DE
	CP	R0,#80H+16	; Exponent in range (16 bits)?
	JP	C,FPINT		; Yes - convert it
	LD	R4,#hi(9080H)
	LD	R5,#lo(9080H)	; BCDE = -32768
	LD	R6,#hi(0000H)
	LD	R7,#lo(0000H)
	PUSH	R3
	PUSH	R2		; Save code string address
	CALL	CMPNUM		; Compare FPREG with BCDE
	POP	R2
	POP	R3		; Restore code string address
	LD	R6,R5		; MSB to D
	JR	NZ,$+3
	RET			; Return if in range
FCERR:
	LD	R7,#FC		; ?FC Error
	JP	ERROR		; Output error-
;
ATOH:
	LD	R1,FLAGS
	DECW	RR2		; ASCII number to DE binary
	LD	FLAGS,R1
GETLN:
	LD	R6,#hi(0)
	LD	R7,#lo(0)	; Get number to DE
GTLNLP:
	CALL	GETCHR		; Get next character
	JR	C,$+3
	RET			; Exit if not a digit
	PUSH	R3
	PUSH	R2		; Save code string address
;	LD	R1,FLAGS
;	PUSH	R1
	PUSH	R0		; Save digit
	LD	R2,#hi(65529/10)
	LD	R3,#lo(65529/10); Largest number 65529
	CALL	CPDEHL		; Number in range?
	JP	C,SNERR		; No - ?SN Error
	LD	R2,R6		; HL = Number
	LD	R3,R7
	ADD	R3,R7
	ADC	R2,R6		; Times 2
	ADD	R3,R3
	ADC	R2,R2		; Times 4
	ADD	R3,R7
	ADC	R2,R6		; Times 5
	ADD	R3,R3
	ADC	R2,R2		; Times 10
	POP	R0		; Restore digit
;	POP	R1
;	LD	FLAGS,R1
	SUB	R0,#'0'		; Make it 0 to 9
	LD	R7,R0		; DE = Value of digit
	LD	R6,#0
	ADD	R3,R7
	ADC	R2,R6		; Add to number
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Number to DE
	POP	R2
	POP	R3		; Restore code string address
	JP	GTLNLP		; Go to next character
;
CLEAR:
	JP	Z,INTVAR	; Just "CLEAR" Keep parameters
	CALL	POSINT		; Get integer 0 to 32767 to DE
	DECW	RR2		; Cancel increment
	CALL	GETCHR		; Get next character
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(LSTRAM)
	LD	R9,#lo(LSTRAM)
	LDC	R2,@RR8
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	R3,@RR8		; Get end of RAM
	JP	Z,STORED	; No value given - Use stored
	POP	R2
	POP	R3		; Restore code string address
	CALL	CHKSYN		; Check for comma
	DB	','
	PUSH	R7
	PUSH	R6		; Save number
	CALL	POSINT		; Get integer 0 to 32767
	DECW	RR2		; Cancel increment
	CALL	GETCHR		; Get next character
	JP	NZ,SNERR	; ?SN Error if more on line
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save code string address
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Number to DE
STORED:
	LD	R0,R3		; Get LSB of new RAM top
	SUB	R0,R7		; Subtract LSB of string space
	LD	R7,R0		; Save LSB
	LD	R0,R2		; Get MSB of new RAM top
	SBC	R0,R6		; Subtract MSB of string space
	LD	R6,R0		; Save MSB
	JP	C,OMERR		; ?OM Error if not enough mem
	PUSH	R3
	PUSH	R2		; Save RAM top
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get program end
	LD	R4,#hi(40)
	LD	R5,#lo(40)	; 40 Bytes minimum working RAM
	ADD	R3,R5
	ADC	R2,R4		; Get lowest address
	CALL	CPDEHL		; Enough memory?
	JP	NC,OMERR	; No - ?OM Error
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; RAM top to HL
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set new string space
	POP	R2
	POP	R3		; End of memory to use
	LD	R8,#hi(LSTRAM)
	LD	R9,#lo(LSTRAM)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set new top of RAM
	POP	R2
	POP	R3		; Restore code string address
	JP	INTVAR		; Initialise variables
;
RUN:
	JP	Z,RUNFST	; RUN from start if just RUN
	CALL	INTVAR		; Initialise variables
	LD	R4,#hi(RUNCNT)
	LD	R5,#lo(RUNCNT)	; Execution driver loop
	JP	RUNLIN		; RUN from line number
;
GOSUB:
	LD	R5,#3		; 3 Levels of stack needed
	CALL	CHKSTK		; Check for 3 levels of stack
	POP	R4
	POP	R5		; Get return address
	PUSH	R3
	PUSH	R2		; Save code string for RETURN
	PUSH	R3
	PUSH	R2		; And for GOSUB routine
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get current line
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Into stack - Code string out
	LD	R0,#ZGOSUB	; "GOSUB" token
;	LD	R1,FLAGS
;	PUSH	R1
	PUSH	R0		; Save token
;	ADD	SPL,#1		; Don't save flags
;	ADC	SPH,#0
;
RUNLIN:
	PUSH	R5
	PUSH	R4		; Save return address
GOTO:
	CALL	ATOH		; ASCII number to DE binary
	CALL	REM		; Get end of line
	PUSH	R3
	PUSH	R2		; Save end of line
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get current line
	CALL	CPDEHL		; Line after current?
	POP	R2
	POP	R3		; Restore end of line
	LD	R1,FLAGS
	INCW	RR2		; Start of next line
	LD	FLAGS,R1
	JR	NC,$+5
	CALL	SRCHLP		; Line is after current line
	JR	C,$+5
	CALL	SRCHLN		; Line is before current line
	LD	R2,R4		; Set up code string address
	LD	R3,R5
	LD	R1,FLAGS
	DECW	RR2		; Incremented after
	LD	FLAGS,R1
	JR	NC,$+3
	RET			; Line found
ULERR:
	LD	R7,#UL		; ?UL Error
	JP	ERROR		; Output error message
;
RETURN:
	JR	Z,$+3
	RET			; Return if not just RETURN
	LD	R6,#-1		; Flag "GOSUB" search
	CALL	BAKSTK		; Look "GOSUB" block
	LD	SPH,R2
	LD	SPL,R3		; Kill all FORs in subroutine
	CP	R0,#ZGOSUB	; Test for "GOSUB" token
	LD	R7,#RG		; ?RG Error
	JP	NZ,ERROR	; Error if no "GOSUB" found
	POP	R2
	POP	R3		; Get RETURN line number
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save as current
	INCW	RR2		; Was it from direct statement?
	LD	R0,R2
	OR	R0,R3		; Return to line
	JP	NZ,RETLIN	; No - Return to line
	LD	R8,#hi(LSTBIN)
	LD	R9,#lo(LSTBIN)
	LDC	R0,@RR8		; Any INPUT in subroutine?
	OR	R0,R0		; If so buffer is corrupted
	JP	NZ,POPNOK	; Yes - Go to command mode
RETLIN:
	LD	R2,#hi(RUNCNT)
	LD	R3,#lo(RUNCNT)	; Execution driver loop
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Into stack - Code string out
	JR	DATA		; Skip "POP HL"
NXTDTA:
	POP	R2
	POP	R3		; Restore code string address
;
DATA:
	LD	R5,#':'		; ':' End of statement
	JR	REM1
REM:
	LD	R5,#0		; 00	End of statement
REM1:
	LD	R4,#0
NXTSTL:
	LD	R0,R5		; Statement and byte
	LD	R5,R4
	LD	R4,R0		; Statement end byte
NXTSTT:
	LDC	R0,@RR2		; Get byte
	OR	R0,R0		; End of line?
	JR	NZ,$+3
	RET			; Yes - Exit
	CP	R0,R4		; End of statement?
	JR	NZ,$+3
	RET			; Yes - Exit
	INCW	RR2		; Next byte
	CP	R0,#'"'		; Literal string?
	JP	Z,NXTSTL	; Yes - Look for another '"'
	JP	NXTSTT		; Keep looking
;
LET:
	CALL	GETVAR		; Get variable name
	CALL	CHKSYN		; Make sure "=" follows
	DB	ZEQUAL		; "=" token
	PUSH	R7
	PUSH	R6		; Save address of variable
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Get data type
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save type
	CALL	EVAL		; Evaluate expression
	POP	R0		; Restore type
	POP	R1
	LD	FLAGS,R1
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save code - Get var addr
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	@RR8,R2
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Save address of variable
	RRC	R0		; Adjust type
	CALL	CHKTYP		; Check types are the same
	JP	Z,LETNUM	; Numeric - Move value
LETSTR:
	PUSH	R3
	PUSH	R2		; Save address of string var
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)
;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; Pointer to string entry
	LDC	R3,@RR8
	INCW	RR8
	LDC	R2,@RR8		; Pointer to string entry

	PUSH	R3
	PUSH	R2		; Save it on stack
	INCW	RR2		; Skip over length
	INCW	RR2
	LDC	R7,@RR2		; LSB of string address
	INCW	RR2
	LDC	R6,@RR2		; MSB of string address
	LD	R8,#hi(BASTXT)
	LD	R9,#lo(BASTXT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Point to start of program
	CALL	CPDEHL		; Is string before program?
	JP	NC,CRESTR	; Yes - Create string entry
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Point to string space
	CALL	CPDEHL		; Is string literal in program?
	POP	R6
	POP	R7		; Restore address of string
	JP	NC,MVSTPT	; Yes - Set up pointer
	LD	R2,#hi(TMPSTR)
	LD	R3,#lo(TMPSTR)	; Temporary string pool
	CALL	CPDEHL		; Is string in temporary pool?
	JP	NC,MVSTPT	; No - Set up pointer
	JR	CRESTR1		; Skip "POP DE"
CRESTR:
	POP	R6
	POP	R7		; Restore address of string
CRESTR1:
	CALL	BAKTMP		; Back to last tmp-str entry
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Address of string entry
	CALL	SAVSTR		; Save string in string area
MVSTPT:
	CALL	BAKTMP		; Back to last tmp-str entry
	POP	R2
	POP	R3		; Get string pointer
	CALL	DETHL4		; Move string pointer to var
	POP	R2
	POP	R3		; Restore code string address
	RET
;
LETNUM:
	PUSH	R3
	PUSH	R2		; Save address of variable
	CALL	FPTHL		; Move value to variable
	POP	R6
	POP	R7		; Restore address of variable
	POP	R2
	POP	R3		; Restore code string address
	RET
;
ON:
	CALL	GETINT		; Get integer 0-255
	LDC	R0,@RR2		; Get "GOTO" or "GOSUB" token
	LD	R4,R0		; Save in B
	CP	R0,#ZGOSUB	; "GOSUB" token?
	JP	Z,ONGO		; Yes - Find line number
	CALL	CHKSYN		; Make sure it's "GOTO"
	DB	ZGOTO		; "GOTO" token
	DECW	RR2		; Cancel increment
ONGO:
	LD	R5,R7		; Integer of branch value
ONGOLP:
	DEC	R5		; Count branches
	LD	R0,R4		; Get "GOTO" or "GOSUB" token
	JP	Z,ONJMP		; Go to that line if right one
	CALL	GETLN		; Get line number to DE
	CP	R0,#','		; Another line number?
	JR	Z,$+3
	RET			; No - Drop through
	JP	ONGOLP		; Yes - loop
;
IF:
	CALL	EVAL		; Evaluate expression
	LDC	R0,@RR2		; Get token
	CP	R0,#ZGOTO	; "GOTO" token?
	JP	Z,IFGO		; Yes - Get line
	CALL	CHKSYN		; Make sure it's "THEN"
	DB	ZTHEN		; "THEN" token
	DECW	RR2		; Cancel increment
IFGO:
	CALL	TSTNUM		; Make sure it's numeric
	CALL	TSTSGN		; Test state of expression
	JP	Z,REM		; False - Drop through
	CALL	GETCHR		; Get next character
	JP	C,GOTO		; Number - GOTO that line
	JP	IFJMP		; Otherwise do statement
;
MRPRNT:
	DECW	RR2		; DEC 'cos GETCHR INCs
	CALL	GETCHR		; Get next character
PRINT:
	JP	Z,PRCRLF	; CRLF if just PRINT
PRNTLP:
	JR	NZ,$+3
	RET			; End of list - Exit
	CP	R0,#ZTAB	; "TAB(" token?
	JP	Z,DOTAB		; Yes - Do TAB routine
	CP	R0,#ZSPC	; "SPC(" token?
	JP	Z,DOTAB		; Yes - Do SPC routine
	PUSH	R3
	PUSH	R2		; Save code string address
	CP	R0,#','		; Comma?
	JP	Z,DOCOM		; Yes - Move to next zone
	CP	R0,#';'		; Semi-colon?
	JP	Z,NEXITM	; Do semi-colon routine
	POP	R4
	POP	R5		; Code string address to BC
	CALL	EVAL		; Evaluate expression
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Get variable type
	OR	R0,R0
	RCF			; Is it a string variable?
	JP	NZ,PRNTST	; Yes - Output string contents
	CALL	NUMASC		; Convert number to text
	CALL	CRTST		; Create temporary string
	LD	R1,#' '
	LDC	@RR2,R1		; Followed by a space
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)
;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; Get length of output
	LDC	R3,@RR8
	INCW	RR8
	LDC	R2,@RR8		; Get length of output

	LDC	R8,@RR2
	INC	R8
	LDC	@RR2,R8		; Plus 1 for the space
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)
;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; < Not needed >
	LDC	R3,@RR8
	INCW	RR8
	LDC	R2,@RR8		; < Not needed >

	LD	R8,#hi(LWIDTH)
	LD	R9,#lo(LWIDTH)
	LDC	R0,@RR8		; Get width of line
	LD	R4,R0		; To B
	INC	R4		; Width 255 (No limit)?
	JP	Z,PRNTNB	; Yes - Output number string
	INC	R4		; Adjust it
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	R0,@RR8		; Get cursor position
	LDC	R8,@RR2
	ADD	R0,R8		; Add length of string
	DEC	R0		; Adjust it
	CP	R0,R4		; Will output fit on this line?
	JR	C,$+5
	CALL	PRCRLF		; No - CRLF first
PRNTNB:
	CALL	PRS1		; Output string at (HL)
	XOR	R0,R0
	RCF			; Skip CALL by setting 'z' flag
PRNTST:
	JR	Z,$+5
	CALL	PRS1		; Output string at (HL)
	POP	R2
	POP	R3		; Restore code string address
	JP	MRPRNT		; See if more to PRINT
;
STTLIN:
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	R0,@RR8		; Make sure on new line
	OR	R0,R0
	RCF			; Already at start?
	JR	NZ,$+3
	RET			; Yes - Do nothing
	JP	PRCRLF		; Start a new line
;
ENDINP:
	LD	R1,#0
	LDC	@RR2,R1		; Mark end of buffer
	LD	R2,#hi(BUFFER-1)
	LD	R3,#lo(BUFFER-1); Point to buffer
PRCRLF:
	LD	R0,#CR		; Load a CR
	CALL	OUTC		; Output character
	LD	R0,#LF		; Load a LF
	CALL	OUTC		; Output character
DONULL:
	XOR	R0,R0
	RCF			; Set to position 0
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	@RR8,R0		; Store it
	LD	R8,#hi(NULLS)
	LD	R9,#lo(NULLS)
	LDC	R0,@RR8		; Get number of nulls
NULLP:
	DEC	R0		; Count them
	JR	NZ,$+3
	RET			; Return if done
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save count
	XOR	R0,R0
	RCF			; Load a null
	CALL	OUTC		; Output it
	POP	R0		; Restore count
	POP	R1
	LD	FLAGS,R1
	JP	NULLP		; Keep counting
;
DOCOM:
	LD	R8,#hi(COMMAN)
	LD	R9,#lo(COMMAN)
	LDC	R0,@RR8		; Get comma width
	LD	R4,R0		; Save in B
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	R0,@RR8		; Get current position
	CP	R0,R4		; Within the limit?
	JR	C,$+5
	CALL	PRCRLF		; No - output CRLF
	JP	NC,NEXITM	; Get next item
ZONELP:
	SUB	R0,#14		; Next zone of 14 characters
	JP	NC,ZONELP	; Repeat if more zones
	COM	R0		; Number of spaces to output
	JP	ASPCS		; Output them
;
DOTAB:
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save token
	CALL	FNDNUM		; Evaluate expression
	CALL	CHKSYN		; Make sure ")" follows
	DB	")"
	DECW	RR2		; Back space on to ")"
	POP	R0		; Restore token
	POP	R1
	LD	FLAGS,R1
	SUB	R0,#ZSPC	; Was it "SPC(" ?
	PUSH	R3
	PUSH	R2		; Save code string address
	JP	Z,DOSPC		; Yes - Do 'E' spaces
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	R0,@RR8		; Get current position
DOSPC:
	COM	R0		; Number of spaces to print to
	ADD	R0,R7		; Total number to print
	JP	NC,NEXITM	; TAB < Current POS(X)
ASPCS:
	INC	R0		; Output A spaces
	LD	R4,R0		; Save number to print
	LD	R0,#' '		; Space
SPCLP:
	CALL	OUTC		; Output character in A
	DEC	R4		; Count them
	JP	NZ,SPCLP	; Repeat if more
NEXITM:
	POP	R2
	POP	R3		; Restore code string address
	CALL	GETCHR		; Get next character
	JP	PRNTLP		; More to print
;
REDO:
	DB	"?Redo from start",CR,LF,0
;
BADINP:
	LD	R8,#hi(READFG)
	LD	R9,#lo(READFG)
	LDC	R0,@RR8		; READ or INPUT?
	OR	R0,R0
	RCF
	JP	NZ,DATSNR	; READ - ?SN Error
	POP	R4
	POP	R5		; Throw away code string addr
	LD	R2,#hi(REDO)
	LD	R3,#lo(REDO)	; "Redo from start" message
	CALL	PRS		; Output string
	JP	DOAGN		; Do last INPUT again
;
INPUT:
	CALL	IDTEST		; Test for illegal direct
	LDC	R0,@RR2		; Get character after "INPUT"
	CP	R0,#'"'		; Is there a prompt string?
	LD	R0,#0		; Clear A and leave flags
	LD	R8,#hi(CTLOFG)
	LD	R9,#lo(CTLOFG)
	LDC	@RR8,R0		; Enable output
	JP	NZ,NOPMPT	; No prompt - get input
	CALL	QTSTR		; Get string terminated by '"'
	CALL	CHKSYN		; Check for ';' after prompt
	DB	';'
	PUSH	R3
	PUSH	R2		; Save code string address
	CALL	PRS1		; Output prompt string
	JR	NOPMPT1		; Skip "PUSH HL"
NOPMPT:
	PUSH	R3
	PUSH	R2		; Save code string address
NOPMPT1:
	CALL	PROMPT		; Get input with "? " prompt
	POP	R4
	POP	R5		; Restore code string address
	JP	C,INPBRK	; Break pressed - Exit
	INCW	RR2		; Next byte
	LDC	R0,@RR2		; Get it
	DECW	RR2		; Back again
	OR	R0,R0		; End of line?
	PUSH	R5
	PUSH	R4		; Re-save code string address
	JP	Z,NXTDTA	; Yes - Find next DATA stmt
	LD	R1,#','
	LDC	@RR2,R1		; Store comma as separator
	JP	NXTITM		; Get next item
;
READ:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(NXTDAT)
	LD	R9,#lo(NXTDAT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Next DATA statement
	OR	R0,#0AFH	; Flag "READ"
	JR	NXTITM1
NXTITM:
	XOR	R0,R0		; Flag "INPUT"
NXTITM1:
	LD	R8,#hi(READFG)
	LD	R9,#lo(READFG)
	LDC	@RR8,R0		; Save "READ"/"INPUT" flag
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Get code str' , Save pointer
	JP	GTVLUS		; Get values
;
NEDMOR:
	CALL	CHKSYN		; Check for comma between items
	DB	','
GTVLUS:
	CALL	GETVAR		; Get variable name
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save code str" , Get pointer
	PUSH	R7
	PUSH	R6		; Save variable address
	LDC	R0,@RR2		; Get next "INPUT"/"DATA" byte
	CP	R0,#','		; Comma?
	JP	Z,ANTVLU	; Yes - Get another value
	LD	R8,#hi(READFG)
	LD	R9,#lo(READFG)
	LDC	R0,@RR8		; Is it READ?
	OR	R0,R0
	RCF
	JP	NZ,FDTLP	; Yes - Find next DATA stmt
	LD	R0,#'?'		; More INPUT needed
	CALL	OUTC		; Output character
	CALL	PROMPT		; Get INPUT with prompt
	POP	R6
	POP	R7		; Variable address
	POP	R4
	POP	R5		; Code string address
	JP	C,INPBRK	; Break pressed
	INCW	RR2		; Point to next DATA byte
	LDC	R0,@RR2		; Get byte
	DECW	RR2		; Back space INPUT pointer
	OR	R0,R0		; Is it zero (No input) ?
	PUSH	R5
	PUSH	R4		; Save code string address
	JP	Z,NXTDTA	; Find end of buffer
	PUSH	R7
	PUSH	R6		; Save variable address
ANTVLU:
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Check data type
	OR	R0,R0		; Is it numeric?
	JP	Z,INPBIN	; Yes - Convert to binary
	CALL	GETCHR		; Get next character
	LD	R6,R0		; Save input character
	LD	R4,R0		; Again
	CP	R0,#'"'		; Start of literal sting?
	JP	Z,STRENT	; Yes - Create string entry
	LD	R8,#hi(READFG)
	LD	R9,#lo(READFG)
	LDC	R0,@RR8		; "READ" or "INPUT" ?
	OR	R0,R0
	LD	R6,R0		; Save 00 if "INPUT"
	JP	Z,ITMSEP	; "INPUT" - End with 00
	LD	R6,#':'		; "DATA" - End with 00 or ':'
ITMSEP:
	LD	R4,#','		; Item separator
	DECW	RR2		; Back space for DTSTR
STRENT:
	CALL	DTSTR		; Get string terminated by D
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; String address to DE
	LD	R2,#hi(LTSTND)
	LD	R3,#lo(LTSTND)	; Where to go after LETSTR
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save HL , get input pointer
	PUSH	R7
	PUSH	R6		; Save address of string
	JP	LETSTR		; Assign string to variable
;
INPBIN:
	CALL	GETCHR		; Get next character
	CALL	ASCTFP		; Convert ASCII to FP number
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save input ptr, Get var addr
	CALL	FPTHL		; Move FPREG to variable
	POP	R2
	POP	R3		; Restore input pointer
LTSTND:
	DECW	RR2		; DEC 'cos GETCHR INCs
	CALL	GETCHR		; Get next character
	JP	Z,MORDT		; End of line - More needed?
	CP	R0,#','		; Another value?
	JP	NZ,BADINP	; No - Bad input
MORDT:
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Get code string address
	DECW	RR2		; DEC 'cos GETCHR INCs
	CALL	GETCHR		; Get next character
	JP	NZ,NEDMOR	; More needed - Get it
	POP	R6
	POP	R7		; Restore DATA pointer
	LD	R8,#hi(READFG)
	LD	R9,#lo(READFG)
	LDC	R0,@RR8		; "READ" or "INPUT" ?
	OR	R0,R0
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; DATA pointer to HL
	JP	NZ,UPDATA	; Update DATA pointer if "READ"
	PUSH	R7
	PUSH	R6		; Save code string address
	LDC	R1,@RR2
	OR	R0,R1		; More input given?
	LD	R2,#hi(EXTIG)
	LD	R3,#lo(EXTIG)	; "?Extra ignored" message
	JR	Z,$+5
	CALL	PRS		; Output string if extra given
	POP	R2
	POP	R3		; Restore code string address
	RET
;
EXTIG:
	DB	"?Extra ignored",CR,LF,0
;
FDTLP:
	CALL	DATA		; Get next statement
	OR	R0,R0		; End of line?
	JP	NZ,FANDT	; No - See if DATA statement
	INCW	RR2
	LDC	R0,@RR2		; End of program?
	INCW	RR2
	LDC	R1,@RR2
	OR	R0,R1		; 00 00 Ends program
	LD	R7,#OD		; ?OD Error
	JP	Z,ERROR		; Yes - Out of DATA
	INCW	RR2
	LDC	R7,@RR2		; LSB of line number
	INCW	RR2
	LDC	R6,@RR2		; MSB of line number
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	LD	R8,#hi(DATLIN)
	LD	R9,#lo(DATLIN)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set line of current DATA item
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
FANDT:
	CALL	GETCHR		; Get next character
	CP	R0,#ZDATA	; "DATA" token
	JP	NZ,FDTLP	; No "DATA" - Keep looking
	JP	ANTVLU		; Found - Convert input
;
NEXT:
	LD	R6,#hi(0)
	LD	R7,#lo(0)	; In case no index given
NEXT1:
	JR	Z,$+5
	CALL	GETVAR		; Get index address
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save code string address
	CALL	BAKSTK		; Look for "FOR" block
	JP	NZ,NFERR	; No "FOR" - ?NF Error
	LD	SPH,R2		; Clear nested loops
	LD	SPL,R3

	LD	R8,R6		; @@@ SWAP DE
	LD	R6,R7
	LD	R7,R8

	PUSH	R7
	PUSH	R6		; Save index address
	LDC	R0,@RR2		; Get sign of STEP
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save sign of STEP
	PUSH	R7
	PUSH	R6		; Save index address
	CALL	PHLTFP		; Move index value to FPREG
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save address of TO value

	LD	R8,R2		; @@@ SWAP HL
	LD	R2,R3
	LD	R3,R8

	PUSH	R3
	PUSH	R2		; Save address of index
	CALL	ADDPHL		; Add STEP to index value
	POP	R2
	POP	R3		; Restore address of index
	CALL	FPTHL		; Move value to index variable
	POP	R2
	POP	R3		; Restore address of TO value
	CALL	LOADFP		; Move TO value to BCDE
	PUSH	R3
	PUSH	R2		; Save address of line of FOR
	CALL	CMPNUM		; Compare index with TO value
	POP	R2
	POP	R3		; Restore address of line num
	POP	R4
	POP	R5		; Address of sign of STEP
	SUB	R0,R4		; Compare with expected sign
	CALL	LOADFP		; BC = Loop stmt,DE = Line num


	LD	R8,R4		; @@@ SWAP BC
	LD	R4,R5
	LD	R5,R8
	LD	R8,R6		; @@@ SWAP DE
	LD	R6,R7
	LD	R7,R8

	JP	Z,KILFOR	; Loop finished - Terminate it
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Loop statement line number
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set loop line number
	LD	R3,R5		; Set code string to loop
	LD	R2,R4
	JP	PUTFID		; Put back "FOR" and continue
;
KILFOR:
	LD	SPH,R2
	LD	SPL,R3		; Remove "FOR" block
	LD	R8,#hi(BRKLIN)
	LD	R9,#lo(BRKLIN)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Code string after "NEXT"
	LDC	R0,@RR2		; Get next byte in code string
	CP	R0,#','		; More NEXTs ?
	JP	NZ,RUNCNT	; No - Do next statement
	CALL	GETCHR		; Position to index name
	CALL	NEXT1		; Re-enter NEXT routine
; < will not RETurn to here , Exit to RUNCNT or Loop >
;
GETNUM:
	CALL	EVAL		; Get a numeric expression
TSTNUM:
	OR	R0,R0
	RCF			; Clear carry (numeric)
	JR	CHKTYP
TSTSTR:
	SCF			; Set carry (string)
CHKTYP:
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Check types match
	ADC	R0,R0		; Expected + actual

	LD	R1,R0		; @@@
	RRC	R1
	RRC	R1
	RRC	R1
	RRC	R1
	XOR	R1,R0
	LD	R8,R1
	RRC	R1
	RRC	R1
	XOR	R1,R8
	LD	R8,R1
	RRC	R1
	XOR	R1,R8
	AND	R1,#01H
	JR	NZ,CHKTYP1
	OR	R0,R0		; Clear carry , set parity
	RCF
	RET
; RET	PE			; Even parity - Types match
CHKTYP1:
	JP	TMERR		; Different types - Error
;
OPNPAR:
	CALL	CHKSYN		; Make sure "(" follows
	DB	"("
EVAL:
	DECW	RR2		; Evaluate expression & save
	LD	R6,#0		; Precedence value
EVAL1:
	PUSH	R7
	PUSH	R6		; Save precedence
	LD	R5,#1
	CALL	CHKSTK		; Check for 1 level of stack
	CALL	OPRND		; Get next expression value
EVAL2:
	LD	R8,#hi(NXTOPR)
	LD	R9,#lo(NXTOPR)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save address of next operator
EVAL3:
	LD	R8,#hi(NXTOPR)
	LD	R9,#lo(NXTOPR)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Restore address of next opr
	POP	R4
	POP	R5		; Precedence value and operator
	LD	R0,R4		; Get precedence value
	CP	R0,#78H		; "AND" or "OR" ?
	JR	C,$+5
	CALL	TSTNUM		; No - Make sure it's a number
	LDC	R0,@RR2		; Get next operator / function
	LD	R6,#0		; Clear Last relation
RLTLP:
	SUB	R0,#ZGTR	; ">" Token
	JP	C,FOPRND	; + - * / ^ AND OR - Test it
	CP	R0,#ZLTH+1-ZGTR	; < = >
	JP	NC,FOPRND	; Function - Call it
	CP	R0,#ZEQUAL-ZGTR	; "="
	RLC	R0		; <- Test for legal
	XOR	R0,R6		; <- combinations of < = >
	CP	R0,R6		; <- by combining last token
	LD	R6,R0		; <- with current one
	JP	C,SNERR		; Error if "<<' '==" or ">>"
	LD	R8,#hi(CUROPR)
	LD	R9,#lo(CUROPR)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save address of current token
	CALL	GETCHR		; Get next character
	JP	RLTLP		; Treat the two as one
;
FOPRND:
	LD	R0,R6		; < = > found ?
	OR	R0,R0
	JP	NZ,TSTRED	; Yes - Test for reduction
	LDC	R0,@RR2		; Get operator token
	LD	R8,#hi(CUROPR)
	LD	R9,#lo(CUROPR)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save operator address
	SUB	R0,#ZPLUS	; Operator or function?
	JR	NC,$+3
	RET			; Neither - Exit
	CP	R0,#ZOR+1-ZPLUS	; Is it + - * / ^ AND OR ?
	JR	C,$+3
	RET			; No - Exit
	LD	R7,R0		; Coded operator
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Get data type
	DEC	R0		; FF = numeric , 00 = string
	OR	R0,R7
	RCF			; Combine with coded operator
	LD	R0,R7		; Get coded operator
	JP	Z,CONCAT	; String concatenation
	RL	R0		; Times 2
	ADD	R0,R7		; Times 3
	LD	R7,R0		; To DE (D is 0)
	LD	R2,#hi(PRITAB)
	LD	R3,#lo(PRITAB)	; Precedence table
	ADD	R3,R7
	ADC	R2,R6		; To the operator concerned
	LD	R0,R4		; Last operator precedence
	LDC	R6,@RR2		; Get evaluation precedence
	CP	R0,R6		; Compare with eval precedence
	JR	C,$+3
	RET			; Exit if higher precedence
	INCW	RR2		; Point to routine address
	CALL	TSTNUM		; Make sure it's a number
;
STKTHS:
	PUSH	R5
	PUSH	R4		; Save last precedence & token
	LD	R4,#hi(EVAL3)
	LD	R5,#lo(EVAL3)	; Where to go on prec' break
	PUSH	R5
	PUSH	R4		; Save on stack for return
	LD	R4,R7		; Save operator
	LD	R5,R6		; Save precedence
	CALL	STAKFP		; Move value to stack
	LD	R7,R4		; Restore operator
	LD	R6,R5		; Restore precedence

;	LDC	R5,@RR2		; Get LSB of routine address
	LDC	R4,@RR2		; Get MSB of routine address

	INCW	RR2

;	LDC	R4,@RR2		; Get MSB of routine address
	LDC	R5,@RR2		; Get LSB of routine address

	INCW	RR2
	PUSH	R5
	PUSH	R4		; Save routine address
	LD	R8,#hi(CUROPR)
	LD	R9,#lo(CUROPR)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Address of current operator
	JP	EVAL1		; Loop until prec' break
;
OPRND:
	XOR	R0,R0		; Get operand routine
	RCF
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	@RR8,R0		; Set numeric expected
	CALL	GETCHR		; Get next character
	LD	R7,#MO		; ?MO Error
	JP	Z,ERROR		; No operand - Error
	JP	C,ASCTFP	; Number - Get value
	CALL	CHKLTR		; See if a letter
	JR	NC,CONVAR	; Letter - Find variable
	CP	R0,#'&'		; &H = HEX, &B = BINARY
	JR	NZ,NOTAMP
	CALL	GETCHR		; Get next character
	CP	R0,#'H'		; Hex number indicated? [function added]
	JP	Z,HEXTFP	; Convert Hex to FPREG
	CP	R0,#'B'		; Binary number indicated? [function added]
	JP	Z,BINTFP	; Convert Bin to FPREG
	LD	R7,#SN		; If neither then a ?SN Error
	JP	Z,ERROR
NOTAMP:
	CP	R0,#ZPLUS	; '+' Token ?
	JR	Z,OPRND		; Yes - Look for operand
	CP	R0,#'.'		; '.' ?
	JP	Z,ASCTFP	; Yes - Create FP number
	CP	R0,#ZMINUS	; '-' Token ?
	JR	Z,MINUS		; Yes - Do minus
	CP	R0,#'"'		; Literal string ?
	JP	Z,QTSTR		; Get string terminated by '"'
	CP	R0,#ZNOT	; "NOT" Token ?
	JP	Z,EVNOT		; Yes - Eval NOT expression
	CP	R0,#ZFN		; "FN" Token ?
	JP	Z,DOFN		; Yes - Do FN routine
	SUB	R0,#ZSGN	; Is it a function?
	JR	NC,FNOFST	; Yes - Evaluate function
EVLPAR:
	CALL	OPNPAR		; Evaluate expression in "()"
	CALL	CHKSYN		; Make sure ")" follows
	DB	")"
	RET
;
MINUS:
	LD	R6,#7DH		; '-' precedence
	CALL	EVAL1		; Evaluate until prec' break
	LD	R8,#hi(NXTOPR)
	LD	R9,#lo(NXTOPR)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get next operator address
	PUSH	R3
	PUSH	R2		; Save next operator address
	CALL	INVSGN		; Negate value
RETNUM:
	CALL	TSTNUM		; Make sure it's a number
	POP	R2
	POP	R3		; Restore next operator address
	RET
;
CONVAR:
	CALL	GETVAR		; Get variable address to DE
FRMEVL:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Variable address to HL
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	@RR8,R2
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	@RR8,R3		; Save address of variable
	LDC	@RR8,R3		; @@@ SWAP HL
	INCW	RR8
	LDC	@RR8,R2		; Save address of variable

	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Get type
	OR	R0,R0
	RCF			; Numeric?
	JR	NZ,$+5
	CALL	PHLTFP		; Yes - Move contents to FPREG
	POP	R2
	POP	R3		; Restore code string address
	RET
;
FNOFST:
	LD	R4,#0		; Get address of function
	RL	R0		; Double function offset
	LD	R5,R0		; BC = Offset in function table
	PUSH	R5
	PUSH	R4		; Save adjusted token value
	CALL	GETCHR		; Get next character
	LD	R0,R5		; Get adjusted token value
	CP	R0,#2*(ZLEFT-ZSGN)-1; Adj' LEFT$,RIGHT$ or MID$ ?
	JP	C,FNVAL		; No - Do function
	CALL	OPNPAR		; Evaluate expression	(X,...
	CALL	CHKSYN		; Make sure ',' follows
	DB	','
	CALL	TSTSTR		; Make sure it's a string
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Save code string address
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; Get address of string
	LDC	R3,@RR8		; @@@ SWAP HL
	INCW	RR8
	LDC	R2,@RR8		; Get address of string

	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save address of string
	PUSH	R3
	PUSH	R2		; Save adjusted token value
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Restore code string address
	CALL	GETINT		; Get integer 0-255
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Save code string address
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save integer,HL = adj' token
	JP	GOFUNC		; Jump to string function
;
FNVAL:
	CALL	EVLPAR		; Evaluate expression
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; HL = Adjusted token value
	LD	R6,#hi(RETNUM)
	LD	R7,#lo(RETNUM)	; Return number from function
	PUSH	R7
	PUSH	R6		; Save on stack
GOFUNC:
	LD	R4,#hi(FNCTAB)
	LD	R5,#lo(FNCTAB)	; Function routine addresses
	ADD	R3,R5
	ADC	R2,R4		; Point to right address

;	LDC	R5,@RR2		; Get LSB of address
	LDC	R4,@RR2		; Get LSB of address

	INCW	RR2

;	LDC	R2,@RR2		; Get MSB of address
	LDC	R3,@RR2		; Get MSB of address
;	LD	R3,R5		; Address to HL
	LD	R2,R4		; Address to HL
	JP	@RR2		; Jump to function
;
SGNEXP:
	DEC	R6		; Dee to flag negative exponent
	CP	R0,#ZMINUS	; '-' token ?
	JR	NZ,$+3
	RET			; Yes - Return
	CP	R0,#'-'		; '-' ASCII ?
	JR	NZ,$+3
	RET			; Yes - Return
	INC	R6		; Inc to flag positive exponent
	CP	R0,#'+'		; '+' ASCII ?
	JR	NZ,$+3
	RET			; Yes - Return
	CP	R0,#ZPLUS	; '+' token ?
	JR	NZ,$+3
	RET			; Yes - Return
;	LD	R1,FLAGS
	DECW	RR2		; DEC 'cos GETCHR INCs
;	LD	FLAGS,R1
	RET			; Return "NZ"
;
POR:
	OR	R0,#0AFH	; Flag "OR"
	RCF
	JR	PAND1
PAND:
	XOR	R0,R0		; Flag "AND"
	RCF
PAND1:
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save "AND" / "OR" flag
	CALL	TSTNUM		; Make sure it's a number
	CALL	DEINT		; Get integer -32768 to 32767
	POP	R0		; Restore "AND" / "OR" flag
	POP	R1
	LD	FLAGS,R1
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; <- Get last
	POP	R4
	POP	R5		; <- value
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; <- from
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; <- stack
	CALL	FPBCDE		; Move last value to FPREG
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save "AND" / "OR" flag
	CALL	DEINT		; Get integer -32768 to 32767
	POP	R0		; Restore "AND" / "OR" flag
	POP	R1
	LD	FLAGS,R1
	POP	R4
	POP	R5		; Get value
	LD	R0,R5		; Get LSB
	LD	R2,#hi(ACPASS)	; Address of save AC as current
	LD	R3,#lo(ACPASS)
	JP	NZ,POR1		; Jump if OR
	AND	R0,R7		; "AND" LSBs
	LD	R5,R0		; Save LSB
	LD	R0,R4		; Get MBS
	AND	R0,R6		; "AND" MSBs
	JP	@RR2		; Save AC as current (ACPASS)
;
POR1:
	OR	R0,R7		; "OR" LSBs
	LD	R5,R0		; Save LSB
	LD	R0,R4		; Get MSB
	OR	R0,R6		; "OR" MSBs
	RCF
	JP	@RR2		; Save AC as current (ACPASS)
;
TSTRED:
	LD	R2,#hi(CMPLOG)	; Logical compare routine
	LD	R3,#lo(CMPLOG)
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Get data type
	RRC	R0		; Carry set = string
	LD	R0,R6		; Get last precedence value
	RLC	R0		; Times 2 plus carry
	LD	R7,R0		; To E
	LD	R6,#64H		; Relational precedence
	LD	R0,R4		; Get current precedence
	CP	R0,R6		; Compare with last
	JR	C,$+3
	RET			; Eval if last was rel' or log'
	JP	STKTHS		; Stack this one and get next
;
CMPLOG:
	DW	CMPLG1		; Compare two values / strings
CMPLG1:
	LD	R0,R5		; Get data type
	OR	R0,R0
	RCF
	RRC	R0
	POP	R4
	POP	R5		; Get last expression to BCDE
	POP	R6
	POP	R7
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save status
	CALL	CHKTYP		; Check that types match
	LD	R2,#hi(CMPRES)	; Result to comparison
	LD	R3,#lo(CMPRES)
	PUSH	R3
	PUSH	R2		; Save for RETurn
	JP	Z,CMPNUM	; Compare values if numeric
	XOR	R0,R0		; Compare two strings
	RCF
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	@RR8,R0		; Set type to numeric
	PUSH	R7
	PUSH	R6		; Save string name
	CALL	GSTRCU		; Get current string
	LDC	R0,@RR2		; Get length of string
	INCW	RR2
	INCW	RR2
	LDC	R5,@RR2		; Get LSB of address
	INCW	RR2
	LDC	R4,@RR2		; Get MSB of address
	POP	R6
	POP	R7		; Restore string name
	PUSH	R5
	PUSH	R4		; Save address of string
;	LD	R1,FLAGS
;	PUSH	R1
	PUSH	R0		; Save length of string
	CALL	GSTRDE		; Get second string
	CALL	LOADFP		; Get address of second string
	POP	R0		; Restore length of string 1
;	POP	R1
;	LD	FLAGS,R1
	LD	R6,R0		; Length to D
	POP	R2
	POP	R3		; Restore address of string 1
CMPSTR:
	LD	R0,R7		; Bytes of string 2 to do
	OR	R0,R6
	RCF			; Bytes of string 1 to do
	JR	NZ,$+3
	RET			; Exit if all bytes compared
	LD	R0,R6		; Get bytes of string 1 to do
	SUB	R0,#1
	JR	NC,$+3
	RET			; Exit if end of string 1
	XOR	R0,R0
	RCF
	CP	R0,R7		; Bytes of string 2 to do
	INC	R0
	JR	C,$+3
	RET			; Exit if end of string 2
	DEC	R6		; Count bytes in string 1
	DEC	R7		; Count bytes in string 2
	LDC	R0,@RR4		; Byte in string 2
	LDC	R8,@RR2
	INCW	RR2		; Move up string 1
	INCW	RR4		; Move up string 2
	CP	R0,R8		; Compare to byte in string 1
	JP	Z,CMPSTR	; Same - Try next bytes
	CCF			; Flag difference (">" or "<")
	JP	FLGDIF		; "<" gives -1 , ">" gives +1
;
CMPRES:
	INC	R0		; Increment current value
	ADC	R0,R0		; Double plus carry
	POP	R4
	POP	R5		; Get other value
	AND	R0,R4		; Combine them
	ADD	R0,#-1		; Carry set if different
	SBC	R0,R0		; 00 - Equal , FF - Different
	JP	FLGREL		; Set current value & continue
;
EVNOT:
	LD	R6,#5AH		; Precedence value for "NOT"
	CALL	EVAL1		; Eval until precedence break
	CALL	TSTNUM		; Make sure it's a number
	CALL	DEINT		; Get integer -32768 - 32767
	LD	R0,R7		; Get LSB
	COM	R0		; Invert LSB
	LD	R5,R0		; Save "NOT" of LSB
	LD	R0,R6		; Get MSB
	COM	R0		; Invert MSB
	CALL	ACPASS		; Save AC as current
	POP	R4
	POP	R5		; Clean up stack
	JP	EVAL3		; Continue evaluation
;
DIMRET:
	DECW	RR2		; DEC 'cos GETCHR INCs
	CALL	GETCHR		; Get next character
	JR	NZ,$+3
	RET			; End of DIM statement
	CALL	CHKSYN		; Make sure ',' follows
	DB	','
DIM:
	LD	R4,#hi(DIMRET)
	LD	R5,#lo(DIMRET)	; Return to "DIMRET"
	PUSH	R5
	PUSH	R4		; Save on stack
	OR	R0,#0AFH
	RCF			; Flag "Create" variable
	JR	GETVAR1
GETVAR:
	XOR	R0,R0
	RCF			; Find variable address,to DE
GETVAR1:
	LD	R8,#hi(LCRFLG)
	LD	R9,#lo(LCRFLG)
	LDC	@RR8,R0		; Set locate / create flag
	LDC	R4,@RR2		; Get First byte of name
GTFNAM:
	CALL	CHKLTR		; See if a letter
	JP	C,SNERR		; ?SN Error if not a letter
	XOR	R0,R0
	RCF
	LD	R5,R0		; Clear second byte of name
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	@RR8,R0		; Set type to numeric
	CALL	GETCHR		; Get next character
	JP	C,SVNAM2	; Numeric - Save in name
	CALL	CHKLTR		; See if a letter
	JP	C,CHARTY	; Not a letter - Check type
SVNAM2:
	LD	R5,R0		; Save second byte of name
ENDNAM:
	CALL	GETCHR		; Get next character
	JP	C,ENDNAM	; Numeric - Get another
	CALL	CHKLTR		; See if a letter
	JP	NC,ENDNAM	; Letter - Get another
CHARTY:
	SUB	R0,#'$'		; String variable?
	JP	NZ,NOTSTR	; No - Numeric variable
	INC	R0		; A = 1 (string type)
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	@RR8,R0		; Set type to string
	RR	R0		; A = 80H , Flag for string
	ADD	R0,R5		; 2nd byte of name has bit 7 on
	LD	R5,R0		; Resave second byte on name
	CALL	GETCHR		; Get next character
NOTSTR:
	LD	R8,#hi(FORFLG)
	LD	R9,#lo(FORFLG)
	LDC	R0,@RR8		; Array name needed ?
	DEC	R0
	JP	Z,ARLDSV	; Yes - Get array name
	JP	PL,NSCFOR	; No array with "FOR" or "FN"
	LDC	R0,@RR2		; Get byte again
	SUB	R0,#'('		; Subscripted variable?
	JP	Z,SBSCPT	; Yes - Sort out subscript
;
NSCFOR:
	XOR	R0,R0
	RCF			; Simple variable
	LD	R8,#hi(FORFLG)
	LD	R9,#lo(FORFLG)
	LDC	@RR8,R0		; Clear "FOR" flag
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R6,R4		; DE = Variable name to find
	LD	R7,R5
	LD	R8,#hi(FNRGNM)
	LD	R9,#lo(FNRGNM)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; FN argument name
	CALL	CPDEHL		; Is it the FN argument?
	LD	R6,#hi(FNARG)
	LD	R7,#lo(FNARG)	; Point to argument value
	JP	Z,POPHRT	; Yes - Return FN argument value
	LD	R8,#hi(VAREND)
	LD	R9,#lo(VAREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; End of variables
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Address of end of search
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Start of variables address
FNDVAR:
	CALL	CPDEHL		; End of variable list table?
	JP	Z,CFEVAL	; Yes - Called from EVAL?
	LD	R0,R5		; Get second byte of name
	LDC	R8,@RR2
	INCW	RR2		; Move on to first byte
	SUB	R0,R8		; Compare with name in list
	JP	NZ,FNTHR	; Different - Find another
	LD	R0,R4		; Get first byte of name
	LDC	R8,@RR2
	SUB	R0,R8		; Compare with name in list
FNTHR:
	LD	R1,FLAGS
	INCW	RR2		; Move on to LSB of value
	LD	FLAGS,R1
	JP	Z,RETADR	; Found - Return address
	INCW	RR2		; <- Skip
	INCW	RR2		; <- over
	INCW	RR2		; <- F.P.
	INCW	RR2		; <- value
	JP	FNDVAR		; Keep looking
;
CFEVAL:
	POP	R2
	POP	R3		; Restore code string address
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Get return address
	PUSH	R7
	PUSH	R6		; Save address of variable
	LD	R6,#hi(FRMEVL)
	LD	R7,#lo(FRMEVL)	; Return address in EVAL
	CALL	CPDEHL		; Called from EVAL ?
	POP	R6
	POP	R7		; Restore address of variable
	JP	Z,RETNUL	; Yes - Return null variable
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Put back return
	PUSH	R3
	PUSH	R2		; Save code string address
	PUSH	R5
	PUSH	R4		; Save variable name
	LD	R4,#hi(6)
	LD	R5,#lo(6)	; 2 byte name plus 4 byte data
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; End of arrays
	PUSH	R3
	PUSH	R2		; Save end of arrays
	ADD	R3,R5
	ADC	R2,R4		; Move up 6 bytes
	POP	R4
	POP	R5		; Source address in BC
	PUSH	R3
	PUSH	R2		; Save new end address
	CALL	MOVUP		; Move arrays up
	POP	R2
	POP	R3		; Restore new end address
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set new end address
	LD	R2,R4		; End of variables to HL
	LD	R3,R5
	LD	R8,#hi(VAREND)
	LD	R9,#lo(VAREND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set new end address
;
ZEROLP:
	DECW	RR2		; Back through to zero variable
	LD	R1,#0
	LDC	@RR2,R1		; Zero byte in variable
	CALL	CPDEHL		; Done them all?
	JP	NZ,ZEROLP	; No - Keep on going
	POP	R6
	POP	R7		; Get variable name
	LDC	@RR2,R7		; Store second character
;	LD	R1,FLAGS
	INCW	RR2
	LDC	@RR2,R6		; Store first character
	INCW	RR2
;	LD	FLAGS,R1
RETADR:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Address of variable in DE
	POP	R2
	POP	R3		; Restore code string address
	RET
;
RETNUL:
	LD	R8,#hi(FPEXP)
	LD	R9,#lo(FPEXP)
	LDC	@RR8,R0		; Set result to zero
	LD	R2,#hi(ZERBYT)
	LD	R3,#lo(ZERBYT)	; Also set a null string
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	@RR8,R2
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	@RR8,R3		; Save for EVAL
	LDC	@RR8,R3		; @@@ SWAP HL
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R2		; Save for EVAL

	POP	R2
	POP	R3		; Restore code string address
	RET
;
SBSCPT:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(LCRFLG)
	LD	R9,#lo(LCRFLG)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Locate/Create and Type
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save and get code string
	LD	R6,R0		; Zero number of dimensions
SCPTLP:
;	PUSH	R7
	PUSH	R6		; Save number of dimensions
	PUSH	R5
	PUSH	R4		; Save array name
	CALL	FPSINT		; Get subscript (0-32767)
	POP	R4
	POP	R5		; Restore array name
	POP	R0		; Get number of dimensions
;	POP	R1
;	LD	FLAGS,R1
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save subscript value
	PUSH	R3
	PUSH	R2		; Save LCRFLG and TYPE
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	INC	R0		; Count dimensions
	LD	R6,R0		; Save in D
	LDC	R0,@RR2		; Get next byte in code string
	CP	R0,#','		; Comma (more to come)?
	JP	Z,SCPTLP	; Yes - More subscripts
	CALL	CHKSYN		; Make sure ")" follows
	DB	")"
	LD	R8,#hi(NXTOPR)
	LD	R9,#lo(NXTOPR)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save code string address
	POP	R2
	POP	R3		; Get LCRFLG and TYPE
	LD	R8,#hi(LCRFLG)
	LD	R9,#lo(LCRFLG)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Restore Locate/create & type
	LD	R7,#0		; Flag not CSAVE* or CLOAD*
	PUSH	R7
	PUSH	R6		; Save number of dimensions (D)
	JR	ARLDSV1		; Skip "PUSH HL" and "PUSH AF'
;
ARLDSV:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; A = 00 , Flags set = Z,N
ARLDSV1:
	LD	R8,#hi(VAREND)
	LD	R9,#lo(VAREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Start of arrays
	JR	FNDARY1		; Skip "ADD HL,DE"
FNDARY:
	ADD	R3,R7
	ADC	R2,R6		; Move to next array start
FNDARY1:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; End of arrays
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Current array pointer
	CALL	CPDEHL		; End of arrays found?
	JP	Z,CREARY	; Yes - Create array
	LDC	R0,@RR2		; Get second byte of name
	INCW	RR2		; Move on
	CP	R0,R5		; Compare with name given
	JP	NZ,NXTARY	; Different - Find next array
	LDC	R0,@RR2		; Get first byte of name
	CP	R0,R4		; Compare with name given
NXTARY:
	LD	R1,FLAGS
	INCW	RR2		; Move on
	LDC	R7,@RR2		; Get LSB of next array address
	INCW	RR2
	LDC	R6,@RR2		; Get MSB of next array address
	INCW	RR2
	LD	FLAGS,R1
	JP	NZ,FNDARY	; Not found - Keep looking
	LD	R8,#hi(LCRFLG)
	LD	R9,#lo(LCRFLG)
	LDC	R0,@RR8		; Found Locate or Create it?
	OR	R0,R0
	RCF
	JP	NZ,DDERR	; Create - ?DD Error
	POP	R0		; Locate - Get number of dim'ns
	POP	R1
	LD	FLAGS,R1
	LD	R4,R2		; BC Points to array dim'ns
	LD	R5,R3
	JP	Z,POPHRT	; Jump if array load/save
	LDC	R8,@RR2
	SUB	R0,R8		; Same number of dimensions?
	JP	Z,FINDEL	; Yes - Find element
BSERR:
	LD	R7,#BS		; ?BS Error
	JP	ERROR		; Output error
;
CREARY:
	LD	R6,#hi(4)
	LD	R7,#lo(4)	; 4 Bytes per entry
	POP	R0		; Array to save or 0 dim'ns?
	POP	R1
	LD	FLAGS,R1
	JP	Z,FCERR		; Yes - ?FC Error
	LDC	@RR2,R5		; Save second byte of name
	INCW	RR2
	LDC	@RR2,R4		; Save first byte of name
	INCW	RR2
	LD	R5,R0		; Number of dimensions to C
	CALL	CHKSTK		; Check if enough memory
	INCW	RR2		; Point to number of dimensions
	INCW	RR2
	LD	R8,#hi(CUROPR)
	LD	R9,#lo(CUROPR)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save address of pointer
	LDC	@RR2,R5		; Set number of dimensions
	INCW	RR2
	LD	R8,#hi(LCRFLG)
	LD	R9,#lo(LCRFLG)
	LDC	R0,@RR8		; Locate of Create?
	RLC	R0		; Carry set = Create
	LD	R0,R5		; Get number of dimensions
CRARLP:
	LD	R4,#hi(10+1)
	LD	R5,#lo(10+1)	; Default dimension size 10
	JP	NC,DEFSIZ	; Locate - Set default size
	POP	R4
	POP	R5		; Get specified dimension size
	INCW	RR4		; Include zero element
DEFSIZ:
	LDC	@RR2,R5		; Save LSB of dimension size
	INCW	RR2
	LDC	@RR2,R4		; Save MSB of dimension size
	INCW	RR2
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save num' of dim'ns an status
	PUSH	R3
	PUSH	R2		; Save address of dim'n size
	CALL	MLDEBC		; Multiply DE by BC to find
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; amount of mem needed (to DE)
	POP	R2
	POP	R3		; Restore address of dimension
	POP	R0		; Restore number of dimensions
	POP	R1
	LD	FLAGS,R1
	DEC	R0		; Count them
	JP	NZ,CRARLP	; Do next dimension if more
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save locate/create flag
	LD	R4,R6		; MSB of memory needed
	LD	R5,R7		; LSB of memory needed
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	ADD	R3,R7
	ADC	R2,R6		; Add bytes to array start
	JP	C,OMERR		; Too big - Error
	CALL	ENFMEM		; See if enough memory
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save new end of array
;
ZERARY:
	DECW	RR2		; Back through array data
	LD	R1,#0
	LDC	@RR2,R1		; Set array element to zero
	CALL	CPDEHL		; All elements zeroed?
	JP	NZ,ZERARY	; No - Keep on going
	INCW	RR4		; Number of bytes + 1
	LD	R6,R0		; A=0
	LD	R8,#hi(CUROPR)
	LD	R9,#lo(CUROPR)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get address of array
	LDC	R7,@RR2		; Number of dimensions
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; To HL
	ADD	R3,R3
	ADC	R2,R2		; Two bytes per dimension size
	ADD	R3,R5
	ADC	R2,R4		; Add number of bytes
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Bytes needed to DE
	LD	R1,FLAGS
	DECW	RR2
	DECW	RR2
	LD	FLAGS,R1
	LDC	@RR2,R7		; Save LSB of bytes needed
	INCW	RR2
	LDC	@RR2,R6		; Save MSB of bytes needed
	INCW	RR2
	LD	FLAGS,R1
	POP	R0
	POP	R1		; Locate / Create?
	LD	FLAGS,R1
	JP	C,ENDDIM	; A is 0 , End if create
FINDEL:
	LD	R4,R0		; Find array element
	LD	R5,R0
	LDC	R0,@RR2		; Number of dimensions
	INCW	RR2
	JR	FNDELP1		; Skip "POP HL"
FNDELP:
	POP	R2
	POP	R3		; Address of next dim' size
FNDELP1:
	LDC	R7,@RR2		; Get LSB of dim'n size
	INCW	RR2
	LDC	R6,@RR2		; Get MSB of dim'n size
	INCW	RR2
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save address - Get index
;	LD	R1,FLAGS
;	PUSH	R1
	PUSH	R0		; Save number of dim'ns
	CALL	CPDEHL		; Dimension too large?
	JP	NC,BSERR	; Yes - ?BS Error
	PUSH	R3
	PUSH	R2		; Save index
	CALL	MLDEBC		; Multiply previous by size
	POP	R6
	POP	R7		; Index supplied to DE
	ADD	R3,R7
	ADC	R2,R6		; Add index to pointer
	POP	R0		; Number of dimensions
;	POP	R1
;	LD	FLAGS,R1
	DEC	R0		; Count them
	LD	R4,R2		; MSB of pointer
	LD	R5,R3		; LSB of pointer
	JP	NZ,FNDELP	; More - Keep going
	ADD	R3,R3
	ADC	R2,R2		; 4 Bytes per element
	ADD	R3,R3
	ADC	R2,R2
	POP	R4
	POP	R5		; Start of array
	ADD	R3,R5
	ADC	R2,R4		; Point to element
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Address of element to DE
ENDDIM:
	LD	R8,#hi(NXTOPR)
	LD	R9,#lo(NXTOPR)
	LDC	R2,@RR8
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	R3,@RR8		; Got code string address
	RET
;
FRE:
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Start of free memory
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; To DE
	LD	R2,#hi(0)
	LD	R3,#lo(0)	; End of free memory
	ADD	R3,SPL
	ADC	R2,SPH		; Current stack value
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	R0,@RR8		; Dummy argument type
	OR	R0,R0
	RCF
	JP	Z,FRENUM	; Numeric - Free variable space
	CALL	GSTRCU		; Current string to pool
	CALL	GARBGE		; Garbage collection
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Bottom of string space in use
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; To DE
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Bottom of string space
FRENUM:
	LD	R0,R3		; Get LSB of end
	SUB	R0,R7		; Subtract LSB of beginning
	LD	R5,R0		; Save difference if C
	LD	R0,R2		; Get MSB of end
	SBC	R0,R6		; Subtract MSB of beginning
ACPASS:
	LD	R4,R5		; Return integer AC
ABPASS:
	LD	R6,R4		; Return integer AB
	LD	R7,#0
	LD	R2,#hi(TYPE)
	LD	R3,#lo(TYPE)	; Point to type
	LDC	@RR2,R7		; Set type to numeric
	LD	R4,#80H+16	; 16 bit integer
	JP	RETINT		; Return the integr
;
POS:
	LD	R8,#hi(CURPOS)
	LD	R9,#lo(CURPOS)
	LDC	R0,@RR8		; Get cursor position
PASSA:
	LD	R4,R0		; Put A into AB
	XOR	R0,R0
	RCF			; Zero A
	JP	ABPASS		; Return integer AB
;
DEF:
	CALL	CHEKFN		; Get "FN" and name
	CALL	IDTEST		; Test for illegal direct
	LD	R4,#hi(DATA)
	LD	R5,#lo(DATA)	; To get next statement
	PUSH	R5
	PUSH	R4		; Save address for RETurn
	PUSH	R7
	PUSH	R6		; Save address of function ptr
	CALL	CHKSYN		; Make sure "(" follows
	DB	"("
	CALL	GETVAR		; Get argument variable name
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Argument address to HL
	DECW	RR2
	LDC	R6,@RR2		; Get first byte of arg name
	DECW	RR2
	LDC	R7,@RR2		; Get second byte of arg name
	POP	R2
	POP	R3		; Restore code string address
	CALL	TSTNUM		; Make sure numeric argument
	CALL	CHKSYN		; Make sure ")" follows
	DB	")"
	CALL	CHKSYN		; Make sure "=" follows
	DB	ZEQUAL		; "=" token
	LD	R4,R2		; Code string address to BC
	LD	R5,R3
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save code str , Get FN ptr
	LDC	@RR2,R5		; Save LSB of FN code string
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	@RR2,R4		; Save MSB of FN code string
	JP	SVSTAD		; Save address and do function
;
DOFN:
	CALL	CHEKFN		; Make sure FN follows
	PUSH	R7
	PUSH	R6		; Save function pointer address
	CALL	EVLPAR		; Evaluate expression in "()"
	CALL	TSTNUM		; Make sure numeric result
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save code str , Get FN ptr
	LDC	R7,@RR2		; Get LSB of FN code string
	INCW	RR2
	LDC	R6,@RR2		; Get MSB of FN code string
	INCW	RR2
	LD	R0,R6		; And function DEFined?
	OR	R0,R7
	RCF
	JP	Z,UFERR		; No - ?UF Error
	LDC	R0,@RR2		; Get LSB of argument address
	INCW	RR2
	LDC	R2,@RR2		; Get MSB of argument address
	LD	R3,R0		; HL = Arg variable address
	PUSH	R3
	PUSH	R2		; Save it
	LD	R8,#hi(FNRGNM)
	LD	R9,#lo(FNRGNM)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get old argument name
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save old , Get new
	LD	R8,#hi(FNRGNM)
	LD	R9,#lo(FNRGNM)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Set new argument name
	LD	R8,#hi(FNARG+2)
	LD	R9,#lo(FNARG+2)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get LSB,NLSB of old arg value
	PUSH	R3
	PUSH	R2		; Save it
	LD	R8,#hi(FNARG)
	LD	R9,#lo(FNARG)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get MSB,EXP of old arg value
	PUSH	R3
	PUSH	R2		; Save it
	LD	R2,#hi(FNARG)
	LD	R3,#lo(FNARG)	; HL = Value of argument
	PUSH	R7
	PUSH	R6		; Save FN code string address
	CALL	FPTHL		; Move FPREG to argument
	POP	R2
	POP	R3		; Get FN code string address
	CALL	GETNUM		; Get value from function
	DECW	RR2		; DEC 'cos GETCHR INCs
	CALL	GETCHR		; Get next character
	JP	NZ,SNERR	; Bad character in FN - Error
	POP	R2
	POP	R3		; Get MSB,EXP of old arg
	LD	R8,#hi(FNARG)
	LD	R9,#lo(FNARG)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Restore it
	POP	R2
	POP	R3		; Get LSB,NLSB of old arg
	LD	R8,#hi(FNARG+2)
	LD	R9,#lo(FNARG+2)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Restore it
	POP	R2
	POP	R3		; Get name of old arg
	LD	R8,#hi(FNRGNM)
	LD	R9,#lo(FNRGNM)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Restore it
	POP	R2
	POP	R3		; Restore code string address
	RET
;
IDTEST:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R8,#hi(LINEAT)
	LD	R9,#lo(LINEAT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get current line number
	INCW	RR2		; -1 means direct statement
	LD	R0,R2
	OR	R0,R3
	POP	R2
	POP	R3		; Restore code string address
	JR	Z,$+3
	RET			; Return if in program
	LD	R7,#ID		; ?ID Error
	JP	ERROR
;
CHEKFN:
	CALL	CHKSYN		; Make sure FN follows
	DB	ZFN		; "FN" token
	LD	R0,#80H
	LD	R8,#hi(FORFLG)
	LD	R9,#lo(FORFLG)
	LDC	@RR8,R0		; Flag FN name to find
	LDC	R1,@RR2
	OR	R0,R1		; FN name has bit 7 set
	LD	R4,R0		; in first byte of name
	CALL	GTFNAM		; Get FN name
	JP	TSTNUM		; Make sure numeric function
;
STR:
	CALL	TSTNUM		; Make sure it's a number
	CALL	NUMASC		; Turn number into text
STR1:
	CALL	CRTST		; Create string entry for it
	CALL	GSTRCU		; Current string to pool
	LD	R4,#hi(TOPOOL)
	LD	R5,#lo(TOPOOL)	; Save in string pool
	PUSH	R5
	PUSH	R4		; Save address on stack
;
SAVSTR:
	LDC	R0,@RR2		; Get string length
	INCW	RR2
	INCW	RR2
	PUSH	R3
	PUSH	R2		; Save pointer to string
	CALL	TESTR		; See if enough string space
	POP	R2
	POP	R3		; Restore pointer to string
	LDC	R5,@RR2		; Get LSB of address
	INCW	RR2
	LDC	R4,@RR2		; Get MSB of address
	CALL	CRTMST		; Create string entry
	PUSH	R3
	PUSH	R2		; Save pointer to MSB of addr
	LD	R3,R0		; Length of string
	CALL	TOSTRA		; Move to string area
	POP	R6
	POP	R7		; Restore pointer to MSB
	RET
;
MKTMST:
	CALL	TESTR		; See if enough string space
CRTMST:
	LD	R2,#hi(TMPSTR)
	LD	R3,#lo(TMPSTR)	; Temporary string
	PUSH	R3
	PUSH	R2		; Save it
	LDC	@RR2,R0		; Save length of string
	INCW	RR2
SVSTAD:
	INCW	RR2
	LDC	@RR2,R7		; Save LSB of address
	INCW	RR2
	LDC	@RR2,R6		; Save MSB of address
	POP	R2
	POP	R3		; Restore pointer
	RET
;
CRTST:
	DECW	RR2		; DEC - INCed after
QTSTR:
	LD	R4,#'"'		; Terminating quote
	LD	R6,R4		; Quote to D
DTSTR:
	PUSH	R3
	PUSH	R2		; Save start
	LD	R5,#-1		; Set counter to -1
QTSTLP:
	INCW	RR2		; Move on
	LDC	R0,@RR2		; Get byte
	INC	R5		; Count bytes
	OR	R0,R0		; End of line?
	JP	Z,CRTSTE	; Yes - Create string entry
	CP	R0,R6		; Terminator D found?
	JP	Z,CRTSTE	; Yes - Create string entry
	CP	R0,R4		; Terminator B found?
	JP	NZ,QTSTLP	; No - Keep looking
CRTSTE:
	CP	R0,#'"'		; End with '"'?
	JR	NZ,$+5
	CALL	GETCHR		; Yes - Get next character
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Starting quote
	INCW	RR2		; First byte of string
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; To DE
	LD	R0,R5		; Get length
	CALL	CRTMST		; Create string entry
TSTOPL:
	LD	R6,#hi(TMPSTR)
	LD	R7,#lo(TMPSTR)	; Temporary string
	LD	R8,#hi(TMSTPT)
	LD	R9,#lo(TMSTPT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Temporary string pool pointer
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	@RR8,R2
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	@RR8,R3		; Save address of string ptr
	LDC	@RR8,R3		; @@@ SWAP HL
	INCW	RR8
	LDC	@RR8,R2		; Save address of string ptr

	LD	R0,#1
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	@RR8,R0		; Set type to string
	CALL	DETHL4		; Move string to pool
	CALL	CPDEHL		; Out of string pool?
	LD	R8,#hi(TMSTPT)
	LD	R9,#lo(TMSTPT)
	LDC	@RR8,R2
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Save new pointer
	POP	R2
	POP	R3		; Restore code string address
	LDC	R0,@RR2		; Get next code byte
	JR	Z,$+3
	RET			; Return if pool OK
	LD	R7,#ST		; ?ST Error
	JP	ERROR		; String pool overflow
;
PRNUMS:
	INCW	RR2		; Skip leading space
PRS:
	CALL	CRTST		; Create string entry for it
PRS1:
	CALL	GSTRCU		; Current string to pool
	CALL	LOADFP		; Move string block to BCDE
	INC	R7		; Length + 1
PRSLP:
	DEC	R7		; Count characters
	JR	NZ,$+3
	RET			; End of string
	LDC	R0,@RR4		; Get byte to output
	CALL	OUTC		; Output character in A
	CP	R0,#CR		; Return?
	JR	NZ,$+5
	CALL	DONULL		; Yes - Do nulls
	INCW	RR4		; Next byte in string
	JP	PRSLP		; More characters to output
;
TESTR:
	OR	R0,R0; Test if enough room
	JR	GRBDON1		; No garbage collection done
GRBDON:
	POP	R0
	POP	R1
	LD	FLAGS,R1	; Garbage collection done
GRBDON1:
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save status
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Bottom of string space in use
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; To DE
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Bottom of string area
	COM	R0		; Negate length (Top down)
	LD	R5,R0		; -Length to BC
	LD	R4,#-1		; BC = -ve length of string
	ADD	R3,R5
	ADC	R2,R4		; Add to bottom of space in use
	INCW	RR2		; Plus one for 2's complement
	CALL	CPDEHL		; Below string RAM area?
	JP	C,TESTOS	; Tidy up if not done else err
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	@RR8,R2
;	LD	R1,FLAGS
	INCW	RR8
;	LD	FLAGS,R1
	LDC	@RR8,R3		; Save new bottom of area
;	LD	R1,FLAGS
	INCW	RR2		; Point to first byte of string
;	LD	FLAGS,R1
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Address to DE
POPAF:
	POP	R0		; Throw away status push
	POP	R1
	LD	FLAGS,R1
	RET
;
TESTOS:
	POP	R0
	POP	R1		; Garbage collect been done?
	LD	FLAGS,R1
	LD	R7,#OS		; ?OS Error
	JP	Z,ERROR		; Yes - Not enough string apace
	CP	R0,R0		; Flag garbage collect done
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save status
	LD	R4,#hi(GRBDON)
	LD	R5,#lo(GRBDON)	; Garbage collection done
	PUSH	R5
	PUSH	R4		; Save for RETurn
GARBGE:
	LD	R8,#hi(LSTRAM)
	LD	R9,#lo(LSTRAM)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get end of RAM pointer
GARBLP:
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Reset string pointer
	LD	R2,#hi(0)
	LD	R3,#lo(0)
	PUSH	R3
	PUSH	R2		; Flag no string found
	LD	R8,#hi(STRSPC)
	LD	R9,#lo(STRSPC)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get bottom of string space
	PUSH	R3
	PUSH	R2		; Save bottom of string space
	LD	R8,#hi(TMSTPL)
	LD	R9,#lo(TMSTPL)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Temporary string pool
GRBLP:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	LD	R8,#hi(TMSTPT)
	LD	R9,#lo(TMSTPT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Temporary string pool pointer
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	CALL	CPDEHL		; Temporary string pool done?
	LD	R4,#hi(GRBLP)
	LD	R5,#lo(GRBLP)	; Loop until string pool done
	JP	NZ,STPOOL	; No - See if in string area
	LD	R8,#hi(PROGND)
	LD	R9,#lo(PROGND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Start of simple variables
SMPVAR:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	LD	R8,#hi(VAREND)
	LD	R9,#lo(VAREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; End of simple variables
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	CALL	CPDEHL		; All simple strings done?
	JP	Z,ARRLP		; Yes - Do string arrays
	LDC	R0,@RR2		; Get type of variable
	INCW	RR2
	INCW	RR2
	OR	R0,R0		; "S" flag set if string
	RCF
	CALL	STRADD		; See if string in string area
	JP	SMPVAR		; Loop until simple ones done
;
GNXARY:
	POP	R4
	POP	R5		; Scrap address of this array
ARRLP:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	LD	R8,#hi(ARREND)
	LD	R9,#lo(ARREND)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; End of string arrays
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	CALL	CPDEHL		; All string arrays done?
	JP	Z,SCNEND	; Yes - Move string if found
	CALL	LOADFP		; Get array name to BCDE
	LD	R0,R7		; Get type of array
	PUSH	R3
	PUSH	R2		; Save address of num of dim'ns
	ADD	R3,R5
	ADC	R2,R4		; Start of next array
	OR	R0,R0
	RCF			; Test type of array
	JP	PL,GNXARY	; Numeric array - Ignore it
	LD	R8,#hi(CUROPR)
	LD	R9,#lo(CUROPR)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save address of next array
	POP	R2
	POP	R3		; Get address of num of dim'ns
	LDC	R5,@RR2		; BC = Number of dimensions
	LD	R4,#0
	ADD	R3,R5
	ADC	R2,R4		; Two bytes per dimension size
	ADD	R3,R5
	ADC	R2,R4
	INCW	RR2		; Plus one for number of dim'ns
GRBARY:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	LD	R8,#hi(CUROPR)
	LD	R9,#lo(CUROPR)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get address of next array
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	CALL	CPDEHL		; Is this array finished?
	JP	Z,ARRLP		; Yes - Get next one
	LD	R4,#hi(GRBARY)
	LD	R5,#lo(GRBARY)	; Loop until array all done
STPOOL:
	PUSH	R5
	PUSH	R4		; Save return address
	OR	R0,#80H
STRADD:
	LDC	R0,@RR2		; Get string length
	LD	R1,FLAGS
	INCW	RR2
	INCW	RR2
	LDC	R7,@RR2		; Get LSB of string address
	INCW	RR2
	LDC	R6,@RR2		; Get MSB of string address
	INCW	RR2
	LD	FLAGS,R1
	JR	MI,$+3
	RET			; Not a string - Return
	OR	R0,R0		; Set flags on string length
	JR	NZ,$+3
	RET			; Null string - Return
	LD	R4,R2		; Save variable pointer
	LD	R5,R3
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Bottom of new area
	CALL	CPDEHL		; String been done?
	LD	R2,R4		; Restore variable pointer
	LD	R3,R5
	JR	NC,$+3
	RET			; String done - Ignore
	POP	R2
	POP	R3		; Return address
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Lowest available string area
	CALL	CPDEHL		; String within string area?
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Lowest available string area
	PUSH	R3
	PUSH	R2		; Re-save return address
	LD	R2,R4		; Restore variable pointer
	LD	R3,R5
	JR	C,$+3
	RET			; Outside string area - Ignore
	POP	R4
	POP	R5		; Get return , Throw 2 away
	POP	R0
	POP	R1
	LD	FLAGS,R1
	POP	R0
	POP	R1
	LD	FLAGS,R1
	PUSH	R3
	PUSH	R2		; Save variable pointer
	PUSH	R7
	PUSH	R6		; Save address of current
	PUSH	R5
	PUSH	R4		; Put back return address
	RET			; Go to it
;
SCNEND:
	POP	R6
	POP	R7		; Addresses of strings
	POP	R2
	POP	R3		;
	LD	R0,R3		; HL = 0 if no more to do
	OR	R0,R2
	RCF
	JR	NZ,$+3
	RET			; No more to do - Return
	DECW	RR2
	LDC	R4,@RR2		; MSB of address of string
	DECW	RR2
	LDC	R5,@RR2		; LSB of address of string
	PUSH	R3
	PUSH	R2		; Save variable address
	DECW	RR2
	DECW	RR2
	LDC	R3,@RR2		; HL = Length of string
	LD	R2,#0
	ADD	R3,R5
	ADC	R2,R4		; Address of end of string+1
	LD	R6,R4		; String address to DE
	LD	R7,R5
	DECW	RR2		; Last byte in string
	LD	R4,R2		; Address to BC
	LD	R5,R3
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Current bottom of string area
	CALL	MOVSTR		; Move string to new address
	POP	R2
	POP	R3		; Restore variable address
	LDC	@RR2,R5		; Save new LSB of address
;	LD	R1,FLAGS
	INCW	RR2
;	LD	FLAGS,R1
	LDC	@RR2,R4		; Save new MSB of address
	LD	R3,R5		; Next string area+1 to HL
	LD	R2,R4
;	LD	R1,FLAGS
	DECW	RR2		; Next string area address
;	LD	FLAGS,R1
	JP	GARBLP		; Look for more strings
;
CONCAT:
	PUSH	R5
	PUSH	R4		; Save prec' opr & code string
	PUSH	R3
	PUSH	R2		;
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; Get first string
	LDC	R3,@RR8		; @@@ SWAP HL
	INCW	RR8
	LDC	R2,@RR8		; Get first string

	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save first string
	CALL	OPRND		; Get second string
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Restore first string
	CALL	TSTSTR		; Make sure it's a string
	LDC	R0,@RR2		; Get length of second string
	PUSH	R3
	PUSH	R2		; Save first string
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; Get second string
	LDC	R3,@RR8		; @@@ SWAP HL
	INCW	RR8
	LDC	R2,@RR8		; Get second string

	PUSH	R3
	PUSH	R2		; Save second string
	LDC	R8,@RR2
	ADD	R0,R8		; Add length of second string
	LD	R7,#LS		; ?LS Error
	JP	C,ERROR		; String too long - Error
	CALL	MKTMST		; Make temporary string
	POP	R6
	POP	R7		; Get second string to DE
	CALL	GSTRDE		; Move to string pool if needed
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Get first string
	CALL	GSTRHL		; Move to string pool if needed
	PUSH	R3
	PUSH	R2		; Save first string
	LD	R8,#hi(TMPSTR+2)
	LD	R9,#lo(TMPSTR+2)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Temporary string address
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; To DE
	CALL	SSTSA		; First string to string area
	CALL	SSTSA		; Second string to string area
	LD	R2,#hi(EVAL2)
	LD	R3,#lo(EVAL2)	; Return to evaluation loop
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save return,get code string
	PUSH	R3
	PUSH	R2		; Save code string address
	JP	TSTOPL		; To temporary string to pool
;
SSTSA:
	POP	R2
	POP	R3		; Return address
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Get string block,save return
	LDC	R0,@RR2		; Get length of string
	INCW	RR2
	INCW	RR2
	LDC	R5,@RR2		; Get LSB of string address
	INCW	RR2
	LDC	R4,@RR2		; Get MSB of string address
	LD	R3,R0		; Length to L
TOSTRA:
	INC	R3		; INC - DECed after
TSALP:
	DEC	R3		; Count bytes moved
	JR	NZ,$+3
	RET			; End of string - Return
	LDC	R0,@RR4		; Get source
	LDC	@RR6,R0		; Save destination
	INCW	RR4		; Next source
	INCW	RR6		; Next destination
	JP	TSALP		; Loop until string moved
;
GETSTR:
	CALL	TSTSTR		; Make sure it's a string
GSTRCU:
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; Get current string
	LDC	R3,@RR8		; @@@ SWAP HL
	INCW	RR8
	LDC	R2,@RR8		; Get current string
GSTRHL:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Save DE
GSTRDE:
	CALL	BAKTMP		; Was it last tmp-str?
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Restore DE
	JR	Z,$+3
	RET			; No - Return
	PUSH	R7
	PUSH	R6		; Save string
	LD	R6,R4		; String block address to DE
	LD	R7,R5
	DECW	RR6		; Point to length
	LDC	R5,@RR2		; Get string length
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Current bottom of string area
	CALL	CPDEHL		; Last one in string area?
	JP	NZ,POPHL	; No - Return
	LD	R4,R0		; Clear B (A=0)
	ADD	R3,R5
	ADC	R2,R4		; Remove string from str' area
	LD	R8,#hi(STRBOT)
	LD	R9,#lo(STRBOT)
	LDC	@RR8,R2
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Save new bottom of str' area
POPHL:
	POP	R2
	POP	R3		; Restore string
	RET
;
BAKTMP:
	LD	R8,#hi(TMSTPT)
	LD	R9,#lo(TMSTPT)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get temporary string pool top
	DECW	RR2		; Back
	LDC	R4,@RR2		; Get MSB of address
	DECW	RR2		; Back
	LDC	R5,@RR2		; Get LSB of address
	DECW	RR2		; Back
	DECW	RR2		; Back
	CALL	CPDEHL		; String last in string pool?
	JR	Z,$+3
	RET			; Yes - Leave it
	LD	R8,#hi(TMSTPT)
	LD	R9,#lo(TMSTPT)
	LDC	@RR8,R2
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R3		; Save new string pool top
	RET
;
LEN:
	LD	R4,#hi(PASSA)
	LD	R5,#lo(PASSA)	; To return integer A
	PUSH	R5
	PUSH	R4		; Save address
GETLEN:
	CALL	GETSTR		; Get string and its length
	XOR	R0,R0
	RCF
	LD	R6,R0		; Clear D
	LD	R8,#hi(TYPE)
	LD	R9,#lo(TYPE)
	LDC	@RR8,R0		; Set type to numeric
	LDC	R0,@RR2		; Get length of string
	OR	R0,R0
	RCF			; Set status flags
	RET
;
ASC:
	LD	R4,#hi(PASSA)
	LD	R5,#lo(PASSA)	; To return integer A
	PUSH	R5
	PUSH	R4		; Save address
GTFLNM:
	CALL	GETLEN		; Get length of string
	JP	Z,FCERR		; Null string - Error
	LD	R1,FLAGS
	INCW	RR2
	INCW	RR2
	LDC	R7,@RR2		; Get LSB of address
	INCW	RR2
	LD	FLAGS,R1
	LDC	R6,@RR2		; Get MSB of address
	LDC	R0,@RR6		; Get first byte of string
	RET
;
CHR:
	LD	R0,#1		; One character string
	CALL	MKTMST		; Make a temporary string
	CALL	MAKINT		; Make it integer A
	LD	R8,#hi(TMPSTR+2)
	LD	R9,#lo(TMPSTR+2)

;	LDC	R2,@RR8
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	R3,@RR8		; Get address of string
	LDC	R3,@RR8		; @@@ SWAP HL
	LD	R1,FLAGS
	INCW	RR8

	LD	FLAGS,R1
	LDC	R2,@RR8		; Get address of string

	LDC	@RR2,R7		; Save character
TOPOOL:
	POP	R4
	POP	R5		; Clean up stack
	JP	TSTOPL		; Temporary string to pool
;
LEFT:
	CALL	LFRGNM		; Get number and ending ")"
	XOR	R0,R0
	RCF			; Start at first byte in string
RIGHT1:
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save code string,Get string
	LD	R5,R0		; Starting position in string
MID1:
	PUSH	R3
	PUSH	R2		; Save string block address
	LDC	R0,@RR2		; Get length of string
	CP	R0,R4		; Compare with number given
	JP	C,ALLFOL	; All following bytes required
	LD	R0,R4		; Get new length
	JR	ALLFOL1		; Skip "LD C,0"
ALLFOL:
	LD	R5,#0		; First byte of string
ALLFOL1:
	PUSH	R5
	PUSH	R4		; Save position in string
	CALL	TESTR		; See if enough string space
	POP	R4
	POP	R5		; Get position in string
	POP	R2
	POP	R3		; Restore string block address
	PUSH	R3
	PUSH	R2		; And re-save it
	INCW	RR2
	INCW	RR2
	LDC	R4,@RR2		; Get LSB of address
	INCW	RR2
	LDC	R2,@RR2		; Get MSB of address
	LD	R3,R4		; HL = address of string
	LD	R4,#0		; BC = starting address
	ADD	R3,R5
	ADC	R2,R4		; Point to that byte
	LD	R4,R2		; BC = source string
	LD	R5,R3
	CALL	CRTMST		; Create a string entry
	LD	R3,R0		; Length of new string
	CALL	TOSTRA		; Move string to string area
	POP	R6
	POP	R7		; Clear stack
	CALL	GSTRDE		; Move to string pool if needed
	JP	TSTOPL		; Temporary string to pool
;
RIGHT:
	CALL	LFRGNM		; Get number and ending ")"
	POP	R6
	POP	R7		; Get string length
	PUSH	R7
	PUSH	R6		; And re-save
	LDC	R0,@RR6		; Get length
	SUB	R0,R4		; Move back N bytes
	JP	RIGHT1		; Go and get sub-string
;
MID:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Get code string address
	LDC	R0,@RR2		; Get next byte ',' or ")"
	CALL	MIDNUM		; Get number supplied
	INC	R4		; Is it character zero?
	DEC	R4
	JP	Z,FCERR		; Yes - Error
	PUSH	R5
	PUSH	R4		; Save starting position
	LD	R7,#255		; All of string
	CP	R0,#')'		; Any length given?
	JP	Z,RSTSTR	; No - Rest of string
	CALL	CHKSYN		; Make sure ',' follows
	DB	','
	CALL	GETINT		; Get integer 0-255
RSTSTR:
	CALL	CHKSYN		; Make sure ")" follows
	DB	")"
	POP	R0		; Restore starting position
	POP	R1
	LD	FLAGS,R1
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Get string,8ave code string
	LD	R4,#hi(MID1)
	LD	R5,#lo(MID1)	; Continuation of MID$ routine
	PUSH	R5
	PUSH	R4		; Save for return
	DEC	R0		; Starting position-1
	LDC	R8,@RR2
	CP	R0,R8		; Compare with length
	LD	R4,#0		; Zero bytes length
	JR	C,$+3
	RET			; Null string if start past end
	LD	R5,R0		; Save starting position-1
	LDC	R0,@RR2		; Get length of string
	SUB	R0,R5		; Subtract start
	CP	R0,R7		; Enough string for it?
	LD	R4,R0		; Save maximum length available
	JR	NC,$+3
	RET			; Truncate string if needed
	LD	R4,R7		; Set specified length
	RET			; Go and create string
;
VAL:
	CALL	GETLEN		; Get length of string
	JP	Z,RESZER	; Result zero
	LD	R7,R0		; Save length
	INCW	RR2
	INCW	RR2
	LDC	R0,@RR2		; Get LSB of address
	INCW	RR2
	LDC	R2,@RR2		; Get MSB of address
	LD	R3,R0		; HL = String address
	PUSH	R3
	PUSH	R2		; Save string address
	ADD	R3,R7
	ADC	R2,R6
	LDC	R4,@RR2		; Get end of string+1 byte
	LDC	@RR2,R6		; Zero it to terminate
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save string end,get start
	PUSH	R5
	PUSH	R4		; Save end+1 byte
	LDC	R0,@RR2		; Get starting byte
	CP	R0,#'$'		; Hex number indicated? [function added]
	JP	NZ,VAL1
	CALL	HEXTFP		; Convert Hex to FPREG
	JR	VAL3
VAL1:
	CP	R0,#'%'		; Binary number indicated? [function added]
	JP	NZ,VAL2
	CALL	BINTFP		; Convert Bin to FPREG
	JR	VAL3
VAL2:
	CALL	ASCTFP		; Convert ASCII string to FP
VAL3:
	POP	R4
	POP	R5		; Restore end+1 byte
	POP	R2
	POP	R3		; Restore end+1 address
	LDC	@RR2,R4		; Put back original byte
	RET
;
LFRGNM:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Code string address to HL
	CALL	CHKSYN		; Make sure ")" follows
	DB	")"
MIDNUM:
	POP	R4
	POP	R5		; Get return address
	POP	R6
	POP	R7		; Get number supplied
	PUSH	R5
	PUSH	R4		; Re-save return address
	LD	R4,R7		; Number to B
	RET
;
INP:
	CALL	MAKINT		; Make it integer A
	LD	R8,#hi(INPORT)
	LD	R9,#lo(INPORT)
	LDC	@RR8,R0		; Set input port
	CALL	INPSUB		; Get input from port
	JP	PASSA		; Return integer A
;
POUT:
	CALL	SETIO		; Set up port number
	JP	OUTSUB		; Output data and return
;
WAIT:
	CALL	SETIO		; Set up port number
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save AND mask
	LD	R7,#0		; Assume zero if none given
	DECW	RR2		; DEC 'cos GETCHR INCs
	CALL	GETCHR		; Get next character
	JP	Z,NOXOR		; No XOR byte given
	CALL	CHKSYN		; Make sure ',' follows
	DB	','
	CALL	GETINT		; Get integer 0-255 to XOR with
NOXOR:
	POP	R4
	POP	R5		; Restore AND mask
WAITLP:
	CALL	INPSUB		; Get input
	XOR	R0,R7		; Flip selected bits
	AND	R0,R4		; Result non-zero?
	JP	Z,WAITLP	; No = keep waiting
	RET
;
SETIO:
	CALL	GETINT		; Get integer 0-255
	LD	R8,#hi(INPORT)
	LD	R9,#lo(INPORT)
	LDC	@RR8,R0		; Set input port
	LD	R8,#hi(OTPORT)
	LD	R9,#lo(OTPORT)
	LDC	@RR8,R0		; Set output port
	CALL	CHKSYN		; Make sure ',' follows
	DB	','
	JP	GETINT		; Get integer 0-255 and return
;
FNDNUM:
	CALL	GETCHR		; Get next character
GETINT:
	CALL	GETNUM		; Get a number from 0 to 255
MAKINT:
	CALL	DEPINT		; Make sure value 0 - 255
	LD	R0,R6		; Get MSB of number
	OR	R0,R0
	RCF			; Zero?
	JP	NZ,FCERR	; No - Error
	DECW	RR2		; DEC 'cos GETCHR INCs
	CALL	GETCHR		; Get next character
	LD	R0,R7		; Get number to A
	RET
;
PEEK:
	CALL	DEINT		; Get memory address
	LDC	R0,@RR6		; Get byte in memory
	JP	PASSA		; Return integer A
;
POKE:
	CALL	GETNUM		; Get memory address
	CALL	DEINT		; Get integer -32768 to 3276
	PUSH	R7
	PUSH	R6		; Save memory address
	CALL	CHKSYN		; Make sure ',' follows
	DB	','
	CALL	GETINT		; Get integer 0-255
	POP	R6
	POP	R7		; Restore memory address
	LDC	@RR6,R0		; Load it into memory
	RET
;
ROUND:
	LD	R2,#hi(HALF)
	LD	R3,#lo(HALF)	; Add 0.5 to FPREG
ADDPHL:
	CALL	LOADFP		; Load FP at (HL) to BCDE
	JP	FPADD		; Add BCDE to FPREG
;
SUBPHL:
	CALL	LOADFP		; FPREG = -FPREG + number at HL
	JR	SUBCDE		; Skip "POP BC" and "POP DE"
PSUB:
	POP	R4
	POP	R5		; Get FP number from stack
	POP	R6
	POP	R7
SUBCDE:
	CALL	INVSGN		; Negate FPREG
FPADD:
	LD	R0,R4		; Get FP exponent
	OR	R0,R0
	RCF			; Is number zero?
	JR	NZ,$+3
	RET			; Yes - Nothing to add
	LD	R8,#hi(FPEXP)
	LD	R9,#lo(FPEXP)
	LDC	R0,@RR8		; Get FPREG exponent
	OR	R0,R0
	RCF			; Is this number zero?
	JP	Z,FPBCDE	; Yes - Move BCDE to FPREG
	SUB	R0,R4		; BCDE number larger?
	JP	NC,NOSWAP	; No - Don't swap them
	COM	R0		; Two's complement
	INC	R0		;	FP exponent
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	CALL	STAKFP		; Put FPREG on stack
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	CALL	FPBCDE		; Move BCDE to FPREG
	POP	R4
	POP	R5		; Restore number from stack
	POP	R6
	POP	R7
NOSWAP:
	CP	R0,#24+1	; Second number insignificant?
	JR	C,$+3
	RET			; Yes - First number is result
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save number of bits to scale
	CALL	SIGNS		; Set MSBs & sign of result
	LD	R2,R0		; Save sign of result
	POP	R0		; Restore scaling factor
	POP	R1
	LD	FLAGS,R1
	CALL	SCALE		; Scale BCDE to same exponent
	OR	R0,R2
	RCF			; Result to be positive?
	LD	R2,#hi(FPREG)
	LD	R3,#lo(FPREG)	; Point to FPREG
	JP	PL,MINCDE	; No - Subtract FPREG from CDE
	CALL	PLUCDE		; Add FPREG to CDE
	JP	NC,RONDUP	; No overflow - Round it up
	INCW	RR2		; Point to exponent
	LDC	R8,@RR2
	INC	R8
	LDC	@RR2,R8		; Increment it
	JP	Z,OVERR		; Number overflowed - Error
	LD	R3,#1		; 1 bit to shift right
	CALL	SHRT1		; Shift result right
	JP	RONDUP		; Round it up
;
MINCDE:
	XOR	R0,R0
	RCF			; Clear A and carry
	SUB	R0,R4		; Negate exponent
	LD	R4,R0		; Re-save exponent
	LDC	R0,@RR2		; Get LSB of FPREG
	SBC	R0,R7		; Subtract LSB of BCDE
	LD	R7,R0		; Save LSB of BCDE
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	R0,@RR2		; Get NMSB of FPREG
	SBC	R0,R6		; Subtract NMSB of BCDE
	LD	R6,R0		; Save NMSB of BCDE
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	R0,@RR2		; Get MSB of FPREG
	SBC	R0,R5		; Subtract MSB of BCDE
	LD	R5,R0		; Save MSB of BCDE
CONPOS:
	JR	NC,$+5
	CALL	COMPL		; Overflow - Make it positive
;
BNORM:
	LD	R3,R4		; L = Exponent
	LD	R2,R7		; H = LSB
	XOR	R0,R0
;	RCF
BNRMLP:
	LD	R4,R0		; Save bit count
	LD	R0,R5		; Get MSB
	OR	R0,R0		; Is it zero?
	JP	NZ,PNORM	; No - Do it bit at a time
	LD	R5,R6		; MSB = NMSB
	LD	R6,R2		; NMSB= LSB
	LD	R2,R3		; LSB = VLSB
	LD	R3,R0		; VLSB= 0
	LD	R0,R4		; Get exponent
	SUB	R0,#8		; Count 8 bits
	CP	R0,#0E0H	; -24-8 Was number zero?
	JP	NZ,BNRMLP	; No - Keep normalising
RESZER:
	XOR	R0,R0		; Result is zero
	RCF
SAVEXP:
	LD	R8,#hi(FPEXP)
	LD	R9,#lo(FPEXP)
	LDC	@RR8,R0		; Save result as zero
	RET
;
NORMAL:
	DEC	R4		; Count bits
	ADD	R3,R3
	ADC	R2,R2		; Shift HL left
	LD	R0,R6		; Get NMSB
	RLC	R0		; Shift left with last bit
	LD	R6,R0		; Save NMSB
	LD	R0,R5		; Get MSB
	ADC	R0,R0		; Shift left with last bit
	LD	R5,R0		; Save MSB
PNORM:
	JP	PL,NORMAL	; Not done - Keep going
	LD	R0,R4		; Number of bits shifted
	LD	R7,R2		; Save HL in EB
	LD	R4,R3
	OR	R0,R0		; Any shifting done?
	JP	Z,RONDUP	; No - Round it up
	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	LDC	R8,@RR2
	ADD	R0,R8		; Add shifted bits
	LDC	@RR2,R0		; Re-save exponent
	JP	NC,RESZER	; Underflow - Result is zero
	JR	NZ,$+3
	RET			; Result is zero
RONDUP:
	LD	R0,R4		; Get VLSB of number
RONDB:
	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	OR	R0,R0		; Any rounding?
	JR	PL,$+5
	CALL	FPROND		; Yes - Round number up
	LDC	R4,@RR2		; B = Exponent
	INCW	RR2
	LDC	R0,@RR2		; Get sign of result
	AND	R0,#10000000B	; Only bit 7 needed
	XOR	R0,R5		; Set correct sign
	LD	R5,R0		; Save correct sign in number
	JP	FPBCDE		; Move BCDE to FPREG
;
FPROND:
	INC	R7		; Round LSB
	JR	Z,$+3
	RET			; Return if ok
	INC	R6		; Round NMSB
	JR	Z,$+3
	RET			; Return if ok
	INC	R5		; Round MSB
	JR	Z,$+3
	RET			; Return if ok
	LD	R5,#80H		; Set normal value
	LDC	R8,@RR2
	INC	R8
	LDC	@RR2,R8		; Increment exponent
	JR	Z,$+3
	RET			; Return if ok
	JP	OVERR		; Overflow error
;
PLUCDE:
	LDC	R0,@RR2		; Get LSB of FPREG
	ADD	R0,R7		; Add LSB of BCDE
	LD	R7,R0		; Save LSB of BCDE
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	R0,@RR2		; Get NMSB of FPREG
	ADC	R0,R6		; Add NMSB of BCDE
	LD	R6,R0		; Save NMSB of BCDE
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	R0,@RR2		; Get MSB of FPREG
	ADC	R0,R5		; Add MSB of BCDE
	LD	R5,R0		; Save MSB of BCDE
	RET
;
COMPL:
	LD	R2,#hi(SGNRES)
	LD	R3,#lo(SGNRES)	; Sign of result
	LDC	R0,@RR2		; Get sign of result
	COM	R0		; Negate it
	LDC	@RR2,R0		; Put it back
	XOR	R0,R0
	RCF
	LD	R3,R0		; Set L to zero
	SUB	R0,R4		; Negate exponent,set carry
	LD	R4,R0		; Re-save exponent
	LD	R0,R3		; Load zero
	SBC	R0,R7		; Negate LSB
	LD	R7,R0		; Re-save LSB
	LD	R0,R3		; Load zero
	SBC	R0,R6		; Negate NMSB
	LD	R6,R0		; Re-save NMSB
	LD	R0,R3		; Load zero
	SBC	R0,R5		; Negate MSB
	LD	R5,R0		; Re-save MSB
	RET
;
SCALE:
	LD	R4,#0		; Clear underflow
SCALLP:
	SUB	R0,#8		; 8 bits (a whole byte)?
	JP	C,SHRITE	; No - Shift right A bits
	LD	R4,R7		; <- Shift
	LD	R7,R6		; <- right
	LD	R6,R5		; <- eight
	LD	R5,#0		; <- bits
	JP	SCALLP		; More bits to shift
;
SHRITE:
	ADD	R0,#8+1		; Adjust count
	LD	R3,R0		; Save bits to shift
SHRLP:
	XOR	R0,R0
	RCF			; Flag for all done
	DEC	R3		; All shifting done?
	JR	NZ,$+3
	RET			; Yes - Return
	LD	R0,R5		; Get MSB
SHRT1:
;	RRC	R0		; Shift it right
;	LD	R5,R0		; Re-save
;	LD	R0,R6		; Get NMSB
;	RRC	R0		; Shift right with last bit
;	LD	R6,R0		; Re-save it
;	LD	R0,R7		; Get LSB
;	RRC	R0		; Shift right with last bit
;	LD	R7,R0		; Re-save it
;	LD	R0,R4		; Get underflow
;	RRC	R0		; Shift right with last bit
;	LD	R4,R0		; Re-save underflow
;	JP	SHRLP		; More bits to do

	RRC	R0		; Shift it right
	LD	R5,R0		; Re-save
	RRC	R6		; Shift NMSB right with last bit
	RRC	R7		; Shift LSB right with last bit
	RRC	R4		; Shift underflow right with last bit
	JP	SHRLP		; More bits to do

;
UNITY:
	DB	 000H,000H,000H,081H	; 1.00000
;
LOGTAB:
	DB	3			; Table used by LOG
	DB	0AAH,056H,019H,080H	; 0.59898
	DB	0F1H,022H,076H,080H	; 0.96147
	DB	045H,0AAH,038H,082H	; 2.88539
;
LOG:
	CALL	TSTSGN		; Test sign of value
; OR	A
; JP	PE,FCERR		; ?FC Error if <= zero

	LD	R1,R0
	RRC	R1
	RRC	R1
	RRC	R1
	RRC	R1
	XOR	R1,R0
	LD	R8,R1
	RRC	R1
	RRC	R1
	XOR	R1,R8
	LD	R8,R1
	RRC	R1
	XOR	R1,R8
	AND	R1,#01H
	JP	Z,FCERR

	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	LDC	R0,@RR2		; Get exponent
	LD	R4,#hi(8035H)
	LD	R5,#lo(8035H)	; BCDE = SQR(1/2)
	LD	R6,#hi(04F3H)
	LD	R7,#lo(04F3H)
	SUB	R0,R4		; Scale value to be < 1
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save scale factor
	LDC	@RR2,R4		; Save new exponent
	PUSH	R7
	PUSH	R6		; Save SQR(1/2)
	PUSH	R5
	PUSH	R4
	CALL	FPADD		; Add SQR(1/2) to value
	POP	R4
	POP	R5		; Restore SQR(1/2)
	POP	R6
	POP	R7
	INC	R4		; Make it SQR(2)
	CALL	DVBCDE		; Divide by SQR(2)
	LD	R2,#hi(UNITY)
	LD	R3,#lo(UNITY)	; Point to 1.
	CALL	SUBPHL		; Subtract FPREG from 1
	LD	R2,#hi(LOGTAB)
	LD	R3,#lo(LOGTAB)	; Coefficient table
	CALL	SUMSER		; Evaluate sum of series
	LD	R4,#hi(8080H)
	LD	R5,#lo(8080H)	; BCDE = -0.5
	LD	R6,#hi(0000H)
	LD	R7,#lo(0000H)
	CALL	FPADD		; Subtract 0.5 from FPREG
	POP	R0		; Restore scale factor
	POP	R1
	LD	FLAGS,R1
	CALL	RSCALE		; Re-scale number
MULLN2:
	LD	R4,#hi(8031H)
	LD	R5,#lo(8031H)	; BCDE = Ln(2)
	LD	R6,#hi(7218H)
	LD	R7,#lo(7218H)
	JR	FPMULT		; Skip "POP BC" and "POP DE"
;
MULT:
	POP	R4
	POP	R5		; Get number from stack
	POP	R6
	POP	R7
FPMULT:
	CALL	TSTSGN		; Test sign of FPREG
	JR	NZ,$+3
	RET			; Return zero if zero
	LD	R3,#0		; Flag add exponents
	CALL	ADDEXP		; Add exponents
	LD	R0,R5		; Get MSB of multiplier
	LD	R8,#hi(MULVAL)
	LD	R9,#lo(MULVAL)
	LDC	@RR8,R0		; Save MSB of multiplier
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	LD	R8,#hi(MULVAL+1); @@
	LD	R9,#lo(MULVAL+1)
	LDC	@RR8,R2
	INCW	RR8
	LDC	@RR8,R3		; Save rest of multiplier
	LD	R4,#hi(0)
	LD	R5,#lo(0)	; Partial product (BCDE) = zero
	LD	R6,R4
	LD	R7,R4
	LD	R2,#hi(BNORM)
	LD	R3,#lo(BNORM)	; Address of normalise
	PUSH	R3
	PUSH	R2		; Save for return
	LD	R2,#hi(MULT8)
	LD	R3,#lo(MULT8)	; Address of 8 bit multiply
	PUSH	R3
	PUSH	R2		; Save for NMSB,MSB
	PUSH	R3
	PUSH	R2		;
	LD	R2,#hi(FPREG)
	LD	R3,#lo(FPREG)	; Point to number
MULT8:
	LDC	R0,@RR2		; Get LSB of number
	INCW	RR2		; Point to NMSB
	OR	R0,R0		; Test LSB
	JP	Z,BYTSFT	; Zero - shift to next byte
	PUSH	R3
	PUSH	R2		; Save address of number
	LD	R3,#8		; 8 bits to multiply by
MUL8LP:
	RRC	R0		; Shift LSB right
	LD	R2,R0		; Save LSB
	LD	R0,R5		; Get MSB
	JP	NC,NOMADD	; Bit was zero - Don't add
	PUSH	R3
	PUSH	R2		; Save LSB and count
	LD	R8,#hi(MULVAL+1)
	LD	R9,#lo(MULVAL+1)
	LDC	R2,@RR8
	INCW	RR8
	LDC	R3,@RR8		; Get LSB and NMSB
	ADD	R3,R7
	ADC	R2,R6		; Add NMSB and LSB
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Leave sum in DE
	POP	R2
	POP	R3		; Restore MSB and count
	LD	R8,#hi(MULVAL)
	LD	R9,#lo(MULVAL)
	LDC	R0,@RR8		; Get MSB of multiplier
	ADC	R0,R5		; Add MSB
NOMADD:
;	RRC	R0		; Shift MSB right
;	LD	R5,R0		; Re-save MSB
;	LD	R0,R6		; Get NMSB
;	RRC	R0		; Shift NMSB right
;	LD	R6,R0		; Re-save NMSB
;	LD	R0,R7		; Get LSB
;	RRC	R0		; Shift LSB right
;	LD	R7,R0		; Re-save LSB
;	LD	R0,R4		; Get VLSB
;	RRC	R0		; Shift VLSB right
;	LD	R4,R0		; Re-save VLSB
;	DEC	R3		; Count bits multiplied
;	LD	R0,R2		; Get LSB of multiplier
;	JP	NZ,MUL8LP	; More - Do it

	RRC	R0		; Shift MSB right
	LD	R5,R0		; Re-save MSB
	RRC	R6		; Shift NMSB right
	RRC	R7		; Shift LSB right
	RRC	R4		; Shift VLSB right
	DEC	R3		; Count bits multiplied
	LD	R0,R2		; Get LSB of multiplier
	JP	NZ,MUL8LP	; More - Do it

POPHRT:
	POP	R2
	POP	R3		; Restore address of number
	RET
;
BYTSFT:
	LD	R4,R7		; Shift partial product left
	LD	R7,R6
	LD	R6,R5
	LD	R5,R0
	RET
;
DIV10:
	CALL	STAKFP		; Save FPREG on stack
	LD	R4,#hi(8420H)
	LD	R5,#lo(8420H)	; BCDE = 10.
	LD	R6,#hi(0000H)
	LD	R7,#lo(0000H)
	CALL	FPBCDE		; Move 10 to FPREG
;
DIV:
	POP	R4
	POP	R5		; Get number from stack
	POP	R6
	POP	R7
DVBCDE:
	CALL	TSTSGN		; Test sign of FPREG
	JP	Z,DZERR		; Error if division by zero
	LD	R3,#-1		; Flag subtract exponents
	CALL	ADDEXP		; Subtract exponents
	LDC	R8,@RR2
	INC	R8
	LDC	@RR2,R8		; Add 2 to exponent to adjust
	LDC	R8,@RR2
	INC	R8
	LDC	@RR2,R8
	DECW	RR2		; Point to MSB
	LDC	R0,@RR2		; Get MSB of dividend
	LD	R8,#hi(DIV3)
	LD	R9,#lo(DIV3)
	LDC	@RR8,R0		; Save for subtraction
	DECW	RR2
	LDC	R0,@RR2		; Get NMSB of dividend
	LD	R8,#hi(DIV2)
	LD	R9,#lo(DIV2)
	LDC	@RR8,R0		; Save for subtraction
	DECW	RR2
	LDC	R0,@RR2		; Get MSB of dividend
	LD	R8,#hi(DIV1)
	LD	R9,#lo(DIV1)
	LDC	@RR8,R0		; Save for subtraction
	LD	R4,R5		; Get MSB
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; NMSB,LSB to HL
	XOR	R0,R0
	LD	R5,R0		; Clear MSB of quotient
	LD	R6,R0		; Clear NMSB of quotient
	LD	R7,R0		; Clear LSB of quotient
	LD	R8,#hi(DIV4)
	LD	R9,#lo(DIV4)
	LDC	@RR8,R0		; Clear overflow count
DIVLP:
	PUSH	R3
	PUSH	R2		; Save divisor
	PUSH	R5
	PUSH	R4
	LD	R0,R3		; Get LSB of number
	CALL	DIVSUP		; Subt' divisor from dividend
	SBC	R0,#0		; Count for overflows
	CCF
	JP	NC,RESDIV	; Restore divisor if borrow
	LD	R8,#hi(DIV4)
	LD	R9,#lo(DIV4)
	LDC	@RR8,R0		; Re-save overflow count
	POP	R0		; Scrap divisor
	POP	R1
	POP	R0
	POP	R1
	LD	FLAGS,R1
	SCF			; Set carry to
	JR	RESDIV1		; Skip "POP BC" and "POP HL"
;
RESDIV:
	POP	R4
	POP	R5		; Restore divisor
	POP	R2
	POP	R3
RESDIV1:
	LD	R0,R5		; Get MSB of quotient
	INC	R0
	DEC	R0
;	RRC	R0		; Bit 0 to bit 7
;	JP	MI,RONDB	; Done - Normalise result
;	RLC	R0		; Restore carry
	JR	PL,RESDIV2
	RRC	R0		; Bit 0 to bit 7
	JP	RONDB		; Done - Normalise result
RESDIV2:
	RRC	R0		; Bit 0 to bit 7
	RLC	R0		; Restore carry

;	LD	R0,R7		; Get LSB of quotient
;	RLC	R0		; Double it
;	LD	R7,R0		; Put it back
;	LD	R0,R6		; Get NMSB of quotient
;	RLC	R0		; Double it
;	LD	R6,R0		; Put it back
;	LD	R0,R5		; Get MSB of quotient
;	RLC	R0		; Double it
;	LD	R5,R0		; Put it back
	RLC	R7		; Double LSB of quotient
	RLC	R6		; Double NMSB of quotient
	RLC	R5		; Double MSB of quotient
	ADD	R3,R3
	ADC	R2,R2		; Double NMSB,LSB of divisor
	LD	R0,R4		; Get MSB of divisor
	RLC	R4		; Double MSB of divisor

	LD	R8,#hi(DIV4)
	LD	R9,#lo(DIV4)
	LDC	R0,@RR8		; Get VLSB of quotient
	RLC	R0		; Double it
	LDC	@RR8,R0		; Put it back
	LD	R0,R5		; Get MSB of quotient
	OR	R0,R6		; Merge NMSB
	OR	R0,R7		; Merge LSB
	RCF
	JP	NZ,DIVLP	; Not done - Keep dividing
	PUSH	R3
	PUSH	R2		; Save divisor
	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	LDC	R8,@RR2
	DEC	R8
	LDC	@RR2,R8		; Divide by 2
	POP	R2
	POP	R3		; Restore divisor
	JP	NZ,DIVLP	; Ok - Keep going
	JP	OVERR		; Overflow error

;
ADDEXP:
	LD	R0,R4		; Get exponent of dividend
	OR	R0,R0
	RCF			; Test it
	JP	Z,OVTST3	; Zero - Result zero
	LD	R0,R3		; Get add/subtract flag
	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	LDC	R8,@RR2
	XOR	R0,R8		; Add or subtract it
	ADD	R0,R4		; Add the other exponent
	LD	R4,R0		; Save new exponent
	RRC	R0			; Test exponent for overflow
	XOR	R0,R4
	LD	R0,R4		; Get exponent
	JP	PL,OVTST2	; Positive - Test for overflow
	ADD	R0,#80H		; Add excess 128
	LDC	@RR2,R0		; Save new exponent
	JP	Z,POPHRT	; Zero - Result zero
	CALL	SIGNS		; Set MSBs and sign of result
	LDC	@RR2,R0		; Save new exponent
	LD	R1,FLAGS
	DECW	RR2		; Point to MSB
	LD	FLAGS,R1
	RET
;
OVTST1:
	CALL	TSTSGN		; Test sign of FPREG
	COM	R0		; Invert sign
	POP	R2
	POP	R3		; Clean up stack
OVTST2:
	OR	R0,R0
	RCF			; Test if new exponent zero
OVTST3:
	POP	R2
	POP	R3		; Clear off return address
	JP	PL,RESZER	; Result zero
	JP	OVERR		; Overflow error
;
MLSP10:
	CALL	BCDEFP		; Move FPREG to BCDE
	LD	R0,R4		; Get exponent
	OR	R0,R0
	RCF			; Is it zero?
	JR	NZ,$+3
	RET			; Yes - Result is zero
	ADD	R0,#2		; Multiply by 4
	JP	C,OVERR		; Overflow - ?OV Error
	LD	R4,R0		; Re-save exponent
	CALL	FPADD		; Add BCDE to FPREG (Times 5)
	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	LDC	R8,@RR2
	INC	R8
	LDC	@RR2,R8		; Double number (Times 10)
	JR	Z,$+3
	RET			; Ok - Return
	JP	OVERR		; Overflow error
;
TSTSGN:
	LD	R8,#hi(FPEXP)
	LD	R9,#lo(FPEXP)
	LDC	R0,@RR8		; Get sign of FPREG
	OR	R0,R0
	RCF
	JR	NZ,$+3
	RET			; RETurn if number is zero
	LD	R8,#hi(FPREG+2)
	LD	R9,#lo(FPREG+2)
	LDC	R0,@RR8		; Get MSB of FPREG
	CP	R0,#'/'		; Test sign
	JR	RETREL1
RETREL:
	COM	R0		; Invert sign
RETREL1:
	RLC	R0		; Sign bit to carry
FLGDIF:
	SBC	R0,R0		; Carry to all bits of A
	JR	Z,$+3
	RET			; Return -1 if negative
	INC	R0		; Bump to +1
	RET			; Positive - Return +1
;
SGN:
	CALL	TSTSGN		; Test sign of FPREG
FLGREL:
	LD	R4,#80H+8	; 8 bit integer in exponent
	LD	R6,#hi(0)
	LD	R7,#lo(0)	; Zero NMSB and LSB
RETINT:
	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	LD	R5,R0		; CDE = MSB,NMSB and LSB
	LDC	@RR2,R4		; Save exponent
	LD	R4,#0		; CDE = integer to normalise
	INCW	RR2		; Point to sign of result
	LD	R1,#80H
	LDC	@RR2,R1		; Set sign of result
	RLC	R0		; Carry = sign of integer
	JP	CONPOS		; Set sign of result
;
ABS:
	CALL	TSTSGN		; Test sign of FPREG
	JR	MI,$+3
	RET			; Return if positive
INVSGN:
	LD	R2,#hi(FPREG+2)
	LD	R3,#lo(FPREG+2)	; Point to MSB
	LDC	R0,@RR2		; Get sign of mantissa
	XOR	R0,#80H		; Invert sign of mantissa
	LDC	@RR2,R0		; Re-save sign of mantissa
	RET
;
STAKFP:
;	LD	R8,R6
;	LD	R9,R7
;	LD	R6,R2
;	LD	R7,R3
;	LD	R2,R8
;	LD	R3,R9		; Save code string address
;	LD	R8,#hi(FPREG)
;	LD	R9,#lo(FPREG)
;	LDC	R3,@RR8		; @@@ SWAP HL
;	INCW	RR8
;	LDC	R2,@RR8		; LSB,NLSB of FPREG
;
;	POP	R8
;	POP	R9
;	PUSH	R3
;	PUSH	R2
;	LD	R2,R8
;	LD	R3,R9		; Stack them,get return
;	PUSH	R3
;	PUSH	R2		; Re-save return
;	LD	R8,#hi(FPREG+2)
;	LD	R9,#lo(FPREG+2)
;	LDC	R3,@RR8		; @@@ SWAP HL
;	INCW	RR8
;	LDC	R2,@RR8		; MSB and exponent of FPREG
;
;	POP	R8
;	POP	R9
;	PUSH	R3
;	PUSH	R2
;	LD	R2,R8
;	LD	R3,R9		; Stack them,get return
;	PUSH	R3
;	PUSH	R2		; Re-save return
;	LD	R8,R6
;	LD	R9,R7
;	LD	R6,R2
;	LD	R7,R3
;	LD	R2,R8
;	LD	R3,R9		; Restore code string address
;	RET

	POP	R10
	POP	R11
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)
	LDC	R1,@RR8		; LSB of FPREG
	INCW	RR8
	PUSH	R1
	LDC	R1,@RR8		; NLSB of FPREG
	INCW	RR8
	PUSH	R1
	LDC	R1,@RR8		; MSB of FPREG
	INCW	RR8
	PUSH	R1
	LDC	R1,@RR8		; exponent of FPREG
	PUSH	R1
	PUSH	R11
	PUSH	R10
	RET

;
PHLTFP:
	CALL	LOADFP		; Number at HL to BCDE
FPBCDE:
;	LD	R8,R6
;	LD	R9,R7
;	LD	R6,R2
;	LD	R7,R3
;	LD	R2,R8
;	LD	R3,R9		; Save code string address
;	LD	R8,#hi(FPREG)
;	LD	R9,#lo(FPREG)
;	LDC	@RR8,R3		; @@@ SWAP HL
;	INCW	RR8
;	LDC	@RR8,R2		; Save LSB,NLSB of number
;
;	LD	R2,R4		; Exponent of number
;	LD	R3,R5		; MSB of number
;	LD	R8,#hi(FPREG+2)
;	LD	R9,#lo(FPREG+2)
;	LDC	@RR8,R3		; @@@ SWAP HL
;	INCW	RR8
;	LDC	@RR8,R2		; Save MSB and exponent
;
;	LD	R8,R6
;	LD	R9,R7
;	LD	R6,R2
;	LD	R7,R3
;	LD	R2,R8
;	LD	R3,R9		; Restore code string address
;	RET

	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)
	LDC	@RR8,R7		; Save LSB of number
	INCW	RR8
	LDC	@RR8,R6		; Save NLSB of number
	INCW	RR8
	LDC	@RR8,R5		; Save MSB exponent of number
	INCW	RR8
	LDC	@RR8,R4		; Save exponent of number
	INCW	RR8
	RET

BCDEFP:
	LD	R2,#hi(FPREG)
	LD	R3,#lo(FPREG)	; Point to FPREG
LOADFP:
	LDC	R7,@RR2		; Get LSB of number
	LD	R1,FLAGS
	INCW	RR2
	LDC	R6,@RR2		; Get NMSB of number
	INCW	RR2
	LDC	R5,@RR2		; Get MSB of number
	INCW	RR2
	LD	FLAGS,R1
	LDC	R4,@RR2		; Get exponent of number
INCHL:
	LD	R1,FLAGS
	INCW	RR2		; Used for conditional "INC HL"
	LD	FLAGS,R1
	RET
;
FPTHL:
	LD	R6,#hi(FPREG)
	LD	R7,#lo(FPREG)	; Point to FPREG
DETHL4:
	LD	R4,#4		; 4 bytes to move
DETHLB:
	LDC	R0,@RR6		; Get source
	LDC	@RR2,R0		; Save destination
	INCW	RR6		; Next source
	INCW	RR2		; Next destination
	DEC	R4		; Count bytes
	JP	NZ,DETHLB	; Loop if more
	RET
;
SIGNS:
	LD	R2,#hi(FPREG+2)
	LD	R3,#lo(FPREG+2)	; Point to MSB of FPREG
	LDC	R0,@RR2		; Get MSB
	RL	R0		; Old sign to carry
	SCF			; Set MSBit
	RRC	R0		; Set MSBit of MSB
	LDC	@RR2,R0		; Save new MSB
	CCF			; Complement sign
	RRC	R0		; Old sign to carry
	INCW	RR2
	INCW	RR2
	LDC	@RR2,R0		; Set sign of result
	LD	R0,R5		; Get MSB
	RL	R0		; Old sign to carry
	SCF			; Set MSBit
	RRC	R0		; Set MSBit of MSB
	LD	R5,R0		; Save MSB
	RRC	R0
	LDC	R8,@RR2
	XOR	R0,R8		; New sign of result
	RET
;
CMPNUM:
	LD	R0,R4		; Get exponent of number
	OR	R0,R0
	RCF
	JP	Z,TSTSGN	; Zero - Test sign of FPREG
	LD	R2,#hi(RETREL)
	LD	R3,#lo(RETREL)	; Return relation routine
	PUSH	R3
	PUSH	R2		; Save for return
	CALL	TSTSGN		; Test sign of FPREG
	LD	R0,R5		; Get MSB of number
	JR	NZ,$+3
	RET			; FPREG zero - Number's MSB
	LD	R2,#hi(FPREG+2)
	LD	R3,#lo(FPREG+2)	; MSB of FPREG
	LDC	R8,@RR2
	XOR	R0,R8		; Combine signs
	LD	R0,R5		; Get MSB of number
	JR	PL,$+3
	RET			; Exit if signs different
	CALL	CMPFP		; Compare FP numbers
	RRC	R0		; Get carry to sign
	XOR	R0,R5		; Combine with MSB of number
	RET
;
CMPFP:
	INCW	RR2		; Point to exponent
	LD	R0,R4		; Get exponent
	LDC	R8,@RR2
	CP	R0,R8		; Compare exponents
	JR	Z,$+3
	RET			; Different
	DECW	RR2		; Point to MBS
	LD	R0,R5		; Get MSB
	LDC	R8,@RR2
	CP	R0,R8		; Compare MSBs
	JR	Z,$+3
	RET			; Different
	DECW	RR2		; Point to NMSB
	LD	R0,R6		; Get NMSB
	LDC	R8,@RR2
	CP	R0,R8		; Compare NMSBs
	JR	Z,$+3
	RET			; Different
	DECW	RR2		; Point to LSB
	LD	R0,R7		; Get LSB
	LDC	R8,@RR2
	SUB	R0,R8		; Compare LSBs
	JR	Z,$+3
	RET			; Different
	POP	R2
	POP	R3		; Drop RETurn
	POP	R2
	POP	R3		; Drop another RETurn
	RET
;
FPINT:
	LD	R4,R0		; <- Move
	LD	R5,R0		; <- exponent
	LD	R6,R0		; <- to all
	LD	R7,R0		; <- bits
	OR	R0,R0
	RCF			; Test exponent
	JR	NZ,$+3
	RET			; Zero - Return zero
	PUSH	R3
	PUSH	R2		; Save pointer to number
	CALL	BCDEFP		; Move FPREG to BCDE
	CALL	SIGNS		; Set MSBs & sign of result
	LDC	R8,@RR2
	XOR	R0,R8		; Combine with sign of FPREG
	LD	R2,R0		; Save combined signs
	JR	PL,$+5
	CALL	DCBCDE		; Negative - Decrement BCDE
	LD	R0,#80H+24	; 24 bits
	SUB	R0,R4		; Bits to shift
	CALL	SCALE		; Shift BCDE
	LD	R0,R2		; Get combined sign
	RLC	R0		; Sign to carry
	JR	NC,$+5
	CALL	FPROND		; Negative - Round number up
	LD	R4,#0		; Zero exponent
	JR	NC,$+5
	CALL	COMPL		; If negative make positive
	POP	R2
	POP	R3		; Restore pointer to number
	RET
;
DCBCDE:
	DECW	RR6		; Decrement BCDE
	LD	R0,R6		; Test LSBs
	AND	R0,R7
	INC	R0
	JR	Z,$+3
	RET			; Exit if LSBs not FFFF
	LD	R1,FLAGS
	DECW	RR4		; Decrement MSBs
	LD	FLAGS,R1
	RET
;
INT:
	LD	R2,#hi(FPEXP)
	LD	R3,#lo(FPEXP)	; Point to exponent
	LDC	R0,@RR2		; Get exponent
	CP	R0,#80H+24	; Integer accuracy only?
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)
	LDC	R0,@RR8		; Get LSB
	JR	C,$+3
	RET			; Yes - Already integer
	LDC	R0,@RR2		; Get exponent
	CALL	FPINT		; F.P to integer
	LD	R1,#80H+24
	LDC	@RR2,R1		; Save 24 bit integer
	LD	R0,R7		; Get LSB of number
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save LSB
	LD	R0,R5		; Get MSB of number
	RLC	R0			; Sign to carry
	CALL	CONPOS		; Set sign of result
	POP	R0		; Restore LSB of number
	POP	R1
	LD	FLAGS,R1
	RET
;
MLDEBC:
	LD	R2,#hi(0)
	LD	R3,#lo(0)	; Clear partial product
	LD	R0,R4		; Test multiplier
	OR	R0,R5
	RCF
	JR	NZ,$+3
	RET			; Return zero if zero
	LD	R0,#16		; 16 bits
MLDBLP:
	ADD	R3,R3
	ADC	R2,R2		; Shift P.P left
	JP	C,BSERR		; ?BS Error if overflow
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	ADD	R3,R3
	ADC	R2,R2		; Shift multiplier left
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9
	JP	NC,NOMLAD	; Bit was zero - No add
	ADD	R3,R5
	ADC	R2,R4		; Add multiplicand
	JP	C,BSERR		; ?BS Error if overflow
NOMLAD:
	DEC	R0		; Count bits
	JP	NZ,MLDBLP	; More
	RET
;
ASCTFP:
	CP	R0,#'-'		; Negative?
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save it and flags
	JP	Z,CNVNUM	; Yes - Convert number
	CP	R0,#'+'		; Positive?
	JP	Z,CNVNUM	; Yes - Convert number
	DECW	RR2		; DEC 'cos GETCHR INCs
CNVNUM:
	CALL	RESZER		; Set result to zero
	LD	R4,R0		; Digits after point counter
	LD	R6,R0		; Sign of exponent
	LD	R7,R0		; Exponent of ten
	COM	R0
	LD	R5,R0		; Before or after point flag
MANLP:
	CALL	GETCHR		; Get next character
	JP	C,ADDIG		; Digit - Add to number
	CP	R0,#'.'
	JP	Z,DPOINT	; '.' - Flag point
	CP	R0,#'E'
	JP	NZ,CONEXP	; Not 'E' - Scale number
	CALL	GETCHR		; Get next character
	CALL	SGNEXP		; Get sign of exponent
EXPLP:
	CALL	GETCHR		; Get next character
	JP	C,EDIGIT	; Digit - Add to exponent
	INC	R6		; Is sign negative?
	JP	NZ,CONEXP	; No - Scale number
	XOR	R0,R0
	RCF
	SUB	R0,R7		; Negate exponent
	LD	R7,R0		; And re-save it
	INC	R5		; Flag end of number
DPOINT:
	INC	R5		; Flag point passed
	JP	Z,MANLP		; Zero - Get another digit
CONEXP:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R0,R7		; Get exponent
	SUB	R0,R4		; Subtract digits after point
SCALMI:
	JR	MI,$+5
	CALL	SCALPL		; Positive - Multiply number
	JP	PL,ENDCON	; Positive - All done
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save number of times to /10
	CALL	DIV10		; Divide by 10
	POP	R0		; Restore count
	POP	R1
	LD	FLAGS,R1
	INC	R0		; Count divides
;
ENDCON:
	JP	NZ,SCALMI	; More to do
	POP	R6
	POP	R7		; Restore code string address
	POP	R0		; Restore sign of number
	POP	R1
	LD	FLAGS,R1
	JR	NZ,$+5
	CALL	INVSGN		; Negative - Negate number
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Code string address to HL
	RET
;
SCALPL:
	JR	NZ,$+3
	RET			; Exit if no scaling needed
MULTEN:
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save count
	CALL	MLSP10		; Multiply number by 10
	POP	R0		; Restore count
	POP	R1
	LD	FLAGS,R1
	DEC	R0		; Count multiplies
	RET
;
ADDIG:
	PUSH	R7
	PUSH	R6		; Save sign of exponent
	LD	R6,R0		; Save digit
	LD	R0,R4		; Get digits after point
	ADC	R0,R5		; Add one if after point
	LD	R4,R0		; Re-save counter
	PUSH	R5
	PUSH	R4		; Save point flags
	PUSH	R3
	PUSH	R2		; Save code string address
	PUSH	R7
	PUSH	R6		; Save digit
	CALL	MLSP10		; Multiply number by 10
	POP	R0		; Restore digit
	POP	R1
	LD	FLAGS,R1
	SUB	R0,#'0'		; Make it absolute
	CALL	RSCALE		; Re-scale number
	POP	R2
	POP	R3		; Restore code string address
	POP	R4
	POP	R5		; Restore point flags
	POP	R6
	POP	R7		; Restore sign of exponent
	JP	MANLP		; Get another digit
;
RSCALE:
	CALL	STAKFP		; Put number on stack
	CALL	FLGREL		; Digit to add to FPREG
PADD:
	POP	R4
	POP	R5		; Restore number
	POP	R6
	POP	R7
	JP	FPADD		; Add BCDE to FPREG and return
;
EDIGIT:
	LD	R0,R7		; Get digit
	RL	R0		; Times 2
	RL	R0		; Times 4
	ADD	R0,R7		; Times 5
	RL	R0		; Times 10
	LDC	R8,@RR2
	ADD	R0,R8		; Add next digit
	SUB	R0,#'0'		; Make it absolute
	LD	R7,R0		; Save new digit
	JP	EXPLP		; Look for another digit
;
LINEIN:
	PUSH	R3
	PUSH	R2		; Save code string address
	LD	R2,#hi(INMSG)
	LD	R3,#lo(INMSG)	; Output " in "
	CALL	PRS		; Output string at HL
	POP	R2
	POP	R3		; Restore code string address
PRNTHL:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Code string address to DE
	XOR	R0,R0
	LD	R4,#80H+24	; 24 bits
	CALL	RETINT		; Return the integer
	LD	R2,#hi(PRNUMS)
	LD	R3,#lo(PRNUMS)	; Print number string
	PUSH	R3
	PUSH	R2		; Save for return
NUMASC:
	LD	R2,#hi(PBUFF)
	LD	R3,#lo(PBUFF)	; Convert number to ASCII
	PUSH	R3
	PUSH	R2		; Save for return
	CALL	TSTSGN		; Test sign of FPREG
	LD	R1,#' '
	LDC	@RR2,R1		; Space at start
	JP	PL,SPCFST	; Positive - Space to start
	LD	R1,#'-'
	LDC	@RR2,R1		; '-' sign at start
SPCFST:
	LD	R1,FLAGS
	INCW	RR2		; First byte of number
	LD	FLAGS,R1
	LD	R1,#'0'
	LDC	@RR2,R1		; '0' if zero
	JP	Z,JSTZER	; Return '0' if zero
	PUSH	R3
	PUSH	R2		; Save buffer address
	JR	PL,$+5
	CALL	INVSGN		; Negate FPREG if negative
	XOR	R0,R0
	RCF			; Zero A
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save it
	CALL	RNGTST		; Test number is in range
SIXDIG:
	LD	R4,#hi(9143H)
	LD	R5,#lo(9143H)	; BCDE - 99999.9
	LD	R6,#hi(4FF8H)
	LD	R7,#lo(4FF8H)
	CALL	CMPNUM		; Compare numbers
; OR	A
; JP	PO,INRNG	; > 99999.9 - Sort it out

	LD	R1,R0		; @@@
	RRC	R1
	RRC	R1
	RRC	R1
	RRC	R1
	XOR	R1,R0
	LD	R8,R1
	RRC	R1
	RRC	R1
	XOR	R1,R8
	LD	R8,R1
	RRC	R1
	XOR	R1,R8
	AND	R1,#01H
	JR	NZ,INRNG

	POP	R0		; Restore count
	POP	R1
	LD	FLAGS,R1
	CALL	MULTEN		; Multiply by ten
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Re-save count
	JP	SIXDIG		; Test it again
;
GTSIXD:
	CALL	DIV10		; Divide by 10
	POP	R0		; Get count
	POP	R1
	LD	FLAGS,R1
	INC	R0		; Count divides
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Re-save count
	CALL	RNGTST		; Test number is in range
INRNG:
	CALL	ROUND		; Add 0.5 to FPREG
	INC	R0
	CALL	FPINT		; F.P to integer
	CALL	FPBCDE		; Move BCDE to FPREG
	LD	R4,#hi(0306H)
	LD	R5,#lo(0306H)	; 1E+06 to 1E-03 range
	POP	R0		; Restore count
	POP	R1
	LD	FLAGS,R1
	ADD	R0,R5		; 6 digits before point
	INC	R0		; Add one
	JP	MI,MAKNUM	; Do it in 'E' form if < 1E-02
	CP	R0,#6+1+1	; More than 999999 ?
	JP	NC,MAKNUM	; Yes - Do it in 'E' form
	INC	R0		; Adjust for exponent
	LD	R4,R0		; Exponent of number
	LD	R0,#2		; Make it zero after
;
MAKNUM:
	DEC	R0		; Adjust for digits to do
	DEC	R0
	POP	R2
	POP	R3		; Restore buffer address
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save count
	LD	R6,#hi(POWERS)
	LD	R7,#lo(POWERS)	; Powers of ten
	DEC	R4		; Count digits before point
	JP	NZ,DIGTXT	; Not zero - Do number
	LD	R1,#'.'
	LDC	@RR2,R1		; Save point
	INCW	RR2		; Move on
	LD	R1,#'0'
	LDC	@RR2,R1	; Save zero
	INCW	RR2		; Move on
DIGTXT:
	DEC	R4		; Count digits before point
	LD	R1,#'.'
	LDC	@RR2,R1		; Save point in case
	JR	NZ,$+5
	CALL	INCHL		; Last digit - move on
	PUSH	R5
	PUSH	R4		; Save digits before point
	PUSH	R3
	PUSH	R2		; Save buffer address
	PUSH	R7
	PUSH	R6		; Save powers of ten
	CALL	BCDEFP		; Move FPREG to BCDE
	POP	R2
	POP	R3		; Powers of ten table
	LD	R4,#'0'-1	; ASCII '0' - 1
TRYAGN:
	INC	R4		; Count subtractions
	LD	R0,R7		; Get LSB
	LDC	R8,@RR2
	SUB	R0,R8		; Subtract LSB
	LD	R7,R0		; Save LSB
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LD	R0,R6		; Get NMSB
	LDC	R8,@RR2
	SBC	R0,R8		; Subtract NMSB
	LD	R6,R0		; Save NMSB
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LD	R0,R5		; Get MSB
	LDC	R8,@RR2
	SBC	R0,R8		; Subtract MSB
	LD	R5,R0		; Save MSB
	LD	R1,FLAGS
	DECW	RR2		; Point back to start
	DECW	RR2
	LD	FLAGS,R1
	JP	NC,TRYAGN	; No overflow - Try again
	CALL	PLUCDE		; Restore number
	LD	R1,FLAGS
	INCW	RR2		; Start of next number
	LD	FLAGS,R1
	CALL	FPBCDE		; Move BCDE to FPREG
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Save point in table
	POP	R2
	POP	R3		; Restore buffer address
	LDC	@RR2,R4		; Save digit in buffer
	INCW	RR2		; And move on
	POP	R4
	POP	R5		; Restore digit count
	DEC	R5		; Count digits
	JP	NZ,DIGTXT	; More - Do them
	DEC	R4		; Any decimal part?
	JP	Z,DOEBIT	; No - Do 'E' bit
SUPTLZ:
	DECW	RR2		; Move back through buffer
	LDC	R0,@RR2		; Get character
	CP	R0,#'0'		; '0' character?
	JP	Z,SUPTLZ	; Yes - Look back for more
	CP	R0,#'.'		; A decimal point?
	JR	Z,$+5
	CALL	INCHL		; Move back over digit
;
DOEBIT:
	POP	R0
	POP	R1		; Get 'E' flag
	LD	FLAGS,R1
	JP	Z,NOENED	; No 'E' needed - End buffer
	LD	R1,#'E'
	LDC	@RR2,R1		; Put 'E' in buffer
	LD	R1,FLAGS
	INCW	RR2		; And move on
	LD	FLAGS,R1
	LD	R1,#'+'
	LDC	@RR2,R1		; Put '+' in buffer
	JP	PL,OUTEXP	; Positive - Output exponent
	LD	R1,#'-'
	LDC	@RR2,R1		; Put '-' in buffer
	COM	R0		; Negate exponent
	INC	R0
OUTEXP:
	LD	R4,#'0'-1	; ASCII '0' - 1
EXPTEN:
	INC	R4		; Count subtractions
	SUB	R0,#10		; Tens digit
	JP	NC,EXPTEN	; More to do
	ADD	R0,#'0'+10	; Restore and make ASCII
	LD	R1,FLAGS
	INCW	RR2		; Move on
	LD	FLAGS,R1
	LDC	@RR2,R4		; Save MSB of exponent
JSTZER:
	LD	R1,FLAGS
	INCW	RR2		;
	LDC	@RR2,R0		; Save LSB of exponent
	INCW	RR2
	LD	FLAGS,R1
NOENED:
	LDC	@RR2,R5		; Mark end of buffer
	POP	R2
	POP	R3		; Restore code string address
	RET
;
RNGTST:
	LD	R4,#hi(9474H)
	LD	R5,#lo(9474H)	; BCDE = 999999.
	LD	R6,#hi(23F7H)
	LD	R7,#lo(23F7H)
	CALL	CMPNUM		; Compare numbers
	POP	R2
	POP	R3		; Return address to HL
; OR	A
; JP	PO,GTSIXD		; Too big - Divide by ten

	LD	R1,R0		; @@@
	RRC	R1
	RRC	R1
	RRC	R1
	RRC	R1
	XOR	R1,R0
	LD	R8,R1
	RRC	R1
	RRC	R1
	XOR	R1,R8
	LD	R8,R1
	RRC	R1
	XOR	R1,R8
	AND	R1,#01H
	JP	NZ,GTSIXD

	JP	@RR2		; Otherwise return to caller
;
HALF:
	DB	00H,00H,00H,80H	; 0.5
;
POWERS:
	DB	0A0H,086H,001H	; 100000
	DB	010H,027H,000H	; 10000
	DB	0E8H,003H,000H	; 1000
	DB	064H,000H,000H	; 100
	DB	00AH,000H,000H	; 10
	DB	001H,000H,000H	; 1
;
NEGAFT:
	LD	R2,#hi(INVSGN)
	LD	R3,#lo(INVSGN)	; Negate result
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; To be done after caller
	JP	@RR2		; Return to caller
;
SQR:
	CALL	STAKFP		; Put value on stack
	LD	R2,#hi(HALF)
	LD	R3,#lo(HALF)	; Set power to 1/2
	CALL	PHLTFP		; Move 1/2 to FPREG
;
POWER:
	POP	R4
	POP	R5		; Get base
	POP	R6
	POP	R7
	CALL	TSTSGN		; Test sign of power
	LD	R0,R4		; Get exponent of base
	JP	Z,EXP		; Make result 1 if zero
	JP	PL,POWER1	; Positive base - Ok
	OR	R0,R0
	RCF			; Zero to negative power?
	JP	Z,DZERR		; Yes - ?/0 Error
POWER1:
	OR	R0,R0
	RCF			; Base zero?
	JP	Z,SAVEXP	; Yes - Return zero
	PUSH	R7
	PUSH	R6		; Save base
	PUSH	R5
	PUSH	R4
	LD	R0,R5		; Get MSB of base
	OR	R0,#01111111B
	RCF			; Get sign status
	CALL	BCDEFP		; Move power to BCDE
	JP	PL,POWER2	; Positive base - Ok
	PUSH	R7
	PUSH	R6		; Save power
	PUSH	R5
	PUSH	R4
	CALL	INT		; Get integer of power
	POP	R4
	POP	R5		; Restore power
	POP	R6
	POP	R7
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; MSB of base
	CALL	CMPNUM		; Power an integer?
	POP	R2
	POP	R3		; Restore MSB of base
	LD	R0,R2		; but don't affect flags
	RRC	R0		; Exponent odd or even?
POWER2:
	POP	R2
	POP	R3		; Restore MSB and exponent
	LD	R8,#hi(FPREG+2)
	LD	R9,#lo(FPREG+2)

;	LDC	@RR8,R2
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	@RR8,R3		; Save base in FPREG
	LDC	@RR8,R3		; @@@SWAP HL
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R2		; Save base in FPREG

	POP	R2
	POP	R3		; LSBs of base
	LD	R8,#hi(FPREG)
	LD	R9,#lo(FPREG)

;	LDC	@RR8,R2
;	LD	R1,FLAGS
;	INCW	RR8
;	LD	FLAGS,R1
;	LDC	@RR8,R3		; Save in FPREG
	LDC	@RR8,R3		; @@@ SWAP HL
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R2		; Save in FPREG

	JR	NC,$+5
	CALL	NEGAFT		; Odd power - Negate result
	JR	NZ,$+5
	CALL	INVSGN		; Negative base - Negate it
	PUSH	R7
	PUSH	R6		; Save power
	PUSH	R5
	PUSH	R4
	CALL	LOG		; Get LOG of base
	POP	R4
	POP	R5		; Restore power
	POP	R6
	POP	R7
	CALL	FPMULT		; Multiply LOG by power
;
EXP:
	CALL	STAKFP		; Put value on stack
	LD	R4,#hi(08138H)
	LD	R5,#lo(08138H)	; BCDE = 1/Ln(2)
	LD	R6,#hi(0AA3BH)
	LD	R7,#lo(0AA3BH)
	CALL	FPMULT		; Multiply value by 1/LN(2)
	LD	R8,#hi(FPEXP)
	LD	R9,#lo(FPEXP)
	LDC	R0,@RR8		; Get exponent
	CP	R0,#80H+8		; Is it in range?
	JP	NC,OVTST1	; No - Test for overflow
	CALL	INT		; Get INT of FPREG
	ADD	R0,#80H		; For excess 128
	ADD	R0,#2		; Exponent > 126?
	JP	C,OVTST1	; Yes - Test for overflow
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save scaling factor
	LD	R2,#hi(UNITY)
	LD	R3,#lo(UNITY)	; Point to 1.
	CALL	ADDPHL		; Add 1 to FPREG
	CALL	MULLN2		; Multiply by LN(2)
	POP	R0		; Restore scaling factor
	POP	R1
	LD	FLAGS,R1
	POP	R4
	POP	R5		; Restore exponent
	POP	R6
	POP	R7
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save scaling factor
	CALL	SUBCDE		; Subtract exponent from FPREG
	CALL	INVSGN		; Negate result
	LD	R2,#hi(EXPTAB)
	LD	R3,#lo(EXPTAB)	; Coefficient table
	CALL	SMSER1		; Sum the series
	LD	R6,#hi(0)
	LD	R7,#lo(0)	; Zero LSBs
	POP	R4
	POP	R5		; Scaling factor
	LD	R5,R6		; Zero MSB
	JP	FPMULT		; Scale result to correct value
;
EXPTAB:
	DB	8			; Table used by EXP
	DB	040H,02EH,094H,074H	; -1/7! (-1/5040)
	DB	070H,04FH,02EH,077H	;  1/6! ( 1/720)
	DB	06EH,002H,088H,07AH	; -1/5! (-1/120)
	DB	0E6H,0A0H,02AH,07CH	;  1/4! ( 1/24)
	DB	050H,0AAH,0AAH,07EH	; -1/3! (-1/6)
	DB	0FFH,0FFH,07FH,07FH	;  1/2! ( 1/2)
	DB	000H,000H,080H,081H	; -1/1! (-1/1)
	DB	000H,000H,000H,081H	;  1/0! ( 1/1)
;
SUMSER:
	CALL	STAKFP		; Put FPREG on stack
	LD	R6,#hi(MULT)
	LD	R7,#lo(MULT)	; Multiply by "X"
	PUSH	R7
	PUSH	R6		; To be done after
	PUSH	R3
	PUSH	R2		; Save address of table
	CALL	BCDEFP		; Move FPREG to BCDE
	CALL	FPMULT		; Square the value
	POP	R2
	POP	R3		; Restore address of table
SMSER1:
	CALL	STAKFP		; Put value on stack
	LDC	R0,@RR2		; Get number of coefficients
	INCW	RR2		; Point to start of table
	CALL	PHLTFP		; Move coefficient to FPREG
	JR	SUMLP1		; Skip "POP AF"
SUMLP:
	POP	R0		; Restore count
	POP	R1
	LD	FLAGS,R1
SUMLP1:
	POP	R4
	POP	R5		; Restore number
	POP	R6
	POP	R7
	DEC	R0		; Cont coefficients
	JR	NZ,$+3
	RET			; All done
	PUSH	R7
	PUSH	R6		; Save number
	PUSH	R5
	PUSH	R4
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save count
	PUSH	R3
	PUSH	R2		; Save address in table
	CALL	FPMULT		; Multiply FPREG by BCDE
	POP	R2
	POP	R3		; Restore address in table
	CALL	LOADFP		; Number at HL to BCDE
	PUSH	R3
	PUSH	R2		; Save address in table
	CALL	FPADD		; Add coefficient to FPREG
	POP	R2
	POP	R3		; Restore address in table
	JP	SUMLP		; More coefficients
;
RND:
	CALL	TSTSGN		; Test sign of FPREG
	LD	R2,#hi(SEED+2)
	LD	R3,#lo(SEED+2)	; Random number seed
	JP	MI,RESEED	; Negative - Re-seed
	LD	R2,#hi(LSTRND)
	LD	R3,#lo(LSTRND)	; Last random number
	CALL	PHLTFP		; Move last RND to FPREG
	LD	R2,#hi(SEED+2)
	LD	R3,#lo(SEED+2)	; Random number seed
	JR	NZ,$+3
	RET			; Return if RND(0)
	LDC	R8,@RR2
	ADD	R0,R8		; Add (SEED)+2)
	AND	R0,#00000111B	; 0 to 7
	LD	R4,#0
	LDC	@RR2,R0		; Re-save seed
	INCW	RR2		; Move to coefficient table
	ADD	R0,R0		; 4 bytes
	ADD	R0,R0		; per entry
	LD	R5,R0		; BC = Offset into table
	ADD	R3,R5
	ADC	R2,R4		; Point to coefficient
	CALL	LOADFP		; Coefficient to BCDE

	LD	R8,R4		; @@@ SWAP BC
	LD	R4,R5
	LD	R5,R8
	LD	R8,R6		; @@@ SWAP DE
	LD	R6,R7
	LD	R7,R8

	CALL	FPMULT	;	; Multiply FPREG by coefficient
	LD	R8,#hi(SEED+1)
	LD	R9,#lo(SEED+1)
	LDC	R0,@RR8		; Get (SEED+1)
	INC	R0		; Add 1
	AND	R0,#00000011B	; 0 to 3
	LD	R4,#0
	CP	R0,#1		; Is it zero?
	ADC	R0,R4		; Yes - Make it 1
	LD	R8,#hi(SEED+1)
	LD	R9,#lo(SEED+1)
	LDC	@RR8,R0		; Re-save seed
	LD	R2,#hi(RNDTAB-4)
	LD	R3,#lo(RNDTAB-4); Addition table
	ADD	R0,R0		; 4 bytes
	ADD	R0,R0		; per entry
	LD	R5,R0		; BC = Offset into table
	ADD	R3,R5
	ADC	R2,R4		; Point to value
	CALL	ADDPHL		; Add value to FPREG
RND1:
	CALL	BCDEFP		; Move FPREG to BCDE
	LD	R0,R7		; Get LSB
	LD	R7,R5		; LSB = MSB
	XOR	R0,#01001111B	; Fiddle around
	LD	R5,R0		; New MSB
	LD	R1,#80H
	LDC	@RR2,R1		; Set exponent
	DECW	RR2		; Point to MSB
	LDC	R4,@RR2		; Get MSB
	LD	R1,#80H
	LDC	@RR2,R1		; Make value -0.5
	LD	R2,#hi(SEED)
	LD	R3,#lo(SEED)	; Random number seed
	LDC	R8,@RR2
	INC	R8
	LDC	@RR2,R8		; Count seed
	LDC	R0,@RR2		; Get seed
	SUB	R0,#171		; Do it modulo 171
	JP	NZ,RND2		; Non-zero - Ok
	LDC	@RR2,R0		; Zero seed
	INC	R5		; Fillde about
	DEC	R6		; with the
	INC	R7		; number
RND2:
	CALL	BNORM		; Normalise number
	LD	R2,#hi(LSTRND)
	LD	R3,#lo(LSTRND)	; Save random number
	JP	FPTHL		; Move FPREG to last and return
;
RESEED:
	LDC	@RR2,R0		; Re-seed random numbers
	DECW	RR2
	LDC	@RR2,R0
	DECW	RR2
	LDC	@RR2,R0
	JP	RND1		; Return RND seed
;
RNDTAB:
	DB	068H,0B1H,046H,068H	; Table used by RND
	DB	099H,0E9H,092H,069H
	DB	010H,0D1H,075H,068H
;
COS:
	LD	R2,#hi(HALFPI)
	LD	R3,#lo(HALFPI)	; Point to PI/2
	CALL	ADDPHL		; Add it to PPREG
SIN:
	CALL	STAKFP		; Put angle on stack
	LD	R4,#hi(8349H)
	LD	R5,#lo(8349H)	; BCDE = 2 PI
	LD	R6,#hi(0FDBH)
	LD	R7,#lo(0FDBH)
	CALL	FPBCDE		; Move 2 PI to FPREG
	POP	R4
	POP	R5		; Restore angle
	POP	R6
	POP	R7
	CALL	DVBCDE		; Divide angle by 2 PI
	CALL	STAKFP		; Put it on stack
	CALL	INT		; Get INT of result
	POP	R4
	POP	R5		; Restore number
	POP	R6
	POP	R7
	CALL	SUBCDE		; Make it 0 <= value < 1
	LD	R2,#hi(QUARTR)
	LD	R3,#lo(QUARTR)	; Point to 0.25
	CALL	SUBPHL		; Subtract value from 0.25
	CALL	TSTSGN		; Test sign of value
	SCF			; Flag positive
	JP	PL,SIN1		; Positive - Ok
	CALL	ROUND		; Add 0.5 to value
	CALL	TSTSGN		; Test sign of value
	OR	R0,R0
	RCF			; Flag negative
SIN1:
	LD	R1,FLAGS
	PUSH	R1
	PUSH	R0		; Save sign
	JR	MI,$+5
	CALL	INVSGN		; Negate value if positive
	LD	R2,#hi(QUARTR)
	LD	R3,#lo(QUARTR)	; Point to 0.25
	CALL	ADDPHL		; Add 0.25 to value
	POP	R0
	POP	R1		; Restore sign
	LD	FLAGS,R1
	JR	C,$+5
	CALL	INVSGN		; Negative - Make positive
	LD	R2,#hi(SINTAB)
	LD	R3,#lo(SINTAB)	; Coefficient table
	JP	SUMSER		; Evaluate sum of series
;
HALFPI:
	DB	0DBH,00FH,049H,081H	; 1.5708 (PI/2)
;
QUARTR:
	DB	000H,000H,000H,07FH	; 0.25
;
SINTAB:
	DB	5			; Table used by SIN
	DB	0BAH,0D7H,01EH,086H	; 39.711
	DB	064H,026H,099H,087H	;-76.575
	DB	058H,034H,023H,087H	; 81.602
	DB	0E0H,05DH,0A5H,086H	;-41.342
	DB	0DAH,00FH,049H,083H	; 6.2832
;
TAN:
	CALL	STAKFP		; Put angle on stack
	CALL	SIN		; Get SIN of angle
	POP	R4
	POP	R5		; Restore angle
	POP	R2
	POP	R3
	CALL	STAKFP		; Save SIN of angle
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; BCDE = Angle
	CALL	FPBCDE		; Angle to FPREG
	CALL	COS		; Get COS of angle
	JP	DIV		; TAN = SIN / COS
;
ATN:
	CALL	TSTSGN		; Test sign of value
	JR	PL,$+5
	CALL	INVSGN		; Negate result after if -ve
	JR	PL,$+5
	CALL	INVSGN		; Negate value if -ve
	LD	R8,#hi(FPEXP)
	LD	R9,#lo(FPEXP)
	LDC	R0,@RR8		; Get exponent
	CP	R0,#81H		; Number less than 1?
	JP	C,ATN1		; Yes - Get arc tangnt
	LD	R4,#hi(8100H)
	LD	R5,#lo(8100H)	; BCDE = 1
	LD	R6,R5
	LD	R7,R5
	CALL	DVBCDE		; Get reciprocal of number
	LD	R2,#hi(SUBPHL)
	LD	R3,#lo(SUBPHL)	; Sub angle from PI/2
	PUSH	R3
	PUSH	R2		; Save for angle > 1
ATN1:
	LD	R2,#hi(ATNTAB)
	LD	R3,#lo(ATNTAB)	; Coefficient table
	CALL	SUMSER		; Evaluate sum of series
	LD	R2,#hi(HALFPI)
	LD	R3,#lo(HALFPI)	; PI/2 - angle in case > 1
	RET		; Number > 1 - Sub from PI/2
;
ATNTAB:
	DB	9			; Table used by ATN
	DB	04AH,0D7H,03BH,078H	; 1/17
	DB	002H,06EH,084H,07BH	;-1/15
	DB	0FEH,0C1H,02FH,07CH	; 1/13
	DB	074H,031H,09AH,07DH	;-1/11
	DB	084H,03DH,05AH,07DH	; 1/9
	DB	0C8H,07FH,091H,07EH	;-1/7
	DB	0E4H,0BBH,04CH,07EH	; 1/5
	DB	06CH,0AAH,0AAH,07FH	;-1/3
	DB	000H,000H,000H,081H	; 1/1
;

ARET:
	RET			; A RETurn instruction
;
GETINP:
	CALL	CONIN		;input a character
	RET
;
CLS:
	LD	R0,#CS		; ASCII Clear screen
	JP	MONOUT		; Output character
;
WIDTH:
	CALL	GETINT		; Get integer 0-255
	LD	R0,R7		; Width to A
	LD	R8,#hi(LWIDTH)
	LD	R9,#lo(LWIDTH)
	LDC	@RR8,R0		; Set width
	RET
;
LINES:
	CALL	GETNUM		; Get a number
	CALL	DEINT		; Get integer -32768 to 32767
	LD	R8,#hi(LINESC)
	LD	R9,#lo(LINESC)
	LDC	@RR8,R6
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R7		; Set lines counter
	LD	R8,#hi(LINESN)
	LD	R9,#lo(LINESN)
	LDC	@RR8,R6
	LD	R1,FLAGS
	INCW	RR8
	LD	FLAGS,R1
	LDC	@RR8,R7		; Set lines number
	RET
;
DEEK:
	CALL	DEINT		; Get integer -32768 to 32767
	PUSH	R7
	PUSH	R6		; Save number
	POP	R2
	POP	R3		; Number to HL
	LDC	R4,@RR2		; Get LSB of contents
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	R0,@RR2		; Get MSB of contents
	JP	ABPASS		; Return integer AB
;
DOKE:
	CALL	GETNUM		; Get a number
	CALL	DEINT		; Get integer -32768 to 32767
	PUSH	R7
	PUSH	R6		; Save address
	CALL	CHKSYN		; Make sure ',' follows
	DB	','
	CALL	GETNUM		; Get a number
	CALL	DEINT		; Get integer -32768 to 32767
	POP	R8
	POP	R9
	PUSH	R3
	PUSH	R2
	LD	R2,R8
	LD	R3,R9		; Save value,get address
	LDC	@RR2,R7		; Save LSB of value
	LD	R1,FLAGS
	INCW	RR2
	LD	FLAGS,R1
	LDC	@RR2,R6		; Save MSB of value
	POP	R2
	POP	R3		; Restore code string address
	RET
;

; HEX$(nn) Convert 16 bit number to Hexadecimal string
;
HEX:
	CALL	TSTNUM		; Verify it's a number
	CALL	DEINT		; Get integer -32768 to 32767
	PUSH	R5
	PUSH	R4		; Save contents of BC
	LD	R2,#hi(PBUFF)
	LD	R3,#lo(PBUFF)
	LD	R0,R6		; Get high order into A
	CP	R0,#0
	JR	Z,HEX2		; Skip output if both high digits are zero
	CALL	BYT2ASC		; Convert D to ASCII
	LD	R0,R4
	CP	R0,#'0'
	JR	Z,HEX1		; Don't store high digit if zero
	LDC	@RR2,R4		; Store it to PBUFF
	INCW	RR2		; Next location
HEX1:
	LDC	@RR2,R5		; Store C to PBUFF+1
	INCW	RR2		; Next location
HEX2:
	LD	R0,R7		; Get lower byte
	CALL	BYT2ASC		; Convert E to ASCII
	LD	R0,R6
	CP	R0,#0
	JR	NZ,HEX3		; If upper byte was not zero then always print lower byte
	LD	R0,R4
	CP	R0,#'0'		; If high digit of lower byte is zero then don't print
	JR	Z,HEX4
HEX3:
	LDC	@RR2,R4		; to PBUFF+2
	INCW	RR2		; Next location
HEX4:
	LDC	@RR2,R5		; to PBUFF+3
	INCW	RR2		; PBUFF+4 to zero
	XOR	R0,R0		; Terminating character
	LDC	@RR2,R0		; Store zero to terminate
	INCW	RR2		; Make sure PBUFF is terminated
	LDC	@RR2,R0		; Store the double zero there
	POP	R4
	POP	R5		; Get BC back
	LD	R2,#hi(PBUFF)
	LD	R3,#lo(PBUFF)	; Reset to start of PBUFF
	JP	STR1		; Convert the PBUFF to a string and return it
;
BYT2ASC:
	LD	R4,R0		; Save original value
	AND	R0,#0FH		; Strip off upper nybble
	CP	R0,#0AH		; 0-9?
	JR	C,ADD30		; If A-F, add 7 more
	ADD	R0,#07H		; Bring value up to ASCII A-F
ADD30:
	ADD	R0,#30H		; And make ASCII
	LD	R5,R0		; Save converted char to C
	LD	R0,R4		; Retrieve original value
	RR	R0		; and Rotate it right
	RR	R0
	RR	R0
	RR	R0
	AND	R0,#0FH		; Mask off upper nybble
	CP	R0,#0AH		; 0-9? < A hex?
	JR	C,ADD301	; Skip Add 7
	ADD	R0,#07H		; Bring it up to ASCII A-F
ADD301:
	ADD	R0,#30H		; And make it full ASCII
	LD	R4,R0		; Store high order byte
	RET
;
; Convert "&Hnnnn" to FPREG
; Gets a character from (HL) checks for Hexadecimal ASCII numbers "&Hnnnn"
; Char is in A, NC if char is;<=>?@ A-z, CY is set if 0-9
HEXTFP:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Move code string pointer to DE
	LD	R2,#hi(0000H)
	LD	R3,#lo(0000H)	; Zero out the value
	CALL	GETHEX		; Check the number for valid hex
	JP	C,HXERR		; First value wasn't hex, HX error
	JR	HEXLP1		; Convert first character
HEXLP:
	CALL	GETHEX		; Get second and addtional characters
	JR	C,HEXIT		; Exit if not a hex character
HEXLP1:
	ADD	R3,R3
	ADC	R2,R2		; Rotate 4 bits to the left
	ADD	R3,R3
	ADC	R2,R2
	ADD	R3,R3
	ADC	R2,R2
	ADD	R3,R3
	ADC	R2,R2
	OR	R0,R3
	RCF			; Add in D0-D3 into L
	LD	R3,R0		; Save new value
	JR	HEXLP		; And continue until all hex characters are in
;
GETHEX:
	INCW	RR6		; Next location
	LDC	R0,@RR6		; Load character at pointer
	CP	R0,#' '
	JP	Z,GETHEX	; Skip spaces
	SUB	R0,#30H		; Get absolute value
	JR	NC,$+3
	RET			; < "0", error
	CP	R0,#0AH
	JR	C,NOSUB7	; Is already in the range 0-9
	SUB	R0,#07H		; Reduce to A-F
	CP	R0,#0AH		; Value should be $0A-$0F at this point
	JR	NC,$+3
	RET			; CY set if was < = > ? @
NOSUB7:
	CP	R0,#10H		; > Greater than "F"?
	CCF
	RET			; CY set if it wasn't valid hex

HEXIT:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Value into DE, Code string into HL
	LD	R0,R6		; Load DE into AC
	LD	R5,R7		; For prep to
	PUSH	R3
	PUSH	R2
	CALL	ACPASS		; ACPASS to set AC as integer into FPREG
	POP	R2
	POP	R3
	RET
;
HXERR:
	LD	R7,#HX		; ?HEX Error
	JP	ERROR
;
; BIN$(NN) Convert integer to a 1-16 char binary string
BIN:
	CALL	TSTNUM		; Verify it's a number
	CALL	DEINT		; Get integer -32768 to 32767
BIN2:
	PUSH	R5
	PUSH	R4		; Save contents of BC
	LD	R2,#hi(PBUFF)
	LD	R3,#lo(PBUFF)
	LD	R4,#17		; One higher than max char count
ZEROSUP:			; Suppress leading zeros
	DEC	R4		; Max 16 chars
	LD	R0,R4
	CP	R0,#01H
	JR	Z,BITOUT	; Always output at least one character
	RLC	R7
	RLC	R6
	JR	NC,ZEROSUP
	JR	BITOUT2
BITOUT:
	RLC	R7
	RLC	R6		; Top bit now in carry
BITOUT2:
	LD	R0,#'0'		; Char for '0'
	ADC	R0,#0		; If carry set then '0' --> '1'
	LDC	@RR2,R0
	INCW	RR2
	DEC	R4
	JR	NZ,BITOUT
	XOR	R0,R0
	RCF			; Terminating character
	LDC	@RR2,R0		; Store zero to terminate
	INCW	RR2		; Make sure PBUFF is terminated
	LDC	@RR2,R0		; Store the double zero there
	POP	R4
	POP	R5
	LD	R2,#hi(PBUFF)
	LD	R3,#lo(PBUFF)
	JP	STR1
;
; Convert "&Bnnnn" to FPREG
; Gets a character from (HL) checks for Binary ASCII numbers "&Bnnnn"
BINTFP:
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Move code string pointer to DE
	LD	R2,#hi(0000H)
	LD	R3,#lo(0000H)	; Zero out the value
	CALL	CHKBIN		; Check the number for valid bin
	JP	C,BINERR	; First value wasn't bin, HX error
BINIT:
	SUB	R0,#'0'
	ADD	R3,R3
	ADC	R2,R2		; Rotate HL left
	OR	R0,R3
	RCF
	LD	R3,R0
	CALL	CHKBIN		; Get second and addtional characters
	JR	NC,BINIT	; Process if a bin character
	LD	R8,R6
	LD	R9,R7
	LD	R6,R2
	LD	R7,R3
	LD	R2,R8
	LD	R3,R9		; Value into DE, Code string into HL
	LD	R0,R6		; Load DE into AC
	LD	R5,R7		; For prep to
	PUSH	R3
	PUSH	R2
	CALL	ACPASS		; ACPASS to set AC as integer into FPREG
	POP	R2
	POP	R3
	RET
;
; Char is in A, NC if char is 0 or 1
CHKBIN:
	LD	R1,FLAGS
	INCW	RR6
	LD	FLAGS,R1
	LDC	R0,@RR6
	CP	R0,#' '
	JP	Z,CHKBIN	; Skip spaces
	CP	R0,#'0'		; Set C if < '0'
	JR	NC,$+3
	RET
	CP	R0,#'2'
	CCF			; Set C if > '1'
	RET
;
BINERR:
	LD	R7,#BN		; ?BIN Error
	JP	ERROR
;
JJUMP1:
	JP	CSTART		; Go and initialise
;
MONOUT:
	JP	CONOUT		; output a char
;
MONITR:
	JP	0000H		; Restart (Normally Monitor Start)
;
INITST:
	LD	R0,#0		; Clear break flag
	LD	R8,#hi(BRKFLG)
	LD	R9,#lo(BRKFLG)
	LDC	@RR8,R0
	JP	INIT
;
OUTNCR:
	CALL	OUTC		; Output character in A
	JP	PRCRLF		; Output CRLF
;
	END
