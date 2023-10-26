# Z8_NASCOM_BASIC

Nascom BASICはマイクロソフトが作成し、Grant Searleさんがサブセットに改編し、鈴木哲哉さんが移植されたコードを元に、奥江聡が8080/Z80からZ8にコンバートしました。  

ファイルは原作者の宣言にしたがってご利用ください。  

https://github.com/vintagechips/emuz80/tree/main  
こちらのemuz80/examples/EMUBASIC/にあるEMUBASIC.ASM ソースコードを元にしてコード変換を行いました。

ターゲット EMUZ80-Z8 / MEZZ8  
https://github.com/satoshiokue/EMUZ80-Z8/tree/main
アセンブラ Macro Assembler 1.42  

BASICのMONITOR命令で0000hへジャンプします。

# メモリーマップ
0000H ROM  
8000H RAM  
  
E000H UART Data Registor  
E001H UART Control Registor  

# 8080/Z80 から Z8 への変換ルール

UART通信のルーチンはUniversal MonitorのSBZ8用を流用しました。  
https://electrelic.com/electrelic/node/1317  

FOR命令、GOSUB命令でスタック上の変数領域を節約するためPUSHしたあとにINC SPを実行して1バイトのブロックマーカーを格納していました。Z8のPUSH/POPは1バイト単位なのでブロックマーカーだけをPUSH/POPするように変更しました。    

IN命令、OUT命令は無効にしました。  

```
——————————————————
レジスタ対応
R0   A
R2   H
R3   L
R4   B
R5   C
R6   D
R7   E
RR2  HL
RR4  BC
RR6  DE

R1   PUSH/POP命令時のフラグ一時保存
R8   2バイトレジスタの一時保存
R9   2バイトレジスタの一時保存
——————————————————
条件ジャンプ
JP C	  JP C    Carry
JP NC	  JP NC   Not Carry
JP Z	  JP Z    Zero
JP NZ	  JP NZ   Not Zero
JP P	  JP PL   Plus
JP M	  JP MI   Minus
——————————————————
PUSH	HL

PUSH	R3
PUSH	R2
——————————————————
POP	HL

POP	R2
POP	R3
——————————————————
PUSH	AF

LD	R1,FLAGS
PUSH	R1
PUSH	R0
——————————————————
POP	AF

POP	R0
POP	R1
LD	FLAGS,R1
——————————————————
INC	HL

LD	R1,FLAGS
INCW	RR2
LD	FLAGS,R1
——————————————————
DEC	HL

LD	R1,FLAGS
DECW	RR2
LD	FLAGS,R1
——————————————————
LD	HL,PBUFF

LD	R2,#hi(PBUFF)
LD	R3,#lo(PBUFF)
——————————————————
LD	A(HL)

LDC	R0,@RR2
——————————————————
LD	HL,(CUROPR)

LD	R8,#hi(CUROPR)
LD	R9,#lo(CUROPR)
LDC	R2,@RR8
LD	R1,FLAGS
INCW	RR8
LD	FLAGS,R1
LDC	R3,@RR8
——————————————————
ADD	A,(HL)

LDC	R8,@RR2
ADD	R0,R8
——————————————————
ADD	HL,BC

ADD	R3,R5
ADC	R2,R4
——————————————————
INC	(HL)

LDC	R8,@RR2
INC	R8
LDC	@RR2,R8
——————————————————
INC	SP

ADD	SPL,#1
ADC	SPH,#0
——————————————————
EX	DE,HL

LD	R8,R6
LD	R9,R7
LD	R6,R2
LD	R7,R3
LD	R2,R8
LD	R3,R9
——————————————————
EX	(SP),HL

POP	R8
POP	R9
PUSH	R3
PUSH	R2
LD	R2,R8
LD	R3,R9
——————————————————
OR	A
JP	PO,INRNG

LD	R1,R0
RR	R1
RR	R1
RR	R1
RR	R1
XOR	R1,R0
LD	R8,R1
RR	R1
RR	R1
XOR	R1,R8
LD	R8,R1
RR	R1
XOR	R1,R8
AND	R1,#01H
JR	NZ,INRNG
——————————————————
OR	A
JP	PE,FCERR

LD	R1,R0
RR	R1
RR	R1
RR	R1
RR	R1
XOR	R1,R0
LD	R8,R1
RR	R1
RR	R1
XOR	R1,R8
LD	R8,R1
RR	R1
XOR	R1,R8
AND	R1,#01H
JP	Z,FCERR
——————————————————
JP	(HL)

JP	@RR2
——————————————————
RET C

JR	NC,$+3
RET
——————————————————
CALL	Z,NEGAFT

JR	NC,$+5
CALL	NEGAFT
——————————————————


```
