JMP <2b>

<2b>
CALL <ac>
CALL <10c>
CALL <136>
MOV AH, 4c
INT 21

<ac>
CLD
MOV AX, a000
MOV ES, AX
IN AL, 40
MOV <85>, AL
MOV <87>, ff
MOV <88>, 00 (is player one a computer?)
MOV <89>, 00 (is player two a computer?)
MOV <8a>, 12
MOV <8b>, 0
MOV <8c>, 0
MOV <8d>, ff
MOV AH, 8
XOR BH, BH
INT 10
MOV <86>, AH (cursor attribute)
MOV AX, 11
INT 10 (Graphics  80x30  8x16  640x480  mono)
MOV DX, 20
CALL <451>
MOV DX, 138
CALL <458>
MOV AX, ffff
MOV DI, 500
MOV CX, 140
REPZ STOSW
MOV DI, 9380
MOV CX, 140
REPZ STOSW
CALL <58C>
RET

<10c>
	CMP <8c>, ff
	JE +18
		CALL <14d>
		CALL <20d>
		CALL <592>
		CALL <5c5>
		CALL <31e>
		CALL <485>
	CALL <5f8>
	CMP <8b>, 0
JE -29
RET

<136>
MOV AX, 3
INT 10
MOV AX, 600
MOV BH, <86>
XOR CX, CX
MOV DX, 184f
INT 10
CALL <58c>
RET

<14d>
CMP <87>, 0
JE +40
	MOV <8e>, FF
	MOV AL, FF
	MOV CL, 01
	CALL <52c>
	JZ +2
		NEG AL
	MOV <92>, AL
	MOV AL, 1a
	CALL <30d>
	MOV <94>, AL (player one Y position)
	MOV BL, 95
	MOV <95>, 0 (player one X position)
	MOV <96>, AL
	MOV <97>, AL (player two Y position)
	MOV <98>, 4f (player two X position)
	MOV <99>, 1
	MOV <9a>, 0
	MOV <9b>, 0
	CALL <195>
	MOV <87>, 0
RET

<195> (draw screen)
XOR DX, DX
CALL <451>
MOV DX, 149
CMP <88>, 0
JE +3
	ADD DX, d
PUSH DX
CALL <458>
MOV DX, 38
CALL <451>
MOV DX, 170
MOV AL, <88>
AND AL, <89>
JE +3
	ADD DX, 5
CALL <458>
MOV DX, 44
CALL <451>
POP DX
CMP <89>, 00
JE +3
	ADD DX, d
CALL <458>
XOR AX, AX
MOV DI, 780
MOV CX, 4600
REPZ STOSW
CALL <1eb>
CALL <45d>
CALL <471>
RET

<1eb>
MOV AL, <88>
OR AL, <89>
JNZ +18
	MOV DX, 400
	CALL <451>
	MOV DX, 886
	CALL <458>
	MOV DX, 1ae
	CALL <451>
	MOV DX, b5a
	CALL <458>
RET

<20d>
CMP <8e>, 0
JE +3a
	MOV <93>, 0
	MOV AL, <97>
	INC AL
	MOV <8f>, AL
	MOV AL, <98>
	DEC AL
	MOV <90>, AL
	CMP <92>, ff
	JE +10
		MOV AL, <94>
		INC AL
		MOV <8f>, AL
		MOV AL, <95>
		INC AL
		MOV <90>, AL
	CALL <24f>
	CALL <261>
	CALL <26e>
	MOV <8e>, 0
RET

<24f>
MOV <91>, ff
MOV CL, 1
CALL <52c>
JZ +5
	MOV <91>, 1
RET

<261>
MOV CX, 4
CALL <52c>
JNE +5
	MOV <91>, 0
RET

<26e>
CMP <92>, ff
JNE <28a>
CMP <88>, ff
JE <29d>
CALL <29e>
CALL <2d8>
CALL <30d>
MOV <96>, AL
JMP <<29d>
<28a>
CMP <89>, ff
JE <29d>
...
<29d>
RET

<29e>
MOV DL, <8f>
MOV BL, <91>
CMP BL, 0
JE +2a
	MOV AL, 4f
	DEC AL
	<2af>
	CMP AL, 37
	JBE +17
		CMP BL, 1
		JE <2be>
		SUB AL, DL
		XOR DL, DL
		JMP <2c6> 
		<2be>
		MOV CL, 37
		SUB CL, DL
		SUB AL, CL
		MOV DL, 37
		<2c6>
		NEG DL
		JMP <2af>
	DEC AL
	CMP BL, 1
	JE <2d7>
	MOV DL, 37
	SUB CL, BL
MOV AL, DL
<2d7>
RET

<2d8>
CMP <73>, 48
JE +2d
	MOV CX, 2
	CMP <73>, 4d
	JE +3
		ADD CX, 2
	PUSH CX
	MOV CL, 2
	CALL <52c>
	JE +15
		...
	...
RET

<30d>
CMP AL, 00
JE +c
	MOV BL, AL
	DEC BL
	CMP AL, 37
	JNE +2
		DEC BL
	MOV AL, BL
RET

<31e>
MOV <94>, AL
CMP AL, <96>
JE +12
	MOV BL, ff
	JNS +2
		NEG BL
	PUSH BX
	CALL <45d>
	POP BX
	ADD <94>, BL
	CALL <45d>
RET

<33a>
MOV AL, <97>
CMP AL, <99>
JE +12
	...
RET

<356>
CMP <90>, 0
JE +22
	CMP <90>, 4f
	JE +1b
		MOV <93>, 0
		CALL <3ac>
		CALL <3d4>
		MOV AL, <91>
		ADD <8f>, AL
		MOV AL, <92>
		ADD <90>, AL
		JMP <3ab>
...
<3ab>
RET

<3ac>
CMP <91>, 0
JE <3d3>
CMP <91>, ff
JNE <3c3>
CMP <8f>, 0
JE <3ca>
JMP <3d3>
<3c3>
CMP <8f>, 37
JNE <3d3>
<3ca>
NEG CH
XCHG CX, AX
CMP <93>, ff
<3d3>
RET

<3d4>
CMP <92>, ff
JNE <3ec>
MOV AL, <95>
INC AL
CMP <90>, AL
JNE <450>
MOV BL, <94>
JMP <3fb>
<3ec>
...
<3fb>
...
<450>
RET

<451> (move cursor to (DH, DL))
MOV AH, 2
XOR BX, BX
INT 10
RET

<458> (display string at DS:DX)
MOV AH, 9
INT 21
RET

<45d> (draw player one)
MOV SI, <9c>
MOV AL, <94>
MOV BL, <95>
CALL <54c>
MOV CX, 3
CALL <4b3>
RET

<471> (draw player two)
MOV SI, <9c>
MOV AL, <97>
MOV BL, <98>
CALL <54c>
MOV CX, 3
CALL <4b3>
RET

<485>
CMP <93>, 0
JE +3
	CALL <72d>
CALL <49f>
MOV <8a>, CL
XOR CH, CH
CALL <55c>
CALL <49f>
RET

<49f> (draw the ball) 
MOV SI, <a4>
MOV AL, <8f>
MOV BL, <90>
CALL <54c>
MOV CX, 1
CALL <4b3>
RET

<4b3> (draw the 8*CX by 8 image stored at SI...)
	PUSH CX
	PUSH SI
	MOV CX, 8
		LODSB
		XOR [ES:DI], AL
		(CS) ADD DI, 50
	LOOP
	POP SI
	POP CX
LOOP
RET

<52c>
PUSH AX
	IN 40
	CMP <85>, AL
JE -8
MOV <85>, AL
SHR AL
MOV BL, 1
XOR CH, CH
DEC CX
JCXZ +7
	SHL BL
	OR BL, 1
	LOOP
AND BL, AL
POP AX
RET

<54c> (DI <- 280*(AL+3) + BL)
ADD AL, 3
XOR AH, AH
XOR BH, BH
MOV DX, 280
MUL DX
ADD AX, BX
MOV DI, AX
RET

<55c>
IN 61
AND AL, fe
OUT 61
	MOV AL, b0
	OUT 43
	MOV AL, ff
	OUT 42
	OUT 42
	IN 61
	OR AL, 1
	OUT 61
		MOV AL, 80
		OUT 43
		IN 42
		XCHG AH, AL
		CMP AX, fb57
	JA -11
	IN 61
	AND AL, fe
	OUT 61
LOOP
RET

<58c> (clear keyboard buffer)
MOV AX, c00
INT 21
RET

<592>
CMP <88>, 0
JE +2b
	...
RET

<5c5>
CMP <89>, 0
JE +2b
	MOV AH, 12
	INT 16
	...
RET

<5f8> (keyboard routine)
MOV DL, ff
MOV AH, 6
INT 21
JZ +6d
	MOV BL, AL
	AND BL, df
	CMP AL, 1b
	JE <638>
	CMP BL, 51
	JE <63d>
	CMP BL, 50
	JE <642>
	CMP BL, 53
	JE <647>
	CMP AL, 2d
	JE <64c>
	CMP AL, 3d
	JE <650>
	CMP BL, 52
	JE <656>
	CMP BL, 49
	JE <65b>
	CMP AL, 31
	JE <660>
	CMP AL, 32
	JE <665>
	CMP AL, 0
	JE <66a>
	JMP <66d>
	...
	<660>
	CALL <6e8>
	JMP <66d>
	<66a>
	CALL <58c>
<66d>
RET

<6e8>
MOV AL, <88>
OR AL, <89>
JNZ +12
	MOV <87>, ff
	MOV <88>, 0
	MOV <89>, ff
	CALL <720>
RET

<720>
CMP <8c>, 0
JE +5
	MOV <8c>, 0
RET
