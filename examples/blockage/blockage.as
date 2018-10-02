-- This prelude copies the whole program to ES:1602h.

CMP SP, 2991
JA +2
	INT 20	-- terminate program
MOV CX, 132f
MOV SI, 142f
MOV DI, 2931
MOV BX, 8000
STD
REPZ MOVS
CLD
XCHG SI, DI
SUB SI, -38
PUSH DI
PUSH DI
JMP +27b5 (<12d5> in copied program)

-- The following code decompresses the game by gradually writing
-- over both copies, ending just before this code in the second copy.

<12d4>
MOVSB
<12d5>
CALL <1312>
JC <12d4>
INC CX
<12db>
CALL <130d>
JCXZ <131b>
JNC <12db>
SUB CX, 3
JC +6
	MOV CL, AL
	LODSB
	NOT AX
	XCHG BP, AX
XOR CX, CX
CALL <130d>
ADC CX, CX
JNZ +8
	INC CX
		CALL <130d>
	JNC -5
CMP BP, f300
ADC CX, 1
LEA AX, [BP+DI]
XCHG SI, AX
REPZ MOVSB
XCHG SI, AX
JMP 12d4

<130d>
CALL <1312>
ADC CX, CX
<1312>
ADD BX, BX
JNZ +4
	LODSW
	ADC AX, AX
	XCHG AX, BX
RET

<131b>
POP SI
MOV CX, 40
		LODSB
		SUB AL -18
		CMP AL, 1
	JA -7
	ROL [SI], 8
	SUB [SI], SI
	LODSW
LOOP
RET
