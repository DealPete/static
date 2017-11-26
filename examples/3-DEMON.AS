This is a disassembly done by hand of the first few functions of 3-DEMON.EXE.

{prelude b s}:
PUSH BP
MOV BP, SP
ADD BP, b
SUB SP, s

{postlude b}
LEA SP, [BP-b]
POP BP
RET

do_nothing: (499:000f)
{prelude 4 4}
{postlude 4}

(0000:1283)
{prelude 4 ?}
call do_nothing
XOR DX, DX
PUSH DX
CALL 487:00e7
{postlude 4}

(487:00e7)
{prelude 6 ?}
PUSH [BP+0]
XOR DX, DX
PUSH DX
MOV DX, 98e6h
PUSH DX
MOV DX, fadah
PUSH DX
CALL <5d74>

<5d74>
{prelude c c}
MOV DX, 000ah
PUSH DX
CALL <5e40>
MOV DX, [BP]

<5e40>
CMP [992a], 0
JNE +21
	MOV BX, SP
	MOV BX, [BX+4]
	NEG BX
	ADD BX, BP
	LEA AX, [BP+2]
	MOV [992a], AX
	MOV AX, [BX-2]
	MOV [992c], AX
	MOV AX, [BX]
	MOV BX, [BX+2]
	MOV [992e], AX
	MOV [9930], BX
RET 0002

6ce0:

MOV AX, fde4h
MOV DS, AX
MOV [9942], ES
MOV DX, 8570h
SHR DX
MOV CX, 8000h
SUB CX, DX
SHR DX
SHR DX
SHR DX
INC DX
ADD DX, AX
MOV BX, 2
MOV BP, [ES:BX]
MOV BX, DX
ADD DX, 1000h
CMP DX, BP
JLE +2
	MOV DX, BP
SUB DX, 1000h
STD
MOV DS, AX
MOV SI, fffeh
MOV ES, DX
MOV DI, SI
REPZ MOVSW
MOV DS, DX
CLI
MOV SS, DX
MOV SP, 0x8770
STI
MOV [9938h], SP
SUB BP, BP
MOV [992ah], BP
MOV [9920h], BP
MOV [9924h], BP
MOV [9926h], BP
MOV [9928h], BP
SUB BX, DX
SHL BX
SHL BX
SHL BX
SHL BX
MOV [9932h], BX
MOV [9934h], BX
MOV [BX], 0x0001
ADD BX, +02
MOV [9936h], BX
ADD BX, 0180h
MOV [993ah], BX
CALL do_nothing
CALL do_nothing
CALL 0000:1283
