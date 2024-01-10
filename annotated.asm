### NOTES ###

;find the part that saves the ACCU to one of the STO memories
;then find where that is called from, i.e. when in the roll mode and the display flips over

;also suspect asm80 isn't running because it disables the read input using byte 4062 when it's busy doing "something else...?"

	ROM0 - 0x0000 to 0x07FF (0x0800 - 0x0FFF duplicated from lower half)
	ROM1 - 0x1000 to 0x17FF (0x1800 - 0x1FFF duplicated from lower half)
	ROM2 - 0x2000 to 0x27FF (0x2800 - 0x2FFF duplicated from lower half)
	ROM3 - 0x3000 to 0x3FFF (A11 is tied to +5V on ROM0, ROM1, ROM2 but connected properly to ROM3 so the full 4k is avilable)

	FREE ROM MEMORY:
		0x17e4 to 0x17fa		;22 bytes
		0x37f4 to 0x37fa		;6 bytes
		0x3951 to 0x3bcf		;638 bytes
		0x3fee to 0x3ffa		;12 bytes

		although, if getting rid of ROM3/IEC interface, 
		would have 0x3000 to 0x3fff			;4095 bytes
	
	would like to take the ROM's out of scope#2 and see if they are the same without the IEC mod
	
	SELECTABLE x10 PROBE MOD:
		!PA0, !PA1, !PB0, !PB1 are checked @ $20a8
		stored in RAM 4050 & 4051 and processed and stored in RAM 4002 & 4005 (ref. hardware memory map)
		if !PA0 = 0 & !PA1 = 1, a 10x probe is connected
		if !PA0 = !PA1, no 10x probe is connected
		
		this mode could be entered by selecting UNCALA/B, before pressing the A/B ON/OFF button
		or toggled by UNCALA/B on/off <--- prefer this tbh
		would need to find a free piece of RAM to store whether we are in x10 mode or not (could use the stack, before it has a chance to write over the stored switch settings)
		reset when the channel is selected off
		swap $194 & $197 but change the CALL address to the new ROM code ($27f5 just returns)
	
	CONTINOUS ROLL MOD:
		just need some code to pick up RAM 4050 & 4051
		process to see if !ROLL and !RECURR are selected at the same time 
		and when the run light goes out, soft press the clear button to reset $4057
	
	RMS MOD:
		a seperate processor/RAM storage processes the storage memories in the background
		the 8085 can select the expansion card to take control of the data bus in order to generate symbology
		or it may be possible to do all this on A4, using maybe STO4 as a memory/symbol storage display
		would require replacing ROM3 with custom ROM
		
		FEATURES
			periodoscity time
			frequency
			Vpp
			Vp (ref. selected when entering mode, maybe it grounds it automatically, takes a reference, then resumes)
			Vrms
	
	order some ROM sized ZIF sockets for prototyping / programming ROMs
	order some +12v power supplys

### CODE ORDER ###

START
CHECK SERVICE JUMPERS

ROM 0 to 3 TEST
	ROM TEST

POST ROM TEST
ALPHA-NUMERIC TEST
POST ALPHA-NUMERIC TEST

A4 RAM TEST
	PART OF RAM TEST

MAIN LOOP
	LOADS A21 SWITCH SETTINGS AND COMPARES TO RAM
		LOAD AND COMPARE SWITCH SETTINGS TO RAM
	ACCU DISPLAY LOOP / save?
	
	STO DISPLAY LOOPS / save?
	
	DELAY LOOP
	
	JMP MAIN LOOP

### ROM 0 ###
	$0    | MVI A, $59			;$ -> A
	$2    | SIM					;set interrupt mask <- A		(SOE, R7.5, MSE, R5.5 set)
	$3    | EI					;enable interrupts
	$4    | LXI SP, $4100		;$$ -> SP						(stack pointer = $4100
	$7    | LXI H, $8020		;$$ -> HL						(HL pair = 8020)
	$a    | MVI M, $80			;$ -> $HL						(addr 8020 = $80 (p.55, D414 0b1000 0000 = !CLR, !REM, !NOT TRIGGERED, ZEN, !RUNL, BOL, INV, CLDT))
	$c    | LXI H, $8040		;$$ -> HL						(HL pair = 8040)
	$f    | MVI M, $f			;$ -> $HL						(addr 8040 = A6 somewhere...)
	$11   | LXI H, $c0ff		;$$ -> HL						(HL pair = c0ff)
	$14   | JMP $0040			;JMP $$							(check service jumpers)

	$17   | LXI H, $80af		;$$ -> HL						(loop called when TRAP activated)
	$1a   | MVI B, $10			;$ -> B
	$1c   | MVI M, $45			;$ -> $HL
	$1e   | DCX H				;HL--
	$1f   | DCR B				;B--
	$20   | JNZ $001c			;Zf = 0 : JMP $$
	$23   | HLT
	$24   | JMP $0017			;JMP $$							(TRAP vector)

	$27   | LDA $405f			;$$ -> A
	$2a   | ANI $3				;A &= $
	$2c   | CALL $3002			;CALL $$						(RST 5.5 vector - used by IEC-bus interface)
	$2f   | RET					;return

	$30   | MOV B, B			;B -> B
	$31   | JNZ $07a3			;Zf = 0 : JMP $$
	$34   | JMP $3000			;JMP $$							(RST 6.5 vector)

	$37   | DSUB				;undocumented double subtract (HL = HL - BC)								the reason the ROM checksum wasn't working!
	$38   | JMP $0000			;JMP $$

	$3b   | RLC					;A rotate left
	$3c   | JMP $26da			;JMP $$							(RST 7.5 vector)

	$3f   | MOV A, B			;B -> A
	
		### CHECK SERVICE JUMPERS ###
	$40   | SHLD $80b0			;HL -> $$						(80b0 = b0, 80b1 = 80 : A2 - unknown...
	$43   | SHLD $80b3			;HL -> $$						(80b3 = b3, 80b4 = 80 : A2 - unknown...
	$46   | MVI A, $13			;$ -> A							(A = $13)
	$48   | STA $8060			;A -> $$						(8060 = 13 : D1221 P mode, sets timebase mode)
	***   |
	$4b   | LDA $8025			;$$ -> A						(A = !PLOT, 0V, !DOTS, !A/B, !CLEAR, !LOCK, !SERV2, !SERV1)
	$4e   | ANI $3				;A &= $							(A = !SERV1, !SERV2 - check service jumpers)
	$50   | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0

		//00 - service 3		;Cf = 1, Zf = 0
		//01 - service 2		;Cf = 0, Zf = 1

	$52   | JC $26da			;Cf = 1 : JMP $$				(SERVICE 3)
	$55   | JZ $272f			;Zf = 1 : JMP $$				(SERVICE 2)
	$58   | CPI $2				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0

		//10 - service 1		;Cf = 0, Zf = 1
		//11 - normal			;Cf & Zf = 0

	$5a   | JZ $27aa			;Zf = 1 : JMP $$				(SERVICE 1)
	***   |														(NORMAL - START OF ROM TEST)
		### ROM 0 TEST ###
	$5d   | LXI H, $0000		;$$ -> HL						(HL = $593E)
	$60   | MVI B, $11			;$ -> B							(B = $11 0b0001 0001)
	$62   | JMP $011f			;JMP $$							

		### ROM 1 TEST ###
	$65   | LXI H, $1000		;$$ -> HL						(HL = $0747)
	$68   | MVI B, $33			;$ -> B							(B = $33 0b0011 0011)
	$6a   | JMP $011f			;JMP $$

		### ROM 2 TEST ###
	$6d   | LXI H, $2000		;$$ -> HL						(HL = $513A)
	$70   | MVI B, $77			;$ -> B							(B = $77 0b0111 0111)
	$72   | JMP $011f			;JMP $$

		### ROM 3 TEST ###
	$75   | LXI H, $3000		;$$ -> HL						(HL = $4009 if installed, not sure what you'd read if it wasn't installed...)
	$78   | MVI B, $ff			;$ -> B							(B = $ff 0b1111 1111)
	$7a   | JMP $011f			;JMP $$
	
		### POST ROM TEST ###
	$7d   | MVI A, $3f			;$ -> A							(A = $3f)
	$7f   | STA $80b4			;A -> $$						(80b4 UNCB, UNCA, LROL, LREC, LB10, LB1,  LA10, LA1 - switch on some front panel lights)
	***   |														       0     0     1     1     1     1     1     1
	$82   | LXI D, $1400		;$$ -> DE						(DE = $1400)
	$85   | MVI A, $4f			;$ -> A							(A  = $4f)
	$87   | JMP $016d			;JMP $$							(fill alpha-numeric displays with the value stored in A 'O')
			(returns here)
	$8a   | MVI A, $2a			;$ -> A
	$8c   | JMP $016d			;JMP $$							(fill alpha-numeric displays with the value stored in A '*')
			(returns here)
	$8f   | MVI A, $2e			;$ -> A
	$91   | JMP $016d			;JMP $$							(fill alpha-numeric displays with the value stored in A '.')
			(returns here)
			
		### POST ALPHA-NUMERIC TEST (not sure what it does) ###
	$94   | SHLD $c0ff			;HL -> $$						(HL = $80B0 -> stored @ $c0ff)
	***   |															
	$97   | DCX D				;DE--							(DE = $13FF - > $13FE)
	$98   | MOV A, E			;E -> A							(A  = FF)
	$99   | ORA D				;A |= D							(A = FF)
	$9a   | JNZ $0085			;Zf = 0 : JMP $$				(jumps to start of POST ROM TEST repeats from DE $13FE to DE $0000 - but all it does is write $80b0 to $c0ff (A6 DAC's))

		### A4 RAM TEST ###
	$9d   | LXI H, $4040		;$$ -> HL						(HL = 4040)
	***   |
	$a0   | MOV M, A			;A -> $HL						(RAM 4040 = 0x00)
	$a1   | RLC					;A rotate left					(A = 0)
	$a2   | INR L				;L++
	$a3   | JNZ $00a0			;Zf = 0 : JMP $$				(loads 0x00 into RAM -> $4040 - $40ff
	***   |
	$a6   | MVI A, $0			;$ -> A							(A = 0)
	$a8   | LXI H, $4040		;$$ -> HL						(HL = 4040)
	$ab   | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cy & Zf = 0
	$ac   | JNZ $0183			;Zf = 0 : JMP $$				(if RAM 0x00 doesn't match A 0x00, jump to RAM FAULT)
	$af   | RLC					;A rotate left
	$b0   | INR L				;L++
	$b1   | JNZ $00ab			;Zf = 0 : JMP $$				(loop through 4040 - 40FF RAM)
	***   |
	$b4   | LXI H, $4040		;$$ -> HL
	$b7   | MVI M, $0			;$ -> $HL
	$b9   | INR L				;L++
	$ba   | JNZ $00b7			;Zf = 0 : JMP $$				(loop through 4040 - 40FF RAM, writing 0 to RAM again)
	***   														### SET 0x4000 to 0x403b to 0 ###
	$bd   | LXI H, $4000		;$$ -> HL						(RAM start address 4000)
	$c0   | MVI B, $b			;$ -> B							(B = 0x0b)
	$c2   | CALL $0159			;CALL $$						(writes 0 to 0x4000 + 0x0b if there is nothing in RAM already)
	***	  |
	$c5   | LXI H, $400c		;$$ -> HL
	$c8   | MVI B, $b			;$ -> B
	$ca   | CALL $0159			;CALL $$						(writes 0 to 0x400c - 0x4017 if there is nothing in RAM already)
	***   |
	$cd   | LXI H, $4018		;$$ -> HL
	$d0   | MVI B, $b			;$ -> B
	$d2   | CALL $0159			;CALL $$						(writes 0 to 0x4018 - 0x4023 if there is nothing in RAM already)
	***   |
	$d5   | LXI H, $4024		;$$ -> HL
	$d8   | MVI B, $b			;$ -> B
	$da   | CALL $0159			;CALL $$						(writes 0 to 0x4024 - 0x402f if there is nothing in RAM already)
	***   |
	$dd   | LXI H, $4030		;$$ -> HL
	$e0   | MVI B, $f			;$ -> B
	$e2   | CALL $0159			;CALL $$						(writes 0 to 0x4030 - 0x403f if there is nothing in RAM already)
	***   														### SET 0x407e to 0x07, SET 0x4058 = 0x4008, 0x4059 = 0x4009, 0x405a = 0x400a ###
	$e5   | MVI A, $7			;$ -> A							(A = 0x07)
	$e7   | STA $407e			;A -> $$						(RAM 407e = 0x07)
	$ea   | LXI H, $4008		;$$ -> HL						(HL = 4008)
	$ed   | LXI D, $4058		;$$ -> DE						(DE = 4058)
	$f0   | MVI B, $3			;$ -> B							(B = 0x03)
	$f2   | MOV A, M			;$HL -> A						(A = RAM 4008 = x)
	$f3   | STAX D				;A -> $DE						(RAM 4058 = x)
	$f4   | INX H				;HL++							(HL = 4009)
	$f5   | INX D				;DE++							(DE = 4057)
	$f6   | DCR B				;B--							(B = 0x02)
	$f7   | JNZ $00f2			;Zf = 0 : JMP $$
	***   |
	$fa   | MVI A, $17			;$ -> A							(A = 17)
	$fc   | STA $4062			;A -> $$						(RAM 4062 = 17)
	$ff   | STA $8020			;A -> $$						($8020 = !CLR, !REM, !NOT TRIGD, ZEN, !RUNL, BOL, INV, CLDT)
	***   |                                                               0     0         0       1     0     1    1    1
	$102  | MVI A, $10			;$ -> A							(A = 10)
	$104  | STA $4069			;A -> $$						(RAM 4069 = 10)
	$107  | STA $4082			;A -> $$						(RAM 4082 = 10)
	$10a  | STA $4080			;A -> $$						(RAM 4080 = 10)
	***	  | check IEC card ID
	$10d  | LDA $80e3			;$$ -> A						(A = 80e3 = p.122 IEC A14 - "Card identification" checks if optional interface has been fitted into X1402 (the serial port to a PM 3325 controller)
	$110  | ANI $8				;A &= $							
	$112  | CZ $3005			;Zf = 1 : CALL $$				(presumably if card fitted, jump here...)
	$115  | MVI A, $c0			;$ -> A							(A = 0xc0)
	$117  | SIM					;set interrupt mask <- A		(SOD, SOE set)
	$118  | MVI A, $48			;$ -> A							(A = 0x48)
	$11a  | SIM					;set interrupt mask <- A		(SOE, MSE set)
	$11b  | EI					;enable interrupts
	$11c  | JMP $0188			;JMP $$

		### ROM TEST ###
	$11f  | MOV A, B			;B -> A							(A = 11)
	$120  | CMA					;1's compliment A (invert)		(A = EE)
	$121  | STA $80b0			;A -> $$						($80b0 = SEL0 and DIS0 lights on)
	$124  | LXI D, $08fe		;$$ -> DE						(D = 0x8fe)
	$127  | XRA A				;A = A XOR A					(A = EE XOR 11 = FF)
	***   |
	$128  | XRA M				;A = A XOR $HL
	$129  | RLC					;A rotate left
	$12a  | INX H				;HL++
	$12b  | DCX D				;DE--
	$12c  | MOV C, A			;A -> C
	$12d  | MOV A, D			;D -> A
	$12e  | ORA A				;A |= A
	$12f  | MOV A, C			;C -> A
	$130  | JNZ $0128			;Zf = 0 : JMP $$
	***   |
	$133  | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cy & Zf = 0
	$134  | JNZ $0148			;Zf = 0 : JMP $$				(A == M, ROM TEST FAILED, jump)
	***   | select next ROM...
	$137  | MOV A, H			;H -> A
	$138  | CPI $17				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$13a  | JZ $006d			;Zf = 1 : JMP $$				(jump to ROM 2 TEST)
	$13d  | JC $0065			;Cf = 1 : JMP $$				(jump to ROM 1 TEST)
	$140  | CPI $37				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$142  | JZ $007d			;Zf = 1 : JMP $$				(continue...)
	$145  | JMP $0075			;JMP $$							(jump to ROM 3 TEST)

	$148  | DCX H				;HL--
	$149  | MOV A, M			;$HL -> A
	$14a  | ANI $f				;A &= $
	$14c  | CPI $5				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$14e  | JNC $0137			;Cf = 0 : JMP $$
	$151  | DCX H				;HL--
	$152  | MOV A, M			;$HL -> A
	$153  | CPI $55				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$155  | JNZ $0137			;Zf = 0 : JMP $$
	$158  | HLT													(ROM TEST FAILED)

		### PART OF RAM TEST ###
		### reads to see if there is something in RAM 0x4000 - 0x400b and returns if there is, otherwise it writes all 0's and returns) ###
	$159  | MOV D, L			;L -> D							(D = 00)
	$15a  | MOV C, B			;B -> C							(C = 0b)
	***   |															(B = 0b)
	$15b  | XRA A				;A = A XOR B					(A = 00 XOR 0b = 0b)
	$15c  | XRA M				;A = A XOR $HL					(A = RAM $4000 which is unknown...?)
	$15d  | RLC					;A rotate left
	$15e  | INX H				;HL++							(4001)
	$15f  | DCR B				;B--							(0x0a (loops 0x0b times 0x4000 - 0x400b))
	$160  | JNZ $015c			;Zf = 0 : JMP $$
	$163  | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cy & Zf = 0
	$164  | RZ					;Zf = 1 : RET					(if RAM 0x400b = A, return)
																(if RAM 0x400b != A, continue)
	$165  | MVI M, $0			;$ -> $HL						(RAM 0x400b - 0x4000 set to 0)
	$167  | DCX H				;HL--							(0x400a)
	$168  | DCR C				;C--							(0a (loops 0xb times (0x400b - 0x4000)
	$169  | JNZ $0165			;Zf = 0 : JMP $$
	$16c  | RET					;return

		### ALPHA-NUMERIC TEST ###
	$16d  | LXI H, $80a0		;$$ -> HL						(HL = $80a0)
	$170  | MVI B, $10			;$ -> B							(B = $10)
	****  |
	$172  | MOV M, A			;A -> $HL						
	$173  | INX H				;HL++
	$174  | DCR B				;B--
	$175  | JNZ $0172			;Zf = 0 : JMP $$
	****  |
	$178  | CPI $2e				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$17a  | JZ $0094			;Zf = 1 : JMP $$
	$17d  | JC $008f			;Cf = 1 : JMP $$
	$180  | JMP $008a			;JMP $$

		### RAM FAULT detected (infinite loop), would display "...." on alpha-numeric displays ###
	$183  | MOV B, M			;$HL -> B
	$184  | MOV M, A			;A -> $HL
	$185  | JMP $0183			;JMP $$


		### MAIN LOOP ###
	$188  | MVI A, $19			;$ -> A
	$18a  | SIM					;set interrupt mask <- A		(R7.5, MSE, M5.5 set)
	$18b  | EI					;enable interrupts
	$18c  | LXI SP, $4100		;$$ -> SP						(set stack pointer to 4100)
	
*** START MAIN LOOP ***
	$18f  | MVI B, $2			;$ -> B							(B = 0x02)
	$191  | CALL $01d3			;CALL $$						(sets !REM to 0, then to 1 - TRIGGER PULSE, START MAIN LOOP)	
	$194  | CALL $01de			;CALL $$						(loads A21 / some A202 switch settings, compares them to ram and if different updates the ram)
	$197  | CALL $27f5				;CALL $$ (27f5 = RET, returns)
	$19a  | CALL $0255			;CALL $$						(unknown ... includes PLOT)
*** ACCU display and possibly moves RAM contents to STO as required ***
	$19d  | CALL $038d			;CALL $$						(does some weird stuff with RAM...?, switches some lights?)
								;								(suspect manages all the storage memories etc. during this time)
	$1a0  | CALL $27f9				;CALL $$ (27f9 = RET, returns)

*** START DISPLAY LOOP ***
	$1a3  | MVI B, $10			;$ -> B							(B = 10)
	$1a5  | CALL $01d3			;CALL $$						(sets !RUNL to 0, then to 1 - TRIGGER PULSE, START DISPLAY LOOP)
	
*** displays STO1, STO2, STO3 - 2002, 4004, 8008 ***
*** part of it checks that SID / NDR from A9 has new data ready ***
*** other part of it calls 10d2 ***
*** accoding to asm80, it seems like the accu loop is called before this ***
	
	$1a8  | LXI B, $2002		;$$ -> BC						(BC = 2002)
	$1ab  | CALL $03d2			;CALL $$						(unknown...?)

	$1ae  | LXI B, $4004		;$$ -> BC
	$1b1  | CALL $03d2			;CALL $$

	$1b4  | LXI B, $8008		;$$ -> BC
	$1b7  | CALL $03d2			;CALL $$

*** START DELAY LOOP ***
	$1ba  | MVI B, $4			;$ -> B
	$1bc  | CALL $01d3			;CALL $$						(sets !NOT TRIG'D to 0, then to 1 -  - TRIGGER PULSE, START DELAY LOOP))
	
	$1bf  | CALL $03fb			;CALL $$						(not looked)
	$1c2  | CALL $0745			;CALL $$						(not looked)
	$1c5  | CALL $0591			;CALL $$						(not looked)
	
	$1c8  | LDA $80e3			;$$ -> A						(A = 80e3 = p.122 IEC A14 - "Card identification" checks if optional interface has been fitted into X1402 (the serial port to a PM 3325 controller)
	$1cb  | ANI $8				;A &= $
	$1cd  | CZ $3008			;Zf = 1 : CALL $$				(presumably if card fitted, or expansion device on network, jump here...)
	$1d0  | JMP $0188			;JMP $$							(maybe this is the main loop, and in some of those subroutines i've not looked in is the operating code)
### END MAIN LOOP ###

		### FLIPS BITS ON A4, D414 0x8020 ###
	$1d3  | LXI H, $8020		;$$ -> HL						(HL = 8020 *** called from 1a5)													(H = 8020) *** called from 0x191 ***
	$1d6  | LDA $4062			;$$ -> A						(A = 4062 = 17 (set @ 0x00fc))													(A = 4062 = 17 (set @ 0x00fc))
	$1d9  | XRA B				;A = A XOR B					(B = 10, A = 0x07)																(B = 0x02, A = 0x15)
	$1da  | MOV M, A			;A -> $HL						(8020 = 0x07 p.55 !CLR, !REM, !NOT TRIGD, ZEN, !RUNL, BOL, INV, CLDT)			(8020 = 0x15 p.55 !CLR, !REM, !NOT TRIGD, ZEN, !RUNL, BOL, INV, CLDT)
	****  |			    															0     0       0        0     0     1    1    1							        0     0        0       1     0     1    0    1
	$1db  | XRA B				;A = A XOR B					(A = 17)																		(0x02 xor 0x15 = 17)
	$1dc  | MOV M, A			;A -> $HL						(8020 = 0x17 p.55 !CLR, !REM, !NOT TRIGD, ZEN, !RUNL, BOL, INV, CLDT)			(8020 = 0x17 p.55 !CLR, !REM, !NOT TRIGD, ZEN, !RUNL, BOL, INV, CLDT)
	****  |																			0     0        0       1     0     1    1    1									0     0        0       1     0     1    1    1
	$1dd  | RET					;return

		### LOADS A21 SWITCH SETTINGS AND COMPARES TO RAM ###
*** loads 801e - 8023 into RAM 4050 - 4055 ***
	$1de  | LDA $4062			;$$ -> A						(A = 4062 = 17 (set @ 0x00fc)
	$1e1  | ANI $2				;A &= $							(A = 0x02)
	$1e3  | RZ					;Zf = 1 : RET					(if bit 1 of 4062 is set, continue)
	$1e4  | LXI H, $4050		;$$ -> HL						(HL = 4050)
	$1e7  | LXI D, $801e		;$$ -> DE						(DE = 801e)
	$1ea  | MVI C, $5			;$ -> C							(C = 5)
	$1ec  | LDA $405e			;$$ -> A						(A = 0)
	$1ef  | ORI $20				;A |= $							(A = 20)
	$1f1  | MOV B, A			;A -> B							(B = 20)
	$1f2  | CALL $0226			;CALL $$						*** (load and compare switch settings to ram (4050 = 801e))
	$1f5  | MOV A, B			;B -> A							(A = 80)
	$1f6  | ANI $20				;A &= $							(A = 0)
	$1f8  | JNZ $021c			;Zf = 0 : JMP $$				(skip to 21c)
		
	;if bit 5 of 405e is set...
	$1fb  | PUSH H				;HL -> SP
	$1fc  | LXI H, $03e8		;$$ -> HL
	$1ff  | SHLD $4083			;HL -> $$
	$202  | POP H				;STACK$$ -> HL
	$203  | LDA $4055			;$$ -> A						(load previous 8023 (not updated yet))
	$206  | CMA					;1's compliment A (invert)
	$207  | ANI $f0				;A &=f0							(if DIS1 selected)
	$209  | LDA $406d			;$$ -> A
	$20c  | JNZ $0214			;Zf = 0 : JMP $$				(if DIS1 selected, jump)
	$20f  | MVI A, $10			;$ -> A
	$211  | STA $406d			;A -> $$						(else set 406d to 0x10)
	$214  | ANI $10				;A &= $
	$216  | JZ $021c			;Zf = 1 : JMP $$				(if 406d = 10, jump})
	$219  | STA $4069			;A -> $$						(else set 4069 = A)
	****  |
	$21c  | MVI C, $3			;$ -> C							(C = 3)
	$21e  | CALL $0226			;CALL $$						(load and compare switch settings to ram (4056 - 4058 = 8024 - 8026?))
	$221  | MOV A, B			;B -> A							(A = B = $20)
	$222  | STA $405e			;A -> $$						(405e = $20)
	$225  | RET					;return

		### LOAD AND COMPAARE SWITCH SETTINGS TO RAM ###
	$226  | LDAX D				;$DE -> A						(A = $801e = A21 switch settings - !A-AC, !A-NUL, A-OFF, !ADD, !PA0, !PA1, !ERUN, RUN)
	$227  | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cy & Zf = 0	(HL 4050)
	$228  | JZ $023e			;Zf = 1 : JMP $$				(if front panel settings A21 801e match RAM 4050, jump 23e, else...)
	$22b  | MOV A, E			;E -> A							(A = 1e)
	$22c  | CPI $23				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0		( Cf = 1, Zf = 0 )
	$22e  | JZ $0245			;Zf = 1 : JMP $$
	$231  | CPI $24				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0		( same )
	$233  | JZ $024b			;Zf = 1 : JMP $$
	$236  | LDAX D				;$DE -> A						(A = 801e = A21 switch settings)
	$237  | MOV M, A			;A -> $HL						(4050 = A)
	$238  | MOV A, B			;B -> A							(A = 20)
	$239  | ANI $df				;A &= $							(A = 0)
	$23b  | ORI $80				;A |= $							(A = 80)
	$23d  | MOV B, A			;A -> B							(B = 80);
	****  |
	$23e  | INX H				;HL++							(HL = 4051)
	$23f  | INX D				;DE++							(DE = 801f)
	$240  | DCR C				;C--							(C = 4)
	$241  | JNZ $0226			;Zf = 0 : JMP $$				(when C = 0, jmp 226)
	$244  | RET					;return

$245  | LDAX D				;$DE -> A
$246  | ANI $fe				;A &= $
$248  | JMP $024e			;JMP $$

$24b  | LDAX D				;$DE -> A
$24c  | ANI $be				;A &= $
$24e  | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cy & Zf = 0
$24f  | JNZ $0237			;Zf = 0 : JMP $$
$252  | JMP $023e			;JMP $$

*** locked/unlocked, sets PDRS/timebase, A/B NUL switches and relays on A21 based on attenuator required ***
*** plus more, only just scratched the surface... ***
	;checks if locked or unlocked, then set's the PDRS / timebase mode
	$255  | CALL $07c6			;CALL $$
	$258  | LHLD $405e			;$$ -> HL			;H = $405f, L = $405e
	$25b  | MOV A, L			;L -> A
	$25c  | ANI $20				;A &= $
	
	;checks A/B-NUL switches, and sets the relays on A21 based on the attenuator position required
#	$25e  | JZ $029f			;Zf = 1 : JMP $$	;jump if bit 5 of 405f is set

$261  | MOV A, H			;H -> A
$262  | ANI $80				;A &= $
$264  | JNZ $026f			;Zf = 0 : JMP $$
$267  | MOV A, L			;L -> A
$268  | ANI $81				;A &= $
$26a  | CPI $81				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$26c  | JNZ $027a			;Zf = 0 : JMP $$
$26f  | MOV A, H			;H -> A
$270  | ANI $1				;A &= $
$272  | JNZ $0296			;Zf = 0 : JMP $$
$275  | PUSH H				;HL -> SP
****  | moves some switch settings into another section of RAM (+ probably does some other A21 switch settings processing)
$276  | CALL $2000			;CALL $$
$279  | POP H				;STACK$$ -> HL
$27a  | MOV A, L			;L -> A
$27b  | ANI $1				;A &= $
$27d  | JZ $0296			;Zf = 1 : JMP $$
$280  | MOV A, L			;L -> A
$281  | ANI $7e				;A &= $
$283  | STA $405e			;A -> $$
$286  | LXI H, $4008		;$$ -> HL
$289  | LXI D, $4058		;$$ -> DE
$28c  | MVI B, $3			;$ -> B
$28e  | LDAX D				;$DE -> A
$28f  | MOV M, A			;A -> $HL
$290  | INX D				;DE++
$291  | INX H				;HL++
$292  | DCR B				;B--
$293  | JNZ $028e			;Zf = 0 : JMP $$
$296  | LXI H, $4000		;$$ -> HL
$299  | MVI C, $b			;$ -> C
$29b  | CALL $21f5			;CALL $$
$29e  | RET					;return

;if bit 5 of 405f is set, jumps here from 025e
	$29f  | MVI A, $0			;$ -> A				
	$2a1  | STA $407a			;A -> $$				407a = 0
	$2a4  | CALL $1241			;CALL $$				;checks A/B-NUL switches, and sets the relays on A21 based on the attenuator position required
### here ###
$2a7  | LDA $4051			;$$ -> A
$2aa  | ANI $48				;A &= $
$2ac  | MOV C, A			;A -> C
$2ad  | CPI $48				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2af  | JNZ $02c2			;Zf = 0 : JMP $$
$2b2  | LDA $405f			;$$ -> A
$2b5  | ANI $3				;A &= $
$2b7  | STA $405f			;A -> $$
$2ba  | LDA $4062			;$$ -> A
$2bd  | ORI $10				;A |= $
$2bf  | STA $4062			;A -> $$
$2c2  | MOV A, C			;C -> A
$2c3  | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2c5  | JZ $02db			;Zf = 1 : JMP $$
$2c8  | LDA $405e			;$$ -> A
$2cb  | ANI $40				;A &= $
$2cd  | JZ $02f1			;Zf = 1 : JMP $$
$2d0  | CALL $1562			;CALL $$
$2d3  | LXI H, $4030		;$$ -> HL
$2d6  | MVI C, $f			;$ -> C
$2d8  | CALL $21f5			;CALL $$
$2db  | LDA $4060			;$$ -> A
$2de  | ANI $40				;A &= $
$2e0  | JZ $036f			;Zf = 1 : JMP $$
$2e3  | CALL $2679			;CALL $$
$2e6  | DAD H				;HL + HL -> HL
$2e7  | XCHG				;HL <-> DE
$2e8  | LXI H, $4067		;$$ -> HL
$2eb  | MOV M, D			;D -> $HL
$2ec  | INX H				;HL++
$2ed  | MOV M, E			;E -> $HL
$2ee  | CALL $17a5			;CALL $$
$2f1  | LDA $405f			;$$ -> A
$2f4  | MOV B, A			;A -> B
$2f5  | ANI $1				;A &= $
$2f7  | JNZ $032e			;Zf = 0 : JMP $$
$2fa  | LDA $4051			;$$ -> A
$2fd  | ANI $48				;A &= $
$2ff  | MOV C, A			;A -> C
$300  | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$302  | JNZ $0311			;Zf = 0 : JMP $$
$305  | MOV A, B			;B -> A
$306  | ANI $82				;A &= $
$308  | CZ $146c			;Zf = 1 : CALL $$
$30b  | CZ $17a5			;Zf = 1 : CALL $$
$30e  | CALL $1488			;CALL $$
$311  | MOV A, C			;C -> A
$312  | CPI $8				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$314  | JNZ $0320			;Zf = 0 : JMP $$
$317  | MOV A, B			;B -> A
$318  | ANI $10				;A &= $
$31a  | CZ $152f			;Zf = 1 : CALL $$
$31d  | CALL $14c7			;CALL $$
$320  | MOV A, C			;C -> A
$321  | ANA A				;A &= A
$322  | JNZ $032e			;Zf = 0 : JMP $$
$325  | MOV A, B			;B -> A
$326  | ANI $4				;A &= $
$328  | CZ $154a			;Zf = 1 : CALL $$
$32b  | CALL $14c7			;CALL $$
$32e  | MOV A, B			;B -> A
$32f  | ANI $c0				;A &= $
$331  | CPI $80				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
*** PLOT ***
$333  | CNZ $14ee			;Cf = 0 : CALL $$
$336  | LDA $4057			;$$ -> A
$339  | ANI $8				;A &= $
$33b  | MVI C, $1			;$ -> C
$33d  | MOV A, B			;B -> A
$33e  | JNZ $0348			;Zf = 0 : JMP $$
$341  | ANI $3				;A &= $
$343  | STA $405f			;A -> $$
$346  | MVI C, $10			;$ -> C
$348  | LDA $4062			;$$ -> A
$34b  | ANI $fe				;A &= $
$34d  | ORA C				;A |= C
$34e  | STA $4062			;A -> $$
$351  | STA $8020			;A -> $$
$354  | LDA $4056			;$$ -> A
$357  | RLC					;A rotate left
$358  | RLC					;A rotate left
$359  | CMA					;1's compliment A (invert)
$35a  | ANI $80				;A &= $
$35c  | MOV B, A			;A -> B
$35d  | LDA $4061			;$$ -> A
$360  | ANI $7f				;A &= $
$362  | ORA B				;A |= B
$363  | STA $4061			;A -> $$
$366  | LDA $405e			;$$ -> A
$369  | ANI $be				;A &= $
$36b  | STA $405e			;A -> $$
$36e  | RET					;return

$36f  | CALL $21ff			;CALL $$
$372  | MOV D, H			;H -> D
$373  | MOV E, L			;L -> E
$374  | DAD H				;HL + HL -> HL
$375  | DAD H				;HL + HL -> HL
$376  | DAD D				;HL + DE -> HL
$377  | LDA $4058			;$$ -> A
$37a  | ANI $f0				;A &= $
$37c  | MVI A, $3a			;$ -> A
$37e  | JZ $0386			;Zf = 1 : JMP $$
$381  | SUB L				;A -= L
$382  | MOV L, A			;A -> L
$383  | JMP $02e7			;JMP $$

$386  | MOV E, A			;A -> E
$387  | MVI D, $0			;$ -> D
$389  | DAD D				;HL + DE -> HL
$38a  | JMP $02e7			;JMP $$

		### ACCU display loop / save ACCU to STOx? ###
	$38d  | LDA $405f			;$$ -> A
	$390  | ANI $1				;A &= $
	$392  | JNZ $03c3			;Zf = 0 : JMP $$				(if 405f bit 0 = 0, jump (1 if locked, skip))
	****  |
	$395  | LDA $4056			;$$ -> A						(4056 = 400a = 0)8024 +5V, FASA, !Yx5, !SEL, !SAV3, !SAV2, !SAV1, DELTRIG
	$398  | CMA					;1's compliment A (invert)		(A = ff)
	$399  | ANI $e				;A &= $							(A = 0x0e)
	$39b  | JZ $03ab			;Zf = 1 : JMP $$				(continue) 		(checks to see if SAV1, SAV2, SAV3 are selected?)
	$39e  | MOV B, A			;A -> B							(B = e)
	$39f  | LDA $4063			;$$ -> A						(A = 0)
	$3a2  | ANI $f0				;A &= $							(A = 0)
	$3a4  | ORA B				;A |= B							(A = e)
	$3a5  | STA $4063			;A -> $$						(RAM 4063 = 0x0e)
	$3a8  | CALL $10ac			;CALL $$						(save ACCU? to STOx?)
	$3ab  | LXI H, $4065		;$$ -> HL						(HL = 4065)
	$3ae  | MOV A, M			;$HL -> A						(A = 0)
	$3af  | ANI $80				;A &= $							(A = 0)
	$3b1  | JZ $03c3			;Zf = 1 : JMP $$				(if nothing in ram, skip to 03c3)
	$3b4  | MOV A, M			;$HL -> A						
	$3b5  | ANI $f				;A &= $
	$3b7  | MOV M, A			;A -> $HL
	$3b8  | MOV B, A			;A -> B
	$3b9  | LDA $4063			;$$ -> A
	$3bc  | ORA M				;A |= $HL
	$3bd  | STA $4063			;A -> $$
	$3c0  | CALL $10ac			;CALL $$						(save ACCU? to STOx?)
	****  |
	$3c3  | MVI B, $10			;$ -> B
	$3c5  | LDA $4063			;$$ -> A
	$3c8  | ORI $f0				;A |= $
	$3ca  | XRA B				;A = A XOR B
	$3cb  | STA $4063			;A -> $$
	$3ce  | CALL $10e3			;CALL $$						(draw ACCU memory)
	$3d1  | RET					;return

		### STO display loop / save? ###
	$3d2  | MOV A, B			;B -> A							(A = 20)
	$3d3  | XRI $f0				;A = A XOR $					(A = d0)
	$3d5  | STA $4063			;A -> $$						(RAM 4063 = 0xd0, was 0x0e)
	$3d8  | LXI H, $405f		;$$ -> HL						(HL = 405f)
	$3db  | MOV A, M			;$HL -> A						(A = 0)
	$3dc  | ANI $69				;A &= $							(A = 0)
	$3de  | JNZ $03f7			;Zf = 0 : JMP $$				(continue) 	(if any bits 0, 3, 5, 6 = 1, jump)
	$3e1  | MOV A, M			;$HL -> A						(A = 0)
	$3e2  | ANI $80				;A &= $							(A = 0)
	$3e4  | JNZ $03ef			;Zf = 0 : JMP $$				(continue)	(if bit 7 = 1, jump)
	$3e7  | LDA $405e			;$$ -> A						(A = 0)
	$3ea  | ANI $1				;A &= $							(A = 0)
	$3ec  | JNZ $03f7			;Zf = 0 : JMP $$				(continue)
	$3ef  | RIM					;read interrupt mask -> A
	$3f0  | ANI $80				;A &= $							(&= SID (serial in))
	$3f2  | PUSH B				;BC -> STACK$$
	$3f3  | CNZ $101b			;Cf = 0 : CALL $$				(if SID = 1 | NDR (new data ready from A9)) 
	$3f6  | POP B				;STACK$$ -> BC
$3f7  | CALL $10d2			;CALL $$
$3fa  | RET					;return

### delay loop stuff ###
	$3fb  | LDA $405f			;$$ -> A
	$3fe  | ANI $81				;A &= $
	$400  | JNZ $0575			;Zf = 0 : JMP $$				;if 405f bits 7 and 0 are set, jump
		;sets 4070 bits 7 & 0 to 0
		;sets 406b bits 3, 2, 1 to 1
		;if 405f bit 7 is 0 returns here
		;else calls 23d3
			;sets 4058 - 405a, 407d, 406f - 4071, 4067 - 4068
		;returns here
$403  | LDA $406f			;$$ -> A
$406  | ANA A				;A &= A
$407  | JZ $0411			;Zf = 1 : JMP $$
$40a  | LDA $407d			;$$ -> A
$40d  | ANA A				;A &= A
$40e  | CZ $2242			;Zf = 1 : CALL $$
$411  | LXI H, $4052		;$$ -> HL
$414  | MOV A, M			;$HL -> A
$415  | ANI $2				;A &= $
$417  | MOV B, A			;A -> B
$418  | INX H				;HL++
$419  | INX H				;HL++
$41a  | MOV A, M			;$HL -> A
$41b  | RLC					;A rotate left
$41c  | RLC					;A rotate left
$41d  | ANI $4				;A &= $
$41f  | ORA B				;A |= B
$420  | MOV B, A			;A -> B
$421  | MOV A, M			;$HL -> A
$422  | RRC					;A rotate right
$423  | ANI $1				;A &= $
$425  | ORA B				;A |= B
$426  | MOV B, A			;A -> B
$427  | CPI $7				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$429  | JZ $0575			;Zf = 1 : JMP $$
$42c  | LDA $406b			;$$ -> A
$42f  | RRC					;A rotate right
$430  | MOV C, A			;A -> C
$431  | LDA $405e			;$$ -> A
$434  | ANI $20				;A &= $
$436  | JNZ $0450			;Zf = 0 : JMP $$
$439  | MOV A, C			;C -> A
$43a  | ANI $7				;A &= $
$43c  | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cy & Zf = 0
$43d  | JZ $0450			;Zf = 1 : JMP $$
$440  | MVI A, $2			;$ -> A
$442  | STA $407c			;A -> $$
$445  | STA $407b			;A -> $$
$448  | LDA $4070			;$$ -> A
$44b  | ANI $7f				;A &= $
$44d  | STA $4070			;A -> $$
$450  | LDA $407c			;$$ -> A
$453  | ANA A				;A &= A
$454  | JNZ $045f			;Zf = 0 : JMP $$
$457  | MOV A, C			;C -> A
$458  | ANI $f8				;A &= $
$45a  | ORA B				;A |= B
$45b  | RLC					;A rotate left
$45c  | STA $406b			;A -> $$
$45f  | LDA $406b			;$$ -> A
$462  | RRC					;A rotate right
$463  | ANI $7				;A &= $
$465  | MOV B, A			;A -> B
$466  | CPI $5				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$468  | MVI C, $0			;$ -> C
$46a  | JNZ $0477			;Zf = 0 : JMP $$
$46d  | LDA $4070			;$$ -> A
$470  | ANI $1				;A &= $
$472  | CZ $23fe			;Zf = 1 : CALL $$
$475  | MVI C, $1			;$ -> C
$477  | LDA $4070			;$$ -> A
$47a  | ANI $fe				;A &= $
$47c  | ORA C				;A |= C
$47d  | STA $4070			;A -> $$
$480  | MOV A, C			;C -> A
$481  | ANA A				;A &= A
$482  | JNZ $0512			;Zf = 0 : JMP $$
$485  | LDA $4070			;$$ -> A
$488  | ANI $80				;A &= $
$48a  | JZ $04ed			;Zf = 1 : JMP $$
$48d  | MOV A, B			;B -> A
$48e  | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$490  | CZ $23d3			;Zf = 1 : CALL $$
$493  | MOV A, B			;B -> A
$494  | CPI $4				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$496  | CZ $23d3			;Zf = 1 : CALL $$
$499  | MOV A, B			;B -> A
$49a  | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$49c  | CZ $240a			;Zf = 1 : CALL $$
$49f  | MOV A, B			;B -> A
$4a0  | CPI $3				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$4a2  | CZ $2440			;Zf = 1 : CALL $$
$4a5  | LDA $405e			;$$ -> A
$4a8  | ANI $fe				;A &= $
$4aa  | STA $405e			;A -> $$
$4ad  | LDA $4060			;$$ -> A
$4b0  | ANI $c0				;A &= $
$4b2  | CPI $c0				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$4b4  | JNZ $0523			;Zf = 0 : JMP $$
$4b7  | CALL $2679			;CALL $$
$4ba  | DAD H				;HL + HL -> HL
$4bb  | LDA $4058			;$$ -> A
$4be  | ANI $f				;A &= $
$4c0  | JZ $04ca			;Zf = 1 : JMP $$
$4c3  | XRA A				;A = A XOR B
$4c4  | STA $406f			;A -> $$
$4c7  | JMP $04df			;JMP $$

$4ca  | LDA $406b			;$$ -> A
$4cd  | ANI $e				;A &= $
$4cf  | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$4d1  | LXI D, $0002		;$$ -> DE
$4d4  | JZ $04e2			;Zf = 1 : JMP $$
$4d7  | CPI $c				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$4d9  | LXI D, $fffe		;$$ -> DE
$4dc  | JZ $04e2			;Zf = 1 : JMP $$
$4df  | LXI D, $0000		;$$ -> DE
$4e2  | DAD D				;HL + DE -> HL
$4e3  | LXI D, $4067		;$$ -> DE
$4e6  | XCHG				;HL <-> DE
$4e7  | MOV M, D			;D -> $HL
$4e8  | INX H				;HL++
$4e9  | MOV M, E			;E -> $HL
$4ea  | CALL $17a5			;CALL $$
$4ed  | MVI D, $18			;$ -> D
$4ef  | LDA $4054			;$$ -> A
$4f2  | CALL $16df			;CALL $$
$4f5  | LXI H, $4033		;$$ -> HL
$4f8  | MOV M, C			;C -> $HL
$4f9  | MOV A, C			;C -> A
$4fa  | STA $4037			;A -> $$
$4fd  | XRA A				;A = A XOR B
$4fe  | STA $403e			;A -> $$
$501  | STA $4038			;A -> $$
$504  | LXI D, $4058		;$$ -> DE
$507  | INX H				;HL++
$508  | MVI B, $3			;$ -> B
$50a  | LDAX D				;$DE -> A
$50b  | MOV M, A			;A -> $HL
$50c  | INX D				;DE++
$50d  | INX H				;HL++
$50e  | DCR B				;B--
$50f  | JNZ $050a			;Zf = 0 : JMP $$
$512  | LXI H, $4030		;$$ -> HL
$515  | MVI C, $f			;$ -> C
$517  | CALL $21f5			;CALL $$
$51a  | LDA $4070			;$$ -> A
$51d  | ANI $7f				;A &= $
$51f  | STA $4070			;A -> $$
$522  | RET					;return

$523  | CALL $21ff			;CALL $$
$526  | MOV D, H			;H -> D
$527  | MOV E, L			;L -> E
$528  | DAD H				;HL + HL -> HL
$529  | DAD H				;HL + HL -> HL
$52a  | DAD D				;HL + DE -> HL
$52b  | LDA $4058			;$$ -> A
$52e  | MOV B, A			;A -> B
$52f  | ANI $f0				;A &= $
$531  | JNZ $0538			;Zf = 0 : JMP $$
$534  | LXI D, $003a		;$$ -> DE
$537  | DAD D				;HL + DE -> HL
$538  | MOV A, B			;B -> A
$539  | ANI $f				;A &= $
$53b  | JZ $0545			;Zf = 1 : JMP $$
$53e  | XRA A				;A = A XOR B
$53f  | STA $406f			;A -> $$
$542  | JMP $055a			;JMP $$

$545  | LDA $406b			;$$ -> A
$548  | ANI $e				;A &= $
$54a  | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$54c  | LXI D, $0005		;$$ -> DE
$54f  | JZ $055d			;Zf = 1 : JMP $$
$552  | CPI $c				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$554  | LXI D, $fffb		;$$ -> DE
$557  | JZ $055d			;Zf = 1 : JMP $$
$55a  | LXI D, $0000		;$$ -> DE
$55d  | MOV A, B			;B -> A
$55e  | ANI $f0				;A &= $
$560  | JZ $04e2			;Zf = 1 : JMP $$
$563  | MOV A, E			;E -> A
$564  | CMA					;1's compliment A (invert)
$565  | ADI $1				;A += $
$567  | MOV E, A			;A -> E
$568  | MOV A, D			;D -> A
$569  | CMA					;1's compliment A (invert)
$56a  | ACI $0				;A += $ + Cf
$56c  | MOV D, A			;A -> D
$56d  | DAD D				;HL + DE -> HL
$56e  | MVI A, $3a			;$ -> A
$570  | SUB L				;A -= L
$571  | MOV L, A			;A -> L
$572  | JMP $04e3			;JMP $$

$575  | LDA $4070			;$$ -> A
$578  | ANI $7e				;A &= $
$57a  | STA $4070			;A -> $$					;sets 4070 bits 7 and 0 to 0
$57d  | LDA $406b			;$$ -> A
$580  | ORI $e				;A |= $
$582  | STA $406b			;A -> $$					;sets 406b bits 3, 2, 1
$585  | LDA $405f			;$$ -> A
$588  | ANI $80				;A &= $
$58a  | RZ					;Zf = 1 : RET				;if 405f bit 7 is 0 return
$58b  | CALL $23d3			;CALL $$					;call and return
$58e  | JMP $04ed			;JMP $$

	### delay loop stuff ###
$591  | LXI H, $407b		;$$ -> HL
$594  | DCR M				;$HL--
$595  | JNZ $05a2			;Zf = 0 : JMP $$
$598  | MVI M, $23			;$ -> $HL
$59a  | LDA $4070			;$$ -> A
$59d  | ORI $80				;A |= $
$59f  | STA $4070			;A -> $$
$5a2  | INX H				;HL++
$5a3  | MOV A, M			;$HL -> A
$5a4  | ANA A				;A &= A
$5a5  | JZ $05a9			;Zf = 1 : JMP $$
$5a8  | DCR M				;$HL--
$5a9  | INX H				;HL++
$5aa  | LDA $406f			;$$ -> A
$5ad  | ANA A				;A &= A
$5ae  | JZ $05b7			;Zf = 1 : JMP $$
$5b1  | MOV A, M			;$HL -> A
$5b2  | ANA A				;A &= A
$5b3  | JZ $05b7			;Zf = 1 : JMP $$
$5b6  | DCR M				;$HL--
$5b7  | INX H				;HL++
$5b8  | DCR M				;$HL--
$5b9  | JNZ $05cd			;Zf = 0 : JMP $$
$5bc  | LDA $4062			;$$ -> A
$5bf  | XRI $20				;A = A XOR $
$5c1  | STA $4062			;A -> $$
$5c4  | ANI $20				;A &= $
$5c6  | MVI M, $3			;$ -> $HL
$5c8  | JZ $05cd			;Zf = 1 : JMP $$
$5cb  | MVI M, $e			;$ -> $HL
$5cd  | INX H				;HL++
$5ce  | LDA $4051			;$$ -> A
$5d1  | ANI $48				;A &= $
$5d3  | MOV C, A			;A -> C
$5d4  | JZ $05dc			;Zf = 1 : JMP $$
$5d7  | CPI $48				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$5d9  | JNZ $05ff			;Zf = 0 : JMP $$
$5dc  | LDA $405f			;$$ -> A
$5df  | ANI $20				;A &= $
$5e1  | JNZ $05f4			;Zf = 0 : JMP $$
$5e4  | MVI B, $0			;$ -> B
$5e6  | LDA $8023			;$$ -> A
$5e9  | ANI $1				;A &= $
$5eb  | JNZ $05f6			;Zf = 0 : JMP $$
$5ee  | MOV A, M			;$HL -> A
$5ef  | ANA A				;A &= A
$5f0  | JZ $05f6			;Zf = 1 : JMP $$
$5f3  | DCR M				;$HL--
$5f4  | MVI B, $4			;$ -> B
$5f6  | LDA $4062			;$$ -> A
$5f9  | ANI $fb				;A &= $
$5fb  | ORA B				;A |= B
$5fc  | STA $4062			;A -> $$
$5ff  | INX H				;HL++
$600  | LDA $407a			;$$ -> A
$603  | ANI $f				;A &= $
$605  | LDA $4066			;$$ -> A
$608  | MOV B, A			;A -> B
$609  | JZ $061e			;Zf = 1 : JMP $$
$60c  | MOV A, C			;C -> A
$60d  | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$60f  | MOV A, B			;B -> A
$610  | JNZ $061e			;Zf = 0 : JMP $$
$613  | DCR M				;$HL--
$614  | JNZ $061e			;Zf = 0 : JMP $$
$617  | XRI $10				;A = A XOR $
$619  | STA $4066			;A -> $$
$61c  | MVI M, $a			;$ -> $HL
$61e  | STA $80b4			;A -> $$					(lights : UNCB, UNCA, LROL, LREC, LB10, LB1,  LA10, LA1)
$621  | INX H				;HL++
$622  | MOV A, M			;$HL -> A
$623  | ANA A				;A &= A
$624  | JZ $062b			;Zf = 1 : JMP $$
$627  | DCR M				;$HL--
$628  | JNZ $0633			;Zf = 0 : JMP $$
$62b  | LDA $4077			;$$ -> A
$62e  | ORI $80				;A |= $
$630  | STA $4077			;A -> $$
$633  | LDA $406a			;$$ -> A
$636  | CALL $1000			;CALL $$
$639  | XRI $f				;A = A XOR $
$63b  | STA $8040			;A -> $$
$63e  | MVI H, $c0			;$ -> H
$640  | LDA $4078			;$$ -> A
$643  | MOV L, A			;A -> L
$644  | LDA $4077			;$$ -> A
$647  | ANI $20				;A &= $
$649  | MOV B, A			;A -> B
$64a  | LDA $4061			;$$ -> A
$64d  | ANI $df				;A &= $
$64f  | ORA B				;A |= B
$650  | MOV D, A			;A -> D
$651  | STA $8083			;A -> $$
$654  | LDA $405f			;$$ -> A
$657  | ANI $2				;A &= $
$659  | LDA $4062			;$$ -> A
$65c  | MOV C, A			;A -> C
$65d  | JZ $0665			;Zf = 1 : JMP $$
$660  | ORI $c8				;A |= $
$662  | STA $8020			;A -> $$
$665  | MOV M, A			;A -> $HL
$666  | INX H				;HL++
$667  | MOV M, A			;A -> $HL
$668  | INX H				;HL++
$669  | MOV M, A			;A -> $HL
$66a  | DCX H				;HL--
$66b  | DCX H				;HL--
$66c  | MOV A, D			;D -> A
$66d  | ORI $8				;A |= $
$66f  | MOV D, A			;A -> D
$670  | MOV A, C			;C -> A
$671  | ORI $80				;A |= $
$673  | STA $8020			;A -> $$
$676  | MOV A, D			;D -> A
$677  | STA $8083			;A -> $$
$67a  | MOV M, A			;A -> $HL
$67b  | INX H				;HL++
$67c  | MOV M, A			;A -> $HL
$67d  | INX H				;HL++
$67e  | MOV M, A			;A -> $HL
$67f  | LXI H, $c000		;$$ -> HL
$682  | MOV A, D			;D -> A
$683  | ANI $f7				;A &= $
$685  | STA $8083			;A -> $$
$688  | MOV A, C			;C -> A
$689  | STA $8020			;A -> $$
$68c  | MOV M, A			;A -> $HL
$68d  | CALL $1185			;CALL $$
$690  | LXI H, $4058		;$$ -> HL
$693  | LXI D, $4008		;$$ -> DE
$696  | CALL $0731			;CALL $$
$699  | CALL $121d			;CALL $$
$69c  | LDA $405f			;$$ -> A
$69f  | ANI $1				;A &= $
$6a1  | JNZ $06d0			;Zf = 0 : JMP $$
$6a4  | LHLD $4083			;$$ -> HL
$6a7  | MOV A, H			;H -> A
$6a8  | ORA L				;A |= L
$6a9  | JZ $06d0			;Zf = 1 : JMP $$
$6ac  | DCX H				;HL--
$6ad  | SHLD $4083			;HL -> $$
$6b0  | LXI H, $4082		;$$ -> HL
$6b3  | DCR M				;$HL--
$6b4  | JNZ $06c8			;Zf = 0 : JMP $$
$6b7  | LDA $4070			;$$ -> A
$6ba  | XRI $2				;A = A XOR $
$6bc  | STA $4070			;A -> $$
$6bf  | ANI $2				;A &= $
$6c1  | MVI M, $2			;$ -> $HL
$6c3  | JZ $06c8			;Zf = 1 : JMP $$
$6c6  | MVI M, $1e			;$ -> $HL
$6c8  | LDA $4070			;$$ -> A
$6cb  | ANI $2				;A &= $
$6cd  | JZ $0707			;Zf = 1 : JMP $$
	****  |													(writes alpha-numeric displays from RAM (4040 - 404f -> 80a0 - 80af)
	$6d0  | LXI H, $80a0		;$$ -> HL					(HL = 80a0)
	$6d3  | LXI D, $4040		;$$ -> DE					(DE = 4040)
	$6d6  | MVI C, $10			;$ -> C						(C = 10
	$6d8  | LDAX D				;$DE -> A					(A = $4040)
	$6d9  | MOV M, A			;A -> $HL					(80a0 = 4040)
	$6da  | INX H				;HL++
	$6db  | INX D				;DE++
	$6dc  | DCR C				;C--						(repeat x10 80a0 - 80a9 = 4040 - 4049)
	$6dd  | JNZ $06d8			;Zf = 0 : JMP $$
	$6e0  | LXI H, $4008		;$$ -> HL
	$6e3  | LXI D, $4058		;$$ -> DE
	$6e6  | CALL $0731			;CALL $$					(sets 4008 - 400a = 4058 - 405a)
$6e9  | LDA $405f			;$$ -> A
$6ec  | ANI $40				;A &= $
$6ee  | JZ $06fe			;Zf = 1 : JMP $$
$6f1  | LDA $4062			;$$ -> A
$6f4  | ANI $ef				;A &= $
$6f6  | MOV B, A			;A -> B
$6f7  | RRC					;A rotate right
$6f8  | ANI $10				;A &= $
$6fa  | ORA B				;A |= B
$6fb  | STA $4062			;A -> $$
$6fe  | LDA $8024			;$$ -> A
$701  | ANI $1				;A &= $
$703  | CNZ $17a5			;Cf = 0 : CALL $$
$706  | RET					;return

$707  | LDA $4069			;$$ -> A
$70a  | CPI $10				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$70c  | JNZ $06d0			;Zf = 0 : JMP $$
$70f  | LDA $4005			;$$ -> A
$712  | ANI $80				;A &= $
$714  | JNZ $06d0			;Zf = 0 : JMP $$
$717  | LDA $4071			;$$ -> A
$71a  | ANI $f				;A &= $
$71c  | JNZ $0724			;Zf = 0 : JMP $$
$71f  | MVI A, $1			;$ -> A
$721  | STA $4071			;A -> $$
$724  | LXI H, $404b		;$$ -> HL
$727  | INX H				;HL++
$728  | RRC					;A rotate right
$729  | JNC $0727			;Cf = 0 : JMP $$
$72c  | MVI M, $0			;$ -> $HL
$72e  | JMP $06d0			;JMP $$

$731  | LDA $405f			;$$ -> A
$734  | ANI $1				;A &= $
$736  | RNZ					;Zf != 0 : RET
$737  | MVI B, $3			;$ -> B
	***   |	sets 4008 - 400a = 4058 - 405a
	$739  | MOV C, M			;$HL -> C
	$73a  | LDAX D				;$DE -> A
	$73b  | MOV M, A			;A -> $HL
	$73c  | MOV A, C			;C -> A
	$73d  | STAX D				;A -> $DE
	$73e  | INX D				;DE++
	$73f  | INX H				;HL++
	$740  | DCR B				;B--
	$741  | JNZ $0739			;Zf = 0 : JMP $$
	$744  | RET					;return

	### delay loop stuff ###
$745  | LDA $405f			;$$ -> A
$748  | ANI $3				;A &= $
$74a  | CPI $3				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$74c  | LXI H, $4077		;$$ -> HL
$74f  | JNZ $07a3			;Zf = 0 : JMP $$
$752  | MOV B, M			;$HL -> B
$753  | MOV A, B			;B -> A
$754  | ANI $8				;A &= $
$756  | MOV A, B			;B -> A
$757  | JNZ $07b3			;Zf = 0 : JMP $$
$75a  | ANI $80				;A &= $
$75c  | RZ					;Zf = 1 : RET
$75d  | MOV A, B			;B -> A
$75e  | ANI $7f				;A &= $
$760  | MOV M, A			;A -> $HL
$761  | DCX H				;HL--
$762  | MOV A, M			;$HL -> A
$763  | INX H				;HL++
$764  | MOV B, A			;A -> B
$765  | CPI $20				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$767  | JNC $0779			;Cf = 0 : JMP $$
$76a  | LXI H, $0797		;$$ -> HL
$76d  | INX H				;HL++
$76e  | INX H				;HL++
$76f  | RRC					;A rotate right
$770  | JNC $076d			;Cf = 0 : JMP $$
$773  | MOV E, M			;$HL -> E
$774  | INX H				;HL++
$775  | MOV D, M			;$HL -> D
$776  | XCHG				;HL <-> DE
$777  | PCHL				;JMP $HL
$778  | RET					;return

$779  | MOV B, M			;$HL -> B
$77a  | MOV A, B			;B -> A
$77b  | ANI $20				;A &= $
$77d  | JNZ $0789			;Zf = 0 : JMP $$
$780  | MOV A, B			;B -> A
$781  | ANI $3				;A &= $
$783  | MVI B, $1			;$ -> B
$785  | MOV A, B			;B -> A
$786  | JNZ $076a			;Zf = 0 : JMP $$
$789  | LDA $405f			;$$ -> A
$78c  | ANI $fd				;A &= $
$78e  | STA $405f			;A -> $$
$791  | XRA A				;A = A XOR B
$792  | MOV M, A			;A -> $HL
$793  | MVI A, $80			;$ -> A
$795  | STA $406a			;A -> $$
$798  | RET					;return

$799  | SBB B				;A -= B - 1
$79a  | INX D				;DE++
$79b  | XRA M				;A = A XOR $HL
$79c  | INX D				;DE++
$79d  | PCHL				;JMP $HL
$79e  | INX D				;DE++
$79f  | STAX D				;A -> $DE
$7a0  | INR D				;D++
$7a1  | POP PSW				;STACK$$ -> AF (ACC and FLAG = PSW processor status word)
$7a2  | INX D				;DE++
$7a3  | LDA $4061			;$$ -> A
$7a6  | ORI $10				;A |= $
$7a8  | STA $4061			;A -> $$
$7ab  | STA $8083			;A -> $$
$7ae  | XRA A				;A = A XOR B
$7af  | MOV M, A			;A -> $HL
$7b0  | INX H				;HL++
$7b1  | MOV M, A			;A -> $HL
$7b2  | RET					;return

$7b3  | ANI $80				;A &= $
$7b5  | RZ					;Zf = 1 : RET
$7b6  | MOV A, B			;B -> A
$7b7  | ANI $7f				;A &= $
$7b9  | MOV M, A			;A -> $HL
$7ba  | DCX H				;HL--
$7bb  | MOV A, M			;$HL -> A
$7bc  | INX H				;HL++
$7bd  | ANA A				;A &= A
$7be  | MVI B, $0			;$ -> B
$7c0  | JNZ $1398			;Zf = 0 : JMP $$
$7c3  | JMP $0789			;JMP $$

		# checks if locked or unlocked, then set's the PDRS / timebase mode #
		# think the reason asm80 isn't running is in here somewhere... #
	$7c6  | LXI H, $405f		;$$ -> HL
	$7c9  | LDA $4057			;$$ -> A
	$7cc  | ANI $4				;A &= $
	$7ce  | JZ $07f3			;Zf = 1 : JMP $$				(if 4057 !LOCKED = 0 (locked), JUMP)
		;(405f sets bit 0 to 1 (locked))
		
	;if not locked, continue
	$7d1  | LDA $4051			;$$ -> A
	$7d4  | ANI $48				;A &= $							
	$7d6  | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$7d8  | JZ $07e6			;Zf = 1 : JMP $$				(if !SINGLE = 1 and !ROLL = 0, jump)
	$7db  | MOV A, M			;$HL -> A						(A = 405f)
	$7dc  | ANI $2				;A &= $
	$7de  | JNZ $07f3			;Zf = 0 : JMP $$				(if 405f bit 1 = 1, skip, else jump and lock the scope)
	$7e1  | MOV A, M			;$HL -> A
	$7e2  | ANI $fe				;A &= $
	$7e4  | MOV M, A			;A -> $HL						(else set 405f bit 0 to 0 (unlocked))
	$7e5  | RET					;return

	$7e6  | MOV A, M			;$HL -> A	
	$7e7  | ANI $40				;A &= $
	$7e9  | JNZ $07f3			;Zf = 0 : JMP $$				(if 405f bit 6 = 0, lock the scope)
		;(405f sets bit 0 to 1 (locked))
	$7ec  | MOV A, M			;$HL -> A
	$7ed  | ANI $2				;A &= $
	$7ef  | JZ $07e1			;Zf = 1 : JMP $$				(if 405f bit 1 = 0, jump)
	$7f2  | RET					;return
	
	;if locked...
	$7f3  | MOV A, M			;$HL -> A
	$7f4  | ORI $1				;A |= $
	$7f6  | MOV M, A			;A -> $HL						(405f sets bit 0 to 1 (locked))
	$7f7  | MOV B, A			;A -> B
	$7f8  | CALL $1451			;CALL $$						(sets the PDRS / timebase mode, updates RAM 406b, 4062)
	$7fb  | RET					;return

$7FC  | NOP				; ff checksum
$7FD  | NOP				; 55 checksum
$7FE  | NOP				; 11 checksum
$7FF  | NOP				; 5c checksum

### ROM 1 ###

$1000 | MOV B, A			;A -> B
$1001 | RLC					;A rotate left
$1002 | MOV D, A			;A -> D
$1003 | ANI $11				;A &= $
$1005 | MOV C, A			;A -> C
$1006 | MOV A, D			;D -> A
$1007 | RLC					;A rotate left
$1008 | RLC					;A rotate left
$1009 | ANI $22				;A &= $
$100b | ORA C				;A |= C
$100c | MOV C, A			;A -> C
$100d | MOV A, B			;B -> A
$100e | RRC					;A rotate right
$100f | MOV D, A			;A -> D
$1010 | ANI $88				;A &= $
$1012 | ORA C				;A |= C
$1013 | MOV C, A			;A -> C
$1014 | MOV A, D			;D -> A
$1015 | RRC					;A rotate right
$1016 | RRC					;A rotate right
$1017 | ANI $44				;A &= $
$1019 | ORA C				;A |= C
$101a | RET					;return

	#called when SID (Serial Input Data) = 1 | NDR (New Data Ready) from A9
$101b | LDA $4051			;$$ -> A							!B-AC, !A-NUL, B-OFF, !ROLL, !PB0, !PB1, !SINGLE, !RECURR
$101e | ANI $48				;A &= $								( 0100 1000 & ^^^ !A-NUL, !PB0
$1020 | CPI $48				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$1022 | JNZ $102a			;Zf = 0 : JMP $$
$1025 | MVI A, $12			;$ -> A
$1027 | STA $407f			;A -> $$
$102a | LDA $8024			;$$ -> A							(read 8024 - +5V, FASA, !Yx5, !SEL, !SAV3, !SAV2, !SAV1, DELTRIG
$102d | CMA					;1's compliment A (invert)
$102e | ANI $40				;A &= $								(looking @ FASA (A9 output phase flip-flop (FASA = 1 : channel A was last sample))))
$1030 | MOV B, A			;A -> B
$1031 | LDA $4002			;$$ -> A
$1034 | ANI $bf				;A &= $
$1036 | ORA B				;A |= B
$1037 | STA $4002			;A -> $$
$103a | LXI H, $405e		;$$ -> HL
$103d | MOV A, M			;$HL -> A
$103e | ORI $5				;A |= $
$1040 | MOV M, A			;A -> $HL
$1041 | INX H				;HL++
$1042 | MOV A, M			;$HL -> A
$1043 | ANI $10				;A &= $
$1045 | JZ $104c			;Zf = 1 : JMP $$
$1048 | MOV A, M			;$HL -> A
$1049 | ORI $8				;A |= $
$104b | MOV M, A			;A -> $HL
$104c | LDA $4062			;$$ -> A
$104f | ORI $4				;A |= $
$1051 | STA $4062			;A -> $$
$1054 | MOV A, M			;$HL -> A
$1055 | ANI $80				;A &= $
$1057 | JZ $1077			;Zf = 1 : JMP $$
$105a | LXI H, $4064		;$$ -> HL
$105d | DCR M				;$HL--
$105e | JNZ $1077			;Zf = 0 : JMP $$
$1061 | INX H				;HL++
$1062 | MOV A, M			;$HL -> A
$1063 | RRC					;A rotate right
$1064 | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$1066 | JZ $10a2			;Zf = 1 : JMP $$
$1069 | ORI $80				;A |= $
$106b | MOV M, A			;A -> $HL
$106c | MVI D, $0			;$ -> D
$106e | LDA $405f			;$$ -> A
$1071 | ANI $bf				;A &= $
$1073 | ORA D				;A |= D
$1074 | STA $405f			;A -> $$
$1077 | LDA $405f			;$$ -> A
$107a | MOV B, A			;A -> B
$107b | ANI $4				;A &= $
$107d | JZ $1099			;Zf = 1 : JMP $$
$1080 | LDA $4065			;$$ -> A
$1083 | RRC					;A rotate right
$1084 | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$1086 | MVI D, $20			;$ -> D
$1088 | JZ $108f			;Zf = 1 : JMP $$
$108b | MVI D, $0			;$ -> D
$108d | ORI $80				;A |= $
$108f | STA $4065			;A -> $$
$1092 | MOV A, B			;B -> A
$1093 | ANI $df				;A &= $
$1095 | ORA D				;A |= D
$1096 | STA $405f			;A -> $$
$1099 | LDA $4063			;$$ -> A
$109c | ORI $1				;A |= $
$109e | STA $4063			;A -> $$
$10a1 | RET					;return

$10a2 | MVI A, $4c			;$ -> A
$10a4 | STA $8060			;A -> $$
$10a7 | MVI D, $40			;$ -> D
$10a9 | JMP $106e			;JMP $$

		### COPIES SOME RAM TO OTHER RAM ###
	$10ac | MVI C, $2			;$ -> C								(C = 2)
	$10ae | LXI H, $4000		;$$ -> HL							(HL = 4000)
	***** |
	$10b1 | LXI D, $000c		;$$ -> DE							(DE = 000c)
	$10b4 | DAD D				;HL + DE -> HL						(HL = 400c)
	$10b5 | MOV A, B			;B -> A								(A = e)
	$10b6 | ANA C				;A &= C								(A = 2)
	$10b7 | JZ $10c9			;Zf = 1 : JMP $$					(continue)
	$10ba | PUSH H				;HL -> SP							(SP + 400c)
	$10bb | PUSH B				;BC -> STACK$$						(SP + 0e02)
	$10bc | LXI B, $4000		;$$ -> BC							(BC = 4000)
	$10bf | LDAX B				;$BC -> A							(A = 0)
	$10c0 | MOV M, A			;A -> $HL							(400c = 4000)
	$10c1 | INX B				;BC++								(4001)
	$10c2 | INX H				;HL++								(400d)
	$10c3 | DCR E				;E--								(000b)
	$10c4 | JNZ $10bf			;Zf = 0 : JMP $$					(continue)
	$10c7 | POP B				;STACK$$ -> BC						(BC = 0e02)
	$10c8 | POP H				;STACK$$ -> HL						(HL + 400c)
	$10c9 | MOV A, C			;C -> A								(A = 02)
	$10ca | RLC					;A rotate left						(A = 04)
	$10cb | MOV C, A			;A -> C								(C = 04)
	$10cc | CPI $10				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0		( Cf = 1, Zf = 0 )
	$10ce | JNZ $10b1			;Zf = 0 : JMP $$					(loop)
	$10d1 | RET					;return

# this section has something to do with the accu and storage memories #
# and possibly the copying of one storage memory to another #
	*** check invert status of STO1 - STO3 ***
	$10d2 | LDA $4055			;$$ -> A							(8023 = !DIS3, !DIS2, !DIS1, !DIS0, !INV3, !INV2, !INV1, FRUN#)
	$10d5 | ANI $f				;A &= $								(&= !INV0 - 3)
	$10d7 | ANA C				;A &= C								(C = 02, 04, 08 on first, second and third calls from MAIN LOOP)
	$10d8 | JNZ $10e3			;Zf = 0 : JMP $$					(if invert not pulled, jump, else continue)
$10db | LDA $4062			;$$ -> A
$10de | ORI $40				;A |= $
$10e0 | STA $4062			;A -> $$
***   | accu loops starts from here
$10e3 | LDA $406d			;$$ -> A
$10e6 | ANI $f0				;A &= $
$10e8 | ANA B				;A &= B
$10e9 | JZ $10f4			;Zf = 1 : JMP $$
$10ec | LDA $4062			;$$ -> A
$10ef | ORI $8				;A |= $
$10f1 | STA $4062			;A -> $$
$10f4 | PUSH B				;BC -> STACK$$
$10f5 | LDA $4063			;$$ -> A
$10f8 | CALL $1000			;CALL $$
$10fb | STA $8040			;A -> $$
$10fe | POP B				;STACK$$ -> BC
$10ff | LDA $4061			;$$ -> A
$1102 | ANI $99				;A &= $
$1104 | MOV E, A			;A -> E
$1105 | LDA $4057			;$$ -> A
$1108 | RRC					;A rotate right
$1109 | RRC					;A rotate right
$110a | RRC					;A rotate right
$110b | ANI $6				;A &= $
$110d | ORA E				;A |= E
$110e | MOV E, A			;A -> E
$110f | LXI H, $117b		;$$ -> HL
$1112 | MOV A, B			;B -> A
$1113 | INX H				;HL++
$1114 | INX H				;HL++
$1115 | RLC					;A rotate left
$1116 | JNC $1113			;Cf = 0 : JMP $$
$1119 | MOV C, M			;$HL -> C
$111a | INX H				;HL++
$111b | MOV B, M			;$HL -> B
$111c | LDAX B				;$BC -> A
$111d | ANI $60				;A &= $
$111f | ORA E				;A |= E
$1120 | MOV E, A			;A -> E
$1121 | ANI $2				;A &= $
$1123 | MOV A, E			;E -> A
$1124 | JZ $1129			;Zf = 1 : JMP $$
$1127 | ANI $bf				;A &= $
$1129 | STA $4061			;A -> $$
$112c | STA $8083			;A -> $$
$112f | LXI H, $c000		;$$ -> HL
$1132 | LDA $4062			;$$ -> A
$1135 | ORI $80				;A |= $
$1137 | XRI $40				;A = A XOR $
$1139 | STA $4062			;A -> $$
$113c | STA $8020			;A -> $$
$113f | LDA $405e			;$$ -> A
$1142 | MOV E, A			;A -> E
$1143 | ANI $4				;A &= $
$1145 | JZ $1151			;Zf = 1 : JMP $$
$1148 | MOV A, E			;E -> A
$1149 | ANI $fb				;A &= $
$114b | STA $405e			;A -> $$
$114e | MVI A, $c0			;$ -> A
$1150 | SIM					;set interrupt mask <- A
$1151 | MOV M, A			;A -> $HL
$1152 | INR L				;L++
$1153 | LDA $0000			;$$ -> A
$1156 | NOP
$1157 | JNZ $1151			;Zf = 0 : JMP $$
	$115a | MVI A, $40			;$ -> A							;SOE, send bit 7 to SOD (i.e. set to 0 = D616 A4 data bus selected to go to RAM memories)
	$115c | SIM					;set interrupt mask <- A
	$115d | MVI A, $f			;$ -> A
	$115f | STA $8040			;A -> $$						;sets all four STO memories to write enable
	$1162 | LXI H, $4063		;$$ -> HL
	$1165 | CMA					;1's compliment A (invert)		;A = f0
	$1166 | MOV M, A			;A -> $HL
	$1167 | DCX H				;HL--							;HL = 4062
	$1168 | MOV A, M			;$HL -> A						;A = 4062
	$1169 | ANI $37				;A &= $							;sets bits 7, 6, 3 to 0
	$116b | MOV M, A			;A -> $HL
	$116c | STA $8020			;A -> $$						;stores 4062 to 8020 (set's !CLR, !REM, !RUN to 0)
	$116f | DCX H				;HL--							;HL = 4061
	$1170 | MOV A, M			;$HL -> A						;A = 4061
	$1171 | ANI $fb				;A &= $							;sets bit 2 to 0
	$1173 | ORI $2				;A |= $							;sets bit 2 to 1
	$1175 | MOV M, A			;A -> $HL						;bit pointless
	$1176 | STA $8083			;A -> $$						;TRIGGER DELAY
	$1179 | STA $c000			;A -> $$						;!DAT = 0?
	$117c | RET					;return

$117d | MVI H, $40			;$ -> H
$117f | LDAX D				;$DE -> A
$1180 | MOV B, B			;B -> B
$1181 | MVI C, $40			;$ -> C
$1183 | STAX B				;A -> $BC
$1184 | MOV B, B			;B -> B
$1185 | LDA $4069			;$$ -> A
$1188 | MOV C, A			;A -> C
$1189 | LDA $406d			;$$ -> A
$118c | MOV B, A			;A -> B
$118d | LDA $4055			;$$ -> A
$1190 | CMA					;1's compliment A (invert)
$1191 | ANI $f0				;A &= $
$1193 | MOV D, A			;A -> D
$1194 | JZ $11dd			;Zf = 1 : JMP $$
$1197 | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cf & Zf = 0
$1198 | JNZ $1203			;Zf = 0 : JMP $$
$119b | LDA $4056			;$$ -> A
$119e | ANI $10				;A &= $
$11a0 | JZ $11bd			;Zf = 1 : JMP $$
$11a3 | LDA $406b			;$$ -> A
$11a6 | ANI $7f				;A &= $
$11a8 | STA $406b			;A -> $$
$11ab | MOV A, C			;C -> A
$11ac | STA $4069			;A -> $$
$11af | MOV A, B			;B -> A
$11b0 | STA $406d			;A -> $$
$11b3 | RRC					;A rotate right
$11b4 | RRC					;A rotate right
$11b5 | RRC					;A rotate right
$11b6 | RRC					;A rotate right
$11b7 | ORA C				;A |= C
$11b8 | CMA					;1's compliment A (invert)
$11b9 | STA $80b0			;A -> $$
$11bc | RET					;return

$11bd | LDA $406b			;$$ -> A
$11c0 | MOV E, A			;A -> E
$11c1 | ANI $80				;A &= $
$11c3 | JNZ $11ab			;Zf = 0 : JMP $$
$11c6 | MVI A, $80			;$ -> A
$11c8 | ORA E				;A |= E
$11c9 | STA $406b			;A -> $$
$11cc | MOV A, C			;C -> A
$11cd | RLC					;A rotate left
$11ce | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$11d0 | JNZ $11d5			;Zf = 0 : JMP $$
$11d3 | MVI A, $10			;$ -> A
$11d5 | MOV C, A			;A -> C
$11d6 | ANA B				;A &= B
$11d7 | JZ $11cc			;Zf = 1 : JMP $$
$11da | JMP $11ab			;JMP $$

$11dd | MOV B, C			;C -> B
$11de | LDA $4056			;$$ -> A
$11e1 | ANI $10				;A &= $
$11e3 | JNZ $11a3			;Zf = 0 : JMP $$
$11e6 | LDA $406b			;$$ -> A
$11e9 | MOV E, A			;A -> E
$11ea | ANI $80				;A &= $
$11ec | JNZ $11ab			;Zf = 0 : JMP $$
$11ef | MVI A, $80			;$ -> A
$11f1 | ORA E				;A |= E
$11f2 | STA $406b			;A -> $$
$11f5 | MOV A, C			;C -> A
$11f6 | RLC					;A rotate left
$11f7 | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$11f9 | JNZ $11fe			;Zf = 0 : JMP $$
$11fc | MVI A, $10			;$ -> A
$11fe | MOV C, A			;A -> C
$11ff | MOV B, C			;C -> B
$1200 | JMP $11ab			;JMP $$

$1203 | MOV A, B			;B -> A
$1204 | CMA					;1's compliment A (invert)
$1205 | ANA D				;A &= D
$1206 | MOV B, A			;A -> B
$1207 | JZ $1217			;Zf = 1 : JMP $$
$120a | MVI C, $8			;$ -> C
$120c | MOV A, C			;C -> A
$120d | RLC					;A rotate left
$120e | MOV C, A			;A -> C
$120f | ANA B				;A &= B
$1210 | JZ $120c			;Zf = 1 : JMP $$
$1213 | MOV B, D			;D -> B
$1214 | JMP $11ab			;JMP $$

$1217 | MVI C, $8			;$ -> C
$1219 | MOV B, D			;D -> B
$121a | JMP $120c			;JMP $$

$121d | LXI H, $4030		;$$ -> HL
$1220 | LXI D, $fff4		;$$ -> DE
$1223 | LDA $4069			;$$ -> A
$1226 | DAD D				;HL + DE -> HL
$1227 | RLC					;A rotate left
$1228 | JNC $1226			;Cf = 0 : JMP $$
$122b | XCHG				;HL <-> DE
$122c | CALL $210b			;CALL $$
$122f | INX D				;DE++
$1230 | INX D				;DE++
$1231 | INX D				;DE++
$1232 | CALL $2142			;CALL $$
$1235 | INX D				;DE++
$1236 | INX D				;DE++
$1237 | INX D				;DE++
$1238 | CALL $2187			;CALL $$
$123b | INX D				;DE++
$123c | INX D				;DE++
$123d | CALL $219e			;CALL $$
$1240 | RET					;return

*** checks B-NUL, A-NUL ***
*** sets the relays on A21 ***
;checks A/B-NUL switches, and sets the relays on A21 based on the attenuator position required
	$1241 | LDA $4054			;$$ -> A			;8022
	$1244 | MVI D, $18			;$ -> D   			;D = $18
	$1246 | CALL $16df			;CALL $$			;not sure what it does, it's pretty weird...
	$1249 | LDA $4066			;$$ -> A
	$124c | ANI $15				;A &= $
	$124e | ORI $15				;A |= $
	$1250 | STA $4066			;A -> $$
	$1253 | LDA $4051			;$$ -> A
	$1256 | ANI $48				;A &= $
	$1258 | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$125a | JZ $1363			;Zf = 1 : JMP $$		;if B-NUL, continue (i.e. if B channel GND selected, else jump)
	$125d | LXI H, $1780		;$$ -> HL
	$1260 | DAD B				;HL + BC -> HL
	$1261 | MOV A, M			;$HL -> A				;HL could be anywhere between 1780 - 1798, but most likley 1783 or 178f
	$1262 | STA $8060			;A -> $$				;store into PDRS timebase settings
	$1265 | STA $406c			;A -> $$
	$1268 | LXI D, $4060		;$$ -> DE
	$126b | LDAX D				;$DE -> A
	$126c | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
	$126d | JZ $12a8			;Zf = 1 : JMP $$			;if 4060 and 406c timebase settings match, jump
$1270 | ANI $c0				;A &= $
$1272 | STAX D				;A -> $DE
$1273 | MVI B, $40			;$ -> B
$1275 | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$1277 | JNZ $1288			;Zf = 0 : JMP $$
$127a | MOV A, M			;$HL -> A
$127b | ANI $c0				;A &= $
$127d | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$127f | MVI B, $0			;$ -> B
$1281 | JZ $1288			;Zf = 1 : JMP $$
$1284 | MOV A, C			;C -> A
$1285 | STA $4033			;A -> $$
$1288 | LDAX D				;$DE -> A
$1289 | MOV C, A			;A -> C
$128a | MOV A, M			;$HL -> A
$128b | ANI $c0				;A &= $
$128d | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$128e | JZ $129d			;Zf = 1 : JMP $$
$1291 | LXI D, $8020		;$$ -> DE
$1294 | LDA $4062			;$$ -> A
$1297 | ANI $fe				;A &= $
$1299 | STAX D				;A -> $DE
$129a | ORI $1				;A |= $
$129c | STAX D				;A -> $DE
$129d | LDA $405e			;$$ -> A
$12a0 | ORA B				;A |= B
$12a1 | STA $405e			;A -> $$
$12a4 | MOV A, M			;$HL -> A
$12a5 | STA $4060			;A -> $$
	$12a8 | LDA $4052			;$$ -> A			;A = 8020 YA1, YA4, YA5, YA6, YA7, !DIG, !CALA
	$12ab | MVI D, $c			;$ -> D				;D = $c
	$12ad | CALL $16df			;CALL $$
	$12b0 | LDA $4050			;$$ -> A			;A = 801e !A-AC, !A-NUL, A-OFF, !ADD, !PA0, !PA1, !ERUN, RUN
	$12b3 | CALL $16d6			;CALL $$			;(D = A; A &= $30; CPI $20; LDA $4066)
	$12b6 | JNZ $12be			;Zf = 0 : JMP $$	;if !A-NUL = 1 jump
$12b9 | XRI $3				;A = A XOR $
$12bb | STA $4066			;A -> $$
	$12be | CALL $17d1			;CALL $$			;does another weird thing with ROM memory
	$12c1 | STA $8003			;A -> $$			;set's A relays
$12c4 | LDA $4053			;$$ -> A
$12c7 | MVI D, $c			;$ -> D
$12c9 | CALL $16df			;CALL $$
$12cc | LDA $4051			;$$ -> A
$12cf | CALL $16d6			;CALL $$
$12d2 | JNZ $12da			;Zf = 0 : JMP $$
$12d5 | XRI $c				;A = A XOR $
$12d7 | STA $4066			;A -> $$
$12da | CALL $17d1			;CALL $$
	$12dd | STA $8005			;A -> $$			;set's B relays
$12e0 | LDA $406c			;$$ -> A
$12e3 | ANI $7				;A &= $
$12e5 | MOV B, A			;A -> B
$12e6 | LDA $4050			;$$ -> A
$12e9 | MOV C, A			;A -> C
$12ea | ANI $8				;A &= $
$12ec | MVI D, $ff			;$ -> D
$12ee | JZ $1335			;Zf = 1 : JMP $$
$12f1 | MVI D, $30			;$ -> D
$12f3 | MOV A, C			;C -> A
$12f4 | ANI $4				;A &= $
$12f6 | JNZ $12ff			;Zf = 0 : JMP $$	;depends on timebase settings, skip
$12f9 | MVI A, $10			;$ -> A
$12fb | ORA B				;A |= B
$12fc | MOV B, A			;A -> B
$12fd | MVI D, $73			;$ -> D
$12ff | LDA $4051			;$$ -> A
$1302 | ANI $4				;A &= $
$1304 | JNZ $130f			;Zf = 0 : JMP $$	;otherwise, if !PB1 = 0, skip
$1307 | MVI A, $20			;$ -> A
$1309 | ORA B				;A |= B
$130a | MOV B, A			;A -> B
$130b | MOV A, D			;D -> A
$130c | ORI $8c				;A |= $
$130e | MOV D, A			;A -> D
$130f | MOV A, B			;B -> A
$1310 | ANI $30				;A &= $
$1312 | JNZ $131b			;Zf = 0 : JMP $$
$1315 | MVI A, $10			;$ -> A
$1317 | ORA B				;A |= B
$1318 | MOV B, A			;A -> B
$1319 | MVI D, $73			;$ -> D
$131b | MOV A, B			;B -> A
$131c | ANI $30				;A &= $
$131e | CPI $30				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$1320 | JNZ $1335			;Zf = 0 : JMP $$
$1323 | MVI A, $8			;$ -> A
$1325 | ORA B				;A |= B
$1326 | MOV B, A			;A -> B
$1327 | LDA $4060			;$$ -> A
$132a | ANI $c0				;A &= $
$132c | JNZ $1335			;Zf = 0 : JMP $$
$132f | MOV A, B			;B -> A
$1330 | ANI $7				;A &= $
$1332 | ORI $40				;A |= $
$1334 | MOV B, A			;A -> B
$1335 | LDA $4053			;$$ -> A
$1338 | ANI $2				;A &= $
$133a | JNZ $1341			;Zf = 0 : JMP $$		;if !BIN (b invert) = 1, skip
$133d | MVI A, $80			;$ -> A
$133f | ORA B				;A |= B
$1340 | MOV B, A			;A -> B
$1341 | MOV A, B			;B -> A
$1342 | STA $406c			;A -> $$
$1345 | STA $8006			;A -> $$
$1348 | LXI H, $4052		;$$ -> HL
$134b | MOV A, M			;$HL -> A
$134c | RRC					;A rotate right
$134d | RRC					;A rotate right
$134e | ANI $40				;A &= $
$1350 | MOV B, A			;A -> B
$1351 | INX H				;HL++
$1352 | MOV A, M			;$HL -> A
$1353 | RRC					;A rotate right
$1354 | ANI $80				;A &= $
$1356 | ORA B				;A |= B
$1357 | MOV B, A			;A -> B
$1358 | LDA $4066			;$$ -> A
$135b | ORA B				;A |= B
$135c | ANA D				;A &= D
$135d | XRI $c0				;A = A XOR $
$135f | STA $4066			;A -> $$
$1362 | RET					;return

$1363 | MVI A, $8			;$ -> A
$1365 | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$1366 | JNC $1371			;Cf = 0 : JMP $$
$1369 | MVI A, $15			;$ -> A
$136b | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$136c | MVI D, $0			;$ -> D
$136e | JNC $1375			;Cf = 0 : JMP $$
$1371 | MVI D, $f			;$ -> D
$1373 | MVI C, $9			;$ -> C
$1375 | LDA $407a			;$$ -> A
$1378 | ANI $f0				;A &= $
$137a | ORA D				;A |= D
$137b | STA $407a			;A -> $$
$137e | LXI H, $1798		;$$ -> HL
$1381 | MOV A, C			;C -> A
$1382 | SBI $9				;A -= $ - Cf
$1384 | MOV C, A			;A -> C
$1385 | DAD B				;HL + BC -> HL
$1386 | MOV A, M			;$HL -> A
$1387 | STA $8060			;A -> $$
$138a | STA $4060			;A -> $$
$138d | LDA $4066			;$$ -> A
$1390 | XRI $30				;A = A XOR $
$1392 | STA $4066			;A -> $$
$1395 | JMP $12a8			;JMP $$

$1398 | LDA $4061			;$$ -> A
$139b | ORI $10				;A |= $
$139d | STA $4061			;A -> $$
$13a0 | STA $8083			;A -> $$
$13a3 | MVI A, $46			;$ -> A
$13a5 | STA $4081			;A -> $$
$13a8 | MOV A, B			;B -> A
$13a9 | RLC					;A rotate left
$13aa | STA $4076			;A -> $$
$13ad | RET					;return

$13ae | LXI H, $13df		;$$ -> HL
$13b1 | LDA $406a			;$$ -> A
$13b4 | INX H				;HL++
$13b5 | INX H				;HL++
$13b6 | RLC					;A rotate left
$13b7 | JNC $13b4			;Cf = 0 : JMP $$
$13ba | MOV E, M			;$HL -> E
$13bb | INX H				;HL++
$13bc | MOV D, M			;$HL -> D
$13bd | LDAX D				;$DE -> A
$13be | ANI $60				;A &= $
$13c0 | MOV C, A			;A -> C
$13c1 | LDA $4077			;$$ -> A
$13c4 | ANI $9f				;A &= $
$13c6 | ORA C				;A |= C
$13c7 | STA $4077			;A -> $$
$13ca | MOV C, A			;A -> C
$13cb | ANI $3				;A &= $
$13cd | MOV A, C			;C -> A
$13ce | JZ $13d2			;Zf = 1 : JMP $$
$13d1 | CMA					;1's compliment A (invert)
$13d2 | ANI $40				;A &= $
$13d4 | MVI A, $1			;$ -> A
$13d6 | JZ $13db			;Zf = 1 : JMP $$
$13d9 | MVI A, $0			;$ -> A
$13db | STA $4078			;A -> $$
$13de | JMP $13a3			;JMP $$

$13e1 | MVI H, $40			;$ -> H
$13e3 | LDAX D				;$DE -> A
$13e4 | MOV B, B			;B -> B
$13e5 | MVI C, $40			;$ -> C
$13e7 | STAX B				;A -> $BC
$13e8 | MOV B, B			;B -> B
$13e9 | LDA $4061			;$$ -> A
$13ec | ANI $ef				;A &= $
$13ee | JMP $139d			;JMP $$

$13f1 | LXI H, $4079		;$$ -> HL
$13f4 | LDA $4061			;$$ -> A
$13f7 | MOV C, A			;A -> C
$13f8 | ANI $10				;A &= $
$13fa | JNZ $1408			;Zf = 0 : JMP $$
$13fd | MOV A, C			;C -> A
$13fe | ORI $10				;A |= $
$1400 | STA $4061			;A -> $$
$1403 | STA $8083			;A -> $$
$1406 | MVI M, $6			;$ -> $HL
$1408 | DCR M				;$HL--
$1409 | JZ $13a3			;Zf = 1 : JMP $$
$140c | MVI A, $46			;$ -> A
$140e | STA $4081			;A -> $$
$1411 | RET					;return

$1412 | LDA $4077			;$$ -> A
$1415 | MOV C, A			;A -> C
$1416 | ANI $20				;A &= $
$1418 | LXI H, $4078		;$$ -> HL
$141b | JNZ $1447			;Zf = 0 : JMP $$
$141e | INR M				;$HL++
$141f | INR M				;$HL++
$1420 | MOV A, M			;$HL -> A
$1421 | CPI $fa				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$1423 | JZ $142b			;Zf = 1 : JMP $$
$1426 | CPI $f9				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$1428 | JNZ $1441			;Zf = 0 : JMP $$
$142b | MOV A, C			;C -> A
$142c | ANI $3				;A &= $
$142e | MVI D, $0			;$ -> D
$1430 | JNZ $1435			;Zf = 0 : JMP $$
$1433 | MVI D, $3			;$ -> D
$1435 | MOV A, C			;C -> A
$1436 | ANI $fc				;A &= $
$1438 | ORA D				;A |= D
$1439 | STA $4077			;A -> $$
$143c | MOV A, B			;B -> A
$143d | RLC					;A rotate left
$143e | STA $4076			;A -> $$
$1441 | MVI A, $e			;$ -> A
$1443 | STA $4081			;A -> $$
$1446 | RET					;return

$1447 | INR M				;$HL++
$1448 | MOV A, M			;$HL -> A
$1449 | CPI $fa				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$144b | JZ $143c			;Zf = 1 : JMP $$
$144e | JMP $1441			;JMP $$

# if not locked #
	$1451 | LDA $406b			;$$ -> A		;sets bits 5, 6 of $406b to 0
	$1454 | ANI $9f				;A &= $
	$1456 | STA $406b			;A -> $$

	$1459 | LDA $4060			;$$ -> A		;sets the P/D/R/S mode and timebase settings from $4060
	$145c | STA $8060			;A -> $$		;bits 6 and 7 make up the PDRS selection 00 - P, 01 - D, 10 - R, 11 - S
												;p2ccd, direct, roll, sampling
	$145f | MOV A, B			;B -> A			;A = $405f ORI $1
	$1460 | ANI $40				;A &= $			;return if bit 6 is 1
	$1462 | RNZ					;Zf != 0 : RET

	$1463 | LDA $4062			;$$ -> A		;A = 4062
	$1466 | ORI $14				;A |= $			;sets bits 2, 4 to 1
	$1468 | STA $4062			;A -> $$
	$146b | RET					;return

$146c | CALL $1451			;CALL $$
$146f | LXI H, $4064		;$$ -> HL
$1472 | MVI M, $0			;$ -> $HL
$1474 | INX H				;HL++
$1475 | MVI M, $10			;$ -> $HL
$1477 | MOV A, B			;B -> A
$1478 | ANI $3				;A &= $
$147a | ORI $80				;A |= $
$147c | MOV B, A			;A -> B
$147d | STA $405f			;A -> $$
$1480 | MVI A, $c0			;$ -> A
$1482 | SIM					;set interrupt mask <- A
$1483 | MVI A, $40			;$ -> A
$1485 | SIM					;set interrupt mask <- A
$1486 | XRA A				;A = A XOR A
$1487 | RET					;return

$1488 | LDA $4050			;$$ -> A
$148b | ANI $80				;A &= $
$148d | LDA $406b			;$$ -> A
$1490 | MVI D, $0			;$ -> D
$1492 | JNZ $14a0			;Zf = 0 : JMP $$
$1495 | MOV D, A			;A -> D
$1496 | ANI $40				;A &= $
$1498 | MOV A, D			;D -> A
$1499 | JNZ $14a7			;Zf = 0 : JMP $$
$149c | XRI $20				;A = A XOR $
$149e | MVI D, $40			;$ -> D
$14a0 | ANI $bf				;A &= $
$14a2 | ORA D				;A |= D
$14a3 | STA $406b			;A -> $$
$14a6 | MOV D, A			;A -> D
$14a7 | LDA $4050			;$$ -> A
$14aa | RRC					;A rotate right
$14ab | XRI $20				;A = A XOR $
$14ad | ORA D				;A |= D
$14ae | ANI $20				;A &= $
$14b0 | MOV D, A			;A -> D
$14b1 | LDA $4060			;$$ -> A
$14b4 | ORA D				;A |= D
$14b5 | STA $8060			;A -> $$
$14b8 | RRC					;A rotate right
$14b9 | CMA					;1's compliment A (invert)
$14ba | ANI $10				;A &= $
$14bc | MOV D, A			;A -> D
$14bd | LDA $4062			;$$ -> A
$14c0 | ANI $ef				;A &= $
$14c2 | ORA D				;A |= D
$14c3 | STA $4062			;A -> $$
$14c6 | RET					;return

$14c7 | LDA $4050			;$$ -> A
$14ca | CMA					;1's compliment A (invert)
$14cb | RRC					;A rotate right
$14cc | ANI $40				;A &= $
$14ce | MOV D, A			;A -> D
$14cf | LDA $406b			;$$ -> A
$14d2 | MOV E, A			;A -> E
$14d3 | ANI $40				;A &= $
$14d5 | CMP D				;A < D : Cf = 1, Zf = 0 | A = D : Cf = 0, Zf = 1 | A > D : Cf & Zf = 0
$14d6 | RZ					;Zf = 1 : RET
$14d7 | MOV A, E			;E -> A
$14d8 | ANI $bf				;A &= $
$14da | ORA D				;A |= D
$14db | STA $406b			;A -> $$
$14de | ANI $40				;A &= $
$14e0 | RZ					;Zf = 1 : RET
$14e1 | MOV A, B			;B -> A
$14e2 | ANI $10				;A &= $
$14e4 | CNZ $1532			;Cf = 0 : CALL $$
$14e7 | MOV A, B			;B -> A
$14e8 | ANI $4				;A &= $
$14ea | CNZ $154d			;Cf = 0 : CALL $$
$14ed | RET					;return

### PLOT ###
$14ee | LDA $4057			;$$ -> A
$14f1 | ANI $80				;A &= $
$14f3 | LDA $406b			;$$ -> A
$14f6 | JNZ $1529			;Zf = 0 : JMP $$
$14f9 | MOV C, A			;A -> C
$14fa | ANI $1				;A &= $
$14fc | RNZ					;Zf != 0 : RET
$14fd | MVI A, $1			;$ -> A
$14ff | ORA C				;A |= C
$1500 | STA $406b			;A -> $$
$1503 | MOV A, B			;B -> A
$1504 | ANI $2				;A &= $
$1506 | MVI C, $8			;$ -> C
$1508 | JNZ $151f			;Zf = 0 : JMP $$
$150b | MVI C, $0			;$ -> C
$150d | LDA $4069			;$$ -> A
$1510 | STA $406a			;A -> $$
$1513 | MVI A, $2			;$ -> A
$1515 | ORA B				;A |= B
$1516 | STA $405f			;A -> $$
$1519 | MOV B, A			;A -> B
$151a | MVI A, $1			;$ -> A
$151c | STA $4076			;A -> $$
$151f | LDA $4077			;$$ -> A
$1522 | ANI $f7				;A &= $
$1524 | ORA C				;A |= C
$1525 | STA $4077			;A -> $$
$1528 | RET					;return

$1529 | ANI $fe				;A &= $
$152b | STA $406b			;A -> $$
$152e | RET					;return

$152f | CALL $1451			;CALL $$
$1532 | MOV A, B			;B -> A
$1533 | ANI $3				;A &= $
$1535 | ORI $10				;A |= $
$1537 | MOV B, A			;A -> B
$1538 | STA $405f			;A -> $$
$153b | LDA $4062			;$$ -> A
$153e | ANI $fb				;A &= $
$1540 | STA $4062			;A -> $$
$1543 | MVI A, $c0			;$ -> A
$1545 | SIM					;set interrupt mask <- A
$1546 | MVI A, $40			;$ -> A
$1548 | SIM					;set interrupt mask <- A
$1549 | RET					;return

$154a | CALL $1451			;CALL $$
$154d | MOV A, B			;B -> A
$154e | ANI $3				;A &= $
$1550 | ORI $4				;A |= $
$1552 | MOV B, A			;A -> B
$1553 | STA $405f			;A -> $$
$1556 | MVI A, $10			;$ -> A
$1558 | STA $4065			;A -> $$
$155b | MVI A, $c0			;$ -> A
$155d | SIM					;set interrupt mask <- A
$155e | MVI A, $40			;$ -> A
$1560 | SIM					;set interrupt mask <- A
$1561 | RET					;return

$1562 | LDA $407a			;$$ -> A
$1565 | ANI $f				;A &= $
$1567 | RNZ					;Zf != 0 : RET
$1568 | LDA $4054			;$$ -> A
$156b | MVI D, $18			;$ -> D
$156d | CALL $16df			;CALL $$
$1570 | MOV A, C			;C -> A
$1571 | STA $403a			;A -> $$
$1574 | LDA $4037			;$$ -> A
$1577 | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$1578 | JNC $157e			;Cf = 0 : JMP $$
$157b | MOV B, C			;C -> B
$157c | MOV C, A			;A -> C
$157d | MOV A, B			;B -> A
$157e | SUB C				;A -= C
$157f | STA $4039			;A -> $$
$1582 | LDA $4037			;$$ -> A
$1585 | MOV C, A			;A -> C
$1586 | LDA $403a			;$$ -> A
$1589 | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$158a | JNC $160a			;Cf = 0 : JMP $$
$158d | LDA $4038			;$$ -> A
$1590 | ANI $e0				;A &= $
$1592 | JNZ $168d			;Zf = 0 : JMP $$
$1595 | LDA $4037			;$$ -> A
$1598 | STA $4038			;A -> $$
$159b | LXI H, $4058		;$$ -> HL
$159e | LXI D, $4030		;$$ -> DE
$15a1 | CALL $16b3			;CALL $$
$15a4 | LDA $4037			;$$ -> A
$15a7 | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$15a9 | JNZ $15b7			;Zf = 0 : JMP $$
$15ac | LXI H, $4058		;$$ -> HL
$15af | LXI D, $403b		;$$ -> DE
$15b2 | CALL $16b3			;CALL $$
$15b5 | MVI A, $6			;$ -> A
$15b7 | STA $403e			;A -> $$
$15ba | LDA $4037			;$$ -> A
$15bd | MOV C, A			;A -> C
$15be | MVI B, $0			;$ -> B
$15c0 | LXI H, $16be		;$$ -> HL
$15c3 | DAD B				;HL + BC -> HL
$15c4 | CALL $2575			;CALL $$
$15c7 | ANA A				;A &= A
$15c8 | JNZ $1682			;Zf = 0 : JMP $$
$15cb | LXI H, $4058		;$$ -> HL
$15ce | MOV A, M			;$HL -> A
$15cf | ANI $f0				;A &= $
$15d1 | JZ $15e1			;Zf = 1 : JMP $$
$15d4 | INX H				;HL++
$15d5 | MOV A, M			;$HL -> A
$15d6 | ANA A				;A &= A
$15d7 | JNZ $169c			;Zf = 0 : JMP $$
$15da | INX H				;HL++
$15db | MOV A, M			;$HL -> A
$15dc | CPI $a				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$15de | JNC $169c			;Cf = 0 : JMP $$
$15e1 | LDA $4037			;$$ -> A
$15e4 | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$15e6 | JNC $15f1			;Cf = 0 : JMP $$
$15e9 | CALL $2679			;CALL $$
$15ec | MOV A, D			;D -> A
$15ed | ANA A				;A &= A
$15ee | JNZ $168d			;Zf = 0 : JMP $$
$15f1 | LXI H, $4037		;$$ -> HL
$15f4 | DCR M				;$HL--
$15f5 | LDA $4033			;$$ -> A
$15f8 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$15f9 | LXI H, $4034		;$$ -> HL
$15fc | LXI D, $4058		;$$ -> DE
$15ff | CZ $16aa			;Zf = 1 : CALL $$
$1602 | LXI H, $4039		;$$ -> HL
$1605 | DCR M				;$HL--
$1606 | JNZ $158d			;Zf = 0 : JMP $$
$1609 | RET					;return

$160a | LDA $4038			;$$ -> A
$160d | ANI $e0				;A &= $
$160f | JNZ $1655			;Zf = 0 : JMP $$
$1612 | LDA $4058			;$$ -> A
$1615 | ANI $f0				;A &= $
$1617 | STA $4058			;A -> $$
$161a | LDA $4037			;$$ -> A
$161d | MOV C, A			;A -> C
$161e | LXI H, $16be		;$$ -> HL
$1621 | MVI B, $0			;$ -> B
$1623 | DAD B				;HL + BC -> HL
$1624 | INX H				;HL++
$1625 | CALL $258b			;CALL $$
$1628 | LXI H, $4037		;$$ -> HL
$162b | INR M				;$HL++
$162c | MOV C, M			;$HL -> C
$162d | LDA $4033			;$$ -> A
$1630 | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$1631 | LXI H, $4034		;$$ -> HL
$1634 | LXI D, $4058		;$$ -> DE
$1637 | CZ $16aa			;Zf = 1 : CALL $$
$163a | MOV A, C			;C -> A
$163b | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$163d | JNZ $164d			;Zf = 0 : JMP $$
$1640 | LDA $403e			;$$ -> A
$1643 | ANA A				;A &= A
$1644 | LXI H, $403b		;$$ -> HL
$1647 | LXI D, $4058		;$$ -> DE
$164a | CNZ $16b3			;Cf = 0 : CALL $$
$164d | LXI H, $4039		;$$ -> HL
$1650 | DCR M				;$HL--
$1651 | JNZ $160a			;Zf = 0 : JMP $$
$1654 | RET					;return

$1655 | LXI H, $4038		;$$ -> HL
$1658 | MOV A, M			;$HL -> A
$1659 | ANI $1f				;A &= $
$165b | MOV C, A			;A -> C
$165c | LDA $403a			;$$ -> A
$165f | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$1660 | JC $1678			;Cf = 1 : JMP $$
$1663 | DCX H				;HL--
$1664 | MOV A, M			;$HL -> A
$1665 | INR A				;A++
$1666 | CMP C				;A < C : Cf = 1, Zf = 0 | A = C : Cf = 0, Zf = 1 | A > C : Cf & Zf = 0
$1667 | JNZ $1628			;Zf = 0 : JMP $$
$166a | INX H				;HL++
$166b | MOV M, C			;C -> $HL
$166c | LXI H, $4030		;$$ -> HL
$166f | LXI D, $4058		;$$ -> DE
$1672 | CALL $16b3			;CALL $$
$1675 | JMP $1628			;JMP $$

$1678 | LDA $4059			;$$ -> A
$167b | ANA A				;A &= A
$167c | JZ $169c			;Zf = 1 : JMP $$
$167f | JMP $1682			;JMP $$

$1682 | LXI H, $4058		;$$ -> HL
$1685 | MVI M, $c			;$ -> $HL
$1687 | INX H				;HL++
$1688 | MVI M, $99			;$ -> $HL
$168a | INX H				;HL++
$168b | MVI M, $99			;$ -> $HL
$168d | LDA $4038			;$$ -> A
$1690 | ORI $e0				;A |= $
$1692 | STA $4038			;A -> $$
$1695 | LDA $403a			;$$ -> A
$1698 | STA $4037			;A -> $$
$169b | RET					;return

$169c | LXI H, $4058		;$$ -> HL
$169f | MVI M, $f3			;$ -> $HL
$16a1 | INX H				;HL++
$16a2 | MVI M, $0			;$ -> $HL
$16a4 | INX H				;HL++
$16a5 | MVI M, $9			;$ -> $HL
$16a7 | JMP $168d			;JMP $$

$16aa | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$16ac | JNC $16b3			;Cf = 0 : JMP $$
$16af | XRA A				;A = A XOR A
$16b0 | STA $403e			;A -> $$
$16b3 | MVI B, $3			;$ -> B
$16b5 | MOV A, M			;$HL -> A
$16b6 | STAX D				;A -> $DE
$16b7 | INX H				;HL++
$16b8 | INX D				;DE++
$16b9 | DCR B				;B--
$16ba | JNZ $16b5			;Zf = 0 : JMP $$
$16bd | RET					;return

$16be | NOP
$16bf | STAX B				;A -> $BC
$16c0 | STAX B				;A -> $BC
$16c1 | DAD D				;HL + DE -> HL
$16c2 | STAX B				;A -> $BC
$16c3 | STAX B				;A -> $BC
$16c4 | DAD D				;HL + DE -> HL
$16c5 | STAX B				;A -> $BC
$16c6 | STAX B				;A -> $BC
$16c7 | DAD D				;HL + DE -> HL
$16c8 | STAX B				;A -> $BC
$16c9 | STAX B				;A -> $BC
$16ca | DAD D				;HL + DE -> HL
$16cb | STAX B				;A -> $BC
$16cc | STAX B				;A -> $BC
$16cd | DAD D				;HL + DE -> HL
$16ce | STAX B				;A -> $BC
$16cf | STAX B				;A -> $BC
$16d0 | DAD D				;HL + DE -> HL
$16d1 | STAX B				;A -> $BC
$16d2 | STAX B				;A -> $BC
$16d3 | DAD D				;HL + DE -> HL
$16d4 | STAX B				;A -> $BC
$16d5 | STAX B				;A -> $BC
$16d6 | MOV D, A			;A -> D
$16d7 | ANI $30				;A &= $
$16d9 | CPI $20				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$16db | LDA $4066			;$$ -> A
$16de | RET					;return

	;jumps here from 1246
	;jumps here from 12ad, A = $4052, D = $c
	;it does some really weird comparison between A and ROM bytes, for B loops, and updates 407a, or returns
	$16df | ANI $fc				;A &= $				;A = $4050 &= everything but bits 0 and 1
	$16e1 | MOV E, D			;D -> E				;E = D = $18
	$16e2 | LXI B, $ffff		;$$ -> BC			;BC = ffff
	$16e5 | LXI H, $1706		;$$ -> HL			;HL = 1706 (CNC = $d4)

	$16e8 | INX B				;BC++				;BC = 0
								;if A = d4, return
	$16e9 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
	$16ea | RZ					;Zf = 1 : RET
	
	$16eb | INX H				;HL++				;HL = 1077 ($c4)
	$16ec | DCR D				;D--				;repeat 18 times, unless
	$16ed | JNZ $16e8			;Zf = 0 : JMP $$

	$16f0 | LXI B, $0003		;$$ -> BC
	$16f3 | MOV A, E			;E -> A
	$16f4 | CPI $c				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
	$16f6 | LXI H, $407a		;$$ -> HL
	$16f9 | MVI A, $f0			;$ -> A
	$16fb | JZ $1703			;Zf = 1 : JMP $$
	$16fe | LXI B, $000f		;$$ -> BC
	$1701 | MVI A, $f			;$ -> A
	$1703 | ORA M				;A |= $HL
	$1704 | MOV M, A			;A -> $HL
	$1705 | RET					;return

$1706 | CNC $84c4			;Cf = 0 : CALL $$
$1709 | ADC H				;H += A + Cy
$170a | CZ $6444			;Zf = 1 : CALL $$
$170d | MOV H, B			;B -> H
$170e | MOV D, B			;B -> D
$170f | RNC
$1710 | RPO					;P = 1 : RET
$1711 | INR H				;H++
$1712 | INR A				;A++
$1713 | LDSI $30				;undocumented (DE = SP + d8)
$1715 | MOV M, B			;B -> $HL
$1716 | MOV A, B			;B -> A
$1717 | RDEL				;undocumented (arithmetic left shift DE)
$1718 | SBB B				;A -= B - 1
$1719 | ADC B				;B += A + Cy
$171a | INR C				;C++
$171b | INR L				;L++
$171c | XRA B				;A = A XOR B
$171d | SUB B				;A -= B
$171e | LXI D, $1111		;$$ -> DE
$1721 | NOP
$1722 | NOP
$1723 | NOP
$1724 | LXI B, $0101		;$$ -> BC
$1727 | STAX B				;A -> $BC
$1728 | STAX B				;A -> $BC
$1729 | STAX B				;A -> $BC
$172a | MOV E, H			;H -> E
$172b | MOV L, H			;H -> L
$172c | MOV C, H			;H -> C
$172d | INR E				;E++
$172e | INR L				;L++
$172f | INR C				;C++
$1730 | LDAX D				;$DE -> A
$1731 | LHLD $160a			;$$ -> HL
$1734 | MVI H, $6			;$ -> H
$1736 | RDEL				;undocumented (arithmetic left shift DE)
$1737 | RAL					;Cy < A rotate left
$1738 | RAL					;Cy < A rotate left
$1739 | RAL					;Cy < A rotate left
$173a | MVI D, $16			;$ -> D
$173c | MVI D, $15			;$ -> D
$173e | DCR D				;D--
$173f | DCR D				;D--
$1740 | INR D				;D++
$1741 | INR D				;D++
$1742 | INR D				;D++
$1743 | INX D				;DE++
$1744 | INX D				;DE++
$1745 | INX D				;DE++
$1746 | STAX D				;A -> $DE
$1747 | STAX D				;A -> $DE
$1748 | STAX D				;A -> $DE
$1749 | LXI D, $1111		;$$ -> DE
$174c | NOP
$174d | NOP
$174e | NOP
$174f | LXI B, $0101		;$$ -> BC
$1752 | STAX B				;A -> $BC
$1753 | STAX B				;A -> $BC
$1754 | STAX B				;A -> $BC
$1755 | STAX B				;A -> $BC
$1756 | INX B				;BC++
$1757 | INX B				;BC++
$1758 | INX B				;BC++
$1759 | INR B				;B++
$175a | INR B				;B++
$175b | MOV D, B			;B -> D
$175c | ARHL				;undocumented (arithmetic right shift HL)
$175d | RIM					;read interrupt mask -> A
$175e | MOV D, B			;B -> D
$175f | ARHL				;undocumented (arithmetic right shift HL)
$1760 | RIM					;read interrupt mask -> A
$1761 | MOV D, B			;B -> D
$1762 | ARHL				;undocumented (arithmetic right shift HL)
$1763 | RIM					;read interrupt mask -> A
$1764 | MOV D, B			;B -> D
$1765 | ARHL				;undocumented (arithmetic right shift HL)
$1766 | RIM					;read interrupt mask -> A
$1767 | MOV D, B			;B -> D
$1768 | ARHL				;undocumented (arithmetic right shift HL)
$1769 | RIM					;read interrupt mask -> A
$176a | MOV D, B			;B -> D
$176b | ARHL				;undocumented (arithmetic right shift HL)
$176c | RIM					;read interrupt mask -> A
$176d | MOV D, B			;B -> D
$176e | ARHL				;undocumented (arithmetic right shift HL)
$176f | RIM					;read interrupt mask -> A
$1770 | MOV D, B			;B -> D
$1771 | ARHL				;undocumented (arithmetic right shift HL)
$1772 | RIM					;read interrupt mask -> A
$1773 | MOV D, B			;B -> D
$1774 | ARHL				;undocumented (arithmetic right shift HL)
$1775 | RIM					;read interrupt mask -> A
$1776 | MOV D, B			;B -> D
$1777 | ARHL				;undocumented (arithmetic right shift HL)
$1778 | RIM					;read interrupt mask -> A
$1779 | SIM					;set interrupt mask <- A
$177a | MOV H, B			;B -> H
$177b | STAX D				;A -> $DE
$177c | MVI M, $90			;$ -> $HL
$177e | RDEL				;undocumented (arithmetic left shift DE)
$177f | MVI M, $c6			;$ -> $HL
$1781 | POP B				;STACK$$ -> BC
$1782 | CNZ $c0c5			;Cf = 0 : CALL $$
$1785 | RST 0				;restart
$1786 | DCR C				;C--
$1787 | MVI C, $a			;$ -> C
$1789 | DCX B				;BC--
$178a | LXI D, $1312		;$$ -> DE
$178d | DAD D				;HL + DE -> HL
$178e | LDAX D				;$DE -> A
$178f | ANA B				;A &= B
$1790 | ANA C				;A &= C
$1791 | ANA D				;A &= D
$1792 | ANA H				;A &= H
$1793 | ANA L				;A &= L
$1794 | ANA M				;A &= $HL
$1795 | XRA B				;A = A XOR B
$1796 | XRA C				;A = A XOR C
$1797 | XRA D				;A = A XOR D
$1798 | MOV C, H			;H -> C
$1799 | MOV C, L			;L -> C
$179a | MOV C, M			;$HL -> C
$179b | MOV D, B			;B -> D
$179c | MOV D, C			;C -> D
$179d | MOV D, D			;D -> D
$179e | MOV D, H			;H -> D
$179f | MOV D, L			;L -> D
$17a0 | MOV D, M			;$HL -> D
$17a1 | MOV E, B			;B -> E
$17a2 | MOV E, L			;L -> E
$17a3 | MOV E, M			;$HL -> E
$17a4 | MOV E, A			;A -> E
$17a5 | LXI H, $4061		;$$ -> HL
$17a8 | MOV A, M			;$HL -> A
$17a9 | ANI $fe				;A &= $
$17ab | STA $8083			;A -> $$
$17ae | ORI $1				;A |= $
$17b0 | MOV M, A			;A -> $HL
$17b1 | LXI D, $8085		;$$ -> DE
$17b4 | MVI A, $ff			;$ -> A
$17b6 | STAX D				;A -> $DE
$17b7 | INX D				;DE++
$17b8 | MOV A, M			;$HL -> A
$17b9 | STA $8083			;A -> $$
$17bc | LHLD $4067			;$$ -> HL
$17bf | XCHG				;HL <-> DE
$17c0 | LDA $4060			;$$ -> A
$17c3 | ANI $40				;A &= $
$17c5 | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$17c7 | MOV A, D			;D -> A
$17c8 | JZ $17cd			;Zf = 1 : JMP $$
$17cb | XRI $f				;A = A XOR $
$17cd | MOV M, A			;A -> $HL
$17ce | DCX H				;HL--
$17cf | MOV M, E			;E -> $HL
$17d0 | RET					;return

	;called from 12be
	$17d1 | LXI H, $172a		;$$ -> HL
	$17d4 | DAD B				;HL + BC -> HL
	$17d5 | MOV A, M			;$HL -> A
	$17d6 | ORI $e				;A |= $
	$17d8 | MOV E, A			;A -> E
	$17d9 | MOV A, D			;D -> A
	$17da | ANI $2				;A &= $
	$17dc | MOV A, E			;E -> A
	$17dd | RZ					;Zf = 1 : RET
	$17de | MOV A, D			;D -> A
	$17df | CMA					;A &= $
	$17e2 | ADD M				;1's compliment A (invert)
	$17e0 | ANI $1				;$HL += A
	$17e3 | RET					;return

$17e4 | RST 7				;restart
$17e5 | RST 7				;restart
$17e6 | RST 7				;restart
$17e7 | RST 7				;restart
$17e8 | RST 7				;restart
$17e9 | RST 7				;restart
$17ea | RST 7				;restart
$17eb | RST 7				;restart
$17ec | RST 7				;restart
$17ed | RST 7				;restart
$17ee | RST 7				;restart
$17ef | RST 7				;restart
$17f0 | RST 7				;restart
$17f1 | RST 7				;restart
$17f2 | RST 7				;restart
$17f3 | RST 7				;restart
$17f4 | RST 7				;restart
$17f5 | RST 7				;restart
$17f6 | RST 7				;restart
$17f7 | RST 7				;restart
$17f8 | RST 7				;restart
$17f9 | RST 7				;restart
$17fa | RST 7				;restart
$17fb | RST 7				;restart
$17fc | NOP				; ff checksum
$17fd | NOP				; 55 checksum
$17fe | NOP				; 22 checksum
$17ff | NOP				; 5c checksum

### ROM 2 ###

$2000 | LDA $4051			;$$ -> A
$2003 | ANI $48				;A &= $
$2005 | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2007 | JZ $20e9			;Zf = 1 : JMP $$
$200a | LDA $4054			;$$ -> A
$200d | MVI D, $18			;$ -> D
$200f | CALL $16df			;CALL $$
$2012 | LXI H, $1736		;$$ -> HL
$2015 | DAD B				;HL + BC -> HL
$2016 | LXI D, $4006		;$$ -> DE
$2019 | MOV A, M			;$HL -> A
$201a | STAX D				;A -> $DE
$201b | INX D				;DE++
$201c | LXI H, $175b		;$$ -> HL
$201f | DAD B				;HL + BC -> HL
$2020 | MOV A, M			;$HL -> A
$2021 | STAX D				;A -> $DE
$2022 | LDA $4052			;$$ -> A
$2025 | MVI D, $c			;$ -> D
$2027 | CALL $16df			;CALL $$
$202a | LXI D, $4000		;$$ -> DE
$202d | LXI H, $171e		;$$ -> HL
$2030 | DAD B				;HL + BC -> HL
$2031 | MOV A, M			;$HL -> A
$2032 | STAX D				;A -> $DE
$2033 | INX D				;DE++
$2034 | LXI H, $175c		;$$ -> HL
$2037 | DAD B				;HL + BC -> HL
$2038 | MOV A, M			;$HL -> A
$2039 | STAX D				;A -> $DE
$203a | LXI H, $4000		;$$ -> HL
$203d | LDA $4050			;$$ -> A
$2040 | CALL $21e1			;CALL $$
$2043 | LDA $4053			;$$ -> A
$2046 | MVI D, $c			;$ -> D
$2048 | CALL $16df			;CALL $$
$204b | LXI D, $4003		;$$ -> DE
$204e | LXI H, $171e		;$$ -> HL
$2051 | DAD B				;HL + BC -> HL
$2052 | MOV A, M			;$HL -> A
$2053 | STAX D				;A -> $DE
$2054 | INX D				;DE++
$2055 | LXI H, $175c		;$$ -> HL
$2058 | DAD B				;HL + BC -> HL
$2059 | MOV A, M			;$HL -> A
$205a | STAX D				;A -> $DE
$205b | LXI H, $4003		;$$ -> HL
$205e | LDA $4051			;$$ -> A
$2061 | CALL $21e1			;CALL $$
****  | moves some A21 settings 801e &= f, 801f &= 7 to 4002 / 4005
	$2064 | LXI D, $4050		;$$ -> DE
	$2067 | LXI H, $4002		;$$ -> HL
	$206a | MOV A, M			;$HL -> A
	$206b | ANI $40				;A &= $
	$206d | MOV B, A			;A -> B
	$206e | LDAX D				;$DE -> A
	$206f | ANI $f				;A &= $
	$2071 | ORA B				;A |= B
	$2072 | MOV M, A			;A -> $HL				(4002 = (4050 &= $f) | (4002 &= $40)) [][1][][][!PA0][!PA1][!ERUN][RUN]
	$2073 | INX D				;DE++					(DE = 4051)
	$2074 | LXI H, $4005		;$$ -> HL				(HL = 4005)
	$2077 | LDAX D				;$DE -> A
	$2078 | ANI $7				;A &= $
	$207a | MOV M, A			;A -> $HL		(4005 = 4051 &= $7) 
$207b | LDAX D				;$DE -> A
$207c | ANI $48				;A &= $
$207e | MVI B, $20			;$ -> B
$2080 | JZ $2093			;Zf = 1 : JMP $$
$2083 | MVI B, $0			;$ -> B
$2085 | CPI $48				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2087 | JZ $2093			;Zf = 1 : JMP $$
$208a | MVI B, $40			;$ -> B
$208c | CPI $8				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$208e | JZ $2093			;Zf = 1 : JMP $$
$2091 | MVI B, $80			;$ -> B
$2093 | MOV A, B			;B -> A
$2094 | ORA M				;A |= $HL
$2095 | MOV M, A			;A -> $HL
$2096 | INX D				;DE++
$2097 | INX D				;DE++
$2098 | LDAX D				;$DE -> A
$2099 | RLC					;A rotate left
$209a | RLC					;A rotate left
$209b | MOV B, A			;A -> B
$209c | ANI $8				;A &= $
$209e | ORA M				;A |= $HL
$209f | MOV M, A			;A -> $HL
$20a0 | MOV A, B			;B -> A
$20a1 | RLC					;A rotate left
$20a2 | RLC					;A rotate left
$20a3 | ANI $10				;A &= $
$20a5 | ORA M				;A |= $HL
$20a6 | MOV M, A			;A -> $HL
$20a7 | DCX D				;DE--
	$20a8 | LXI H, $4002		;$$ -> HL				(HL = 4002)
	$20ab | LDAX D				;$DE -> A				(DE = 4052, A = [YA1, YA4, YA5, YA6, YA7, !DIG, !CALA]
	$20ac | RLC					;A rotate left
	$20ad | RLC					;A rotate left
	$20ae | RLC					;A rotate left
	$20af | RLC					;A rotate left			A = [YA7, !DIG, !CALA, YA1, YA4, YA5, YA6]
	$20b0 | ANI $10				;A &= $					A = [][][][!CALA][][][][]
	$20b2 | ORA M				;A |= $HL				A = [][][][!CALA][!PA0][!PA1][!ERUN][RUN]
	$20b3 | MOV M, A			;A -> $HL				4002 = A
$20b4 | LXI D, $4005		;$$ -> DE				(DE = 4005)
$20b7 | MOV A, M			;$HL -> A				(A = 4002)
$20b8 | ANI $8				;A &= $					(A = [][][][][!PA0][][][])
$20ba | MVI B, $fb			;$ -> B					(B = fb)
$20bc | JNZ $20c5			;Zf = 0 : JMP $$		(if !PA0 != 0, jump)
$20bf | MOV A, M			;$HL -> A				(A = 4002)
$20c0 | ANA B				;A &= B					(A = A except !PA1)
$20c1 | MOV M, A			;A -> $HL				(4002 = A)
$20c2 | LDAX D				;$DE -> A				(A = 4005)
$20c3 | ANA B				;A &= B					(A = everything but !PB1)
$20c4 | STAX D				;A -> $DE				(4005 = A)
$20c5 | LDAX D				;$DE -> A				(A = 4005)
$20c6 | ANI $4				;A &= $
$20c8 | JZ $20d4			;Zf = 1 : JMP $$		(if !PB1 = 0, jump)
$20cb | MOV A, M			;$HL -> A				
$20cc | ANI $4				;A &= $
$20ce | JZ $20d4			;Zf = 1 : JMP $$
$20d1 | MOV A, M			;$HL -> A
$20d2 | ANA B				;A &= B
$20d3 | MOV M, A			;A -> $HL
$20d4 | MOV A, M			;$HL -> A
$20d5 | ANI $c				;A &= $
$20d7 | MOV B, A			;A -> B
$20d8 | LDAX D				;$DE -> A
$20d9 | ANI $4				;A &= $
$20db | ORA B				;A |= B
$20dc | CPI $8				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$20de | MVI B, $20			;$ -> B
$20e0 | JNZ $20e5			;Zf = 0 : JMP $$
$20e3 | MVI B, $0			;$ -> B
$20e5 | MOV A, M			;$HL -> A
$20e6 | ORA B				;A |= B
$20e7 | MOV M, A			;A -> $HL
$20e8 | RET					;return

$20e9 | LDA $4060			;$$ -> A
$20ec | LXI H, $1797		;$$ -> HL
$20ef | LXI B, $ffff		;$$ -> BC
$20f2 | INX H				;HL++
$20f3 | INX B				;BC++
$20f4 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$20f5 | JNZ $20f2			;Zf = 0 : JMP $$
$20f8 | LXI H, $174e		;$$ -> HL
$20fb | DAD B				;HL + BC -> HL
$20fc | LXI D, $4006		;$$ -> DE
$20ff | MOV A, M			;$HL -> A
$2100 | STAX D				;A -> $DE
$2101 | INX D				;DE++
$2102 | LXI H, $1773		;$$ -> HL
$2105 | DAD B				;HL + BC -> HL
$2106 | MOV A, M			;$HL -> A
$2107 | STAX D				;A -> $DE
$2108 | JMP $2022			;JMP $$

$210b | LXI H, $4040		;$$ -> HL
$210e | INX D				;DE++
$210f | LDAX D				;$DE -> A
$2110 | DCX D				;DE--
$2111 | ORA A				;A |= A
$2112 | JZ $213a			;Zf = 1 : JMP $$
$2115 | INX D				;DE++
$2116 | INX D				;DE++
$2117 | LDAX D				;$DE -> A
$2118 | STA $4075			;A -> $$
$211b | DCX D				;DE--
$211c | DCX D				;DE--
$211d | ANI $4				;A &= $
$211f | JNZ $213e			;Zf = 0 : JMP $$
$2122 | PUSH D				;DE -> STACK$$
$2123 | LDAX D				;$DE -> A
$2124 | MOV C, A			;A -> C
$2125 | INX D				;DE++
$2126 | LDAX D				;$DE -> A
$2127 | MOV B, A			;A -> B
$2128 | LDA $4061			;$$ -> A
$212b | ANI $80				;A &= $
$212d | CNZ $229b			;Cf = 0 : CALL $$
$2130 | MOV A, C			;C -> A
$2131 | XCHG				;HL <-> DE
$2132 | CALL $22b7			;CALL $$
$2135 | CALL $22b6			;CALL $$
$2138 | POP D				;STACK$$ -> DE
$2139 | RET					;return

$213a | CALL $2276			;CALL $$
$213d | RET					;return

$213e | CALL $226a			;CALL $$
$2141 | RET					;return

$2142 | LXI H, $4044		;$$ -> HL
$2145 | INX D				;DE++
$2146 | LDAX D				;$DE -> A
$2147 | DCX D				;DE--
$2148 | ORA A				;A |= A
$2149 | JZ $213a			;Zf = 1 : JMP $$
$214c | INX D				;DE++
$214d | INX D				;DE++
$214e | LDAX D				;$DE -> A
$214f | STA $4075			;A -> $$
$2152 | DCX D				;DE--
$2153 | DCX D				;DE--
$2154 | ANI $4				;A &= $
$2156 | JNZ $213e			;Zf = 0 : JMP $$
$2159 | DCX D				;DE--
$215a | LDAX D				;$DE -> A
$215b | INX D				;DE++
$215c | PUSH D				;DE -> STACK$$
$215d | ANI $8				;A &= $
$215f | JZ $2165			;Zf = 1 : JMP $$
$2162 | JMP $2123			;JMP $$

$2165 | INX D				;DE++
$2166 | INX D				;DE++
$2167 | LDAX D				;$DE -> A
$2168 | LXI D, $217f		;$$ -> DE
$216b | ANI $8				;A &= $
$216d | JNZ $2173			;Zf = 0 : JMP $$
$2170 | LXI D, $2183		;$$ -> DE
$2173 | MVI B, $4			;$ -> B
$2175 | LDAX D				;$DE -> A
$2176 | MOV M, A			;A -> $HL
$2177 | INX D				;DE++
$2178 | INX H				;HL++
$2179 | DCR B				;B--
$217a | JNZ $2175			;Zf = 0 : JMP $$
$217d | POP D				;STACK$$ -> DE
$217e | RET					;return

$217f | MOV B, H			;H -> B
$2180 | MOV B, H			;H -> B
$2181 | MOV B, C			;C -> B
$2182 | NOP
$2183 | MOV B, D			;D -> B
$2184 | MOV D, L			;L -> D
$2185 | MOV D, E			;E -> D
$2186 | NOP
$2187 | XRA A				;A = A XOR A
$2188 | STA $4075			;A -> $$
$218b | LXI H, $4048		;$$ -> HL
$218e | INX D				;DE++
$218f | LDAX D				;$DE -> A
$2190 | DCX D				;DE--
$2191 | ORA A				;A |= A
$2192 | JZ $213a			;Zf = 1 : JMP $$
$2195 | PUSH D				;DE -> STACK$$
$2196 | LDAX D				;$DE -> A
$2197 | MOV C, A			;A -> C
$2198 | INX D				;DE++
$2199 | LDAX D				;$DE -> A
$219a | MOV B, A			;A -> B
$219b | JMP $2130			;JMP $$

$219e | LXI H, $404c		;$$ -> HL
$21a1 | DCX D				;DE--
$21a2 | LDAX D				;$DE -> A
$21a3 | ANA A				;A &= A
$21a4 | JZ $213a			;Zf = 1 : JMP $$
$21a7 | DCX D				;DE--
$21a8 | DCX D				;DE--
$21a9 | LDAX D				;$DE -> A
$21aa | ANI $80				;A &= $
$21ac | JNZ $213e			;Zf = 0 : JMP $$
$21af | INX D				;DE++
$21b0 | INX D				;DE++
$21b1 | INX D				;DE++
$21b2 | MVI B, $2			;$ -> B
$21b4 | LXI H, $404f		;$$ -> HL
$21b7 | LDAX D				;$DE -> A
$21b8 | INX D				;DE++
$21b9 | ANI $f0				;A &= $
$21bb | JZ $21c8			;Zf = 1 : JMP $$
$21be | INX D				;DE++
$21bf | LDAX D				;$DE -> A
$21c0 | DCX D				;DE--
$21c1 | ANI $f				;A &= $
$21c3 | MVI M, $2d			;$ -> $HL
$21c5 | JNZ $21d2			;Zf = 0 : JMP $$
$21c8 | LDAX D				;$DE -> A
$21c9 | RRC					;A rotate right
$21ca | RRC					;A rotate right
$21cb | RRC					;A rotate right
$21cc | RRC					;A rotate right
$21cd | ANI $f				;A &= $
$21cf | ADI $30				;A += $
$21d1 | MOV M, A			;A -> $HL
$21d2 | DCX H				;HL--
$21d3 | LDAX D				;$DE -> A
$21d4 | ANI $f				;A &= $
$21d6 | ADI $30				;A += $
$21d8 | MOV M, A			;A -> $HL
$21d9 | DCX H				;HL--
$21da | INX D				;DE++
$21db | LDAX D				;$DE -> A
$21dc | DCR B				;B--
$21dd | JNZ $21c9			;Zf = 0 : JMP $$
$21e0 | RET					;return

$21e1 | ANI $30				;A &= $
$21e3 | CPI $20				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$21e5 | RNZ					;Zf != 0 : RET
$21e6 | MOV A, M			;$HL -> A
$21e7 | ANI $10				;A &= $
$21e9 | JNZ $21ee			;Zf = 0 : JMP $$
$21ec | INR M				;$HL++
$21ed | INR M				;$HL++
$21ee | DCR M				;$HL--
$21ef | MOV A, M			;$HL -> A
$21f0 | ANI $f				;A &= $
$21f2 | RNZ					;Zf != 0 : RET
$21f3 | MOV M, A			;A -> $HL
$21f4 | RET					;return

$21f5 | XRA A				;A = A XOR A
$21f6 | XRA M				;A = A XOR $HL
$21f7 | RLC					;A rotate left
$21f8 | INX H				;HL++
$21f9 | DCR C				;C--
$21fa | JNZ $21f6			;Zf = 0 : JMP $$
$21fd | MOV M, A			;A -> $HL
$21fe | RET					;return

$21ff | LXI D, $4059		;$$ -> DE
$2202 | LXI H, $0000		;$$ -> HL
$2205 | MVI C, $2			;$ -> C
$2207 | XRA A				;A = A XOR A
$2208 | MOV B, A			;A -> B
$2209 | LDAX D				;$DE -> A
$220a | PUSH PSW			;AF (ACC and FLAG = PSW processor status word) -> STACK$$
$220b | ANI $f0				;A &= $
$220d | RRC					;A rotate right
$220e | RRC					;A rotate right
$220f | RRC					;A rotate right
$2210 | RRC					;A rotate right
$2211 | STAX D				;A -> $DE
$2212 | MOV A, B			;B -> A
$2213 | CALL $2229			;CALL $$
$2216 | MOV B, A			;A -> B
$2217 | POP PSW				;STACK$$ -> AF (ACC and FLAG = PSW processor status word)
$2218 | PUSH PSW			;AF (ACC and FLAG = PSW processor status word) -> STACK$$
$2219 | ANI $f				;A &= $
$221b | STAX D				;A -> $DE
$221c | MOV A, B			;B -> A
$221d | CALL $2229			;CALL $$
$2220 | MOV B, A			;A -> B
$2221 | POP PSW				;STACK$$ -> AF (ACC and FLAG = PSW processor status word)
$2222 | STAX D				;A -> $DE
$2223 | INX D				;DE++
$2224 | DCR C				;C--
$2225 | JNZ $2209			;Zf = 0 : JMP $$
$2228 | RET					;return

$2229 | PUSH D				;DE -> STACK$$
$222a | PUSH H				;HL -> SP
$222b | MOV B, A			;A -> B
$222c | DAD H				;HL + HL -> HL
$222d | ADC A				;A += A + Cy
$222e | DAD H				;HL + HL -> HL
$222f | ADC A				;A += A + Cy
$2230 | POP D				;STACK$$ -> DE
$2231 | DAD D				;HL + DE -> HL
$2232 | ADC B				;B += A + Cy
$2233 | DAD H				;HL + HL -> HL
$2234 | ADC A				;A += A + Cy
$2235 | MOV B, A			;A -> B
$2236 | POP D				;STACK$$ -> DE
$2237 | LDAX D				;$DE -> A
$2238 | ADD L				;L += A
$2239 | MOV L, A			;A -> L
$223a | MOV A, H			;H -> A
$223b | ACI $0				;A += $ + Cf
$223d | MOV H, A			;A -> H
$223e | MOV A, B			;B -> A
$223f | ACI $0				;A += $ + Cf
$2241 | RET					;return

$2242 | LXI H, $406e		;$$ -> HL
$2245 | MOV A, M			;$HL -> A
$2246 | STA $407d			;A -> $$
$2249 | INX H				;HL++
$224a | DCR M				;$HL--
$224b | LXI H, $4067		;$$ -> HL
$224e | MOV D, M			;$HL -> D
$224f | INX H				;HL++
$2250 | MOV E, M			;$HL -> E
$2251 | LDA $4070			;$$ -> A
$2254 | ANI $18				;A &= $
$2256 | CPI $8				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2258 | JZ $2262			;Zf = 1 : JMP $$
$225b | DCX D				;DE--
$225c | CPI $10				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$225e | JZ $2263			;Zf = 1 : JMP $$
$2261 | RET					;return

$2262 | INX D				;DE++
$2263 | MOV M, E			;E -> $HL
$2264 | DCX H				;HL--
$2265 | MOV M, D			;D -> $HL
$2266 | CALL $17a5			;CALL $$
$2269 | RET					;return

$226a | MVI M, $46			;$ -> $HL
$226c | INX H				;HL++
$226d | MVI M, $46			;$ -> $HL
$226f | INX H				;HL++
$2270 | MVI M, $4f			;$ -> $HL
$2272 | INX H				;HL++
$2273 | MVI M, $0			;$ -> $HL
$2275 | RET					;return

$2276 | MVI M, $50			;$ -> $HL
$2278 | INX H				;HL++
$2279 | MVI M, $4f			;$ -> $HL
$227b | INX H				;HL++
$227c | MVI M, $4e			;$ -> $HL
$227e | INX H				;HL++
$227f | MVI M, $0			;$ -> $HL
$2281 | RET					;return

$2282 | LXI D, $2297		;$$ -> DE
$2285 | JMP $228b			;JMP $$

$2288 | LXI D, $2299		;$$ -> DE
$228b | LDA $4075			;$$ -> A
$228e | ANI $10				;A &= $
$2290 | JZ $2294			;Zf = 1 : JMP $$
$2293 | INX D				;DE++
$2294 | LDAX D				;$DE -> A
$2295 | MOV M, A			;A -> $HL
$2296 | RET					;return

$2297 | DCR L				;L--
$2298 | LHLD $2a00			;$$ -> HL
$229b | MOV A, B			;B -> A
$229c | ADD A				;A += A
$229d | DAA
$229e | MOV B, A			;A -> B
$229f | JNZ $22a5			;Zf = 0 : JMP $$
$22a2 | MVI B, $10			;$ -> B
$22a4 | RET					;return

$22a5 | MOV A, C			;C -> A
$22a6 | ANA A				;A &= A
$22a7 | JNZ $22ac			;Zf = 0 : JMP $$
$22aa | MVI C, $10			;$ -> C
$22ac | MOV A, C			;C -> A
$22ad | ANI $10				;A &= $
$22af | JZ $22b4			;Zf = 1 : JMP $$
$22b2 | INR C				;C++
$22b3 | INR C				;C++
$22b4 | DCR C				;C--
$22b5 | RET					;return

$22b6 | PCHL				;JMP $HL
$22b7 | PUSH D				;DE -> STACK$$
$22b8 | ANI $f				;A &= $
$22ba | LXI H, $22c8		;$$ -> HL
$22bd | ADD A				;A += A
$22be | MOV E, A			;A -> E
$22bf | MVI D, $0			;$ -> D
$22c1 | DAD D				;HL + DE -> HL
$22c2 | MOV E, M			;$HL -> E
$22c3 | INX H				;HL++
$22c4 | MOV D, M			;$HL -> D
$22c5 | XCHG				;HL <-> DE
$22c6 | POP D				;STACK$$ -> DE
$22c7 | RET					;return

$22c8 | JC $e922			;Cf = 1 : JMP $$
$22cb | SHLD $230c			;HL -> $$
$22ce | CMA					;1's compliment A (invert)
$22cf | INX H				;HL++
$22d0 | MOV L, D			;D -> L
$22d1 | INX H				;HL++
$22d2 | ADC H				;H += A + Cy
$22d3 | INX H				;HL++
$22d4 | SBB D				;A -= D - 1
$22d5 | INX H				;HL++
$22d6 | XRA B				;A = A XOR B
$22d7 | INX H				;HL++
$22d8 | ORA M				;A |= $HL
$22d9 | INX H				;HL++
$22da | XCHG				;HL <-> DE
$22db | CALL $2288			;CALL $$
$22de | INX H				;HL++
$22df | CALL $23c4			;CALL $$
$22e2 | INX H				;HL++
$22e3 | MVI M, $2e			;$ -> $HL
$22e5 | INX H				;HL++
$22e6 | MVI M, $20			;$ -> $HL
$22e8 | RET					;return

$22e9 | XCHG				;HL <-> DE
$22ea | MOV A, C			;C -> A
$22eb | ANI $10				;A &= $
$22ed | JZ $22fe			;Zf = 1 : JMP $$
$22f0 | MVI M, $33			;$ -> $HL
$22f2 | INX H				;HL++
$22f3 | CALL $2282			;CALL $$
$22f6 | INX H				;HL++
$22f7 | MVI M, $30			;$ -> $HL
$22f9 | INX H				;HL++
$22fa | CALL $23c4			;CALL $$
$22fd | RET					;return

$22fe | CALL $2288			;CALL $$
$2301 | INX H				;HL++
$2302 | CALL $23c4			;CALL $$
$2305 | INX H				;HL++
$2306 | MVI M, $20			;$ -> $HL
$2308 | INX H				;HL++
$2309 | MVI M, $20			;$ -> $HL
$230b | RET					;return

$230c | XCHG				;HL <-> DE
$230d | MOV A, C			;C -> A
$230e | ANI $10				;A &= $
$2310 | JZ $2321			;Zf = 1 : JMP $$
$2313 | MVI M, $33			;$ -> $HL
$2315 | INX H				;HL++
$2316 | CALL $2282			;CALL $$
$2319 | INX H				;HL++
$231a | CALL $23c4			;CALL $$
$231d | INX H				;HL++
$231e | MVI M, $20			;$ -> $HL
$2320 | RET					;return

$2321 | CALL $2288			;CALL $$
$2324 | INX H				;HL++
$2325 | MVI M, $30			;$ -> $HL
$2327 | INX H				;HL++
$2328 | CALL $23c4			;CALL $$
$232b | INX H				;HL++
$232c | MVI M, $20			;$ -> $HL
$232e | RET					;return

$232f | XCHG				;HL <-> DE
$2330 | MOV A, C			;C -> A
$2331 | ANI $10				;A &= $
$2333 | JZ $2343			;Zf = 1 : JMP $$
$2336 | MVI M, $33			;$ -> $HL
$2338 | INX H				;HL++
$2339 | MVI M, $2d			;$ -> $HL
$233b | INX H				;HL++
$233c | CALL $23c4			;CALL $$
$233f | INX H				;HL++
$2340 | MVI M, $2e			;$ -> $HL
$2342 | RET					;return

$2343 | MOV A, B			;B -> A
$2344 | CPI $90				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2346 | JZ $235c			;Zf = 1 : JMP $$
$2349 | ANI $f				;A &= $
$234b | JNZ $235c			;Zf = 0 : JMP $$
$234e | CALL $2288			;CALL $$
$2351 | INX H				;HL++
$2352 | MVI M, $30			;$ -> $HL
$2354 | INX H				;HL++
$2355 | MVI M, $30			;$ -> $HL
$2357 | INX H				;HL++
$2358 | CALL $23c4			;CALL $$
$235b | RET					;return

$235c | MVI M, $20			;$ -> $HL
$235e | INX H				;HL++
$235f | MVI M, $30			;$ -> $HL
$2361 | INX H				;HL++
$2362 | CALL $23cc			;CALL $$
$2365 | INX H				;HL++
$2366 | CALL $23c4			;CALL $$
$2369 | RET					;return

$236a | XCHG				;HL <-> DE
$236b | MOV A, C			;C -> A
$236c | ANI $10				;A &= $
$236e | JZ $237e			;Zf = 1 : JMP $$
$2371 | MVI M, $36			;$ -> $HL
$2373 | INX H				;HL++
$2374 | MVI M, $2d			;$ -> $HL
$2376 | INX H				;HL++
$2377 | MVI M, $30			;$ -> $HL
$2379 | INX H				;HL++
$237a | CALL $23c4			;CALL $$
$237d | RET					;return

$237e | MVI M, $30			;$ -> $HL
$2380 | INX H				;HL++
$2381 | MVI M, $30			;$ -> $HL
$2383 | INX H				;HL++
$2384 | CALL $23cc			;CALL $$
$2387 | INX H				;HL++
$2388 | CALL $23c4			;CALL $$
$238b | RET					;return

$238c | XCHG				;HL <-> DE
$238d | MVI M, $36			;$ -> $HL
$238f | INX H				;HL++
$2390 | MVI M, $2d			;$ -> $HL
$2392 | INX H				;HL++
$2393 | CALL $23c4			;CALL $$
$2396 | INX H				;HL++
$2397 | MVI M, $20			;$ -> $HL
$2399 | RET					;return

$239a | XCHG				;HL <-> DE
$239b | MVI M, $36			;$ -> $HL
$239d | INX H				;HL++
$239e | MVI M, $2d			;$ -> $HL
$23a0 | INX H				;HL++
$23a1 | CALL $23c4			;CALL $$
$23a4 | INX H				;HL++
$23a5 | MVI M, $2e			;$ -> $HL
$23a7 | RET					;return

$23a8 | XCHG				;HL <-> DE
$23a9 | MVI M, $39			;$ -> $HL
$23ab | INX H				;HL++
$23ac | MVI M, $2d			;$ -> $HL
$23ae | INX H				;HL++
$23af | MVI M, $30			;$ -> $HL
$23b1 | INX H				;HL++
$23b2 | CALL $23c4			;CALL $$
$23b5 | RET					;return

$23b6 | XCHG				;HL <-> DE
$23b7 | MVI M, $39			;$ -> $HL
$23b9 | INX H				;HL++
$23ba | MVI M, $2d			;$ -> $HL
$23bc | INX H				;HL++
$23bd | CALL $23c4			;CALL $$
$23c0 | INX H				;HL++
$23c1 | MVI M, $20			;$ -> $HL
$23c3 | RET					;return

$23c4 | MOV A, B			;B -> A
$23c5 | RRC					;A rotate right
$23c6 | RRC					;A rotate right
$23c7 | RRC					;A rotate right
$23c8 | RRC					;A rotate right
$23c9 | JMP $23cd			;JMP $$

$23cc | MOV A, B			;B -> A
$23cd | ANI $f				;A &= $
$23cf | ADI $30				;A += $
$23d1 | MOV M, A			;A -> $HL
$23d2 | RET					;return

$23d3 | LXI H, $4058		;$$ -> HL
$23d6 | XRA A				;A = A XOR A
$23d7 | MOV M, A			;A -> $HL
$23d8 | INX H				;HL++
$23d9 | MOV M, A			;A -> $HL
$23da | INX H				;HL++
$23db | MOV M, A			;A -> $HL
$23dc | STA $407d			;A -> $$
$23df | LXI H, $406f		;$$ -> HL
$23e2 | MOV M, A			;A -> $HL
$23e3 | INX H				;HL++
$23e4 | MOV A, M			;$HL -> A
$23e5 | ANI $2				;A &= $
$23e7 | MOV M, A			;A -> $HL
$23e8 | INX H				;HL++
$23e9 | MVI M, $1			;$ -> $HL
$23eb | LXI H, $4067		;$$ -> HL
$23ee | MVI M, $0			;$ -> $HL
$23f0 | INX H				;HL++
$23f1 | MVI M, $0			;$ -> $HL
$23f3 | LDA $4060			;$$ -> A
$23f6 | ANI $c0				;A &= $
$23f8 | CPI $c0				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$23fa | RZ					;Zf = 1 : RET
$23fb | MVI M, $3a			;$ -> $HL
$23fd | RET					;return

$23fe | LXI H, $4071		;$$ -> HL
$2401 | MOV A, M			;$HL -> A
$2402 | RLC					;A rotate left
$2403 | MOV M, A			;A -> $HL
$2404 | CPI $10				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2406 | RNZ					;Zf != 0 : RET
$2407 | MVI M, $1			;$ -> $HL
$2409 | RET					;return

$240a | CALL $248b			;CALL $$
$240d | ANI $c				;A &= $
$240f | RNZ					;Zf != 0 : RET
$2410 | LDA $4070			;$$ -> A
$2413 | ANI $ef				;A &= $
$2415 | ORI $8				;A |= $
$2417 | STA $4070			;A -> $$
$241a | MOV A, C			;C -> A
$241b | ANI $f0				;A &= $
$241d | STA $4058			;A -> $$
$2420 | PUSH B				;BC -> STACK$$
$2421 | CALL $24cc			;CALL $$
$2424 | POP B				;STACK$$ -> BC
$2425 | CALL $24b1			;CALL $$
$2428 | ANA A				;A &= A
$2429 | LXI H, $4058		;$$ -> HL
$242c | JNZ $2435			;Zf = 0 : JMP $$
$242f | MOV A, M			;$HL -> A
$2430 | ANI $f				;A &= $
$2432 | JZ $247e			;Zf = 1 : JMP $$
$2435 | MVI M, $c			;$ -> $HL
$2437 | INX H				;HL++
$2438 | MVI M, $99			;$ -> $HL
$243a | INX H				;HL++
$243b | MVI M, $99			;$ -> $HL
$243d | JMP $247e			;JMP $$

$2440 | CALL $248b			;CALL $$
$2443 | ANI $3				;A &= $
$2445 | RNZ					;Zf != 0 : RET
$2446 | LDA $4070			;$$ -> A
$2449 | ANI $f7				;A &= $
$244b | ORI $10				;A |= $
$244d | STA $4070			;A -> $$
$2450 | MOV A, C			;C -> A
$2451 | ANI $f0				;A &= $
$2453 | STA $4058			;A -> $$
$2456 | PUSH B				;BC -> STACK$$
$2457 | CALL $24c4			;CALL $$
$245a | POP B				;STACK$$ -> BC
$245b | CALL $24b1			;CALL $$
$245e | LXI H, $4058		;$$ -> HL
$2461 | MOV A, M			;$HL -> A
$2462 | ANA A				;A &= A
$2463 | JZ $247e			;Zf = 1 : JMP $$
$2466 | INX H				;HL++
$2467 | MOV A, M			;$HL -> A
$2468 | ANA A				;A &= A
$2469 | JNZ $2473			;Zf = 0 : JMP $$
$246c | INX H				;HL++
$246d | MOV A, M			;$HL -> A
$246e | ANI $f0				;A &= $
$2470 | JZ $247e			;Zf = 1 : JMP $$
$2473 | LXI H, $4058		;$$ -> HL
$2476 | MVI M, $f3			;$ -> $HL
$2478 | INX H				;HL++
$2479 | MVI M, $0			;$ -> $HL
$247b | INX H				;HL++
$247c | MVI M, $9			;$ -> $HL
$247e | MVI A, $7			;$ -> A
$2480 | STA $407d			;A -> $$
$2483 | LXI H, $406e		;$$ -> HL
$2486 | MOV M, A			;A -> $HL
$2487 | INX H				;HL++
$2488 | MVI M, $5			;$ -> $HL
$248a | RET					;return

$248b | LXI H, $24a7		;$$ -> HL
$248e | LDA $4071			;$$ -> A
$2491 | INX H				;HL++
$2492 | INX H				;HL++
$2493 | RRC					;A rotate right
$2494 | JNC $2491			;Cf = 0 : JMP $$
$2497 | LXI D, $4072		;$$ -> DE
$249a | MVI A, $0			;$ -> A
$249c | STAX D				;A -> $DE
$249d | INX D				;DE++
$249e | MOV A, M			;$HL -> A
$249f | STAX D				;A -> $DE
$24a0 | INX D				;DE++
$24a1 | INX H				;HL++
$24a2 | MOV A, M			;$HL -> A
$24a3 | STAX D				;A -> $DE
$24a4 | LDA $4058			;$$ -> A
$24a7 | MOV C, A			;A -> C
$24a8 | RET					;return

$24a9 | NOP
$24aa | LXI B, $1000		;$$ -> BC
$24ad | LXI B, $1000		;$$ -> BC
$24b0 | NOP
$24b1 | MOV B, A			;A -> B
$24b2 | LXI H, $4058		;$$ -> HL
$24b5 | LXI D, $4072		;$$ -> DE
$24b8 | MVI C, $3			;$ -> C
$24ba | LDAX D				;$DE -> A
$24bb | MOV M, A			;A -> $HL
$24bc | INX H				;HL++
$24bd | INX D				;DE++
$24be | DCR C				;C--
$24bf | JNZ $24ba			;Zf = 0 : JMP $$
$24c2 | MOV A, B			;B -> A
$24c3 | RET					;return

$24c4 | LDA $4072			;$$ -> A
$24c7 | XRI $f0				;A = A XOR $
$24c9 | STA $4072			;A -> $$
$24cc | LXI H, $4058		;$$ -> HL
$24cf | LXI D, $4072		;$$ -> DE
$24d2 | LDAX D				;$DE -> A
$24d3 | ORA A				;A |= A
$24d4 | MVI A, $0			;$ -> A
$24d6 | STAX D				;A -> $DE
$24d7 | JNZ $250d			;Zf = 0 : JMP $$
$24da | MOV A, M			;$HL -> A
$24db | ORA A				;A |= A
$24dc | JNZ $24f3			;Zf = 0 : JMP $$
$24df | CALL $2546			;CALL $$
$24e2 | MVI A, $0			;$ -> A
$24e4 | RNC
$24e5 | LXI H, $4074		;$$ -> HL
$24e8 | MVI M, $99			;$ -> $HL
$24ea | DCX H				;HL--
$24eb | MVI M, $99			;$ -> $HL
$24ed | DCX H				;HL--
$24ee | MVI M, $0			;$ -> $HL
$24f0 | MVI A, $ff			;$ -> A
$24f2 | RET					;return

$24f3 | INX H				;HL++
$24f4 | INX D				;DE++
$24f5 | LDAX D				;$DE -> A
$24f6 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$24f7 | JC $2504			;Cf = 1 : JMP $$
$24fa | JNZ $252f			;Zf = 0 : JMP $$
$24fd | INX H				;HL++
$24fe | INX D				;DE++
$24ff | LDAX D				;$DE -> A
$2500 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$2501 | JNC $252f			;Cf = 0 : JMP $$
$2504 | MVI A, $f0			;$ -> A
$2506 | STA $4072			;A -> $$
$2509 | CALL $255a			;CALL $$
$250c | RET					;return

$250d | MOV A, M			;$HL -> A
$250e | ORA A				;A |= A
$250f | JZ $2518			;Zf = 1 : JMP $$
$2512 | MVI A, $f0			;$ -> A
$2514 | STAX D				;A -> $DE
$2515 | JMP $24df			;JMP $$

$2518 | INX H				;HL++
$2519 | INX D				;DE++
$251a | XCHG				;HL <-> DE
$251b | LDAX D				;$DE -> A
$251c | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$251d | JC $252a			;Cf = 1 : JMP $$
$2520 | JNZ $2542			;Zf = 0 : JMP $$
$2523 | INX H				;HL++
$2524 | INX D				;DE++
$2525 | LDAX D				;$DE -> A
$2526 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$2527 | JNC $2542			;Cf = 0 : JMP $$
$252a | MVI A, $f0			;$ -> A
$252c | STA $4072			;A -> $$
$252f | LXI H, $4059		;$$ -> HL
$2532 | LXI D, $4073		;$$ -> DE
$2535 | MVI C, $2			;$ -> C
$2537 | LDAX D				;$DE -> A
$2538 | MOV B, M			;$HL -> B
$2539 | MOV M, A			;A -> $HL
$253a | MOV A, B			;B -> A
$253b | STAX D				;A -> $DE
$253c | INX D				;DE++
$253d | INX H				;HL++
$253e | DCR C				;C--
$253f | JNZ $2537			;Zf = 0 : JMP $$
$2542 | CALL $255a			;CALL $$
$2545 | RET					;return

$2546 | LXI H, $405a		;$$ -> HL
$2549 | LXI D, $4074		;$$ -> DE
$254c | MVI C, $2			;$ -> C
$254e | XRA A				;A = A XOR A
$254f | LDAX D				;$DE -> A
$2550 | ADC M				;$HL += A + Cy
$2551 | DAA
$2552 | STAX D				;A -> $DE
$2553 | DCX H				;HL--
$2554 | DCX D				;DE--
$2555 | DCR C				;C--
$2556 | JNZ $254f			;Zf = 0 : JMP $$
$2559 | RET					;return

$255a | LXI D, $405a		;$$ -> DE
$255d | LXI H, $4074		;$$ -> HL
$2560 | MVI C, $2			;$ -> C
$2562 | STC					;set Cy flag
$2563 | MVI A, $99			;$ -> A
$2565 | ACI $0				;A += $ + Cf
$2567 | SUB M				;A -= $HL
$2568 | XCHG				;HL <-> DE
$2569 | ADD M				;$HL += A
$256a | DAA
$256b | STAX D				;A -> $DE
$256c | XCHG				;HL <-> DE
$256d | DCX D				;DE--
$256e | DCX H				;HL--
$256f | DCR C				;C--
$2570 | JNZ $2563			;Zf = 0 : JMP $$
$2573 | XRA A				;A = A XOR A
$2574 | RET					;return

$2575 | CALL $259a			;CALL $$
$2578 | CZ $25b7			;Zf = 1 : CALL $$
$257b | CNZ $2607			;Cf = 0 : CALL $$
$257e | CALL $25ae			;CALL $$
$2581 | LDA $405b			;$$ -> A
$2584 | ANA A				;A &= A
$2585 | MVI A, $0			;$ -> A
$2587 | RZ					;Zf = 1 : RET
$2588 | MVI A, $ff			;$ -> A
$258a | RET					;return

$258b | CALL $259a			;CALL $$
$258e | DCX H				;HL--
$258f | DCX H				;HL--
$2590 | CZ $25c7			;Zf = 1 : CALL $$
$2593 | CNZ $263f			;Cf = 0 : CALL $$
$2596 | CALL $25ae			;CALL $$
$2599 | RET					;return

$259a | MOV B, M			;$HL -> B
$259b | LXI H, $405b		;$$ -> HL
$259e | LXI D, $4059		;$$ -> DE
$25a1 | MVI M, $0			;$ -> $HL
$25a3 | INX H				;HL++
$25a4 | LDAX D				;$DE -> A
$25a5 | MOV M, A			;A -> $HL
$25a6 | INX D				;DE++
$25a7 | INX H				;HL++
$25a8 | LDAX D				;$DE -> A
$25a9 | MOV M, A			;A -> $HL
$25aa | MOV A, B			;B -> A
$25ab | CPI $2				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$25ad | RET					;return

$25ae | LXI H, $4059		;$$ -> HL
$25b1 | LXI D, $405c		;$$ -> DE
$25b4 | JMP $25a4			;JMP $$

$25b7 | XRA A				;A = A XOR A
$25b8 | MOV A, M			;$HL -> A
$25b9 | ADC A				;A += A + Cy
$25ba | DAA
$25bb | MOV M, A			;A -> $HL
$25bc | DCX H				;HL--
$25bd | MOV A, M			;$HL -> A
$25be | ADC A				;A += A + Cy
$25bf | DAA
$25c0 | MOV M, A			;A -> $HL
$25c1 | DCX H				;HL--
$25c2 | MOV A, M			;$HL -> A
$25c3 | ADC A				;A += A + Cy
$25c4 | MOV M, A			;A -> $HL
$25c5 | XRA A				;A = A XOR A
$25c6 | RET					;return

$25c7 | XRA A				;A = A XOR A
$25c8 | MVI B, $3			;$ -> B
$25ca | MOV A, M			;$HL -> A
$25cb | ANI $f				;A &= $
$25cd | MOV C, A			;A -> C
$25ce | MOV A, M			;$HL -> A
$25cf | RAR					;A > Cy rotate right
$25d0 | MOV E, A			;A -> E
$25d1 | ANI $8				;A &= $
$25d3 | MOV A, C			;C -> A
$25d4 | JZ $25d9			;Zf = 1 : JMP $$
$25d7 | ADI $a				;A += $
$25d9 | RAR					;A > Cy rotate right
$25da | MOV C, A			;A -> C
$25db | MVI A, $0			;$ -> A
$25dd | ACI $0				;A += $ + Cf
$25df | MOV D, A			;A -> D
$25e0 | MOV A, E			;E -> A
$25e1 | ANI $f0				;A &= $
$25e3 | ORA C				;A |= C
$25e4 | MOV M, A			;A -> $HL
$25e5 | INX H				;HL++
$25e6 | DCR B				;B--
$25e7 | JZ $25f9			;Zf = 1 : JMP $$
$25ea | MOV A, M			;$HL -> A
$25eb | ANI $f				;A &= $
$25ed | MOV C, A			;A -> C
$25ee | MOV A, D			;D -> A
$25ef | ANA A				;A &= A
$25f0 | MOV A, M			;$HL -> A
$25f1 | JZ $25ce			;Zf = 1 : JMP $$
$25f4 | ADI $a0				;A += $
$25f6 | JMP $25cf			;JMP $$

$25f9 | MOV A, D			;D -> A
$25fa | LXI D, $405b		;$$ -> DE
$25fd | MVI B, $5			;$ -> B
$25ff | ANA A				;A &= A
$2600 | JNZ $2605			;Zf = 0 : JMP $$
$2603 | MVI B, $0			;$ -> B
$2605 | XRA A				;A = A XOR A
$2606 | RET					;return

$2607 | MOV A, M			;$HL -> A
$2608 | RLC					;A rotate left
$2609 | RLC					;A rotate left
$260a | RLC					;A rotate left
$260b | RLC					;A rotate left
$260c | MOV C, A			;A -> C
$260d | ANI $f0				;A &= $
$260f | MOV M, A			;A -> $HL
$2610 | DCX H				;HL--
$2611 | MOV A, M			;$HL -> A
$2612 | RLC					;A rotate left
$2613 | RLC					;A rotate left
$2614 | RLC					;A rotate left
$2615 | RLC					;A rotate left
$2616 | MOV B, A			;A -> B
$2617 | ANI $f0				;A &= $
$2619 | MOV E, A			;A -> E
$261a | MOV A, C			;C -> A
$261b | ANI $f				;A &= $
$261d | ORA E				;A |= E
$261e | MOV M, A			;A -> $HL
$261f | DCX H				;HL--
$2620 | MOV A, M			;$HL -> A
$2621 | RLC					;A rotate left
$2622 | RLC					;A rotate left
$2623 | RLC					;A rotate left
$2624 | RLC					;A rotate left
$2625 | ANI $f0				;A &= $
$2627 | MOV E, A			;A -> E
$2628 | MOV A, B			;B -> A
$2629 | ANI $f				;A &= $
$262b | ORA E				;A |= E
$262c | MOV M, A			;A -> $HL
$262d | LXI H, $405b		;$$ -> HL
$2630 | CALL $25c7			;CALL $$
$2633 | XCHG				;HL <-> DE
$2634 | CALL $25c7			;CALL $$
$2637 | LDAX D				;$DE -> A
$2638 | ANA A				;A &= A
$2639 | MVI B, $0			;$ -> B
$263b | RZ					;Zf = 1 : RET
$263c | MVI A, $ff			;$ -> A
$263e | RET					;return

$263f | INX H				;HL++
$2640 | INX H				;HL++
$2641 | CALL $25b7			;CALL $$
$2644 | INX H				;HL++
$2645 | INX H				;HL++
$2646 | CALL $25b7			;CALL $$
$2649 | MOV A, M			;$HL -> A
$264a | RRC					;A rotate right
$264b | RRC					;A rotate right
$264c | RRC					;A rotate right
$264d | RRC					;A rotate right
$264e | MOV C, A			;A -> C
$264f | ANI $f				;A &= $
$2651 | MOV M, A			;A -> $HL
$2652 | INX H				;HL++
$2653 | MOV A, M			;$HL -> A
$2654 | RRC					;A rotate right
$2655 | RRC					;A rotate right
$2656 | RRC					;A rotate right
$2657 | RRC					;A rotate right
$2658 | MOV B, A			;A -> B
$2659 | ANI $f				;A &= $
$265b | MOV E, A			;A -> E
$265c | MOV A, C			;C -> A
$265d | ANI $f0				;A &= $
$265f | ORA E				;A |= E
$2660 | MOV M, A			;A -> $HL
$2661 | INX H				;HL++
$2662 | MOV A, M			;$HL -> A
$2663 | RRC					;A rotate right
$2664 | RRC					;A rotate right
$2665 | RRC					;A rotate right
$2666 | RRC					;A rotate right
$2667 | MOV C, A			;A -> C
$2668 | ANI $f				;A &= $
$266a | MOV E, A			;A -> E
$266b | MOV A, B			;B -> A
$266c | ANI $f0				;A &= $
$266e | ORA E				;A |= E
$266f | MOV M, A			;A -> $HL
$2670 | MOV A, C			;C -> A
$2671 | RRC					;A rotate right
$2672 | RRC					;A rotate right
$2673 | RRC					;A rotate right
$2674 | RRC					;A rotate right
$2675 | ANI $f				;A &= $
$2677 | MOV B, A			;A -> B
$2678 | RET					;return

$2679 | LXI D, $4058		;$$ -> DE
$267c | LXI H, $4059		;$$ -> HL
$267f | LDAX D				;$DE -> A
$2680 | ANI $c				;A &= $
$2682 | JNZ $26c3			;Zf = 0 : JMP $$
$2685 | MOV A, M			;$HL -> A
$2686 | CPI $2				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2688 | JNC $26c3			;Cf = 0 : JMP $$
$268b | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$268d | JNZ $2698			;Zf = 0 : JMP $$
$2690 | INX H				;HL++
$2691 | MOV A, M			;$HL -> A
$2692 | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$2694 | JNC $26c3			;Cf = 0 : JMP $$
$2697 | DCX H				;HL--
$2698 | DCX H				;HL--
$2699 | MOV A, M			;$HL -> A
$269a | ANI $f0				;A &= $
$269c | JNZ $26cc			;Zf = 0 : JMP $$
$269f | MVI M, $0			;$ -> $HL
$26a1 | MVI D, $0			;$ -> D
$26a3 | LXI H, $406f		;$$ -> HL
$26a6 | MOV A, M			;$HL -> A
$26a7 | CPI $5				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$26a9 | MVI A, $0			;$ -> A
$26ab | MOV B, A			;A -> B
$26ac | JNZ $26b7			;Zf = 0 : JMP $$
$26af | MVI C, $2			;$ -> C
$26b1 | LDA $406e			;$$ -> A
$26b4 | RLC					;A rotate left
$26b5 | MOV B, A			;A -> B
$26b6 | MOV A, C			;C -> A
$26b7 | MOV M, A			;A -> $HL
$26b8 | DCX H				;HL--
$26b9 | MOV M, B			;B -> $HL
$26ba | STA $407d			;A -> $$
$26bd | PUSH D				;DE -> STACK$$
$26be | CALL $21ff			;CALL $$
$26c1 | POP D				;STACK$$ -> DE
$26c2 | RET					;return

$26c3 | XCHG				;HL <-> DE
$26c4 | MVI M, $c			;$ -> $HL
$26c6 | INX H				;HL++
$26c7 | MVI M, $1			;$ -> $HL
$26c9 | JMP $26d2			;JMP $$

$26cc | XCHG				;HL <-> DE
$26cd | MVI M, $f3			;$ -> $HL
$26cf | INX H				;HL++
$26d0 | MVI M, $0			;$ -> $HL
$26d2 | INX H				;HL++
$26d3 | MVI M, $0			;$ -> $HL
$26d5 | MVI D, $ff			;$ -> D
$26d7 | JMP $26a3			;JMP $$

	### SERVICE 3 ###
$26da | LXI H, $80a0		;$$ -> HL
$26dd | MVI B, $10			;$ -> B
$26df | MVI M, $31			;$ -> $HL
$26e1 | INX H				;HL++
$26e2 | DCR B				;B--
$26e3 | JNZ $26df			;Zf = 0 : JMP $$
$26e6 | MVI A, $c0			;$ -> A
$26e8 | SIM					;set interrupt mask <- A
$26e9 | MVI A, $40			;$ -> A
$26eb | SIM					;set interrupt mask <- A
$26ec | MVI A, $aa			;$ -> A
$26ee | STA $4000			;A -> $$
$26f1 | LXI H, $8003		;$$ -> HL
$26f4 | MOV M, A			;A -> $HL
$26f5 | INX H				;HL++
$26f6 | INX H				;HL++
$26f7 | MOV M, A			;A -> $HL
$26f8 | INX H				;HL++
$26f9 | MOV M, A			;A -> $HL
$26fa | LXI H, $8083		;$$ -> HL
$26fd | MOV M, A			;A -> $HL
$26fe | INX H				;HL++
$26ff | INX H				;HL++
$2700 | MOV M, A			;A -> $HL
$2701 | INX H				;HL++
$2702 | MOV M, A			;A -> $HL
$2703 | LDA $4000			;$$ -> A
$2706 | STA $8020			;A -> $$
$2709 | MVI A, $87			;$ -> A
$270b | STA $8040			;A -> $$
$270e | MVI A, $3f			;$ -> A
$2710 | STA $c000			;A -> $$
$2713 | MVI A, $7			;$ -> A
$2715 | STA $8040			;A -> $$
$2718 | MVI A, $a1			;$ -> A
$271a | STA $8060			;A -> $$
$271d | LDA $c000			;$$ -> A
$2720 | STA $80b4			;A -> $$
$2723 | LDA $8023			;$$ -> A
$2726 | STA $80b0			;A -> $$
$2729 | SHLD $c0ff			;HL -> $$
$272c | JMP $26da			;JMP $$

		### SERVICE 2 ###
	$272f | LXI H, $5000		;$$ -> HL
	$2732 | DCX H				;HL--
	$2733 | CALL $27d0			;CALL $$
	$2736 | MOV A, H			;H -> A
	$2737 | ORA L				;A |= L
	$2738 | JNZ $2732			;Zf = 0 : JMP $$
	$273b | LXI SP, $4100		;$$ -> SP
	$273e | MVI A, $32			;$ -> A
	$2740 | CALL $27d9			;CALL $$
	$2743 | MVI A, $ff			;$ -> A
	$2745 | STA $8020			;A -> $$
	$2748 | STA $8083			;A -> $$
	$274b | STA $4000			;A -> $$
	$274e | CALL $275a			;CALL $$
	$2751 | CALL $2780			;CALL $$
	$2754 | CALL $27d0			;CALL $$
	$2757 | JMP $2743			;JMP $$

	$275a | MVI A, $87			;$ -> A
	$275c | STA $8040			;A -> $$
	$275f | MVI A, $1			;$ -> A
	$2761 | LXI H, $c000		;$$ -> HL
	$2764 | MOV M, A			;A -> $HL
	$2765 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
	$2766 | JZ $2776			;Zf = 1 : JMP $$
	$2769 | MOV B, A			;A -> B
	$276a | LDA $4000			;$$ -> A
	$276d | ANI $fb				;A &= $
	$276f | STA $8020			;A -> $$
	$2772 | MOV A, B			;B -> A
	$2773 | JMP $2764			;JMP $$

	$2776 | RAL					;Cy < A rotate left
	$2777 | JNC $2764			;Cf = 0 : JMP $$
	$277a | MVI A, $f			;$ -> A
	$277c | STA $8040			;A -> $$
	$277f | RET					;return

	$2780 | MVI A, $87			;$ -> A
	$2782 | STA $8040			;A -> $$
	$2785 | XRA A				;A = A XOR A
	$2786 | LXI H, $c000		;$$ -> HL
	$2789 | MOV M, A			;A -> $HL
	$278a | INR A				;A++
	$278b | INR L				;L++
	$278c | JNZ $2789			;Zf = 0 : JMP $$
	$278f | XRA A				;A = A XOR A
	$2790 | LXI H, $c000		;$$ -> HL
	$2793 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
	$2794 | JZ $27a4			;Zf = 1 : JMP $$
	$2797 | MOV B, A			;A -> B
	$2798 | LDA $4000			;$$ -> A
	$279b | ANI $fd				;A &= $
	$279d | STA $8020			;A -> $$
	$27a0 | MOV A, B			;B -> A
	$27a1 | JMP $2793			;JMP $$

	$27a4 | INR A				;A++
	$27a5 | INR L				;L++
	$27a6 | JNZ $2793			;Zf = 0 : JMP $$
	$27a9 | RET					;return

		### SERVICE 1 ###
	$27aa | LXI H, $5000		;$$ -> HL
	$27ad | DCX H				;HL--
	$27ae | CALL $27d0			;CALL $$
	$27b1 | MOV A, H			;H -> A
	$27b2 | ORA L				;A |= L
	$27b3 | JNZ $27ad			;Zf = 0 : JMP $$
	$27b6 | LXI SP, $4100		;$$ -> SP
	$27b9 | MVI A, $33			;$ -> A
	$27bb | CALL $27d9			;CALL $$
	$27be | CALL $01de			;CALL $$
	$27c1 | CALL $1241			;CALL $$
	$27c4 | LDA $4066			;$$ -> A
	$27c7 | STA $80b4			;A -> $$
	$27ca | CALL $27d0			;CALL $$
	$27cd | JMP $27be			;JMP $$

	$27d0 | MVI A, $ff			;$ -> A
	$27d2 | STA $8020			;A -> $$
	$27d5 | SHLD $c0ff			;HL -> $$
	$27d8 | RET					;return

	$27d9 | LXI H, $80a0		;$$ -> HL
	$27dc | MVI B, $10			;$ -> B
	$27de | MOV M, A			;A -> $HL
	$27df | INX H				;HL++
	$27e0 | DCR B				;B--
	$27e1 | JNZ $27de			;Zf = 0 : JMP $$
	$27e4 | RET					;return

	$27e5 | RST 7				;restart
	$27e6 | RST 7				;restart
	$27e7 | RST 7				;restart
	$27e8 | RST 7				;restart
	$27e9 | RST 7				;restart
	$27ea | RST 7				;restart
	$27eb | RST 7				;restart
	$27ec | RST 7				;restart
	$27ed | RST 7				;restart
	$27ee | RST 7				;restart
	$27ef | RST 7				;restart
	$27f0 | RST 7				;restart
	$27f1 | RST 7				;restart
	$27f2 | RST 7				;restart
	$27f3 | RST 7				;restart
	$27f4 | RST 7				;restart
	$27f5 | RET					;return

	$27f6 | RST 7				;restart
	$27f7 | RST 7				;restart
	$27f8 | RST 7				;restart
	$27f9 | RET					;return

	$27fa | RST 7				;restart
	$27fb | RST 7				;restart
	$27fc | NOP				; ff checksum
	$27fd | NOP				; 55 checksum
	$27fe | NOP				; 33 checksum
	$27ff | NOP				; f5 checksum

### ROM 3 ###

$3000 | SUB B				;A -= B
$3001 | MOV B, B			;B -> B
$3002 | JMP $3031			;JMP $$

$3005 | JMP $300b			;JMP $$

$3008 | JMP $3061			;JMP $$

$300b | LXI H, $4090		;$$ -> HL
$300e | MVI B, $4f			;$ -> B
$3010 | XRA A				;A = A XOR A
$3011 | MOV M, A			;A -> $HL
$3012 | INR L				;L++
$3013 | DCR B				;B--
$3014 | JNZ $3011			;Zf = 0 : JMP $$
$3017 | LXI H, $4094		;$$ -> HL
$301a | MVI M, $1			;$ -> $HL
$301c | INX H				;HL++
$301d | MVI M, $2			;$ -> $HL
$301f | MVI A, $3			;$ -> A
$3021 | STA $40b6			;A -> $$
$3024 | CALL $3bdc			;CALL $$
$3027 | LXI H, $0001		;$$ -> HL
$302a | SHLD $40bd			;HL -> $$
$302d | STA $80e3			;A -> $$
$3030 | RET					;return

$3031 | PUSH PSW			;AF (ACC and FLAG = PSW processor status word) -> STACK$$
$3032 | PUSH H				;HL -> SP
$3033 | PUSH D				;DE -> STACK$$
$3034 | PUSH B				;BC -> STACK$$
$3035 | LDA $4062			;$$ -> A
$3038 | ANI $fd				;A &= $
$303a | STA $4062			;A -> $$
$303d | STA $8020			;A -> $$
$3040 | CALL $3bd6			;CALL $$
$3043 | XRA A				;A = A XOR A
$3044 | STA $40bb			;A -> $$
$3047 | LDA $40ac			;$$ -> A
$304a | ANI $f0				;A &= $
$304c | CPI $40				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$304e | JZ $305b			;Zf = 1 : JMP $$
$3051 | CPI $20				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3053 | JZ $305b			;Zf = 1 : JMP $$
$3056 | POP B				;STACK$$ -> BC
$3057 | POP D				;STACK$$ -> DE
$3058 | POP H				;STACK$$ -> HL
$3059 | POP PSW				;STACK$$ -> AF (ACC and FLAG = PSW processor status word)
$305a | RET					;return

$305b | POP B				;STACK$$ -> BC
$305c | POP D				;STACK$$ -> DE
$305d | POP H				;STACK$$ -> HL
$305e | POP PSW				;STACK$$ -> AF (ACC and FLAG = PSW processor status word)
$305f | EI					;enable interrupts
$3060 | RET					;return

$3061 | LDA $4062			;$$ -> A
$3064 | ANI $2				;A &= $
$3066 | JNZ $30ab			;Zf = 0 : JMP $$
$3069 | CALL $387c			;CALL $$
$306c | LDA $4057			;$$ -> A
$306f | ORI $80				;A |= $
$3071 | STA $4057			;A -> $$
$3074 | LDA $406b			;$$ -> A
$3077 | ANI $fe				;A &= $
$3079 | STA $406b			;A -> $$
$307c | MVI A, $c0			;$ -> A
$307e | STA $4066			;A -> $$
$3081 | STA $80b4			;A -> $$
$3084 | LXI H, $40bf		;$$ -> HL
$3087 | DCR M				;$HL--
$3088 | JNZ $3095			;Zf = 0 : JMP $$
$308b | MVI M, $1			;$ -> $HL
$308d | LDA $4054			;$$ -> A
$3090 | ORI $3				;A |= $
$3092 | STA $4054			;A -> $$
$3095 | LDA $406b			;$$ -> A
$3098 | ANI $40				;A &= $
$309a | JZ $30b0			;Zf = 1 : JMP $$
$309d | LDA $4050			;$$ -> A
$30a0 | ORI $80				;A |= $
$30a2 | STA $4050			;A -> $$
$30a5 | CALL $3871			;CALL $$
$30a8 | JMP $30b0			;JMP $$

$30ab | MVI A, $ff			;$ -> A
$30ad | STA $40b8			;A -> $$
$30b0 | CALL $3302			;CALL $$
$30b3 | MVI A, $8			;$ -> A
$30b5 | SIM					;set interrupt mask <- A
$30b6 | EI					;enable interrupts
$30b7 | LDA $0000			;$$ -> A
$30ba | LDA $0000			;$$ -> A
$30bd | LDA $40ac			;$$ -> A
$30c0 | ANA A				;A &= A
$30c1 | JNZ $30fa			;Zf = 0 : JMP $$
$30c4 | LDA $40bb			;$$ -> A
$30c7 | ANA A				;A &= A
$30c8 | RNZ					;Zf != 0 : RET
$30c9 | MVI A, $ff			;$ -> A
$30cb | STA $40bb			;A -> $$
$30ce | DI					;disable interrupts
$30cf | LDA $4090			;$$ -> A
$30d2 | ANI $20				;A &= $
$30d4 | JNZ $310e			;Zf = 0 : JMP $$
$30d7 | LDA $4090			;$$ -> A
$30da | ANI $df				;A &= $
$30dc | STA $4090			;A -> $$
$30df | ANI $2				;A &= $
$30e1 | JNZ $3188			;Zf = 0 : JMP $$
$30e4 | LDA $4097			;$$ -> A
$30e7 | RRC					;A rotate right
$30e8 | RRC					;A rotate right
$30e9 | RRC					;A rotate right
$30ea | ANI $2				;A &= $
$30ec | MOV B, A			;A -> B
$30ed | LDA $4062			;$$ -> A
$30f0 | ANI $fd				;A &= $
$30f2 | ORA B				;A |= B
$30f3 | STA $4062			;A -> $$
$30f6 | STA $8020			;A -> $$
$30f9 | RET					;return

$30fa | RRC					;A rotate right
$30fb | RRC					;A rotate right
$30fc | RRC					;A rotate right
$30fd | ANI $1e				;A &= $
$30ff | MOV E, A			;A -> E
$3100 | MVI D, $0			;$ -> D
$3102 | LXI H, $32e2		;$$ -> HL
$3105 | DAD D				;HL + DE -> HL
$3106 | MOV E, M			;$HL -> E
$3107 | INX H				;HL++
$3108 | MOV D, M			;$HL -> D
$3109 | XCHG				;HL <-> DE
$310a | PCHL				;JMP $HL
$310b | JMP $30b3			;JMP $$

$310e | CALL $3871			;CALL $$
$3111 | LDA $4097			;$$ -> A
$3114 | MOV B, A			;A -> B
$3115 | ANI $80				;A &= $
$3117 | JZ $3153			;Zf = 1 : JMP $$
$311a | MVI D, $2f			;$ -> D
$311c | LXI H, $40a0		;$$ -> HL
$311f | XRA A				;A = A XOR A
$3120 | MOV M, A			;A -> $HL
$3121 | INR L				;L++
$3122 | DCR D				;D--
$3123 | JNZ $3120			;Zf = 0 : JMP $$
$3126 | LXI H, $32da		;$$ -> HL
$3129 | LXI D, $4050		;$$ -> DE
$312c | MVI C, $8			;$ -> C
$312e | MOV A, M			;$HL -> A
$312f | STAX D				;A -> $DE
$3130 | INX H				;HL++
$3131 | INR E				;E++
$3132 | DCR C				;C--
$3133 | JNZ $312e			;Zf = 0 : JMP $$
$3136 | LXI H, $0000		;$$ -> HL
$3139 | SHLD $4058			;HL -> $$
$313c | XRA A				;A = A XOR A
$313d | STA $405a			;A -> $$
$3140 | MVI A, $3			;$ -> A
$3142 | STA $40b6			;A -> $$
$3145 | STA $40bf			;A -> $$
$3148 | LXI H, $0001		;$$ -> HL
$314b | SHLD $40bd			;HL -> $$
$314e | MVI A, $ff			;$ -> A
$3150 | STA $40b8			;A -> $$
$3153 | MOV A, B			;B -> A
$3154 | ANI $40				;A &= $
$3156 | JZ $3175			;Zf = 1 : JMP $$
$3159 | LDA $4050			;$$ -> A
$315c | ANI $7f				;A &= $
$315e | STA $4050			;A -> $$
$3161 | XRA A				;A = A XOR A
$3162 | STA $40b8			;A -> $$
$3165 | LDA $405f			;$$ -> A
$3168 | ANI $97				;A &= $
$316a | STA $405f			;A -> $$
$316d | LDA $405e			;$$ -> A
$3170 | ANI $fe				;A &= $
$3172 | STA $405e			;A -> $$
$3175 | MOV A, B			;B -> A
$3176 | ANI $30				;A &= $
$3178 | CPI $30				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$317a | CZ $3bd0			;Zf = 1 : CALL $$
$317d | LDA $4097			;$$ -> A
$3180 | ANI $1f				;A &= $
$3182 | STA $4097			;A -> $$
$3185 | JMP $30d7			;JMP $$

$3188 | CALL $3bd3			;CALL $$
$318b | LDA $40ab			;$$ -> A
$318e | ADI $a4				;A += $
$3190 | MOV L, A			;A -> L
$3191 | MVI H, $40			;$ -> H
$3193 | LDA $40a0			;$$ -> A
$3196 | MOV M, A			;A -> $HL
$3197 | INR L				;L++
$3198 | MOV A, L			;L -> A
$3199 | SUI $a4				;A -= $
$319b | STA $40ab			;A -> $$
$319e | CPI $2				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$31a0 | JC $328f			;Cf = 1 : JMP $$
$31a3 | LDA $40a4			;$$ -> A
$31a6 | CPI $1b				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$31a8 | JNZ $31b3			;Zf = 0 : JMP $$
$31ab | LDA $40a5			;$$ -> A
$31ae | CPI $30				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$31b0 | JZ $31ef			;Zf = 1 : JMP $$
$31b3 | LDA $40a4			;$$ -> A
$31b6 | CPI $52				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$31b8 | JZ $326a			;Zf = 1 : JMP $$
$31bb | LDA $40ad			;$$ -> A
$31be | ANA A				;A &= A
$31bf | MVI B, $ff			;$ -> B
$31c1 | JZ $31d4			;Zf = 1 : JMP $$
$31c4 | MVI A, $ff			;$ -> A
$31c6 | STA $40ac			;A -> $$
$31c9 | LDA $40ac			;$$ -> A
$31cc | ANA A				;A &= A
$31cd | MVI B, $ff			;$ -> B
$31cf | JZ $31d4			;Zf = 1 : JMP $$
$31d2 | MVI B, $0			;$ -> B
$31d4 | MOV A, B			;B -> A
$31d5 | STA $40b7			;A -> $$
$31d8 | LDA $4090			;$$ -> A
$31db | ANI $bf				;A &= $
$31dd | STA $4090			;A -> $$
$31e0 | XRA A				;A = A XOR A
$31e1 | STA $4098			;A -> $$
$31e4 | LDA $409b			;$$ -> A
$31e7 | ANI $7f				;A &= $
$31e9 | STA $409b			;A -> $$
$31ec | JMP $30b3			;JMP $$

$31ef | LXI H, $40ac		;$$ -> HL
$31f2 | LXI D, $40a6		;$$ -> DE
$31f5 | LDA $40ab			;$$ -> A
$31f8 | CPI $7				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$31fa | JNZ $3253			;Zf = 0 : JMP $$
$31fd | LDAX D				;$DE -> A
$31fe | CPI $4d				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3200 | JNZ $321b			;Zf = 0 : JMP $$
$3203 | INR E				;E++
$3204 | LDAX D				;$DE -> A
$3205 | DCR E				;E--
$3206 | CPI $54				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3208 | JNZ $321b			;Zf = 0 : JMP $$
$320b | LDA $40aa			;$$ -> A
$320e | MOV B, A			;A -> B
$320f | LDA $40b6			;$$ -> A
$3212 | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cf & Zf = 0
$3213 | MVI B, $20			;$ -> B
$3215 | JZ $321a			;Zf = 1 : JMP $$
$3218 | MVI B, $30			;$ -> B
$321a | MOV M, B			;B -> $HL
$321b | LDAX D				;$DE -> A
$321c | CPI $52				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$321e | JNZ $3240			;Zf = 0 : JMP $$
$3221 | INR E				;E++
$3222 | LDAX D				;$DE -> A
$3223 | DCR E				;E--
$3224 | CPI $4b				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3226 | JNZ $322b			;Zf = 0 : JMP $$
$3229 | MVI M, $41			;$ -> $HL
$322b | CPI $4c				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$322d | JNZ $3232			;Zf = 0 : JMP $$
$3230 | MVI M, $42			;$ -> $HL
$3232 | CPI $4d				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3234 | JNZ $3239			;Zf = 0 : JMP $$
$3237 | MVI M, $44			;$ -> $HL
$3239 | CPI $4e				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$323b | JNZ $3240			;Zf = 0 : JMP $$
$323e | MVI M, $48			;$ -> $HL
$3240 | LDAX D				;$DE -> A
$3241 | CPI $44				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3243 | JNZ $3250			;Zf = 0 : JMP $$
$3246 | INR E				;E++
$3247 | LDAX D				;$DE -> A
$3248 | DCR E				;E--
$3249 | CPI $57				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$324b | JNZ $3250			;Zf = 0 : JMP $$
$324e | MVI M, $70			;$ -> $HL
$3250 | JMP $31c9			;JMP $$

$3253 | CPI $6				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3255 | JNZ $31d2			;Zf = 0 : JMP $$
$3258 | LDAX D				;$DE -> A
$3259 | CPI $44				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$325b | JNZ $31d2			;Zf = 0 : JMP $$
$325e | INR E				;E++
$325f | LDAX D				;$DE -> A
$3260 | CPI $44				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3262 | JNZ $31d2			;Zf = 0 : JMP $$
$3265 | MVI M, $60			;$ -> $HL
$3267 | JMP $31d2			;JMP $$

$326a | LDA $40a5			;$$ -> A
$326d | LXI H, $40ac		;$$ -> HL
$3270 | CPI $4b				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3272 | JNZ $3277			;Zf = 0 : JMP $$
$3275 | MVI M, $51			;$ -> $HL
$3277 | CPI $4c				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3279 | JNZ $327e			;Zf = 0 : JMP $$
$327c | MVI M, $52			;$ -> $HL
$327e | CPI $4d				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3280 | JNZ $3285			;Zf = 0 : JMP $$
$3283 | MVI M, $54			;$ -> $HL
$3285 | CPI $4e				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3287 | JNZ $328c			;Zf = 0 : JMP $$
$328a | MVI M, $58			;$ -> $HL
$328c | JMP $31c9			;JMP $$

$328f | LDA $40a4			;$$ -> A
$3292 | MOV B, A			;A -> B
$3293 | LDA $40b6			;$$ -> A
$3296 | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cf & Zf = 0
$3297 | MVI A, $0			;$ -> A
$3299 | STA $40ac			;A -> $$
$329c | JZ $32ae			;Zf = 1 : JMP $$
$329f | LXI H, $32c3		;$$ -> HL
$32a2 | MOV A, B			;B -> A
$32a3 | MVI B, $17			;$ -> B
$32a5 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$32a6 | JZ $31d2			;Zf = 1 : JMP $$
$32a9 | INX H				;HL++
$32aa | DCR B				;B--
$32ab | JNZ $32a5			;Zf = 0 : JMP $$
$32ae | LDA $40ad			;$$ -> A
$32b1 | CPI $30				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$32b3 | JNZ $32bc			;Zf = 0 : JMP $$
$32b6 | LXI H, $0000		;$$ -> HL
$32b9 | SHLD $40ad			;HL -> $$
$32bc | XRA A				;A = A XOR A
$32bd | STA $40ab			;A -> $$
$32c0 | JMP $31d2			;JMP $$

$32c3 | MOV D, D			;D -> D
$32c4 | MOV C, E			;E -> C
$32c5 | MOV C, H			;H -> C
$32c6 | MOV C, L			;L -> C
$32c7 | MOV C, M			;$HL -> C
$32c8 | MOV D, H			;H -> D
$32c9 | SIM					;set interrupt mask <- A
$32ca | LXI SP, $3332		;$$ -> SP
$32cd | INR M				;$HL++
$32ce | DCR M				;$HL--
$32cf | MVI M, $37			;$ -> $HL
$32d1 | LDSI $39				;undocumented (DE = SP + d8)
$32d3 | MOV B, C			;C -> B
$32d4 | MOV B, D			;D -> B
$32d5 | MOV B, E			;E -> B
$32d6 | MOV B, H			;H -> B
$32d7 | MOV B, L			;L -> B
$32d8 | MOV B, M			;$HL -> B
$32d9 | DCX D				;DE--
$32da | RST 7				;restart
$32db | RST 7				;restart
$32dc | RST 7				;restart
$32dd | RST 7				;restart
$32de | CM $ffef			;S = 1 : CALL $$ (call if negative)
$32e1 | RST 7				;restart
$32e2 | DCX B				;BC--
$32e3 | LXI SP, $310b		;$$ -> SP
$32e6 | STAX B				;A -> $BC
$32e7 | DCR M				;$HL--
$32e8 | ORA C				;A |= C
$32e9 | DCR M				;$HL--
$32ea | MOV L, B			;B -> L
$32eb | INR M				;$HL++
$32ec | MOV H, M			;$HL -> H
$32ed | INX SP				;SP++
$32ee | MOV H, C			;C -> H
$32ef | LDSI $2e				;undocumented (DE = SP + d8)
$32f1 | DAD SP				;HL + SP -> HL
$32f2 | DCX B				;BC--
$32f3 | LXI SP, $310b		;$$ -> SP
$32f6 | DCX B				;BC--
$32f7 | LXI SP, $310b		;$$ -> SP
$32fa | DCX B				;BC--
$32fb | LXI SP, $310b		;$$ -> SP
$32fe | DCX B				;BC--
$32ff | LXI SP, $37dd		;$$ -> SP
$3302 | LXI H, $4091		;$$ -> HL
$3305 | MOV A, M			;$HL -> A
$3306 | ANI $7f				;A &= $
$3308 | MOV M, A			;A -> $HL
$3309 | INX H				;HL++
$330a | MOV A, M			;$HL -> A
$330b | ANI $c0				;A &= $
$330d | MOV M, A			;A -> $HL
$330e | INX H				;HL++
$330f | MVI M, $0			;$ -> $HL
$3311 | DCX H				;HL--
$3312 | LDA $40b7			;$$ -> A
$3315 | ANA A				;A &= A
$3316 | MVI B, $40			;$ -> B
$3318 | MOV C, B			;B -> C
$3319 | JNZ $333f			;Zf = 0 : JMP $$
$331c | LDA $405f			;$$ -> A
$331f | MOV D, A			;A -> D
$3320 | ANI $94				;A &= $
$3322 | JZ $334f			;Zf = 1 : JMP $$
$3325 | MOV A, D			;D -> A
$3326 | MVI B, $0			;$ -> B
$3328 | MVI C, $80			;$ -> C
$332a | ANI $68				;A &= $
$332c | JZ $333f			;Zf = 1 : JMP $$
$332f | MVI C, $0			;$ -> C
$3331 | MVI B, $40			;$ -> B
$3333 | LDA $40b8			;$$ -> A
$3336 | ANA A				;A &= A
$3337 | JNZ $3346			;Zf = 0 : JMP $$
$333a | MVI A, $ff			;$ -> A
$333c | STA $40b8			;A -> $$
$333f | DCX H				;HL--
$3340 | MOV A, M			;$HL -> A
$3341 | ANI $bf				;A &= $
$3343 | ORA B				;A |= B
$3344 | MOV M, A			;A -> $HL
$3345 | INX H				;HL++
$3346 | MOV A, M			;$HL -> A
$3347 | ANI $1f				;A &= $
$3349 | ORA C				;A |= C
$334a | MOV M, A			;A -> $HL
$334b | CALL $3bd0			;CALL $$
$334e | RET					;return

$334f | LDA $8023			;$$ -> A
$3352 | ANI $1				;A &= $
$3354 | MVI C, $c0			;$ -> C
$3356 | JNZ $3331			;Zf = 0 : JMP $$
$3359 | MVI C, $80			;$ -> C
$335b | LDA $405e			;$$ -> A
$335e | ANI $1				;A &= $
$3360 | JNZ $332f			;Zf = 0 : JMP $$
$3363 | JMP $3346			;JMP $$

$3366 | LDA $40ac			;$$ -> A
$3369 | LXI H, $0000		;$$ -> HL
$336c | SHLD $40ab			;HL -> $$
$336f | SHLD $40ad			;HL -> $$
$3372 | STA $40ad			;A -> $$
$3375 | LXI H, $3ff4		;$$ -> HL
$3378 | LXI D, $000c		;$$ -> DE
$337b | DAD D				;HL + DE -> HL
$337c | RRC					;A rotate right
$337d | JNC $337b			;Cf = 0 : JMP $$
$3380 | SHLD $40af			;HL -> $$
$3383 | JMP $310b			;JMP $$

$3386 | LXI H, $40a4		;$$ -> HL
$3389 | CALL $381b			;CALL $$
$338c | LHLD $40af			;$$ -> HL
$338f | MOV M, B			;B -> $HL
$3390 | INX H				;HL++
$3391 | SHLD $40af			;HL -> $$
$3394 | LXI H, $40ae		;$$ -> HL
$3397 | INR M				;$HL++
$3398 | MOV A, M			;$HL -> A
$3399 | CPI $b				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$339b | JNZ $340f			;Zf = 0 : JMP $$
$339e | LXI H, $c000		;$$ -> HL
$33a1 | SHLD $40b1			;HL -> $$
$33a4 | MVI A, $7f			;$ -> A
$33a6 | STA $40ae			;A -> $$
$33a9 | LHLD $40af			;$$ -> HL
$33ac | LXI D, $fff5		;$$ -> DE
$33af | DAD D				;HL + DE -> HL
$33b0 | MVI C, $b			;$ -> C
$33b2 | XRA A				;A = A XOR A
$33b3 | XRA M				;A = A XOR $HL
$33b4 | RLC					;A rotate left
$33b5 | INX H				;HL++
$33b6 | DCR C				;C--
$33b7 | JNZ $33b3			;Zf = 0 : JMP $$
$33ba | MOV M, A			;A -> $HL
$33bb | LXI D, $fff7		;$$ -> DE
$33be | DAD D				;HL + DE -> HL
$33bf | MOV A, M			;$HL -> A
$33c0 | ANI $60				;A &= $
$33c2 | STA $40b5			;A -> $$
$33c5 | JMP $340f			;JMP $$

$33c8 | LDA $40ad			;$$ -> A
$33cb | ORI $f1				;A |= $
$33cd | ANI $ef				;A &= $
$33cf | CALL $3800			;CALL $$
$33d2 | STA $8040			;A -> $$
$33d5 | STA $40bc			;A -> $$
$33d8 | LXI H, $40a4		;$$ -> HL
$33db | CALL $381b			;CALL $$
$33de | LHLD $40b1			;$$ -> HL
$33e1 | LDA $40b5			;$$ -> A
$33e4 | MOV C, A			;A -> C
$33e5 | ANI $20				;A &= $
$33e7 | JNZ $3436			;Zf = 0 : JMP $$
$33ea | MOV A, L			;L -> A
$33eb | ANA A				;A &= A
$33ec | JNZ $33fc			;Zf = 0 : JMP $$
$33ef | MOV A, C			;C -> A
$33f0 | ANI $f				;A &= $
$33f2 | JNZ $33fc			;Zf = 0 : JMP $$
$33f5 | MOV A, C			;C -> A
$33f6 | ANI $40				;A &= $
$33f8 | JNZ $33fc			;Zf = 0 : JMP $$
$33fb | INR L				;L++
$33fc | CALL $3449			;CALL $$
$33ff | INR L				;L++
$3400 | INR L				;L++
$3401 | MOV A, L			;L -> A
$3402 | CPI $1				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3404 | JZ $3418			;Zf = 1 : JMP $$
$3407 | CPI $0				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3409 | JZ $342a			;Zf = 1 : JMP $$
$340c | SHLD $40b1			;HL -> $$
$340f | LXI H, $0000		;$$ -> HL
$3412 | SHLD $40ab			;HL -> $$
$3415 | JMP $3383			;JMP $$

$3418 | MOV A, C			;C -> A
$3419 | ANI $f				;A &= $
$341b | JNZ $343d			;Zf = 0 : JMP $$
$341e | LXI H, $c000		;$$ -> HL
$3421 | MOV A, C			;C -> A
$3422 | ORI $f				;A |= $
$3424 | STA $40b5			;A -> $$
$3427 | JMP $340c			;JMP $$

$342a | MOV A, C			;C -> A
$342b | ANI $f				;A &= $
$342d | JNZ $343d			;Zf = 0 : JMP $$
$3430 | LXI H, $c001		;$$ -> HL
$3433 | JMP $3421			;JMP $$

$3436 | CALL $3449			;CALL $$
$3439 | INR L				;L++
$343a | JNZ $340c			;Zf = 0 : JMP $$
$343d | LXI H, $0000		;$$ -> HL
$3440 | SHLD $40ad			;HL -> $$
$3443 | LXI H, $c000		;$$ -> HL
$3446 | JMP $340c			;JMP $$

$3449 | LDA $40bc			;$$ -> A
$344c | ANI $70				;A &= $
$344e | JZ $3452			;Zf = 1 : JMP $$
$3451 | MOV A, M			;$HL -> A
$3452 | MOV M, B			;B -> $HL
$3453 | MOV B, A			;A -> B
$3454 | LDA $40bc			;$$ -> A
$3457 | ANI $70				;A &= $
$3459 | JZ $3462			;Zf = 1 : JMP $$
$345c | MVI A, $8f			;$ -> A
$345e | STA $8040			;A -> $$
$3461 | MOV M, B			;B -> $HL
$3462 | MVI A, $f			;$ -> A
$3464 | STA $8040			;A -> $$
$3467 | RET					;return

$3468 | MVI A, $3			;$ -> A
$346a | STA $4096			;A -> $$
$346d | LDA $40ac			;$$ -> A
$3470 | MVI B, $4a			;$ -> B
$3472 | INR B				;B++
$3473 | RRC					;A rotate right
$3474 | JNC $3472			;Cf = 0 : JMP $$
$3477 | MVI L, $52			;$ -> L
$3479 | MOV H, B			;B -> H
$347a | SHLD $40a1			;HL -> $$
$347d | CALL $38d9			;CALL $$
$3480 | CALL $3794			;CALL $$
$3483 | MVI C, $b			;$ -> C
$3485 | XCHG				;HL <-> DE
$3486 | LXI H, $40a1		;$$ -> HL
$3489 | CALL $3839			;CALL $$
$348c | DCR C				;C--
$348d | JNZ $3486			;Zf = 0 : JMP $$
$3490 | LDA $40ac			;$$ -> A
$3493 | ANI $f				;A &= $
$3495 | XRI $f				;A = A XOR $
$3497 | CALL $3800			;CALL $$
$349a | RRC					;A rotate right
$349b | RRC					;A rotate right
$349c | RRC					;A rotate right
$349d | RRC					;A rotate right
$349e | STA $8040			;A -> $$
$34a1 | LHLD $40b1			;$$ -> HL
$34a4 | XCHG				;HL <-> DE
$34a5 | LDA $40b9			;$$ -> A
$34a8 | MOV C, A			;A -> C
$34a9 | LXI H, $40a1		;$$ -> HL
$34ac | CALL $3839			;CALL $$
$34af | DCR C				;C--
$34b0 | LDA $40b5			;$$ -> A
$34b3 | MOV B, A			;A -> B
$34b4 | JZ $34c0			;Zf = 1 : JMP $$
$34b7 | ANI $80				;A &= $
$34b9 | JZ $34a9			;Zf = 1 : JMP $$
$34bc | INR E				;E++
$34bd | JMP $34a9			;JMP $$

$34c0 | ANI $80				;A &= $
$34c2 | JZ $34dc			;Zf = 1 : JMP $$
$34c5 | MOV A, B			;B -> A
$34c6 | ANI $40				;A &= $
$34c8 | JZ $34dc			;Zf = 1 : JMP $$
$34cb | MOV A, B			;B -> A
$34cc | ANI $bf				;A &= $
$34ce | STA $40b5			;A -> $$
$34d1 | LHLD $40b3			;$$ -> HL
$34d4 | XCHG				;HL <-> DE
$34d5 | LDA $40ba			;$$ -> A
$34d8 | MOV C, A			;A -> C
$34d9 | JMP $34a9			;JMP $$

$34dc | LXI H, $c000		;$$ -> HL
$34df | SHLD $40b1			;HL -> $$
$34e2 | SHLD $40b3			;HL -> $$
$34e5 | LXI H, $40b9		;$$ -> HL
$34e8 | XRA A				;A = A XOR A
$34e9 | MOV M, A			;A -> $HL
$34ea | INX H				;HL++
$34eb | MOV M, A			;A -> $HL
$34ec | STA $40b5			;A -> $$
$34ef | LXI H, $0000		;$$ -> HL
$34f2 | SHLD $40ab			;HL -> $$
$34f5 | SHLD $40ad			;HL -> $$
$34f8 | MVI A, $f			;$ -> A
$34fa | STA $8040			;A -> $$
$34fd | ORI $ff				;A |= $
$34ff | JMP $310b			;JMP $$

$3502 | MVI A, $3			;$ -> A
$3504 | STA $4096			;A -> $$
$3507 | LXI H, $40a1		;$$ -> HL
$350a | MVI M, $4d			;$ -> $HL
$350c | INX H				;HL++
$350d | MVI M, $54			;$ -> $HL
$350f | CALL $38d9			;CALL $$
$3512 | LXI D, $4052		;$$ -> DE
$3515 | LXI H, $40a1		;$$ -> HL
$3518 | LDAX D				;$DE -> A
$3519 | ANI $fc				;A &= $
$351b | CALL $383a			;CALL $$
$351e | DCX D				;DE--
$351f | LDAX D				;$DE -> A
$3520 | RRC					;A rotate right
$3521 | ANI $80				;A &= $
$3523 | MOV B, A			;A -> B
$3524 | LDA $4050			;$$ -> A
$3527 | ANI $3f				;A &= $
$3529 | ORA B				;A |= B
$352a | CALL $383a			;CALL $$
$352d | LDAX D				;$DE -> A
$352e | ANI $fc				;A &= $
$3530 | CALL $383a			;CALL $$
$3533 | DCX D				;DE--
$3534 | LDAX D				;$DE -> A
$3535 | MOV C, A			;A -> C
$3536 | RLC					;A rotate left
$3537 | RLC					;A rotate left
$3538 | ANI $8				;A &= $
$353a | MOV B, A			;A -> B
$353b | MOV A, C			;C -> A
$353c | RRC					;A rotate right
$353d | ANI $80				;A &= $
$353f | ORA B				;A |= B
$3540 | MOV B, A			;A -> B
$3541 | LDA $4051			;$$ -> A
$3544 | ANI $37				;A &= $
$3546 | ORA B				;A |= B
$3547 | CALL $383a			;CALL $$
$354a | LDAX D				;$DE -> A
$354b | ANI $fc				;A &= $
$354d | CALL $383a			;CALL $$
$3550 | LDA $4005			;$$ -> A
$3553 | ANI $e0				;A &= $
$3555 | JNZ $355a			;Zf = 0 : JMP $$
$3558 | ORI $10				;A |= $
$355a | MOV B, A			;A -> B
$355b | LDA $405f			;$$ -> A
$355e | ANI $68				;A &= $
$3560 | JZ $3567			;Zf = 1 : JMP $$
$3563 | MOV A, B			;B -> A
$3564 | ORI $8				;A |= $
$3566 | MOV B, A			;A -> B
$3567 | LDA $405e			;$$ -> A
$356a | ANI $1				;A &= $
$356c | MOV A, B			;B -> A
$356d | JZ $3572			;Zf = 1 : JMP $$
$3570 | ORI $8				;A |= $
$3572 | CALL $383a			;CALL $$
$3575 | DCX D				;DE--
$3576 | CALL $3839			;CALL $$
$3579 | LDAX D				;$DE -> A
$357a | RLC					;A rotate left
$357b | ANI $40				;A &= $
$357d | MOV B, A			;A -> B
$357e | LDA $4057			;$$ -> A
$3581 | ANI $3f				;A &= $
$3583 | ORA B				;A |= B
$3584 | ANI $fc				;A &= $
$3586 | CALL $383a			;CALL $$
$3589 | LDA $405f			;$$ -> A
$358c | RRC					;A rotate right
$358d | ANI $1				;A &= $
$358f | MOV B, A			;A -> B
$3590 | DCX D				;DE--
$3591 | LDAX D				;$DE -> A
$3592 | ANI $e				;A &= $
$3594 | ORA B				;A |= B
$3595 | MOV B, A			;A -> B
$3596 | LDA $4069			;$$ -> A
$3599 | ORA B				;A |= B
$359a | CALL $383a			;CALL $$
$359d | LXI D, $4058		;$$ -> DE
$35a0 | CALL $3839			;CALL $$
$35a3 | CALL $3839			;CALL $$
$35a6 | MVI A, $3			;$ -> A
$35a8 | STA $4096			;A -> $$
$35ab | CALL $3839			;CALL $$
$35ae | JMP $34dc			;JMP $$

$35b1 | LDA $40ac			;$$ -> A
$35b4 | STA $40ad			;A -> $$
$35b7 | XRA A				;A = A XOR A
$35b8 | STA $40ac			;A -> $$
$35bb | MVI A, $1			;$ -> A
$35bd | STA $40ae			;A -> $$
$35c0 | LXI H, $40a8		;$$ -> HL
$35c3 | CALL $381b			;CALL $$
$35c6 | MOV A, B			;B -> A
$35c7 | ANI $fc				;A &= $
$35c9 | MOV B, A			;A -> B
$35ca | LDA $4052			;$$ -> A
$35cd | ANI $3				;A &= $
$35cf | ORA B				;A |= B
$35d0 | STA $4052			;A -> $$
$35d3 | MOV A, M			;$HL -> A
$35d4 | STA $40a4			;A -> $$
$35d7 | MVI A, $1			;$ -> A
$35d9 | STA $40ab			;A -> $$
$35dc | CALL $3871			;CALL $$
$35df | JMP $310b			;JMP $$

$35e2 | LXI H, $0000		;$$ -> HL
$35e5 | SHLD $40ab			;HL -> $$
$35e8 | LXI H, $40ae		;$$ -> HL
$35eb | INR M				;$HL++
$35ec | JMP $35dc			;JMP $$

$35ef | LDA $40ae			;$$ -> A
$35f2 | RLC					;A rotate left
$35f3 | MOV E, A			;A -> E
$35f4 | MVI D, $0			;$ -> D
$35f6 | LXI H, $3602		;$$ -> HL
$35f9 | DAD D				;HL + DE -> HL
$35fa | MOV E, M			;$HL -> E
$35fb | INX H				;HL++
$35fc | MOV D, M			;$HL -> D
$35fd | XCHG				;HL <-> DE
$35fe | LXI D, $40a4		;$$ -> DE
$3601 | PCHL				;JMP $HL
$3602 | LDAX D				;$DE -> A
$3603 | MVI M, $22			;$ -> $HL
$3605 | MVI M, $36			;$ -> $HL
$3607 | MVI M, $49			;$ -> $HL
$3609 | MVI M, $6d			;$ -> $HL
$360b | MVI M, $7a			;$ -> $HL
$360d | MVI M, $b2			;$ -> $HL
$360f | MVI M, $cc			;$ -> $HL
$3611 | MVI M, $f0			;$ -> $HL
$3613 | MVI M, $1d			;$ -> $HL
$3615 | STC					;set Cy flag
$3616 | LDHI $37				;undocumented (DE = HL + d8)
$3618 | INX SP				;SP++
$3619 | STC					;set Cy flag
$361a | MVI A, $ff			;$ -> A
$361c | STA $40ae			;A -> $$
$361f | JMP $35dc			;JMP $$

$3622 | XCHG				;HL <-> DE
$3623 | CALL $381b			;CALL $$
$3626 | MOV A, B			;B -> A
$3627 | ANI $f				;A &= $
$3629 | MOV B, A			;A -> B
$362a | LDA $4050			;$$ -> A
$362d | ANI $f0				;A &= $
$362f | ORA B				;A |= B
$3630 | STA $4050			;A -> $$
$3633 | JMP $35e2			;JMP $$

$3636 | XCHG				;HL <-> DE
$3637 | LXI D, $4053		;$$ -> DE
$363a | CALL $381b			;CALL $$
$363d | MOV A, B			;B -> A
$363e | ANI $fc				;A &= $
$3640 | MOV B, A			;A -> B
$3641 | LDAX D				;$DE -> A
$3642 | ANI $3				;A &= $
$3644 | ORA B				;A |= B
$3645 | STAX D				;A -> $DE
$3646 | JMP $35e2			;JMP $$

$3649 | XCHG				;HL <-> DE
$364a | CALL $381b			;CALL $$
$364d | MOV A, B			;B -> A
$364e | ANI $7				;A &= $
$3650 | MOV C, B			;B -> C
$3651 | MOV B, A			;A -> B
$3652 | LDA $4051			;$$ -> A
$3655 | ANI $f8				;A &= $
$3657 | ORA B				;A |= B
$3658 | STA $4051			;A -> $$
$365b | MOV A, C			;C -> A
$365c | RRC					;A rotate right
$365d | RRC					;A rotate right
$365e | ANI $2				;A &= $
$3660 | MOV B, A			;A -> B
$3661 | LDA $4053			;$$ -> A
$3664 | ANI $fd				;A &= $
$3666 | ORA B				;A |= B
$3667 | STA $4053			;A -> $$
$366a | JMP $35e2			;JMP $$

$366d | XCHG				;HL <-> DE
$366e | CALL $381b			;CALL $$
$3671 | MOV A, B			;B -> A
$3672 | ORI $3				;A |= $
$3674 | STA $4054			;A -> $$
$3677 | JMP $363a			;JMP $$

$367a | LXI H, $4051		;$$ -> HL
$367d | MOV A, M			;$HL -> A
$367e | ORI $c8				;A |= $
$3680 | MOV M, A			;A -> $HL
$3681 | LDAX D				;$DE -> A
$3682 | CPI $38				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3684 | JNZ $368b			;Zf = 0 : JMP $$
$3687 | MOV A, M			;$HL -> A
$3688 | ANI $f7				;A &= $
$368a | MOV M, A			;A -> $HL
$368b | LDAX D				;$DE -> A
$368c | CPI $34				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$368e | JNZ $3695			;Zf = 0 : JMP $$
$3691 | MOV A, M			;$HL -> A
$3692 | ANI $bf				;A &= $
$3694 | MOV M, A			;A -> $HL
$3695 | LDAX D				;$DE -> A
$3696 | CPI $32				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3698 | JNZ $369f			;Zf = 0 : JMP $$
$369b | MOV A, M			;$HL -> A
$369c | ANI $bb				;A &= $
$369e | MOV M, A			;A -> $HL
$369f | INX D				;DE++
$36a0 | DCX H				;HL--
$36a1 | MOV A, M			;$HL -> A
$36a2 | ORI $80				;A |= $
$36a4 | MOV M, A			;A -> $HL
$36a5 | LDAX D				;$DE -> A
$36a6 | CPI $34				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$36a8 | JZ $36af			;Zf = 1 : JMP $$
$36ab | MOV A, M			;$HL -> A
$36ac | ANI $7f				;A &= $
$36ae | MOV M, A			;A -> $HL
$36af | JMP $35e2			;JMP $$

$36b2 | XCHG				;HL <-> DE
$36b3 | CALL $381b			;CALL $$
$36b6 | MOV A, B			;B -> A
$36b7 | ANI $fe				;A &= $
$36b9 | MOV B, A			;A -> B
$36ba | LDA $4055			;$$ -> A
$36bd | ANI $1				;A &= $
$36bf | ORA B				;A |= B
$36c0 | STA $4055			;A -> $$
$36c3 | CMA					;1's compliment A (invert)
$36c4 | ANI $f0				;A &= $
$36c6 | STA $406d			;A -> $$
$36c9 | JMP $35e2			;JMP $$

$36cc | XCHG				;HL <-> DE
$36cd | CALL $381b			;CALL $$
$36d0 | MOV A, B			;B -> A
$36d1 | RRC					;A rotate right
$36d2 | ANI $20				;A &= $
$36d4 | MOV C, A			;A -> C
$36d5 | LDA $4056			;$$ -> A
$36d8 | ANI $df				;A &= $
$36da | ORI $90				;A |= $
$36dc | ORA C				;A |= C
$36dd | STA $4056			;A -> $$
$36e0 | MOV A, B			;B -> A
$36e1 | ANI $3c				;A &= $
$36e3 | MOV C, A			;A -> C
$36e4 | LDA $4057			;$$ -> A
$36e7 | ANI $c0				;A &= $
$36e9 | ORA C				;A |= C
$36ea | STA $4057			;A -> $$
$36ed | JMP $35e2			;JMP $$

$36f0 | XCHG				;HL <-> DE
$36f1 | CALL $381b			;CALL $$
$36f4 | MOV A, B			;B -> A
$36f5 | ANI $f0				;A &= $
$36f7 | MOV C, A			;A -> C
$36f8 | STA $4069			;A -> $$
$36fb | MOV A, B			;B -> A
$36fc | ANI $1				;A &= $
$36fe | MOV A, C			;C -> A
$36ff | JZ $370d			;Zf = 1 : JMP $$
$3702 | STA $406a			;A -> $$
$3705 | LDA $4057			;$$ -> A
$3708 | ANI $7f				;A &= $
$370a | STA $4057			;A -> $$
$370d | MOV A, B			;B -> A
$370e | ANI $e				;A &= $
$3710 | MOV C, A			;A -> C
$3711 | LDA $4056			;$$ -> A
$3714 | ANI $f1				;A &= $
$3716 | ORA C				;A |= C
$3717 | STA $4056			;A -> $$
$371a | JMP $35e2			;JMP $$

$371d | XCHG				;HL <-> DE
$371e | CALL $381b			;CALL $$
$3721 | MOV A, B			;B -> A
$3722 | STA $4058			;A -> $$
$3725 | JMP $373b			;JMP $$

$3728 | XCHG				;HL <-> DE
$3729 | CALL $381b			;CALL $$
$372c | MOV A, B			;B -> A
$372d | STA $4059			;A -> $$
$3730 | JMP $373b			;JMP $$

$3733 | XCHG				;HL <-> DE
$3734 | CALL $381b			;CALL $$
$3737 | MOV A, B			;B -> A
$3738 | STA $405a			;A -> $$
$373b | LDA $4054			;$$ -> A
$373e | ANI $fc				;A &= $
$3740 | STA $4054			;A -> $$
$3743 | LXI H, $3764		;$$ -> HL
$3746 | LXI D, $0002		;$$ -> DE
$3749 | MVI B, $18			;$ -> B
$374b | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$374c | JZ $3757			;Zf = 1 : JMP $$
$374f | DAD D				;HL + DE -> HL
$3750 | DCR B				;B--
$3751 | JNZ $374b			;Zf = 0 : JMP $$
$3754 | LXI H, $3782		;$$ -> HL
$3757 | INX H				;HL++
$3758 | MOV A, M			;$HL -> A
$3759 | STA $4060			;A -> $$
$375c | MVI A, $3			;$ -> A
$375e | STA $40bf			;A -> $$
$3761 | JMP $35e2			;JMP $$

$3764 | CNC $c4c6			;Cf = 0 : CALL $$
$3767 | POP B				;STACK$$ -> BC
$3768 | ADD H				;H += A
$3769 | CNZ $c58c			;Cf = 0 : CALL $$
$376c | CZ $44c0			;Zf = 1 : CALL $$
$376f | RST 0				;restart
$3770 | MOV H, H			;H -> H
$3771 | DCR C				;C--
$3772 | MOV H, B			;B -> H
$3773 | MVI C, $50			;$ -> C
$3775 | LDAX B				;$BC -> A
$3776 | RNC
$3777 | DCX B				;BC--
$3778 | RPO					;P = 1 : RET
$3779 | LXI D, $1224		;$$ -> DE
$377c | INR A				;A++
$377d | INX D				;DE++
$377e | LDSI $19				;undocumented (DE = SP + d8)
$3780 | SIM					;set interrupt mask <- A
$3781 | LDAX D				;$DE -> A
$3782 | MOV M, B			;B -> $HL
$3783 | ANA B				;A &= B
$3784 | MOV A, B			;B -> A
$3785 | ANA C				;A &= C
$3786 | RDEL				;undocumented (arithmetic left shift DE)
$3787 | ANA D				;A &= D
$3788 | SBB B				;A -= B - 1
$3789 | ANA H				;A &= H
$378a | ADC B				;B += A + Cy
$378b | ANA L				;A &= L
$378c | INR C				;C++
$378d | ANA M				;A &= $HL
$378e | INR L				;L++
$378f | XRA B				;A = A XOR B
$3790 | XRA B				;A = A XOR B
$3791 | XRA C				;A = A XOR C
$3792 | SUB B				;A -= B
$3793 | XRA D				;A = A XOR D
$3794 | LXI H, $3ff4		;$$ -> HL
$3797 | LXI D, $000c		;$$ -> DE
$379a | LDA $40ac			;$$ -> A
$379d | DAD D				;HL + DE -> HL
$379e | RRC					;A rotate right
$379f | JNC $379d			;Cf = 0 : JMP $$
$37a2 | PUSH H				;HL -> SP
$37a3 | LXI B, $0002		;$$ -> BC
$37a6 | DAD B				;HL + BC -> HL
$37a7 | MOV A, M			;$HL -> A
$37a8 | MOV B, A			;A -> B
$37a9 | LXI H, $c000		;$$ -> HL
$37ac | SHLD $40b1			;HL -> $$
$37af | SHLD $40b3			;HL -> $$
$37b2 | LXI H, $40b5		;$$ -> HL
$37b5 | ANI $20				;A &= $
$37b7 | JNZ $37db			;Zf = 0 : JMP $$
$37ba | MOV A, M			;$HL -> A
$37bb | ORI $c0				;A |= $
$37bd | MOV M, A			;A -> $HL
$37be | MVI A, $80			;$ -> A
$37c0 | STA $40b9			;A -> $$
$37c3 | STA $40ba			;A -> $$
$37c6 | MOV A, B			;B -> A
$37c7 | ANI $40				;A &= $
$37c9 | MVI B, $0			;$ -> B
$37cb | MVI A, $1			;$ -> A
$37cd | JNZ $37d4			;Zf = 0 : JMP $$
$37d0 | MVI B, $1			;$ -> B
$37d2 | MVI A, $0			;$ -> A
$37d4 | STA $40b3			;A -> $$
$37d7 | MOV A, B			;B -> A
$37d8 | STA $40b1			;A -> $$
$37db | POP H				;STACK$$ -> HL
$37dc | RET					;return

$37dd | XRA A				;A = A XOR A
$37de | STA $40ab			;A -> $$
$37e1 | LDA $40ad			;$$ -> A
$37e4 | CPI $30				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$37e6 | JZ $35ef			;Zf = 1 : JMP $$
$37e9 | LDA $40ae			;$$ -> A
$37ec | CPI $7f				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$37ee | JZ $33c8			;Zf = 1 : JMP $$
$37f1 | JMP $3386			;JMP $$

$37f4 | RST 7				;restart
$37f5 | RST 7				;restart
$37f6 | RST 7				;restart
$37f7 | RST 7				;restart
$37f8 | RST 7				;restart
$37f9 | RST 7				;restart
$37fa | RST 7				;restart
$37fb | RST 7				;restart
$37fc | RST 7				;restart
$37fd | MOV D, L			;L -> D
$37fe | MOV B, H			;H -> B
$37ff | ORA E				;A |= E
$3800 | MOV B, A			;A -> B
$3801 | RLC					;A rotate left
$3802 | MOV D, A			;A -> D
$3803 | ANI $11				;A &= $
$3805 | MOV C, A			;A -> C
$3806 | MOV A, D			;D -> A
$3807 | RLC					;A rotate left
$3808 | RLC					;A rotate left
$3809 | ANI $22				;A &= $
$380b | ORA C				;A |= C
$380c | MOV C, A			;A -> C
$380d | MOV A, B			;B -> A
$380e | RRC					;A rotate right
$380f | MOV D, A			;A -> D
$3810 | ANI $88				;A &= $
$3812 | ORA C				;A |= C
$3813 | MOV C, A			;A -> C
$3814 | MOV A, D			;D -> A
$3815 | RRC					;A rotate right
$3816 | RRC					;A rotate right
$3817 | ANI $44				;A &= $
$3819 | ORA C				;A |= C
$381a | RET					;return

$381b | MOV A, M			;$HL -> A
$381c | CPI $3a				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$381e | JC $3823			;Cf = 1 : JMP $$
$3821 | ADI $9				;A += $
$3823 | RRC					;A rotate right
$3824 | RRC					;A rotate right
$3825 | RRC					;A rotate right
$3826 | RRC					;A rotate right
$3827 | ANI $f0				;A &= $
$3829 | MOV B, A			;A -> B
$382a | INX H				;HL++
$382b | MOV A, M			;$HL -> A
$382c | CPI $3a				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$382e | JC $3833			;Cf = 1 : JMP $$
$3831 | ADI $9				;A += $
$3833 | ANI $f				;A &= $
$3835 | ORA B				;A |= B
$3836 | MOV B, A			;A -> B
$3837 | INX H				;HL++
$3838 | RET					;return

$3839 | LDAX D				;$DE -> A
$383a | PUSH H				;HL -> SP
$383b | PUSH PSW			;AF (ACC and FLAG = PSW processor status word) -> STACK$$
$383c | RRC					;A rotate right
$383d | RRC					;A rotate right
$383e | RRC					;A rotate right
$383f | RRC					;A rotate right
$3840 | CALL $3851			;CALL $$
$3843 | POP PSW				;STACK$$ -> AF (ACC and FLAG = PSW processor status word)
$3844 | JMP $3848			;JMP $$

$3847 | PUSH H				;HL -> SP
$3848 | CALL $3851			;CALL $$
$384b | CALL $38d9			;CALL $$
$384e | INX D				;DE++
$384f | POP H				;STACK$$ -> HL
$3850 | RET					;return

$3851 | ANI $f				;A &= $
$3853 | CPI $a				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3855 | JC $385c			;Cf = 1 : JMP $$
$3858 | SBI $9				;A -= $ - Cf
$385a | ORI $10				;A |= $
$385c | ADI $30				;A += $
$385e | MOV M, A			;A -> $HL
$385f | INX H				;HL++
$3860 | RET					;return

$3861 | LXI H, $0000		;$$ -> HL
$3864 | SHLD $40ab			;HL -> $$
$3867 | SHLD $40ad			;HL -> $$
$386a | LDA $40a9			;$$ -> A
$386d | STA $40b6			;A -> $$
$3870 | RET					;return

$3871 | LDA $405e			;$$ -> A
$3874 | ANI $df				;A &= $
$3876 | ORI $80				;A |= $
$3878 | STA $405e			;A -> $$
$387b | RET					;return

$387c | LDA $405e			;$$ -> A
$387f | ORI $20				;A |= $
$3881 | STA $405e			;A -> $$
$3884 | LXI H, $4050		;$$ -> HL
$3887 | LXI D, $801e		;$$ -> DE
$388a | LDAX D				;$DE -> A
$388b | ANI $70				;A &= $
$388d | MOV B, A			;A -> B
$388e | MOV A, M			;$HL -> A
$388f | ANI $70				;A &= $
$3891 | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cf & Zf = 0
$3892 | JZ $389d			;Zf = 1 : JMP $$
$3895 | MOV A, M			;$HL -> A
$3896 | ANI $8f				;A &= $
$3898 | ORA B				;A |= B
$3899 | MOV M, A			;A -> $HL
$389a | CALL $38ce			;CALL $$
$389d | INX H				;HL++
$389e | INX D				;DE++
$389f | LDAX D				;$DE -> A
$38a0 | ANI $30				;A &= $
$38a2 | MOV B, A			;A -> B
$38a3 | MOV A, M			;$HL -> A
$38a4 | ANI $30				;A &= $
$38a6 | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cf & Zf = 0
$38a7 | JZ $38b2			;Zf = 1 : JMP $$
$38aa | MOV A, M			;$HL -> A
$38ab | ANI $cf				;A &= $
$38ad | ORA B				;A |= B
$38ae | MOV M, A			;A -> $HL
$38af | CALL $38ce			;CALL $$
$38b2 | MVI C, $2			;$ -> C
$38b4 | INX H				;HL++
$38b5 | INX D				;DE++
$38b6 | LDAX D				;$DE -> A
$38b7 | ANI $1				;A &= $
$38b9 | MOV B, A			;A -> B
$38ba | MOV A, M			;$HL -> A
$38bb | ANI $1				;A &= $
$38bd | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cf & Zf = 0
$38be | JZ $38c9			;Zf = 1 : JMP $$
$38c1 | MOV A, M			;$HL -> A
$38c2 | ANI $fe				;A &= $
$38c4 | ORA B				;A |= B
$38c5 | MOV M, A			;A -> $HL
$38c6 | CALL $38ce			;CALL $$
$38c9 | DCR C				;C--
$38ca | JNZ $38b3			;Zf = 0 : JMP $$
$38cd | RET					;return

$38ce | LDA $405e			;$$ -> A
$38d1 | ANI $df				;A &= $
$38d3 | ORI $80				;A |= $
$38d5 | STA $405e			;A -> $$
$38d8 | RET					;return

$38d9 | PUSH H				;HL -> SP
$38da | PUSH D				;DE -> STACK$$
$38db | PUSH B				;BC -> STACK$$
$38dc | LHLD $40bd			;$$ -> HL
$38df | CALL $3fda			;CALL $$
$38e2 | DCX H				;HL--
$38e3 | MOV A, H			;H -> A
$38e4 | ORA L				;A |= L
$38e5 | JNZ $38df			;Zf = 0 : JMP $$
$38e8 | LDA $40b6			;$$ -> A
$38eb | STA $40a3			;A -> $$
$38ee | LDA $409b			;$$ -> A
$38f1 | ANI $80				;A &= $
$38f3 | STA $409b			;A -> $$
$38f6 | LDA $4090			;$$ -> A
$38f9 | ORI $10				;A |= $
$38fb | STA $4090			;A -> $$
$38fe | CALL $3bd9			;CALL $$
$3901 | LDA $4090			;$$ -> A
$3904 | ANI $18				;A &= $
$3906 | JZ $392a			;Zf = 1 : JMP $$
$3909 | CALL $3fda			;CALL $$
$390c | LDA $4090			;$$ -> A
$390f | ANI $2				;A &= $
$3911 | JZ $3909			;Zf = 1 : JMP $$
$3914 | CALL $3bd3			;CALL $$
$3917 | XRA A				;A = A XOR A
$3918 | STA $4098			;A -> $$
$391b | LDA $4090			;$$ -> A
$391e | MOV B, A			;A -> B
$391f | ANI $3f				;A &= $
$3921 | STA $4090			;A -> $$
$3924 | MOV A, B			;B -> A
$3925 | ANI $18				;A &= $
$3927 | JNZ $3909			;Zf = 0 : JMP $$
$392a | POP B				;STACK$$ -> BC
$392b | POP D				;STACK$$ -> DE
$392c | POP H				;STACK$$ -> HL
$392d | RET					;return

$392e | LXI H, $0000		;$$ -> HL
$3931 | SHLD $40ab			;HL -> $$
$3934 | SHLD $40ad			;HL -> $$
$3937 | LXI H, $0001		;$$ -> HL
$393a | LXI D, $40a8		;$$ -> DE
$393d | LDAX D				;$DE -> A
$393e | CPI $30				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3940 | JNZ $394a			;Zf = 0 : JMP $$
$3943 | INX D				;DE++
$3944 | LDAX D				;$DE -> A
$3945 | CPI $30				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3947 | JZ $394d			;Zf = 1 : JMP $$
$394a | LXI H, $0100		;$$ -> HL
$394d | SHLD $40bd			;HL -> $$
$3950 | RET					;return

$3951 | RST 7				;restart
$3952 | RST 7				;restart
$3953 | RST 7				;restart
$3954 | RST 7				;restart
$3955 | RST 7				;restart
$3956 | RST 7				;restart
$3957 | RST 7				;restart
$3958 | RST 7				;restart
$3959 | RST 7				;restart
$395a | RST 7				;restart
$395b | RST 7				;restart
$395c | RST 7				;restart
$395d | RST 7				;restart
$395e | RST 7				;restart
$395f | RST 7				;restart
$3960 | RST 7				;restart
$3961 | RST 7				;restart
$3962 | RST 7				;restart
$3963 | RST 7				;restart
$3964 | RST 7				;restart
$3965 | RST 7				;restart
$3966 | RST 7				;restart
$3967 | RST 7				;restart
$3968 | RST 7				;restart
$3969 | RST 7				;restart
$396a | RST 7				;restart
$396b | RST 7				;restart
$396c | RST 7				;restart
$396d | RST 7				;restart
$396e | RST 7				;restart
$396f | RST 7				;restart
$3970 | RST 7				;restart
$3971 | RST 7				;restart
$3972 | RST 7				;restart
$3973 | RST 7				;restart
$3974 | RST 7				;restart
$3975 | RST 7				;restart
$3976 | RST 7				;restart
$3977 | RST 7				;restart
$3978 | RST 7				;restart
$3979 | RST 7				;restart
$397a | RST 7				;restart
$397b | RST 7				;restart
$397c | RST 7				;restart
$397d | RST 7				;restart
$397e | RST 7				;restart
$397f | RST 7				;restart
$3980 | RST 7				;restart
$3981 | RST 7				;restart
$3982 | RST 7				;restart
$3983 | RST 7				;restart
$3984 | RST 7				;restart
$3985 | RST 7				;restart
$3986 | RST 7				;restart
$3987 | RST 7				;restart
$3988 | RST 7				;restart
$3989 | RST 7				;restart
$398a | RST 7				;restart
$398b | RST 7				;restart
$398c | RST 7				;restart
$398d | RST 7				;restart
$398e | RST 7				;restart
$398f | RST 7				;restart
$3990 | RST 7				;restart
$3991 | RST 7				;restart
$3992 | RST 7				;restart
$3993 | RST 7				;restart
$3994 | RST 7				;restart
$3995 | RST 7				;restart
$3996 | RST 7				;restart
$3997 | RST 7				;restart
$3998 | RST 7				;restart
$3999 | RST 7				;restart
$399a | RST 7				;restart
$399b | RST 7				;restart
$399c | RST 7				;restart
$399d | RST 7				;restart
$399e | RST 7				;restart
$399f | RST 7				;restart
$39a0 | RST 7				;restart
$39a1 | RST 7				;restart
$39a2 | RST 7				;restart
$39a3 | RST 7				;restart
$39a4 | RST 7				;restart
$39a5 | RST 7				;restart
$39a6 | RST 7				;restart
$39a7 | RST 7				;restart
$39a8 | RST 7				;restart
$39a9 | RST 7				;restart
$39aa | RST 7				;restart
$39ab | RST 7				;restart
$39ac | RST 7				;restart
$39ad | RST 7				;restart
$39ae | RST 7				;restart
$39af | RST 7				;restart
$39b0 | RST 7				;restart
$39b1 | RST 7				;restart
$39b2 | RST 7				;restart
$39b3 | RST 7				;restart
$39b4 | RST 7				;restart
$39b5 | RST 7				;restart
$39b6 | RST 7				;restart
$39b7 | RST 7				;restart
$39b8 | RST 7				;restart
$39b9 | RST 7				;restart
$39ba | RST 7				;restart
$39bb | RST 7				;restart
$39bc | RST 7				;restart
$39bd | RST 7				;restart
$39be | RST 7				;restart
$39bf | RST 7				;restart
$39c0 | RST 7				;restart
$39c1 | RST 7				;restart
$39c2 | RST 7				;restart
$39c3 | RST 7				;restart
$39c4 | RST 7				;restart
$39c5 | RST 7				;restart
$39c6 | RST 7				;restart
$39c7 | RST 7				;restart
$39c8 | RST 7				;restart
$39c9 | RST 7				;restart
$39ca | RST 7				;restart
$39cb | RST 7				;restart
$39cc | RST 7				;restart
$39cd | RST 7				;restart
$39ce | RST 7				;restart
$39cf | RST 7				;restart
$39d0 | RST 7				;restart
$39d1 | RST 7				;restart
$39d2 | RST 7				;restart
$39d3 | RST 7				;restart
$39d4 | RST 7				;restart
$39d5 | RST 7				;restart
$39d6 | RST 7				;restart
$39d7 | RST 7				;restart
$39d8 | RST 7				;restart
$39d9 | RST 7				;restart
$39da | RST 7				;restart
$39db | RST 7				;restart
$39dc | RST 7				;restart
$39dd | RST 7				;restart
$39de | RST 7				;restart
$39df | RST 7				;restart
$39e0 | RST 7				;restart
$39e1 | RST 7				;restart
$39e2 | RST 7				;restart
$39e3 | RST 7				;restart
$39e4 | RST 7				;restart
$39e5 | RST 7				;restart
$39e6 | RST 7				;restart
$39e7 | RST 7				;restart
$39e8 | RST 7				;restart
$39e9 | RST 7				;restart
$39ea | RST 7				;restart
$39eb | RST 7				;restart
$39ec | RST 7				;restart
$39ed | RST 7				;restart
$39ee | RST 7				;restart
$39ef | RST 7				;restart
$39f0 | RST 7				;restart
$39f1 | RST 7				;restart
$39f2 | RST 7				;restart
$39f3 | RST 7				;restart
$39f4 | RST 7				;restart
$39f5 | RST 7				;restart
$39f6 | RST 7				;restart
$39f7 | RST 7				;restart
$39f8 | RST 7				;restart
$39f9 | RST 7				;restart
$39fa | RST 7				;restart
$39fb | RST 7				;restart
$39fc | RST 7				;restart
$39fd | RST 7				;restart
$39fe | RST 7				;restart
$39ff | RST 7				;restart
$3a00 | RST 7				;restart
$3a01 | RST 7				;restart
$3a02 | RST 7				;restart
$3a03 | RST 7				;restart
$3a04 | RST 7				;restart
$3a05 | RST 7				;restart
$3a06 | RST 7				;restart
$3a07 | RST 7				;restart
$3a08 | RST 7				;restart
$3a09 | RST 7				;restart
$3a0a | RST 7				;restart
$3a0b | RST 7				;restart
$3a0c | RST 7				;restart
$3a0d | RST 7				;restart
$3a0e | RST 7				;restart
$3a0f | RST 7				;restart
$3a10 | RST 7				;restart
$3a11 | RST 7				;restart
$3a12 | RST 7				;restart
$3a13 | RST 7				;restart
$3a14 | RST 7				;restart
$3a15 | RST 7				;restart
$3a16 | RST 7				;restart
$3a17 | RST 7				;restart
$3a18 | RST 7				;restart
$3a19 | RST 7				;restart
$3a1a | RST 7				;restart
$3a1b | RST 7				;restart
$3a1c | RST 7				;restart
$3a1d | RST 7				;restart
$3a1e | RST 7				;restart
$3a1f | RST 7				;restart
$3a20 | RST 7				;restart
$3a21 | RST 7				;restart
$3a22 | RST 7				;restart
$3a23 | RST 7				;restart
$3a24 | RST 7				;restart
$3a25 | RST 7				;restart
$3a26 | RST 7				;restart
$3a27 | RST 7				;restart
$3a28 | RST 7				;restart
$3a29 | RST 7				;restart
$3a2a | RST 7				;restart
$3a2b | RST 7				;restart
$3a2c | RST 7				;restart
$3a2d | RST 7				;restart
$3a2e | RST 7				;restart
$3a2f | RST 7				;restart
$3a30 | RST 7				;restart
$3a31 | RST 7				;restart
$3a32 | RST 7				;restart
$3a33 | RST 7				;restart
$3a34 | RST 7				;restart
$3a35 | RST 7				;restart
$3a36 | RST 7				;restart
$3a37 | RST 7				;restart
$3a38 | RST 7				;restart
$3a39 | RST 7				;restart
$3a3a | RST 7				;restart
$3a3b | RST 7				;restart
$3a3c | RST 7				;restart
$3a3d | RST 7				;restart
$3a3e | RST 7				;restart
$3a3f | RST 7				;restart
$3a40 | RST 7				;restart
$3a41 | RST 7				;restart
$3a42 | RST 7				;restart
$3a43 | RST 7				;restart
$3a44 | RST 7				;restart
$3a45 | RST 7				;restart
$3a46 | RST 7				;restart
$3a47 | RST 7				;restart
$3a48 | RST 7				;restart
$3a49 | RST 7				;restart
$3a4a | RST 7				;restart
$3a4b | RST 7				;restart
$3a4c | RST 7				;restart
$3a4d | RST 7				;restart
$3a4e | RST 7				;restart
$3a4f | RST 7				;restart
$3a50 | RST 7				;restart
$3a51 | RST 7				;restart
$3a52 | RST 7				;restart
$3a53 | RST 7				;restart
$3a54 | RST 7				;restart
$3a55 | RST 7				;restart
$3a56 | RST 7				;restart
$3a57 | RST 7				;restart
$3a58 | RST 7				;restart
$3a59 | RST 7				;restart
$3a5a | RST 7				;restart
$3a5b | RST 7				;restart
$3a5c | RST 7				;restart
$3a5d | RST 7				;restart
$3a5e | RST 7				;restart
$3a5f | RST 7				;restart
$3a60 | RST 7				;restart
$3a61 | RST 7				;restart
$3a62 | RST 7				;restart
$3a63 | RST 7				;restart
$3a64 | RST 7				;restart
$3a65 | RST 7				;restart
$3a66 | RST 7				;restart
$3a67 | RST 7				;restart
$3a68 | RST 7				;restart
$3a69 | RST 7				;restart
$3a6a | RST 7				;restart
$3a6b | RST 7				;restart
$3a6c | RST 7				;restart
$3a6d | RST 7				;restart
$3a6e | RST 7				;restart
$3a6f | RST 7				;restart
$3a70 | RST 7				;restart
$3a71 | RST 7				;restart
$3a72 | RST 7				;restart
$3a73 | RST 7				;restart
$3a74 | RST 7				;restart
$3a75 | RST 7				;restart
$3a76 | RST 7				;restart
$3a77 | RST 7				;restart
$3a78 | RST 7				;restart
$3a79 | RST 7				;restart
$3a7a | RST 7				;restart
$3a7b | RST 7				;restart
$3a7c | RST 7				;restart
$3a7d | RST 7				;restart
$3a7e | RST 7				;restart
$3a7f | RST 7				;restart
$3a80 | RST 7				;restart
$3a81 | RST 7				;restart
$3a82 | RST 7				;restart
$3a83 | RST 7				;restart
$3a84 | RST 7				;restart
$3a85 | RST 7				;restart
$3a86 | RST 7				;restart
$3a87 | RST 7				;restart
$3a88 | RST 7				;restart
$3a89 | RST 7				;restart
$3a8a | RST 7				;restart
$3a8b | RST 7				;restart
$3a8c | RST 7				;restart
$3a8d | RST 7				;restart
$3a8e | RST 7				;restart
$3a8f | RST 7				;restart
$3a90 | RST 7				;restart
$3a91 | RST 7				;restart
$3a92 | RST 7				;restart
$3a93 | RST 7				;restart
$3a94 | RST 7				;restart
$3a95 | RST 7				;restart
$3a96 | RST 7				;restart
$3a97 | RST 7				;restart
$3a98 | RST 7				;restart
$3a99 | RST 7				;restart
$3a9a | RST 7				;restart
$3a9b | RST 7				;restart
$3a9c | RST 7				;restart
$3a9d | RST 7				;restart
$3a9e | RST 7				;restart
$3a9f | RST 7				;restart
$3aa0 | RST 7				;restart
$3aa1 | RST 7				;restart
$3aa2 | RST 7				;restart
$3aa3 | RST 7				;restart
$3aa4 | RST 7				;restart
$3aa5 | RST 7				;restart
$3aa6 | RST 7				;restart
$3aa7 | RST 7				;restart
$3aa8 | RST 7				;restart
$3aa9 | RST 7				;restart
$3aaa | RST 7				;restart
$3aab | RST 7				;restart
$3aac | RST 7				;restart
$3aad | RST 7				;restart
$3aae | RST 7				;restart
$3aaf | RST 7				;restart
$3ab0 | RST 7				;restart
$3ab1 | RST 7				;restart
$3ab2 | RST 7				;restart
$3ab3 | RST 7				;restart
$3ab4 | RST 7				;restart
$3ab5 | RST 7				;restart
$3ab6 | RST 7				;restart
$3ab7 | RST 7				;restart
$3ab8 | RST 7				;restart
$3ab9 | RST 7				;restart
$3aba | RST 7				;restart
$3abb | RST 7				;restart
$3abc | RST 7				;restart
$3abd | RST 7				;restart
$3abe | RST 7				;restart
$3abf | RST 7				;restart
$3ac0 | RST 7				;restart
$3ac1 | RST 7				;restart
$3ac2 | RST 7				;restart
$3ac3 | RST 7				;restart
$3ac4 | RST 7				;restart
$3ac5 | RST 7				;restart
$3ac6 | RST 7				;restart
$3ac7 | RST 7				;restart
$3ac8 | RST 7				;restart
$3ac9 | RST 7				;restart
$3aca | RST 7				;restart
$3acb | RST 7				;restart
$3acc | RST 7				;restart
$3acd | RST 7				;restart
$3ace | RST 7				;restart
$3acf | RST 7				;restart
$3ad0 | RST 7				;restart
$3ad1 | RST 7				;restart
$3ad2 | RST 7				;restart
$3ad3 | RST 7				;restart
$3ad4 | RST 7				;restart
$3ad5 | RST 7				;restart
$3ad6 | RST 7				;restart
$3ad7 | RST 7				;restart
$3ad8 | RST 7				;restart
$3ad9 | RST 7				;restart
$3ada | RST 7				;restart
$3adb | RST 7				;restart
$3adc | RST 7				;restart
$3add | RST 7				;restart
$3ade | RST 7				;restart
$3adf | RST 7				;restart
$3ae0 | RST 7				;restart
$3ae1 | RST 7				;restart
$3ae2 | RST 7				;restart
$3ae3 | RST 7				;restart
$3ae4 | RST 7				;restart
$3ae5 | RST 7				;restart
$3ae6 | RST 7				;restart
$3ae7 | RST 7				;restart
$3ae8 | RST 7				;restart
$3ae9 | RST 7				;restart
$3aea | RST 7				;restart
$3aeb | RST 7				;restart
$3aec | RST 7				;restart
$3aed | RST 7				;restart
$3aee | RST 7				;restart
$3aef | RST 7				;restart
$3af0 | RST 7				;restart
$3af1 | RST 7				;restart
$3af2 | RST 7				;restart
$3af3 | RST 7				;restart
$3af4 | RST 7				;restart
$3af5 | RST 7				;restart
$3af6 | RST 7				;restart
$3af7 | RST 7				;restart
$3af8 | RST 7				;restart
$3af9 | RST 7				;restart
$3afa | RST 7				;restart
$3afb | RST 7				;restart
$3afc | RST 7				;restart
$3afd | RST 7				;restart
$3afe | RST 7				;restart
$3aff | RST 7				;restart
$3b00 | RST 7				;restart
$3b01 | RST 7				;restart
$3b02 | RST 7				;restart
$3b03 | RST 7				;restart
$3b04 | RST 7				;restart
$3b05 | RST 7				;restart
$3b06 | RST 7				;restart
$3b07 | RST 7				;restart
$3b08 | RST 7				;restart
$3b09 | RST 7				;restart
$3b0a | RST 7				;restart
$3b0b | RST 7				;restart
$3b0c | RST 7				;restart
$3b0d | RST 7				;restart
$3b0e | RST 7				;restart
$3b0f | RST 7				;restart
$3b10 | RST 7				;restart
$3b11 | RST 7				;restart
$3b12 | RST 7				;restart
$3b13 | RST 7				;restart
$3b14 | RST 7				;restart
$3b15 | RST 7				;restart
$3b16 | RST 7				;restart
$3b17 | RST 7				;restart
$3b18 | RST 7				;restart
$3b19 | RST 7				;restart
$3b1a | RST 7				;restart
$3b1b | RST 7				;restart
$3b1c | RST 7				;restart
$3b1d | RST 7				;restart
$3b1e | RST 7				;restart
$3b1f | RST 7				;restart
$3b20 | RST 7				;restart
$3b21 | RST 7				;restart
$3b22 | RST 7				;restart
$3b23 | RST 7				;restart
$3b24 | RST 7				;restart
$3b25 | RST 7				;restart
$3b26 | RST 7				;restart
$3b27 | RST 7				;restart
$3b28 | RST 7				;restart
$3b29 | RST 7				;restart
$3b2a | RST 7				;restart
$3b2b | RST 7				;restart
$3b2c | RST 7				;restart
$3b2d | RST 7				;restart
$3b2e | RST 7				;restart
$3b2f | RST 7				;restart
$3b30 | RST 7				;restart
$3b31 | RST 7				;restart
$3b32 | RST 7				;restart
$3b33 | RST 7				;restart
$3b34 | RST 7				;restart
$3b35 | RST 7				;restart
$3b36 | RST 7				;restart
$3b37 | RST 7				;restart
$3b38 | RST 7				;restart
$3b39 | RST 7				;restart
$3b3a | RST 7				;restart
$3b3b | RST 7				;restart
$3b3c | RST 7				;restart
$3b3d | RST 7				;restart
$3b3e | RST 7				;restart
$3b3f | RST 7				;restart
$3b40 | RST 7				;restart
$3b41 | RST 7				;restart
$3b42 | RST 7				;restart
$3b43 | RST 7				;restart
$3b44 | RST 7				;restart
$3b45 | RST 7				;restart
$3b46 | RST 7				;restart
$3b47 | RST 7				;restart
$3b48 | RST 7				;restart
$3b49 | RST 7				;restart
$3b4a | RST 7				;restart
$3b4b | RST 7				;restart
$3b4c | RST 7				;restart
$3b4d | RST 7				;restart
$3b4e | RST 7				;restart
$3b4f | RST 7				;restart
$3b50 | RST 7				;restart
$3b51 | RST 7				;restart
$3b52 | RST 7				;restart
$3b53 | RST 7				;restart
$3b54 | RST 7				;restart
$3b55 | RST 7				;restart
$3b56 | RST 7				;restart
$3b57 | RST 7				;restart
$3b58 | RST 7				;restart
$3b59 | RST 7				;restart
$3b5a | RST 7				;restart
$3b5b | RST 7				;restart
$3b5c | RST 7				;restart
$3b5d | RST 7				;restart
$3b5e | RST 7				;restart
$3b5f | RST 7				;restart
$3b60 | RST 7				;restart
$3b61 | RST 7				;restart
$3b62 | RST 7				;restart
$3b63 | RST 7				;restart
$3b64 | RST 7				;restart
$3b65 | RST 7				;restart
$3b66 | RST 7				;restart
$3b67 | RST 7				;restart
$3b68 | RST 7				;restart
$3b69 | RST 7				;restart
$3b6a | RST 7				;restart
$3b6b | RST 7				;restart
$3b6c | RST 7				;restart
$3b6d | RST 7				;restart
$3b6e | RST 7				;restart
$3b6f | RST 7				;restart
$3b70 | RST 7				;restart
$3b71 | RST 7				;restart
$3b72 | RST 7				;restart
$3b73 | RST 7				;restart
$3b74 | RST 7				;restart
$3b75 | RST 7				;restart
$3b76 | RST 7				;restart
$3b77 | RST 7				;restart
$3b78 | RST 7				;restart
$3b79 | RST 7				;restart
$3b7a | RST 7				;restart
$3b7b | RST 7				;restart
$3b7c | RST 7				;restart
$3b7d | RST 7				;restart
$3b7e | RST 7				;restart
$3b7f | RST 7				;restart
$3b80 | RST 7				;restart
$3b81 | RST 7				;restart
$3b82 | RST 7				;restart
$3b83 | RST 7				;restart
$3b84 | RST 7				;restart
$3b85 | RST 7				;restart
$3b86 | RST 7				;restart
$3b87 | RST 7				;restart
$3b88 | RST 7				;restart
$3b89 | RST 7				;restart
$3b8a | RST 7				;restart
$3b8b | RST 7				;restart
$3b8c | RST 7				;restart
$3b8d | RST 7				;restart
$3b8e | RST 7				;restart
$3b8f | RST 7				;restart
$3b90 | RST 7				;restart
$3b91 | RST 7				;restart
$3b92 | RST 7				;restart
$3b93 | RST 7				;restart
$3b94 | RST 7				;restart
$3b95 | RST 7				;restart
$3b96 | RST 7				;restart
$3b97 | RST 7				;restart
$3b98 | RST 7				;restart
$3b99 | RST 7				;restart
$3b9a | RST 7				;restart
$3b9b | RST 7				;restart
$3b9c | RST 7				;restart
$3b9d | RST 7				;restart
$3b9e | RST 7				;restart
$3b9f | RST 7				;restart
$3ba0 | RST 7				;restart
$3ba1 | RST 7				;restart
$3ba2 | RST 7				;restart
$3ba3 | RST 7				;restart
$3ba4 | RST 7				;restart
$3ba5 | RST 7				;restart
$3ba6 | RST 7				;restart
$3ba7 | RST 7				;restart
$3ba8 | RST 7				;restart
$3ba9 | RST 7				;restart
$3baa | RST 7				;restart
$3bab | RST 7				;restart
$3bac | RST 7				;restart
$3bad | RST 7				;restart
$3bae | RST 7				;restart
$3baf | RST 7				;restart
$3bb0 | RST 7				;restart
$3bb1 | RST 7				;restart
$3bb2 | RST 7				;restart
$3bb3 | RST 7				;restart
$3bb4 | RST 7				;restart
$3bb5 | RST 7				;restart
$3bb6 | RST 7				;restart
$3bb7 | RST 7				;restart
$3bb8 | RST 7				;restart
$3bb9 | RST 7				;restart
$3bba | RST 7				;restart
$3bbb | RST 7				;restart
$3bbc | RST 7				;restart
$3bbd | RST 7				;restart
$3bbe | RST 7				;restart
$3bbf | RST 7				;restart
$3bc0 | RST 7				;restart
$3bc1 | RST 7				;restart
$3bc2 | RST 7				;restart
$3bc3 | RST 7				;restart
$3bc4 | RST 7				;restart
$3bc5 | RST 7				;restart
$3bc6 | RST 7				;restart
$3bc7 | RST 7				;restart
$3bc8 | RST 7				;restart
$3bc9 | RST 7				;restart
$3bca | RST 7				;restart
$3bcb | RST 7				;restart
$3bcc | RST 7				;restart
$3bcd | RST 7				;restart
$3bce | RST 7				;restart
$3bcf | RST 7				;restart
$3bd0 | JMP $3c62			;JMP $$

$3bd3 | JMP $3c84			;JMP $$

$3bd6 | JMP $3ceb			;JMP $$

$3bd9 | JMP $3c3f			;JMP $$

$3bdc | LDA $80e3			;$$ -> A
$3bdf | ANI $f				;A &= $
$3be1 | CPI $2				;A < $ : Cf = 1, Zf = 0 | A = $ : Cf = 0, Zf = 1 | A > $ : Cf & Zf = 0
$3be3 | JNZ $3c39			;Zf = 0 : JMP $$
$3be6 | MVI A, $0			;$ -> A
$3be8 | CALL $3fd0			;CALL $$
$3beb | MVI M, $1			;$ -> $HL
$3bed | MVI A, $1			;$ -> A
$3bef | CALL $3fd0			;CALL $$
$3bf2 | MVI M, $0			;$ -> $HL
$3bf4 | MVI A, $6			;$ -> A
$3bf6 | CALL $3fd0			;CALL $$
$3bf9 | MVI M, $0			;$ -> $HL
$3bfb | MVI A, $7			;$ -> A
$3bfd | CALL $3fd0			;CALL $$
$3c00 | MVI M, $10			;$ -> $HL
$3c02 | MVI A, $8			;$ -> A
$3c04 | CALL $3fd0			;CALL $$
$3c07 | MVI M, $0			;$ -> $HL
$3c09 | MVI A, $b			;$ -> A
$3c0b | CALL $3fd0			;CALL $$
$3c0e | MVI M, $0			;$ -> $HL
$3c10 | MVI A, $9			;$ -> A
$3c12 | CALL $3fd0			;CALL $$
$3c15 | MVI M, $0			;$ -> $HL
$3c17 | MVI A, $a			;$ -> A
$3c19 | CALL $3fd0			;CALL $$
$3c1c | MVI M, $0			;$ -> $HL
$3c1e | MVI A, $4			;$ -> A
$3c20 | CALL $3fd0			;CALL $$
$3c23 | MOV B, M			;$HL -> B
$3c24 | MVI A, $c			;$ -> A
$3c26 | CALL $3fd0			;CALL $$
$3c29 | MOV A, B			;B -> A
$3c2a | ANA A				;A &= A
$3c2b | MVI M, $82			;$ -> $HL
$3c2d | JNZ $3c31			;Zf = 0 : JMP $$
$3c30 | INR M				;$HL++
$3c31 | MOV A, M			;$HL -> A
$3c32 | STA $80e1			;A -> $$
$3c35 | CALL $3bd6			;CALL $$
$3c38 | RET					;return

$3c39 | MVI D, $0			;$ -> D
$3c3b | CALL $3f84			;CALL $$
$3c3e | RET					;return

$3c3f | MVI D, $49			;$ -> D
$3c41 | CALL $3f93			;CALL $$
$3c44 | JZ $3c59			;Zf = 1 : JMP $$
$3c47 | MVI D, $9			;$ -> D
$3c49 | CALL $3f93			;CALL $$
$3c4c | JZ $3c55			;Zf = 1 : JMP $$
$3c4f | CALL $3f84			;CALL $$
$3c52 | CALL $3cbe			;CALL $$
$3c55 | CALL $3e40			;CALL $$
$3c58 | RET					;return

$3c59 | MVI D, $9			;$ -> D
$3c5b | CALL $3f8c			;CALL $$
$3c5e | CALL $3cbe			;CALL $$
$3c61 | RET					;return

$3c62 | MVI D, $71			;$ -> D
$3c64 | MVI E, $3c			;$ -> E
$3c66 | CALL $3f6c			;CALL $$
$3c69 | STA $80e1			;A -> $$
$3c6c | MVI D, $61			;$ -> D
$3c6e | MVI E, $69			;$ -> E
$3c70 | CALL $3f6c			;CALL $$
$3c73 | CALL $3cbe			;CALL $$
$3c76 | MVI D, $71			;$ -> D
$3c78 | CALL $3f84			;CALL $$
$3c7b | MVI D, $3c			;$ -> D
$3c7d | CALL $3f84			;CALL $$
$3c80 | STA $80e1			;A -> $$
$3c83 | RET					;return

$3c84 | MVI D, $10			;$ -> D
$3c86 | CALL $3f84			;CALL $$
$3c89 | MVI D, $79			;$ -> D
$3c8b | CALL $3f93			;CALL $$
$3c8e | CNZ $3d9a			;Cf = 0 : CALL $$
$3c91 | MVI D, $59			;$ -> D
$3c93 | CALL $3f93			;CALL $$
$3c96 | JZ $3cae			;Zf = 1 : JMP $$
$3c99 | CALL $3ee7			;CALL $$
$3c9c | MVI D, $61			;$ -> D
$3c9e | CALL $3f84			;CALL $$
$3ca1 | MVI D, $9			;$ -> D
$3ca3 | CALL $3f84			;CALL $$
$3ca6 | MVI D, $69			;$ -> D
$3ca8 | CALL $3f84			;CALL $$
$3cab | CALL $3cbe			;CALL $$
$3cae | MVI D, $49			;$ -> D
$3cb0 | CALL $3f93			;CALL $$
$3cb3 | RZ					;Zf = 1 : RET
$3cb4 | MVI D, $40			;$ -> D
$3cb6 | CALL $3f93			;CALL $$
$3cb9 | RZ					;Zf = 1 : RET
$3cba | CALL $3bd9			;CALL $$
$3cbd | RET					;return

$3cbe | MVI A, $9			;$ -> A
$3cc0 | CALL $3fd0			;CALL $$
$3cc3 | MOV A, M			;$HL -> A
$3cc4 | ANI $41				;A &= $
$3cc6 | JZ $3cdd			;Zf = 1 : JMP $$
$3cc9 | MVI D, $52			;$ -> D
$3ccb | CALL $3f93			;CALL $$
$3cce | RNZ					;Zf != 0 : RET
$3ccf | MVI D, $29			;$ -> D
$3cd1 | CALL $3f8c			;CALL $$
$3cd4 | MVI D, $2c			;$ -> D
$3cd6 | CALL $3f8c			;CALL $$
$3cd9 | STA $80e1			;A -> $$
$3cdc | RET					;return

$3cdd | MVI D, $29			;$ -> D
$3cdf | CALL $3f84			;CALL $$
$3ce2 | MVI D, $2c			;$ -> D
$3ce4 | CALL $3f84			;CALL $$
$3ce7 | STA $80e1			;A -> $$
$3cea | RET					;return

$3ceb | LDA $80e1			;$$ -> A
$3cee | MOV B, A			;A -> B
$3cef | ANI $1				;A &= $
$3cf1 | JZ $3d06			;Zf = 1 : JMP $$
$3cf4 | MVI D, $79			;$ -> D
$3cf6 | CALL $3f8c			;CALL $$
$3cf9 | MVI D, $6c			;$ -> D
$3cfb | CALL $3f8c			;CALL $$
$3cfe | STA $80e1			;A -> $$
$3d01 | MVI D, $10			;$ -> D
$3d03 | CALL $3f8c			;CALL $$
$3d06 | MVI D, $50			;$ -> D
$3d08 | CALL $3f93			;CALL $$
$3d0b | JNZ $3d17			;Zf = 0 : JMP $$
$3d0e | MVI A, $7			;$ -> A
$3d10 | CALL $3fd0			;CALL $$
$3d13 | MVI A, $10			;$ -> A
$3d15 | ANA M				;A &= $HL
$3d16 | MOV M, A			;A -> $HL
$3d17 | MVI D, $47			;$ -> D
$3d19 | CALL $3f93			;CALL $$
$3d1c | MOV D, A			;A -> D
$3d1d | MOV A, B			;B -> A
$3d1e | ANI $8				;A &= $
$3d20 | RLC					;A rotate left
$3d21 | XRA D				;A = A XOR D
$3d22 | JZ $3d3a			;Zf = 1 : JMP $$
$3d25 | MOV A, B			;B -> A
$3d26 | ANI $8				;A &= $
$3d28 | JZ $3d92			;Zf = 1 : JMP $$
$3d2b | MVI D, $47			;$ -> D
$3d2d | CALL $3f8c			;CALL $$
$3d30 | MVI D, $d7			;$ -> D
$3d32 | CALL $3f8c			;CALL $$
$3d35 | MVI D, $50			;$ -> D
$3d37 | CALL $3f8c			;CALL $$
$3d3a | MOV A, B			;B -> A
$3d3b | ANI $4				;A &= $
$3d3d | JZ $3d4a			;Zf = 1 : JMP $$
$3d40 | MVI D, $67			;$ -> D
$3d42 | CALL $3f8c			;CALL $$
$3d45 | MVI D, $50			;$ -> D
$3d47 | CALL $3f8c			;CALL $$
$3d4a | MOV A, B			;B -> A
$3d4b | ANI $2				;A &= $
$3d4d | JZ $3d5a			;Zf = 1 : JMP $$
$3d50 | MVI D, $77			;$ -> D
$3d52 | CALL $3f8c			;CALL $$
$3d55 | MVI D, $50			;$ -> D
$3d57 | CALL $3f8c			;CALL $$
$3d5a | LDA $80e1			;$$ -> A
$3d5d | ANI $20				;A &= $
$3d5f | JNZ $3d6d			;Zf = 0 : JMP $$
$3d62 | MVI D, $59			;$ -> D
$3d64 | CALL $3f84			;CALL $$
$3d67 | MVI D, $49			;$ -> D
$3d69 | CALL $3f84			;CALL $$
$3d6c | RET					;return

$3d6d | LDA $80e1			;$$ -> A
$3d70 | ANI $10				;A &= $
$3d72 | JZ $3d82			;Zf = 1 : JMP $$
$3d75 | MVI D, $59			;$ -> D
$3d77 | CALL $3f8c			;CALL $$
$3d7a | MVI D, $49			;$ -> D
$3d7c | CALL $3f84			;CALL $$
$3d7f | JMP $3d8c			;JMP $$

$3d82 | MVI D, $59			;$ -> D
$3d84 | CALL $3f84			;CALL $$
$3d87 | MVI D, $49			;$ -> D
$3d89 | CALL $3f8c			;CALL $$
$3d8c | MVI D, $10			;$ -> D
$3d8e | CALL $3f8c			;CALL $$
$3d91 | RET					;return

$3d92 | MVI D, $47			;$ -> D
$3d94 | CALL $3f84			;CALL $$
$3d97 | JMP $3d30			;JMP $$

$3d9a | MVI D, $70			;$ -> D
$3d9c | CALL $3f93			;CALL $$
$3d9f | JNZ $3db3			;Zf = 0 : JMP $$
$3da2 | MVI A, $8			;$ -> A
$3da4 | CALL $3fd0			;CALL $$
$3da7 | MVI M, $0			;$ -> $HL
$3da9 | MVI D, $60			;$ -> D
$3dab | CALL $3f84			;CALL $$
$3dae | MVI D, $7b			;$ -> D
$3db0 | CALL $3f84			;CALL $$
$3db3 | MVI D, $60			;$ -> D
$3db5 | CALL $3f93			;CALL $$
$3db8 | JNZ $3dc3			;Zf = 0 : JMP $$
$3dbb | LDA $80e1			;$$ -> A
$3dbe | ANI $1				;A &= $
$3dc0 | JNZ $3dd1			;Zf = 0 : JMP $$
$3dc3 | MVI D, $79			;$ -> D
$3dc5 | CALL $3f84			;CALL $$
$3dc8 | MVI D, $6c			;$ -> D
$3dca | CALL $3f84			;CALL $$
$3dcd | STA $80e1			;A -> $$
$3dd0 | RET					;return

$3dd1 | MVI B, $0			;$ -> B
$3dd3 | MVI A, $5			;$ -> A
$3dd5 | CALL $3fd0			;CALL $$
$3dd8 | MOV C, M			;$HL -> C
$3dd9 | MVI A, $8			;$ -> A
$3ddb | CALL $3fd0			;CALL $$
$3dde | MOV A, M			;$HL -> A
$3ddf | ADD C				;C += A
$3de0 | MOV C, A			;A -> C
$3de1 | JNC $3de6			;Cf = 0 : JMP $$
$3de4 | MVI B, $1			;$ -> B
$3de6 | MVI A, $e			;$ -> A
$3de8 | CALL $3fd0			;CALL $$
$3deb | DAD B				;HL + BC -> HL
$3dec | LDA $80e0			;$$ -> A
$3def | MOV M, A			;A -> $HL
$3df0 | LDA $80e1			;$$ -> A
$3df3 | ANI $40				;A &= $
$3df5 | JZ $3e02			;Zf = 1 : JMP $$
$3df8 | MVI D, $7b			;$ -> D
$3dfa | CALL $3f8c			;CALL $$
$3dfd | MVI D, $60			;$ -> D
$3dff | CALL $3f8c			;CALL $$
$3e02 | MVI D, $c			;$ -> D
$3e04 | CALL $3f8c			;CALL $$
$3e07 | STA $80e1			;A -> $$
$3e0a | MVI A, $8			;$ -> A
$3e0c | CALL $3fd0			;CALL $$
$3e0f | INR M				;$HL++
$3e10 | LDA $80e1			;$$ -> A
$3e13 | CALL $3fda			;CALL $$
$3e16 | ANI $1				;A &= $
$3e18 | JNZ $3e10			;Zf = 0 : JMP $$
$3e1b | MVI D, $c			;$ -> D
$3e1d | CALL $3f84			;CALL $$
$3e20 | STA $80e1			;A -> $$
$3e23 | MVI D, $70			;$ -> D
$3e25 | CALL $3f8c			;CALL $$
$3e28 | MVI A, $4			;$ -> A
$3e2a | CALL $3fd0			;CALL $$
$3e2d | MOV C, M			;$HL -> C
$3e2e | MVI A, $8			;$ -> A
$3e30 | CALL $3fd0			;CALL $$
$3e33 | MOV A, C			;C -> A
$3e34 | CMP M				;A < $HL : Cf = 1, Zf = 0 | A = $HL : Cf = 0, Zf = 1 | A > $HL : Cf & Zf = 0
$3e35 | JNZ $3db3			;Zf = 0 : JMP $$
$3e38 | MVI D, $60			;$ -> D
$3e3a | CALL $3f8c			;CALL $$
$3e3d | JMP $3db3			;JMP $$

$3e40 | MVI D, $30			;$ -> D
$3e42 | CALL $3f8c			;CALL $$
$3e45 | MVI D, $40			;$ -> D
$3e47 | CALL $3f93			;CALL $$
$3e4a | RZ					;Zf = 1 : RET
$3e4b | MVI D, $49			;$ -> D
$3e4d | CALL $3f93			;CALL $$
$3e50 | RZ					;Zf = 1 : RET
$3e51 | MVI A, $a			;$ -> A
$3e53 | CALL $3fd0			;CALL $$
$3e56 | INR M				;$HL++
$3e57 | MOV E, M			;$HL -> E
$3e58 | MVI A, $4			;$ -> A
$3e5a | CALL $3fd0			;CALL $$
$3e5d | MOV C, M			;$HL -> C
$3e5e | MVI A, $5			;$ -> A
$3e60 | CALL $3fd0			;CALL $$
$3e63 | MOV A, M			;$HL -> A
$3e64 | ADD C				;C += A
$3e65 | ADI $d				;A += $
$3e67 | ADD E				;E += A
$3e68 | CALL $3fd0			;CALL $$
$3e6b | MOV A, M			;$HL -> A
$3e6c | STA $80e0			;A -> $$
$3e6f | MVI A, $6			;$ -> A
$3e71 | CALL $3fd0			;CALL $$
$3e74 | MOV A, M			;$HL -> A
$3e75 | MOV B, M			;$HL -> B
$3e76 | CMP E				;A < E : Cf = 1, Zf = 0 | A = E : Cf = 0, Zf = 1 | A > E : Cf & Zf = 0
$3e77 | JNZ $3e8a			;Zf = 0 : JMP $$
$3e7a | MVI D, $b			;$ -> D
$3e7c | CALL $3f93			;CALL $$
$3e7f | JZ $3e8a			;Zf = 1 : JMP $$
$3e82 | MVI D, $5c			;$ -> D
$3e84 | CALL $3f8c			;CALL $$
$3e87 | STA $80e1			;A -> $$
$3e8a | MVI D, $7c			;$ -> D
$3e8c | CALL $3f84			;CALL $$
$3e8f | STA $80e1			;A -> $$
$3e92 | LDA $80e1			;$$ -> A
$3e95 | ANI $80				;A &= $
$3e97 | JNZ $3ea5			;Zf = 0 : JMP $$
$3e9a | CALL $3fda			;CALL $$
$3e9d | MVI D, $49			;$ -> D
$3e9f | CALL $3f93			;CALL $$
$3ea2 | JNZ $3e92			;Zf = 0 : JMP $$
$3ea5 | LDA $80e1			;$$ -> A
$3ea8 | ANI $80				;A &= $
$3eaa | JNZ $3eb3			;Zf = 0 : JMP $$
$3ead | MVI A, $a			;$ -> A
$3eaf | CALL $3fd0			;CALL $$
$3eb2 | DCR M				;$HL--
$3eb3 | MVI D, $7c			;$ -> D
$3eb5 | CALL $3f8c			;CALL $$
$3eb8 | STA $80e1			;A -> $$
$3ebb | LDA $80e1			;$$ -> A
$3ebe | CALL $3fda			;CALL $$
$3ec1 | ANI $80				;A &= $
$3ec3 | JNZ $3ebb			;Zf = 0 : JMP $$
$3ec6 | MVI D, $5c			;$ -> D
$3ec8 | CALL $3f84			;CALL $$
$3ecb | STA $80e1			;A -> $$
$3ece | MOV A, E			;E -> A
$3ecf | CMP B				;A < B : Cf = 1, Zf = 0 | A = B : Cf = 0, Zf = 1 | A > B : Cf & Zf = 0
$3ed0 | JNZ $3e40			;Zf = 0 : JMP $$
$3ed3 | MVI D, $40			;$ -> D
$3ed5 | CALL $3f84			;CALL $$
$3ed8 | MVI D, $30			;$ -> D
$3eda | CALL $3f84			;CALL $$
$3edd | MVI A, $a			;$ -> A
$3edf | CALL $3fd0			;CALL $$
$3ee2 | MVI M, $0			;$ -> $HL
$3ee4 | JMP $3e45			;JMP $$

$3ee7 | MVI D, $42			;$ -> D
$3ee9 | MVI E, $7d			;$ -> E
$3eeb | CALL $3f6c			;CALL $$
$3eee | MVI D, $29			;$ -> D
$3ef0 | MVI E, $6d			;$ -> E
$3ef2 | CALL $3f6c			;CALL $$
$3ef5 | MVI D, $62			;$ -> D
$3ef7 | MVI E, $5d			;$ -> E
$3ef9 | CALL $3f6c			;CALL $$
$3efc | MVI D, $72			;$ -> D
$3efe | MVI E, $4d			;$ -> E
$3f00 | CALL $3f6c			;CALL $$
$3f03 | MVI D, $5d			;$ -> D
$3f05 | CALL $3f93			;CALL $$
$3f08 | JNZ $3f56			;Zf = 0 : JMP $$
$3f0b | MVI A, $3			;$ -> A
$3f0d | CALL $3fd0			;CALL $$
$3f10 | MOV A, M			;$HL -> A
$3f11 | ANI $f0				;A &= $
$3f13 | RRC					;A rotate right
$3f14 | RRC					;A rotate right
$3f15 | RRC					;A rotate right
$3f16 | RRC					;A rotate right
$3f17 | MOV B, A			;A -> B
$3f18 | MVI A, $d			;$ -> A
$3f1a | CALL $3fd0			;CALL $$
$3f1d | MVI A, $f0			;$ -> A
$3f1f | ANA M				;A &= $HL
$3f20 | ORA B				;A |= B
$3f21 | MOV M, A			;A -> $HL
$3f22 | ANI $80				;A &= $
$3f24 | JNZ $3f2e			;Zf = 0 : JMP $$
$3f27 | MVI D, $40			;$ -> D
$3f29 | MVI E, $3d			;$ -> E
$3f2b | CALL $3f6c			;CALL $$
$3f2e | MOV A, M			;$HL -> A
$3f2f | STA $80e0			;A -> $$
$3f32 | MVI D, $7c			;$ -> D
$3f34 | CALL $3f84			;CALL $$
$3f37 | STA $80e1			;A -> $$
$3f3a | LDA $80e1			;$$ -> A
$3f3d | CALL $3fda			;CALL $$
$3f40 | ANI $80				;A &= $
$3f42 | JNZ $3f4d			;Zf = 0 : JMP $$
$3f45 | MVI D, $59			;$ -> D
$3f47 | CALL $3f93			;CALL $$
$3f4a | JNZ $3f3a			;Zf = 0 : JMP $$
$3f4d | MVI D, $7c			;$ -> D
$3f4f | CALL $3f8c			;CALL $$
$3f52 | STA $80e1			;A -> $$
$3f55 | RET					;return

$3f56 | MVI A, $3			;$ -> A
$3f58 | CALL $3fd0			;CALL $$
$3f5b | MOV A, M			;$HL -> A
$3f5c | ANI $f				;A &= $
$3f5e | MOV B, A			;A -> B
$3f5f | MVI A, $d			;$ -> A
$3f61 | CALL $3fd0			;CALL $$
$3f64 | MOV A, M			;$HL -> A
$3f65 | ANI $f0				;A &= $
$3f67 | ORA B				;A |= B
$3f68 | MOV M, A			;A -> $HL
$3f69 | JMP $3f2e			;JMP $$

$3f6c | CALL $3f99			;CALL $$
$3f6f | MOV A, C			;C -> A
$3f70 | ANA M				;A &= $HL
$3f71 | MOV D, E			;E -> D
$3f72 | MOV E, A			;A -> E
$3f73 | CALL $3f99			;CALL $$
$3f76 | MOV A, E			;E -> A
$3f77 | ANA A				;A &= A
$3f78 | JZ $3f7f			;Zf = 1 : JMP $$
$3f7b | MOV A, M			;$HL -> A
$3f7c | ORA C				;A |= C
$3f7d | MOV M, A			;A -> $HL
$3f7e | RET					;return

$3f7f | MOV A, C			;C -> A
$3f80 | CMA					;1's compliment A (invert)
$3f81 | ANA M				;A &= $HL
$3f82 | MOV M, A			;A -> $HL
$3f83 | RET					;return

$3f84 | CALL $3f99			;CALL $$
$3f87 | MOV A, C			;C -> A
$3f88 | CMA					;1's compliment A (invert)
$3f89 | ANA M				;A &= $HL
$3f8a | MOV M, A			;A -> $HL
$3f8b | RET					;return

$3f8c | CALL $3f99			;CALL $$
$3f8f | MOV A, M			;$HL -> A
$3f90 | ORA C				;A |= C
$3f91 | MOV M, A			;A -> $HL
$3f92 | RET					;return

$3f93 | CALL $3f99			;CALL $$
$3f96 | MOV A, M			;$HL -> A
$3f97 | ANA C				;A &= C
$3f98 | RET					;return

$3f99 | MOV A, D			;D -> A
$3f9a | ANI $f				;A &= $
$3f9c | CALL $3fd0			;CALL $$
$3f9f | MOV A, D			;D -> A
$3fa0 | RRC					;A rotate right
$3fa1 | RRC					;A rotate right
$3fa2 | RRC					;A rotate right
$3fa3 | RRC					;A rotate right
$3fa4 | ANI $3				;A &= $
$3fa6 | JZ $3fc1			;Zf = 1 : JMP $$
$3fa9 | SUI $1				;A -= $
$3fab | JZ $3fc6			;Zf = 1 : JMP $$
$3fae | SUI $1				;A -= $
$3fb0 | JZ $3fcb			;Zf = 1 : JMP $$
$3fb3 | ADI $7				;A += $
$3fb5 | MOV C, A			;A -> C
$3fb6 | MOV A, D			;D -> A
$3fb7 | ANI $40				;A &= $
$3fb9 | RZ					;Zf = 1 : RET
$3fba | MOV A, C			;C -> A
$3fbb | RLC					;A rotate left
$3fbc | RLC					;A rotate left
$3fbd | RLC					;A rotate left
$3fbe | RLC					;A rotate left
$3fbf | MOV C, A			;A -> C
$3fc0 | RET					;return

$3fc1 | ADI $1				;A += $
$3fc3 | JMP $3fb5			;JMP $$

$3fc6 | ADI $2				;A += $
$3fc8 | JMP $3fb5			;JMP $$

$3fcb | ADI $4				;A += $
$3fcd | JMP $3fb5			;JMP $$

$3fd0 | LHLD $3000			;$$ -> HL
$3fd3 | ADD L				;L += A
$3fd4 | JNC $3fd8			;Cf = 0 : JMP $$
$3fd7 | INR H				;H++
$3fd8 | MOV L, A			;A -> L
$3fd9 | RET					;return

$3fda | PUSH PSW			;AF (ACC and FLAG = PSW processor status word) -> STACK$$
$3fdb | LDA $4062			;$$ -> A
$3fde | ORI $88				;A |= $
$3fe0 | STA $8020			;A -> $$
$3fe3 | SHLD $c0ff			;HL -> $$
$3fe6 | LDA $4062			;$$ -> A
$3fe9 | STA $8020			;A -> $$
$3fec | POP PSW				;STACK$$ -> AF (ACC and FLAG = PSW processor status word)
$3fed | RET					;return

$3fee | RST 7				;restart
$3fef | RST 7				;restart
$3ff0 | RST 7				;restart
$3ff1 | RST 7				;restart
$3ff2 | RST 7				;restart
$3ff3 | RST 7				;restart
$3ff4 | RST 7				;restart
$3ff5 | RST 7				;restart
$3ff6 | RST 7				;restart
$3ff7 | RST 7				;restart
$3ff8 | RST 7				;restart
$3ff9 | RST 7				;restart
$3ffa | RST 7				;restart
$3ffb | RST 7				;restart
$3ffc | NOP				; ff checksum
$3ffd | NOP				; 55 checksum
$3ffe | NOP				; 66 checksum
$3fff | NOP				; 0 checksum

### END ###
