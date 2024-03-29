; AVF MOVPLAY by Avery Lee 2013
; Sound quality fixes by Jakub Husak 2022
;
; Player runs from SIO device (or other external)
; and plays raw data stored on
; mass storage device SIDE, SIDEII or INCOGNITO
;
; To prepare media to play simply write the avf movie
; using a sector copy software (dd for example) 
; to the compact flash card, insert device with card,
; boot the Atari from another device with player
; and enjoy 50/60 fps movie :)
;
; logic:
; start - toggle pause
; select+start (playing) - begin
; select (playing) - backwards, cancels pause
; option (playing) - ff, cancels pause
; select (paused) - one frame-
; option (paused) - one frame+
;
; 4C00-4EFF		Playback display list
; 4F00-4FFF		Error display list
; 5000-5FFF		Framebuffer
;
		icl		'hardware.inc'
		icl		'os.inc'

; due to nature of setting labels in the command line,
; those are comments, but in the command line set them to
; 1 or 2.
CODE_FOR_SIDE	equ	1
CODE_FOR_INCOGNITO	equ	2
BETA	equ	1

; please define CODE to 1 or 2 in mads command line: -d:CODE=(1|2)
	.error (.not .def (CODE))

	.error (CODE<>1 .and CODE <>2)

;START_SECTOR = 33792+16
START_SECTOR = 16
;START_SECTOR = 74*60*50*17+16

	.if (CODE == CODE_FOR_INCOGNITO)
IDE_BASE = $d1e0
	.endif

	.if (CODE == CODE_FOR_SIDE)
IDE_BASE = $d5f0
	.endif

side_sdx_control	equ	$d5e0
side_cart_control	equ	$d5e4

ide_data	equ	IDE_BASE+0
ide_feature	equ	IDE_BASE+1
ide_errors	equ	IDE_BASE+1
ide_nsecs	equ	IDE_BASE+2
ide_lba0	equ	IDE_BASE+3
ide_lba1	equ	IDE_BASE+4
ide_lba2	equ	IDE_BASE+5
ide_lba3	equ	IDE_BASE+6
ide_cmd		equ	IDE_BASE+7
ide_status	equ	IDE_BASE+7

IDE_CMD_READ				equ		$20
IDE_CMD_READ_MULTIPLE		equ		$c4
IDE_CMD_SET_MULTIPLE_MODE	equ		$c6
IDE_CMD_SET_FEATURES		equ		$ef
;============================================================================

pause_imaddr	equ	$8150
pause_imaddr2	equ	$9000

		org	$0
		opt	o-
zpsndbuf:
	
		org	$c0
zp_start:
log_curx	dta		0
log_curln	dta		a(0)
log_srcptr	dta		a(0)
log_lncnt	dta		0
back_consol	dta		0
pause		dta		0
waitcnt	dta		0
;nextpg	dta		0
delycnt	dta		0
.if (COVOX==0)
volume	dta		0
.endif
sector	dta	0
		dta	0
		dta	0
		dta	0
		
d0		dta		0
d1		dta		0
d2		dta		0
d3		dta		0
d4		dta		0
;d5		dta		0
;d6		dta		0
;d7		dta		0
a0		dta		a(0)
a1		dta		a(0)
a2		dta		a(0)
a3		dta		a(0)
zp_end:

;============================================================================
		org		$2800
		opt		o+

.proc	main
		jsr reset_sound
		.if (COVOX=$D300)
		lda	#$ff
		sta	$d300
		lda	$D302
		and	#$fb
		sta	$D302
		lda 	#$ff
		sta	$D300
		lda	$D302
		ora	#$4
		sta	$D302
		.endif
		.if (COVOX!=0)
		lda 	#$ff
		sta	COVOX
		.endif
		jsr graphics0

		mva #0	goodcnt
		sei

		;clear PIA interrupts
		mva		#$3c pactl
		;lda		porta
		;lda		portb

		jsr store_zp
		
		;zero working variables

		ldx		#0
		lda		#0
clear_zp:
		sta		zp_start,x
		dex
		bne		clear_zp

		ldx		#$bf
		ldy		#$33
clear_zp1:
		tya
		sta		zpsndbuf,x
		dex
		beq cend
		cpy #$ff
		beq clear_zp1
		iny
		iny
		
		bne		clear_zp1
cend
		sty		zpsndbuf

		lda		#$ff
		ldx 		#0
fillsndbuff
		sta		soundbuf,x
		dex
		bne		fillsndbuff
		; store to force 3 cycle command in main loop
		mva 	#$e0	$e0

		;set up audio
		; timer 1: 16-bit linked, audio enabled
		; timer 2: 16-bit linked, audio disabled
		lda		#$a0
		sta		audc1
		sta		audc2
		sta		audc3
		sta		audc4
		mva		#$ff audf2
		mva		#$71 audctl
		mva		#$03 skctl

		;initialize text display
		jsr		FlipToTextDisplay
		
		jsr		LogImprint
		dta		' 50/60fps video player by Avery Lee '*,$9b
		dta		' Further development by Jakub Husak '*,$9b

	.if (CODE == CODE_FOR_INCOGNITO)
		dta		' Incognito version '*
		ins		'date.inc'+128
		dta		'       '*,$9b
	.endif
	.if (CODE == CODE_FOR_SIDE)
		dta		' SIDE/SIDEII version '*
		ins		'date.inc'+128
		dta		'     '*,$9b
	.endif
	.if (COVOX==0)
		dta		' AUDIO ON (FIRST) POKEY CHIP        '*
	.elif (COVOX==$d280)
		dta		' AUDIO ON COVOX UNDER $D280         '*
	.elif (COVOX==$d500)
		dta		' AUDIO ON COVOX UNDER $D500         '*
	.elif (COVOX==$d600)
		dta		' AUDIO ON COVOX UNDER $D600         '*
	.elif (COVOX==$d700)
		dta		' AUDIO ON COVOX UNDER $D700         '*
	.endif
		dta		$9b,$9b,0

		;set up NTSC/PAL differences
		lda		#$08
		bit		pal
		bne		is_ntsc
		; PAL VARIABLES SECTION
		mva 		#{bit.b 0 } ntsc_eat_cycle
		mva		#$40 prior_byte_1
		mva		#$c7 prior_byte_2
		mva		#<(-67) wait_loop_count
		mva		#<(soundbuf-$100+68) wait_loop_offset
		
		jsr		LogImprint
		dta		'Video mode: PAL (need PAL video)',$9b,0
		
		jmp		is_pal
		; NTSC VARIABLES SECTION
is_ntsc:
		mva		#$c0 prior_byte_1
		mva		#$47 prior_byte_2
		mva		#<(-17) wait_loop_count
		mva		#<(soundbuf-$100+18) wait_loop_offset

		jsr		LogImprint
		dta		'Video mode: NTSC (need NTSC video)',$9b,0		
is_pal:

		;turn off SIDE cart
		mva		#$c0 side_sdx_control
		mva		#$80 side_cart_control
		lda $d013 ; ???
		sta $3fa ; ???

		; needed because sometimes background is not black.
		mva		#$00 colbk

		;reset drive
		jsr		LogImprint
		dta		'Resetting IDE device... ',0
		
		ldx		#$7e
		stx		$d5f8
		sta		wsync
		sta		wsync
		ldx		#$7f
		stx		$d5f8

		ldy		#$40
		ldx		#0
reset_loop:
		sta:dex:rne	wsync
		dey
		bne		reset_loop

		;set LBA 0 and select drive 0
		mva		#$e0 ide_lba3
		sta		sector+3
		mva		#<[START_SECTOR/65536] ide_lba2
		sta		sector+2
		mva		#<[START_SECTOR/256] ide_lba1
		sta		sector+1
		mva		#<[START_SECTOR] ide_lba0
		sta		sector+0

		;set up for PIO 6 transfers
		mva		#$03 ide_feature
		mva		#$0c ide_nsecs
		lda		#IDE_CMD_SET_FEATURES
		jsr		IdeDoCmd
		bcc		cmd_ok
fatal_cmd_error:
		jsr		LogImprint
		dta		$9b,'IDE command error: ',0
		jsr		LogCmdErrorData
		jmp		ExitToDos
cmd_ok:
		
		;set up for 8-bit transfers
		mva		#32 ide_nsecs
		lda		#IDE_CMD_SET_MULTIPLE_MODE
		jsr		IdeDoCmd
;		bcs		fatal_cmd_error

		;set up for 8-bit transfers
		mva		#$01 ide_feature
		lda		#IDE_CMD_SET_FEATURES
		jsr		IdeDoCmd
		bcs		fatal_cmd_error
		
		jsr		LogImprint
		dta		'OK',$9b,0

		mva		#17 ide_nsecs
		
		;start on sector 16 (-15 for first inc)
		mva		#<[START_SECTOR] sector
		mva		#<[START_SECTOR/256] sector+1
		mva		#<[START_SECTOR/65536] sector+2
		
		jsr		LogImprint
		dta		$9b
		.if (RELEASE == BETA)
		dta		'BETA VERSION',$9b
		dta		$9b
		.endif
		dta		'Movie ready to play.',$9b
		dta		'Press ','SPACE'*,' to play once',$9b
		dta		'Press ','RETURN'*,' to play looped',$9b
		dta		$9b
		dta		'During playback:',$9B,$9B
		dta		'  ',' SELECT '*,'+',' START '*,' - Restart',$9b
		dta		'  ',' START  '*,' - Pause On/Off',$9b
		dta		'  ',' OPTION '*,' - Fast Forward',$9b
		dta		'  ',' SELECT '*,' - Wind Back',$9b
		.if (COVOX==0)
		dta		'  ',' SHIFT '*,'+',' OPTION '*,' - Volume UP',$9b
		dta		'  ',' SHIFT '*,'+',' SELECT '*,' - Volume DOWN',$9b
		.endif
		dta		0

getagain
		ldy		#1
wait_key
		INC_RTC
		WAITFRAME
		bit		irqst
		bvs		wait_key
		lda 		$d209
		cmp		#12 ; RETURN
		beq		play_loop
		cmp		#33 ; SPACE
		beq		play_once
		cmp		#28 ; ESC
		bne		getagain
		jmp		ExitToDosNow
play_once
		dey
play_loop
		sty		loopmode
		mva		#0 irqen
		mva		#$40 irqen
		
		jsr		FlipToVideoDisplay
		
		;set up for reading
		lda		#248/2
		cmp:rne	vcount
		
		mwa		#dlist_wait dlistl
		mva		#$22 dmactl
		
		mva		#>ide_base chbase
		sta		nmires

		sta		wsync
		jmp		main_loop_start
	
err:
		lda		loopmode
		sne
		jmp		FatalReadError
		mva		#<[START_SECTOR] sector
		mva		#<[START_SECTOR/256] sector+1
		mva		#<[START_SECTOR/65536] sector+2
		lda		#$e0					;2
		sta		sector+3				;3
		.if (COVOX==0)
		lda #0
		sta volume
		sta audc1
		.endif

		jmp main_loop_start

		// eat whole frame when ide was not ready
main_loop_delay:
		mva		#0 dmactl
		sta		audc1
		
		lda		#124
		cmp:rne	vcount
		mwa		#dlist dlistl
		
		mva		#$22 dmactl

main_loop:

		;MAIN KERNEL
		;
		;With normal width lines (40 bytes), we need some pad bytes to ensure that
		;sector boundaries are maintained.
		
		;DLI should be on by now; if not, wait for it.
		lda:rpl	nmist
		sta		nmires
		
		;if the drive is busy, we need to blow a frame (BOO)
		lda		ide_status
		bmi		main_loop_delay
		lsr
		bcs		err
		and		#$04
		beq		main_loop

		.if (COVOX==0)
		lda	volume
		sta	audc1
		bne	chk_pause

		lda	init_volume:#$af
		sta	volume
		.endif
chk_pause
		lda	pause
		beq	nopause
		; IDE Ready to read frame, so read by hand
		; and display paused frame from memory
		jsr	FlipToPauseDisplay
nopause
		lda $d209
		cmp #28
		sne
		jmp ExitToDosNow

		ldx		#$c0			;2 (changed to $47 for PAL)
prior_byte_1 = * - 1
		lda		#$47			;2 (changed to $c0 for PAL)
prior_byte_2 = * - 1
		
		sta		wsync
		bit		$00
		
		
;          1         2         3         4         5         6         7         8         9         0         1   
;012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
;===========================================================================================================....... -> 7+16 = 23 cycles
;.D..............F.FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCRVV.V... -> 7+16 = 23 cycles
;.D..............F.FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCRVV.V... -> 7+25 = 32 cycles
;.D................F.FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCRC..............


.rept 192
		;jump to right before even line (start vscrol region - vscrol=7)
		;jump to right before odd line (end vscrol region - vscrol=0)
		;24 cycles
		
.if (#%2)==0
		sta		prior			;106, 107, 108, 109
		sta		vscrol			;110, 111, 112, 113
		sta		chactl			;0, 2, 3, 4
.else	
		stx		prior			;4
		stx		vscrol			;4
		stx		chactl			;4
.endif
		
.if [(#%3)==2]
		ldy.w		zpsndbuf+#		;5, 6, 7
		PLAY_SAMPLE				;8,9,10,11,12,13,14,15
.if (#!=191)	
		:4 nop
.endif
.else
		ldy		zpsndbuf+#		;5, 6, 7
		PLAY_SAMPLE				;8,9,10,11,12,13,14,15
.endif

.endr
			
		;With 192 scanlines, there are 320 bytes left over. 262 of these are used for
		;sound, and the other 58 we toss. We read 10 bytes a scanline and so this
		;takes 32 scanlines.
				
		ldx		$e0 ; #$e0
		
		;we are coming in hot from the last visible scanline, so we need to skip
		;the wsync
		bne		sndread_loop_start
		
sndread_loop:
		sta		wsync						;4
		bit.w		$00
sndread_loop_start:
		ldy		ide_data					;4
		mva		ide_data zpsndbuf+$20,x		;9
		lda		ide_data					;4
		PLAY_SAMPLE				;8
		sta		zpsndbuf+$40,x				;4
		mva		ide_data zpsndbuf+$60,x		;9
		mva		ide_data zpsndbuf+$80,x		;9
		mva		ide_data zpsndbuf+$a0,x		;9
		mva		ide_data zpsndbuf+$c0,x		;9
		mva		ide_data soundbuf-$e0,x		;9
		mva		ide_data soundbuf-$c0,x		;9
		lda		ide_data					;4

		inx									;2
		bne		sndread_loop				;3
		
		sta		wsync
		ldy		ide_data
		mva		ide_data soundbuf+$40
		lda		ide_data
		bit.w		$00
		PLAY_SAMPLE				;8
		:7 lda	ide_data		;28
		mwa		#dlist dlistl	;12

		INC_RTC		; max 6+2+6+2+6 = 22

		; ROOM FOR SOMETHING

		ldx		#<(-18) 	;2
eat_loop:
		sta		wsync
		ldy		ide_data
		mva		ide_data soundbuf+$40-<(-19),x
		cpx		#$fb
ntsc_eat_cycle = *
		bne		*+2
		nop
		PLAY_SAMPLE				;8
		:8 lda	ide_data		;32
		inx
		bne		eat_loop
		
		.if (COVOX==0)
		; here update, because time room
		lda volume
		sta init_volume
		.endif
		inc goodcnt
		sne
		dec goodcnt

		;Do a line of audio, so we get some time again.
		sta		wsync
		ldy		ide_data	; 4
		lda		ide_data 	; 4
		pha:pla				; 7
		bit		$00		; 3
		nop				; 2
		PLAY_SAMPLE				;8 - 28
		
main_loop_start:				
		;Okay, now we can issue the next read. Bump the sector number at
		;this point.
		lda		sector			;3
		sta		ide_lba0		;4
		ldx		sector+1		;3
		stx		ide_lba1		;4
		ldx		sector+2		;3
		stx		ide_lba2		;4
		ldx		sector+3		;3
		stx		ide_lba3		;4 - 28
		
		; due to 3-byte sectors number increased, max sector number is
		; 256*256*256=16777216, which gives about 274 minutes.
		; after that the movie plays again out of sync.
		; someday we will fix this.

		add		#17			;4
		sta		sector			;4
		bcc		no_carry		;2+1
		inc		sector+1		;5
		bne		no_carry		;2+1
		inc		sector+2		;5 - 23
no_carry:
		
		; :3	nop ; 7 - skips at about one minute when uncommented
		;Kick the read.
		lda		#17
		;We have 47 scanlines to wait (~4ms), so in the meantime let's play
		;some audio.
		sta		wsync

		sta		ide_nsecs
		lda		#IDE_CMD_READ
		sta		ide_cmd ; 4 - 12
		ldy		soundbuf

		; logic:
		; start - toggle pause
		; select+start (playing) - begin
		; select (playing) - backwards, cancels pause
		; option (playing) - ff, cancels pause
		; select (paused) - one frame-
		; option (paused) - one frame+
		lda		consol
		cmp		#$6 ; bare start key

		PLAY_SAMPLE		; 8

		bne		no_switch
		cmp		back_consol
		beq		no_switch
		sta		back_consol
		lda		pause
		eor		#$ff
		sta		pause
		jmp no_consol

no_switch:
		sta		back_consol
		cmp		#$4	; select + start
		bne		no_start				;2
reset_play:
		lda		#<[START_SECTOR]		;2
		sta		sector					;3
		lda		#<[START_SECTOR/256]	;2
		sta		sector+1				;3
		lda		#<[START_SECTOR/65536]	;2
		sta		sector+2				;3
		lda		#$e0					;2
		sta		sector+3				;3
		bne		no_consol
no_start:	cmp		#$5 ; select
		bne		no_select
		.if (COVOX==0)
		lda		skctl
		and		#$8
		bne		do_rewind
		; tricky dec if greater then a0
		lda		#$a0
		cmp		volume
		bcs		no_consol
		dec		volume
		bne		no_consol
		.endif
;

;
do_rewind:
		;lda		sector+3
		;bne		nextcheck
		lda		sector+2
		bne		nextcheck
		lda		sector+1
		cmp 		#9
		bcs		nextcheck
		bcc		reset_play
nextcheck:
		lda		sector
		sec
		sbc		#248
		sta		sector
		lda		sector+1
		sbc		#7
		sta		sector+1
		scs
		dec		sector+2
		;sbc		#0
		;sta		sector+2
		clc
		bcc		no_consol
no_select:	cmp		#$3 ; option
		bne		no_consol
		.if (COVOX==0)
		lda		skctl
		and		#$8
		bne		fastforward

		lda		#$ae
		cmp		volume
		bcc		no_consol
		inc		volume
		bne		no_consol
		.endif

fastforward:
		lda		sector
		add		#248			;4
		;add		#0			;4
		sta		sector			;4
		lda		sector+1
		adc		#7
		sta		sector+1
		bcc		no_consol		;2+1
		inc		sector+2		;5 - 23
		bne		no_consol		;2+1
		inc		sector+3		;5 - 23

no_consol:
		ldx		#<(-17)			;modified to -67 for PAL
wait_loop_count = *-1

wait_loop:
		ldy		soundbuf-$100+18,x
wait_loop_offset = *-2
		sta		wsync

		cpx		#$e8	;2
		bne		*+2 	;3/2 ;skip dl dma
		cpx		#$f0	;2
		bne		*+2 	;3/2 ;skip dl dma
		cpx		#$f8	;2
		bne		*+2 	;3/2 ;skip dl dma
		nop
		bit.b 		0
		
		PLAY_SAMPLE		; 8

		lda		consol
		lsr

		
		inx
		bne		wait_loop
		jmp		main_loop
.endp

; This macro has to be exactly 8 cycles long
.if (COVOX == 0)
PLAY_SAMPLE	.macro
		; pokey PWM play
		sty		audf1
		sty		stimer
.endm
.elseif (COVOX==$D300)
PLAY_SAMPLE	.macro
		; COVOX PCM play
		sty	COVOX
		sty 	COVOX
.endm

.else
PLAY_SAMPLE	.macro
		; COVOX PCM play
		sty	COVOX
		sty 	COVOX+2
.endm
.fi


INC_RTC		.macro
		inc zp_store+20
		bne iend
		inc zp_store+19
		bne iend
		inc zp_store+18
iend
		.endm


ExitToDosNow
		ldy #1
		dta {bit.w}
ExitToDos
		ldy #150
		mva 		#$a0	audc1
		.if (COVOX=$D300)
exit
		ldx	$d300
		cpx	#$ff
		beq	exit_cont
		sta	wsync
		sta	wsync
		sta	wsync
		sta	wsync
		inc	$d300
		bne	exit
exit_cont
		lda	#$ff
		lda	$D302
		and	#$fb
		sta	$D302
		lda 	#$00
		sta	$D300
		lda	$D302
		ora	#$4
		sta	$D302
		.endif
endwait
		INC_RTC
		WAITFRAME
		dey
		bne	endwait
		jsr	FlipToTextDisplay
		jsr	restore_zp
		mva		#$40 nmien
		jsr	reset_sound
		cli
		rts
		;jmp	(dosvec)

reset_sound
		lda #3
		ldx #$0f
again
		sta	audf1,x
		sta	audf1+$10,x
		sta	audf1+$20,x
		sta	audf1+$30,x
		lda #0
		dex
		bpl again
		rts

WAITFRAME	.macro
		bit VCOUNT
		bpl *-3
		bit VCOUNT
		bmi *-3
		.endm

.proc	restore_zp
		ldx		#0
		lda 		zp_store,x
		sta		zpsndbuf,x
		inx
		bne		restore_zp+2
		rts

.endp
.proc	store_zp
		ldx		#0
		lda		zpsndbuf,x
		sta 		zp_store,x
		inx
		bne		store_zp+2
		rts
.endp
;============================================================================
.proc	IdeDoCmd
		sta		ide_cmd
		
		;wait for BSY to go low or for ERR to go high
		lda		#0
		sta		delycnt
		tax
		mva		#4 waitcnt		;~2 seconds
wait_loop:
		lda		ide_status
		bpl		wait_done
		lsr
		bcs		wait_error
		dex
		bne		wait_loop
		dec		delycnt
		bne		wait_loop
		dec		waitcnt
		bne		wait_loop
		
		;timeout!
		sec
wait_error:
		rts
wait_done:
		clc
		rts
.endp

;============================================================================
.proc FatalReadError
		sei

		jsr		FlipToTextDisplay
		jsr		LogClear

		jsr		LogImprint
		dta		'Read error ',0

		jsr		LogCmdErrorData
		lda		loopmode
		beq		exit_app
		jsr 		restore_zp
		cli
		; prepare zp
		jmp		main
exit_app
		jmp 		ExitToDos
.endp

;============================================================================
.proc LogCmdErrorData
		mva		ide_status d1
		mva		ide_errors d2
		jsr		LogImprintf
		dta		'%x%x',0

		mva		ide_lba3 d1
		mva		ide_lba3 d2
		mva		ide_lba3 d3
		mva		ide_lba3 d4
		jsr		LogImprintf
		dta		' %x%x%x%x',$9b,0
		
		rts
.endp

;============================================================================
.proc FlipToVideoDisplay

		;shut off all interrupts and kill display
		mva		#0 nmien
		mva		#0 dmactl
		sta		nmires

		;move sprites out of the way
		ldx		#7
		lda		#0
sprclear:
		sta		hposp0,x
		dex
		bpl		sprclear

		;clear playfield page
		lda		#[(ide_data&$3ff)/8]
		ldx		#>framebuf
		ldy		#0
clear_loop:
		stx		clear_loop_2+2
clear_loop_2:
		sta		framebuf,y
		iny
		bne		clear_loop_2
		inx
		cpx		#(>framebuf)+$10
		bne		clear_loop
		
		;prime memory scan counter to $4000
		lda		#124
		cmp:rne	vcount
		
		mwx		#dlist_init dlistl
		mva		#$20 dmactl

		sta		wsync
		sta		wsync
		cmp:rne	vcount

		mva		#12 hscrol
		mva		#7 vscrol
		;mva		#$af audc1
		rts
.endp

;============================================================================
.proc FlipToPauseDisplay
		;shut off all interrupts and kill display
		mva             #0 nmien
		mva             #0 dmactl
		sta             nmires

		mva 		#$a0	audc1
		mva		#64	lcnt
		mwa		#pause_imaddr	a3
		ldy		#0
line_next
		:1 lda ide_data	; eat sound
		ldx		#40
@		mva		ide_data (a3),y+ ; transfer line
		dex
		bne @-

		:4 lda ide_data	; eat sound
		ldx		#40
@		mva		ide_data (a3),y+ ; transfer line
		dex
		bne @-

		:3 lda ide_data	; eat sound
		ldx		#40
@		mva		ide_data (a3),y+ ; transfer line
		dex
		bne @-
		tya
		clc
		adc	a3
		sta	a3
		scc:inc a3+1
		ldy	#0

		dec lcnt
		bne	line_next

		ldy		#248/2 ; wait for screen for be displayed
		cpy:rne	vcount

		mwa		#dlist_paused dlistl
		mva		#$22 dmactl
		lda		#$08
		bit		pal
		bne		is_ntsc
		
		ldx		#$40
		lda		#$c0
		
		jmp		is_pal
is_ntsc:
		ldx		#$c0
		lda		#$40
is_pal:

pause_loop
		ldy:rpl	nmist	; wait for sync line (dliint set in line)
		sty		nmires
		ldy		#96

pause_engine
		sta 		wsync
		sta		prior			;106, 107, 108, 109
		sta 		wsync
		stx		prior			;4
		dey
		bne	pause_engine

; keyboard and consol handling
		INC_RTC
		bit		irqst
		bvs		chk_consol
		ldy 		$d209
		cpy		#28 ; ESC
		bne		chk_consol
		jmp		ExitToDosNow
chk_consol
		ldy		consol
		cpy		#$6 ; bare start key
		bne		no_switch ; 
		cpy		back_consol
		beq		no_switch
		sty		back_consol
		ldy		#0
		sty		pause
		jmp no_consol

no_switch:
		sty		back_consol

		jmp		pause_loop

no_consol:
		jsr FlipToVideoDisplay
		mva		#$22 dmactl

		rts
lcnt	dta 0
.endp

;============================================================================
.proc FlipToTextDisplay
		sei
		; kill audio
		mva		#$a0 audc1
		;kill VBI
		mva		#0 nmien
		;turn ROM back on
		mva		#$ff portb
		
		; wait for the display to be finished
		lda		#248/2
		cmp:rne	vcount
		
		jsr		LogClear
		
		;reset display list
		mwa		#dlist_text dlistl
		lda		#0
		sta		prior
		sta		colbk
		mva		#$22 dmactl
		mva		#$e0 chbase
		rts
.endp

;=======================================================================
;	A = column
;	X = row
.nowarn .proc LogGotoxy
		;save new X
		pha
		
		;erase existing cursor
		ldy		log_curx
		lda		(log_curln),y
		eor		#$80
		sta		(log_curln),y
		
		;recompute line addr
		lda		pos_table,x
		asl
		sta		log_curln
		lda		#>[framebuf/2]
		rol
		sta		log_curln+1
		
		;update X and redraw cursor
		pla
		tay
		sty		log_curx
		lda		(log_curln),y
		eor		#$80
		sta		(log_curln),y
		rts
		
pos_table:
		:12 dta [#*40+(<framebuf)]/2
.endp

;=======================================================================
.nowarn .proc LogClear
		ldy		#0
		sty		log_lncnt
		tya
clearloop:
		sta		framebuf,y
		sta		framebuf+$0100,y
		sta		framebuf+$0200,y
		sta		framebuf+$0300,y
		iny
		bne		clearloop
		mwa		#framebuf log_curln
		bne		LogClearLine.xit
.endp

;=======================================================================
.nowarn .proc LogClearLine
		ldy		#39
		lda		#0
		sta:rpl	(log_curln),y-
xit:
		ldy		#2
		sty		log_curx
		lda		#$80
		sta		(log_curln),y
		rts
.endp

;=======================================================================
; Modified:
;	A, Y
;
; Preserved:
;	X
;
; Control codes handled:
;	$7D		clear
;	$9B		end of line
;	$9C		clear line
;
.nowarn .proc LogPutChar
		;check for EOL
		cmp		#$9b
		bne		not_eol
		ldy		log_curx
		lda		#0
		sta		(log_curln),y
		beq		wrap
not_eol:
		;check for clear
		cmp		#$7d
		beq		LogClear
		
		;check for clear line
		cmp		#$9c
		beq		LogClearLine
		
		;convert ATASCII to INTERNAL
		pha
		rol
		rol
		rol
		rol
		and		#$03
		tay
		pla
		eor		conv_tab,y

		ldy		log_curx
		sta		(log_curln),y
		iny
		cpy		#40
		bcs		wrap
		sty		log_curx
done:
		lda		#$80
		sta		(log_curln),y
		rts
wrap:
		ldy		#2
		sty		log_curx
		lda		log_curln
		adc		#39
		sta		log_curln
		bcc		done
		inc		log_curln+1
		lda		log_curln+1
		cmp		#(>framebuf)+4
		bne		done
		
scroll:
		ldy		#0
		ldx		#>framebuf
scroll_loop_2:
		stx		scroll_loop+2
		stx		scroll_loop+5
scroll_loop:
		lda		framebuf+40,y
		sta		framebuf,y
		iny
		bne		scroll_loop
		inx
		cpx		#(>framebuf)+4
		bne		scroll_loop_2
		
		mva		#$d8 log_curln
		dec		log_curln+1
		jmp		done

conv_tab:
		dta		$40
		dta		$20
		dta		$60
		dta		$00
.endp

;=======================================================================
.nowarn .proc LogImprint
		pla
		sta		log_srcptr
		pla
		sta		log_srcptr+1
print_entry:
		jsr		LogPrintM1
		lda		log_srcptr+1
		pha
		lda		log_srcptr
		pha
		rts
.endp

;=======================================================================
LogPrint = LogPrintM1.alt_entry

.nowarn .proc LogPrintM1
		ldx		#0
prloop:
		inw		log_srcptr
prloop1:
		lda		(log_srcptr,x)
		beq		done
		jsr		LogPutChar
		jmp		prloop
alt_entry:
		ldx		#0
		beq		prloop1
done:
		rts
.endp

;=======================================================================
.nowarn .proc LogImprintf
		pla
		tax
		pla
		tay
		inx
		sne:iny
		txa
		jsr		LogPrintf
		lda		a0+1
		pha
		lda		a0
		pha
		rts	
.endp

;=======================================================================
.nowarn .proc LogPrintf
		sta		a0
		sty		a0+1
		ldy		#0
		sty		d0
		
charloop:
		lda		(a0),y
		beq		done
		inw		a0
		cmp		#'%'
		beq		special
escaped:
		jsr		LogPutChar
nextchar:
		ldy		#0
		beq		charloop

done:
		rts

special:
		inc		d0
		ldx		d0
		lda		(a0),y
		inw		a0
		cmp		 #'d'
		bne		notdec
		tya
		sty		a1
		ldy		#6
		sei
		sed
decloop1:
		rol		d0,x
		adc		a1
		sta		a1
		dey
		bne		decloop1
		sty		a1+1
		ldy		#2
decloop2:
		rol		d0,x
		adc		a1
		sta		a1
		rol		a1+1
		dey
		bne		decloop2
		cld
		cli
		lda		a1+1
		beq		decdigit2
		tax
		lda		_hexdig,x
		jsr		putbyte
		lda		a1
		jmp		puthex_nextchar
		
decdigit2:
		lda		a1
		cmp		#$10
		bcc		decdigit1
		jmp		puthex_nextchar
		
decdigit1:
		tax
		lda		_hexdig,x
		jmp		escaped

notdec:
		cmp		#'x'
		bne		nothex
		lda		d0,x
		jmp		puthex_nextchar

nothex:
		cmp		#'X'
		bne		nothexword
		inc		d0
		lda		d1,x
		jsr		puthex
		ldx		d0
		lda		d0-1,x
		jmp		puthex_nextchar

nothexword:
		cmp		 #'D'
		bne		notdecword

		ldy		#8
		lda		#0
		sta		a1
		sta		 a1+1
		sta		a2
		sei
		sed
decword1:
		rol		d1,x
		lda		a1
		adc		a1
		sta		a1
		lda		a1+1
		adc		a1+1
		sta		a1+1
		dey
		bne		decword1
	
		ldy		#8
decword2:
		rol		d0,x
		lda		a1
		adc		a1
		sta		a1
		lda		a1+1
		adc		a1+1
		sta		a1+1
		lda		a2
		adc		a2
		sta		a2
		dey
		bne		decword2
		cld
		cli
	
		tax
		beq		decword3
		jsr		puthexcond
		
		lda		a1+1
		jsr		puthex
		lda		a1
		jmp		puthex_nextchar

decword3:
		lda		a1+1
		beq		decword4
		jsr		puthexcond
		lda		a1
puthex_nextchar:
		jsr		puthex
		jmp		nextchar
	
decword4:
		lda		a1
		jsr		puthexcond
		jmp		nextchar

notdecword:
		cmp		 #'c'
		bne		notchar
		inc		d0
		lda		d0,x
		jsr		putbyte
strdone:
		jmp		nextchar
	
notchar:
		cmp		 #'s'
		bne		notstr
	
		inc		d0
		lda		d0,x
		sta		a1
		lda		d0+1,x
		sta		a1+1

strloop:
		ldy		#0
		lda		(a1),y
		beq		strdone
		jsr		LogPutChar
		inw		a1
		jmp		strloop
	
notstr:
		dec		d0
		jmp		escaped
	
puthexcond:
		cmp		#$10
		bcs		puthex
		tax
		lda		_hexdig,x
		jmp		putbyte

puthex:	
		pha
		lsr
		lsr
		lsr
		lsr
		tax
		lda		_hexdig,x
		jsr		putbyte
		pla
		and		#$0f
		tax
		lda		_hexdig,x
putbyte:
		tax
		jsr		LogPutChar
		txa
		rts
		
_hexdig:
		dta		'0123456789ABCDEF'
.endp


.proc 		graphics0
gr0    ldx #$00        ;zamkniecie IOCB #0
       lda #$0c        ;CLOSE
       jsr ?xcio
;
       lda #<ename
       sta icbufa,x
       lda #>ename
       sta icbufa+1,x
       lda #$0c        ;READ/WRITE
       sta icax1,x
       lda #$00
       sta icax2,x
       lda #$03        ;OPEN
?xcio  sta iccmd,x
       jmp jciomain
;
ename  .byte "E:",$9b

.endp
loopmode	.byte 0
goodcnt		.byte 0

		org		$4a00
zp_store:

;============================================================================
		org		$4b00
soundbuf:

;============================================================================
		org		$4c00
dlist:
		dta		$70
		dta		$70
		dta		$f0

.rept 32
		dta		$32,$12,$22
		dta		$12,$32,$02
.endr

dlist_wait:
		dta		$41,a(dlist)
		
dlist_init:
		dta		$4f,a(framebuf)
		dta		$41,a(dlist_init)
		
;============================================================================
		org		$4d00
dlist_paused:
		dta		$70
		dta		$70
		dta		$f0

		dta	$4F, a(pause_imaddr)
		:93 dta $0f
		dta	$4F, a(pause_imaddr2)
		:97 dta $0f
		dta		$41,a(dlist_paused)

;============================================================================
		org		$4f00
dlist_text:
		dta		$70
		dta		$70
		dta		$70
		dta		$42,a(framebuf)
		:23 dta	$02
		dta		$41,a(dlist_text)
		
		org		$5000
framebuf:

		run	main

	end
