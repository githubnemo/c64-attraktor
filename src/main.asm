* =$0801


!macro SetBorderColor .color {
	lda #.color
	sta $d020
}

//----------------------------------------------------------
//----------------------------------------------------------
//					Simple IRQ
//----------------------------------------------------------
//----------------------------------------------------------
* = $0900

			lda #$00
			sta $d020       ; init black border
			sta $d021       ; and black infill
			lda #$00
			jsr musicinit	; init music
			jsr $e544       ; clear screen routine
			sei             ; disable interrupts
			lda #<irq1
			sta $0314       ;
			lda #>irq1      ;
			sta $0315       ; fill interrupt table entry

            ; VIC-II can generate interrupts, these have to be enabled
            ; and, once on occurs, a the bit in the interrupt latch
            ; register ($d019) needs to be cleared.
            ;
            ; $d01a is the interrupt enable register - a bit in the
            ; first 4 bits will enable one of the 4 interrupts.
            ;
            ; here we will enable the 'reached certain raster line' (RST)
            ; interrupt. The raster line is stored in $d012 and $d011.
			asl $d019
			lda #$7b
			sta $dc0d
			lda #$81
			sta $d01a       ; write to VIC-II interrupt register
			lda #$1b
			sta $d011
			lda #$80
			sta $d012
			cli
this:	jmp this
//----------------------------------------------------------
irq1:
			asl $d019          ; clear latch bit of RST interrupt
			+SetBorderColor 2
			lda $d012
			sta timer
			jsr musicplay ; play music
			lda $d012
			sec
			sbc timer
			clc
			adc #$30
			cmp $0400          ; read first 'character' of screen memory
			bcc notbigger
			sta $0400
notbigger:
            +SetBorderColor 0
			pla
			tay
			pla
			tax
			pla
			rti ; restore Y, X, A and return from interrupt

timer
    !byte $00






* = $1000
musicinit:	jmp init
musicplay:	jmp play

!set hardrestartcounter=3

hardrestartindex:		;value to put into wave in hardrestartframes (from right to left)
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

!text "player and music by the syndrom!"


;zeropage-variables
;uses $30-$47 by default - if you have to split, insert new calculation inbetween (i=$xx)


!set i =$30
!set voice1pointer=i
!set i = i + 2
!set voice2pointer=i
!set i = i + 2
!set voice3pointer=i
!set i = i + 2
!set sound1pointer=i
!set i = i + 2
!set sound2pointer=i
!set i = i + 2
!set sound3pointer=i
!set i = i + 2
!set duration1=i
!set i = i + 1
!set duration2=i
!set i = i + 1
!set duration3=i
!set i = i + 1
!set sound1index=i
!set i = i + 1
!set note3=i
!set i = i + 1
!set sound2index=i
!set i = i + 1
!set note2=i
!set i = i + 1
!set sound3index=i
!set i = i + 1
!set pulsecontrol=i
!set i = i + 1
!set vibratopointer=i
!set i = i + 2
!set vibratoindex=i




; this code initializes the SID memory starting at $d400.
;
; SID has 3 configurable voices which are initialized here.
;
init:	ldy #$18		; clear the sid
		lda #$00
loop1:	sta $d400,y
		dey
		bpl loop1
		ldy #$0e
		ldx #$02        ; x = voice index
loop2:  lda pulseinit,x	; pulsehigh ???
		sta $d403,y
		lda waveinit,x
		sta $d404,y		; wave
		lda #$00
		sta $d405,y		; attack
		lda #$01
		sta duration1,x
		lda voiceinit,x
		sta voice1pointer,x
		lda voiceinit+3,x
		sta voice1pointer+3,x
		lda sidvalues,x
		sta $d416,x     ; set filter cutoff, resonance and mode / main volume
		tya
		sec
		sbc #$07
		tay
		dex
		bpl loop2
		rts

; initial pulse wave duty cycles for each voice
;
pulseinit:
!byte $08,$03,$03

; initial wave form for each voice
;
; bit  desc.
; 7    noise
; 6    pulse
; 5    sawtooth
; 4    triangle
; 3    test
; 2    ring modulation with voice N (1:3, 2:1, 3:1)
; 1    sync with voice N (1:3, 2:1, 3:1)
; 0    gate
;
waveinit:
!byte $08,$08,$08

voiceinit:
!word voice1
!word voice2
!word voice3

voiceloop:
!word voice1loop
!word voice2loop
!word voice3loop

; global filter and main volume config for
; register $d416, $d417 and $d418
;
; $d416: filter cutoff freq high byte (bits $d415:{3..0} are the low byte)
; $d417: filter resonance and routing config
;   7..4: filter resonance
;      3: external input into filter
;      2: voice 3 into filter?
;      1: voice 2 into filter?
;      0: voice 1 into filter?
; $d418: filter mode and main volume control
;      7: mute voice 3
;      6: high pass
;      5: band pass
;      4: low pass
;   3..0: main volume
sidvalues:
!byte $00,$f4,$1f

play:	ldx #$00
		dec duration1
		beq branch1
		lda duration1
		cmp #hardrestartcounter
		bcs branch2
		stx $d405
		stx $d406
		stx $d404
		jmp branch1109

branch1:ldy #$00			//voice1
		lda (voice1pointer),y
		sta sound1pointer
		iny
		lda (voice1pointer),y
		beq restartmusic
		sta sound1pointer+1
		iny
		lda (voice1pointer),y
		sta duration1
		lda voice1pointer
		clc
		adc #$03
		sta voice1pointer
		lda voice1pointer+1
		adc #$00
		sta voice1pointer+1
		ldy #$00
		lda (sound1pointer),y
		sta $d406
		sty $d405
		iny
		sty $d404
		sty sound1index
		jmp branch1109

restartmusic:	ldx #$02
loop3:		lda voiceloop,x
		sta voice1pointer,x
		lda voiceloop+3,x
		sta voice1pointer+3,x
		lda #$01
		sta duration1,x
		dex
		bpl loop3
		lda #$08
		sta $d404
		sta $d40b
		sta $d412
		rts

branch2:ldy sound1index
		lda (sound1pointer),y
		beq branch1109
		sta $d401
		iny
		lda (sound1pointer),y
		sta $d404
		iny
		sty sound1index
branch1109:
        dec duration3		//voice3
		beq branch111c
		lda duration3
		cmp #hardrestartcounter
		bcs branch1151
		stx $d405
		stx $d406
		stx $d404
		jmp branch1185
branch111c:
        ldy #$00
		lda (voice3pointer),y
		sta sound3pointer
		iny
		lda (voice3pointer),y
		sta sound3pointer+1
		iny
		lda (voice3pointer),y
		sta duration3
		iny
		lda (voice3pointer),y
		sta note3
		lda voice3pointer
		clc
		adc #$04
		sta voice3pointer
		lda voice3pointer+1
		adc #$00
		sta voice3pointer+1
		ldy #$00
		lda (sound3pointer),y
		sta $d414		//sr
		sty $d413		//ad
		iny
		sty $d412		//wave
		sty sound3index
		jmp branch1185

branch1151:	ldy sound3index
		lda (sound3pointer),y
		beq branch115e
		cmp #$ff
		bne branch1166
		jmp branch1185
branch115e:	iny
		lda (sound3pointer),y
		sta sound3index
		tay
		lda (sound3pointer),y
branch1166:	sta $d416		//filter
		iny
		lda (sound3pointer),y
		sta $d412		//wave
		iny
		lda (sound3pointer),y
		iny
		sty sound3index
		clc
		adc note3
		tay
		lda freqhi,y
		sta $d40f
		lda freqlo,y
		sta $d40e
branch1185:	dec duration2			//voice2
		beq branch1196
		lda duration2
		cmp #hardrestartcounter
		bcs branch11da
		stx $d405
		stx $d406
		stx $d404
		rts
branch1196:	ldy #$00
		lda (voice2pointer),y
		sta sound2pointer
		iny
		lda (voice2pointer),y
		sta sound2pointer+1
		iny
		lda (voice2pointer),y
		sta duration2
		iny
		lda (voice2pointer),y
		sta note2
		lda voice2pointer
		clc
		adc #$04
		sta voice2pointer
		lda voice2pointer+1
		adc #$00
		sta voice2pointer+1
		ldy #$00
		sty vibratoindex
		lda (sound2pointer),y
		sta $d40d		//sr
		sty $d40c		//ad
		iny
		sty $d40b		//wave
		lda (sound2pointer),y
		sta pulsecontrol
		iny
		lda (sound2pointer),y
		sta vibratopointer
		iny
		lda (sound2pointer),y
		sta vibratopointer+1
		iny
		sty sound2index
		rts
branch11da:	ldy sound2index
		lda (sound2pointer),y
		beq branch11e5
		cmp #$ff
		bne branch11ed
		rts
branch11e5:	iny
		lda(sound2pointer),y
		sta sound2index
		tay
		lda (sound2pointer),y


branch11ed:	sta $d40b		//wave
		lda pulsecontrol
		beq branch1200
		iny
		lda (sound2pointer),y
		sta $d409		//pulselow
		iny
		lda (sound2pointer),y
		sta $d40a		//pulsehigh
branch1200:	iny
		lda (sound2pointer),y
		iny
		sty sound2index
		clc
		adc note2
		tax
		lda freqlo,x
		ldy vibratoindex
		iny
		clc
		adc (vibratopointer),y
		sta $d407
		dey
		lda freqhi,x
		adc (vibratopointer),y
		sta $d408
		iny
		iny
		lda (vibratopointer),y
		cmp #$80
		beq branch122a
		sty vibratoindex
		rts
branch122a:	iny
		lda (vibratopointer),y
		sta vibratoindex
		rts
branch1230:	sta $d407		//obsolete ?
		lda freqhi,x
		sta $d408
		rts





freqlo:
!byte 	$0c,$1c,$2d,$3e,$47,$66,$7b,$91
!byte	$a9,$c3,$dd,$fa,$18,$38,$5a,$7d
!byte	$a3,$cc,$f6,$23,$53,$86,$bb,$f4
!byte	$30,$70,$b4,$fb,$47,$98,$ed,$47
!byte	$a7,$0c,$77,$e9,$61,$e1,$68,$f7
!byte	$8f,$30,$da,$8f,$4e,$18,$ef,$d2
!byte	$c3,$c3,$d1,$ef,$1f,$60,$b5,$1e
!byte	$9c,$31,$df,$a5,$87,$86,$a2,$df
!byte	$3e,$c1,$6b,$3c,$39,$63,$be,$4b
!byte	$0f,$0c,$45,$bf,$7d,$83,$d6,$79
!byte	$73,$c7,$7c,$97,$1e,$18,$8b,$7e
!byte	$fa,$06,$ac,$f3,$e6,$8f,$f8,$fc

freqhi:
!byte	$01,$01,$01,$01,$01,$01,$01,$01
!byte	$01,$01,$01,$01,$02,$02,$02,$02
!byte	$02,$02,$02,$03,$03,$03,$03,$03
!byte	$04,$04,$04,$04,$05,$05,$05,$06
!byte	$06,$07,$07,$07,$08,$08,$09,$09
!byte	$0a,$0b,$0b,$0c,$0d,$0e,$0e,$0f
!byte	$10,$11,$12,$13,$15,$16,$17,$19
!byte	$1a,$1c,$1d,$1f,$21,$23,$25,$27
!byte	$2a,$2c,$2f,$32,$35,$38,$3b,$3f
!byte	$43,$47,$4b,$4f,$54,$59,$5e,$64
!byte	$6a,$70,$77,$7e,$86,$8e,$96,$9f
!byte	$a8,$b3,$bd,$c8,$d4,$e1,$ee,$fd

//------------------------------------------------------------
//sounddata
//format voice1 (Drumtrack):
//.byte SR Value
//.byte Freqhi,wave
//.byte Freqhi,wave - if freqhi=0 -> end of sound

basedrum:					//basedrum
!byte $f7,$dd,$81,$0c,$11,$0a,$11,$08,$11,$06,$10,$03,$10,$00

snare:
!byte $f9,$fc,$81,$0e,$41,$5c,$81,$0d,$40,$80,$3c,$0a,$40,$3b,$80,$00

hihat:
!byte $84,$fe,$81,$d0,$80,$a0,$80,$00


//------------------------------------------------------------
//format voice2 (vibratotrack):
//first frame
//.byte SR Value
//.byte pulsecontrol    =$0 -> pulse off, other -> pulse on
//.word vibratooffset
//following frames
//.byte wave		=$0 -> next byte is loopindex, =$FF -> end
//.byte noteoffset
//if pulse = on
//.byte wave,pulselow,pulsehigh,noteoffset

silence02:					//silence
!byte $00,$00
!word novibrato
!byte $08,$00,$ff


chord:
!byte $6a,$01
!word novibrato
!byte $41,$00,$04,$00
!byte $41,$20,$04,$00
!byte $40,$40,$04,$00
!byte $40,$60,$04,$07
!byte $40,$80,$04,$07
!byte $40,$60,$04,$07
!byte $40,$40,$04,$0c
!byte $40,$20,$04,$0c
!byte $40,$00,$04,$0c
!byte $40,$00,$04,$00
!byte $40,$20,$04,$00
!byte $00,$0c

chord1:
!byte $6a,$01
!word novibrato
!byte $41,$00,$04,$00
!byte $41,$20,$04,$00
!byte $40,$40,$04,$00
!byte $40,$60,$04,$07
!byte $40,$80,$04,$07
!byte $40,$60,$04,$07
!byte $40,$40,$04,$0a
!byte $40,$20,$04,$0a
!byte $40,$00,$04,$0a
!byte $40,$00,$04,$00
!byte $40,$20,$04,$00
!byte $00,$0c

chord2:
!byte $6a,$01
!word novibrato
!byte $41,$00,$04,$02
!byte $41,$20,$04,$02
!byte $40,$40,$04,$02
!byte $40,$60,$04,$07
!byte $40,$80,$04,$07
!byte $40,$60,$04,$07
!byte $40,$40,$04,$0e
!byte $40,$20,$04,$0e
!byte $40,$00,$04,$0e
!byte $40,$00,$04,$02
!byte $40,$20,$04,$02
!byte $00,$0c




//------------------------------------------------------------
//format voice3 (filtertrack):
//first frame
//!byte SR Value
//following frames
//!byte Filterhigh,wave,noteoffset
//note: if filterhigh=$00, next byte is loopindex. if filterhigh=$ff ->end

silence03:					//silence
!byte $00
!byte $fe,$08,$00
!byte $ff

filterbass:
!byte $b9
!byte $f0,$41,$00
!byte $a0,$41,$00
!byte $50,$41,$00
!byte $20,$41,$00
!byte $18,$41,$01
!byte $14,$41,$00
!byte $10,$41,$00
!byte $0c,$40,$ff
!byte $08,$40,$00
!byte $ff




//------------------------------------------------------------
//vibratotable
//.byte addvalue-high,addvalue-low	if highbyte=$80 -> next byte=loopindex

novibrato:			//empty
!byte $00,$00,$80,$00



//------------------------------------------------------------
//musicdata

voice1:
voice1loop:

//format .word soundoffset, .byte duration   if soundoffset=0000 then loop


//simple rythm
//-------------
!word basedrum
!byte $0c
!word hihat
!byte $06
!word hihat
!byte $06
!word snare
!byte $0c
!word hihat
!byte $06
!word hihat
!byte $06

//simple rythm
//-------------
!word basedrum
!byte $0c
!word hihat
!byte $06
!word hihat
!byte $06
!word snare
!byte $0c
!word hihat
!byte $06
!word hihat
!byte $06

//simple rythm
//-------------
!word basedrum
!byte $0c
!word hihat
!byte $06
!word hihat
!byte $06
!word snare
!byte $0c
!word hihat
!byte $06
!word hihat
!byte $06

//simple rythm+doublesnare
//-------------
!word basedrum
!byte $0c
!word hihat
!byte $06
!word hihat
!byte $06
!word snare
!byte $06
!word snare
!byte $06
!word hihat
!byte $06
!word snare
!byte $06


!word $0000
//------------------------------------------------------------
voice2:
voice2loop:

//format .word soundoffset, .byte duration,note


!word silence02
!byte $0c,$00
!word chord
!byte $24,$34

!word chord1
!byte $30,$34

!word silence02
!byte $0c,$00
!word chord
!byte $24,$2d

!word chord2
!byte $30,$2d


//------------------------------------------------------------
voice3:
voice3loop:

//format .word soundoffset, .byte duration,note

!word filterbass
!byte $12,$1c
!word filterbass
!byte $12,$1c
!word filterbass
!byte $0c,$1a

!word filterbass
!byte $12,$17
!word silence03
!byte $1e,$00

!word filterbass
!byte $12,$15
!word filterbass
!byte $12,$15
!word filterbass
!byte $0c,$17

!word filterbass
!byte $12,$10
!word silence03
!byte $1e,$00




!eof

*=$0801

sysline:
    !byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00 ;= SYS 2061

* = $080d ;=2061

; set screencolor and border to black (0)

start

   lda#$00
   sta$d020
   sta$d021        ; black border + screen

   lda#$02
   sta$0400        ; draw B

   ; enable 'high-res' bitmap mode; this gives us 320x200 pixel (=64000)
   ; in graphics memory but only 40x25 (=1000) bytes for color.
   lda$d011        ; set BMM=1
   ora#0b00100000
   sta$d011
   lda$d016        ; unset MCM
   and#0b11101111
   sta$d016

   lda$d018
   ora#0b00001000
   sta$d018        ; move graphics to $2000 instead of $1000


   ; colors are defined for 8x8 pixels at once, upper nibble for 'on' pixels.
   ; for simplicity we'll fill all 40x25 byte with white for on-pixels and
   ; black for off-pixels.
   ldx#$00
   lda#0b01010000
colorfill_loop
   sta$0400,x
   sta$0500,x
   sta$0600,x
   sta$0700,x
   dex
   bne colorfill_loop


   ; overwrite all pixels with 0 to blank the screen
   ;
   ; there are 64000 pixels, 1 bit for each -> 8000 byte for the whole screen.
   ; therefore we have 8000/256 = 31.25 pages to fill, starting at $2000.
   ; $2000, $2100, ...
   ;
   ; since we only have 16 bit registers we can use zero-page adressing:
   ; we write $2000 (high byte $20 and low byte $00 respectively) to
   ; memory location $00fc and $00fb. Then we can do something like
   ; sta ($fb), y to set ($2000 + y) to the content of register A.
   ldx#32
   ldy#$00

   lda#$00
   sta$fb
   lda#$20
   sta$fc

   lda#0

clearscr_loop
   sta($fb),y
   dey
   bne clearscr_loop
   inc$fc
   ldy#$00
   dex
   bne clearscr_loop


; 320x200 resolution, 40x25 bytes, therefore 256/40=6.4 rows per page
;
; 0b0 0b1 0b2 0b3 ... 0b7     8b0 8b1 8b2 ... 8b7
; 1b0 1b1 1b2 ...             9b0 9b1 ...
; 2b0 ... ...                 Ab0 ...
; 3b0                         Bb0
; 4b0                         Cb0
; 5b0                         Db0
; 6b0                         Eb0
; 7b0                         Fb0
;
; when base addr. = $2000, then 0b0 is bit 0 at $2000, 0b1 is bit 1 at $2000.
;



!addr FP_A  = $C400
!addr FP_B  = $C430
!addr FP_C  = $C460

!addr FP_XCUR = $C4C0 ; 6 byte per float
!addr FP_YCUR = $C4F0
!addr FP_ZCUR = $C520

!addr FP_TEMP     = $C550
!addr FP_SCALE_Y  = $C560
!addr FP_OFFSET_X = $C570

!addr INT_X = $C600
!addr INT_Y = $C602

!addr SCREEN_ADDR = $C630

; LUT for ORing patterns
!addr SCREEN_MASK_0 = $C640
!addr SCREEN_MASK_1 = $C641
!addr SCREEN_MASK_2 = $C642
!addr SCREEN_MASK_3 = $C643
!addr SCREEN_MASK_4 = $C644
!addr SCREEN_MASK_5 = $C645
!addr SCREEN_MASK_6 = $C646
!addr SCREEN_MASK_7 = $C647


lda#0b00000001
sta SCREEN_MASK_0
lda#0b00000010
sta SCREEN_MASK_1
lda#0b00000100
sta SCREEN_MASK_2
lda#0b00001000
sta SCREEN_MASK_3
lda#0b00010000
sta SCREEN_MASK_4
lda#0b00100000
sta SCREEN_MASK_5
lda#0b01000000
sta SCREEN_MASK_6
lda#0b10000000
sta SCREEN_MASK_7


!macro lshift_16bit .hb, .lb {
    asl .lb
    rol .hb
}

!macro rshift_16bit .hb, .lb {
    lsr .hb
    ror .lb
}


; X is 16 bit (FC FB)
; Y is  8 bit (Y)
;lda#00
;sta$FC
;lda#$02
;sta$FB

;ldy#$0
;jsr blit_xy



;lda#00
;sta$FC
;lda#$04
;sta$FB
;ldy #0
;jsr blit_xy







; store FAC to RAM (X=Addr.LB, Y=Addr.HB)
!addr MOVMF = $BBD4
; load FAC from RAM (A=Addr.LB, Y=Addr.HB)
!addr MOVFM = $BBA2
; FAC to 16-bit signed int (Y=Addr.LB, A=Addr.HB)
!addr FACINX = $B1AA
; Add FAC + number in RAM (A=Addr.LB, Y=Addr.HB)
!addr FADD = $B867
; Subtract FAC - number in RAM (A=Addr.LB, Y=Addr.HB)
; FAC = Mem - FAC
!addr FSUB = $B850
; Divide number in RAM by FAC (A=Addr.LB, Y=Addr.HB)
!addr FDIV = $BB0F
; Multiply number from RAM * FAC (clobbers ARG, A=Addr.LB, Y=Addr.HB)
!addr FMULT = $BA28
; Convert 16-bit signed to float in FAC (Y=LB, A=HB)
!addr GIVAYF = $B391
; Copy ARG to FAC
!addr MOVEF = $BBFC
; Copy FAC to ARG
!addr MOVFA = $BC0F
; Subtract ARG from FAC1
; FAC = ARG - FAC
!addr FSUBT = $B853
; Convert FAC1 to 32 bit integer
!addr QINT = $BC9B


!macro set_int_param .name, .value {
    ldy#.value
    lda#0
    jsr GIVAYF
    ldx #< .name
    ldy #> .name
    jsr MOVMF
}

!macro float_to_fac1 .name {
    lda#< .name
    ldy#> .name
    jsr MOVFM
}

!macro fdiv .other {
    lda#< .other
    ldy#> .other
    jsr FDIV
}

!macro fsub .other {
    lda#< .other
    ldy#> .other
    jsr FSUB
}

!macro fadd .other {
    lda#< .other
    ldy#> .other
    jsr FADD
}

!macro fmult .other {
    lda#< .other
    ldy#> .other
    jsr FMULT
}

!macro movmf .other {
    ldx#< .other
    ldy#> .other
    jsr MOVMF
}

!macro fac1_to_int16 .location {
    jsr FACINX
    sty .location
    sta .location + 1
}





+set_int_param FP_XCUR, 2
+set_int_param FP_YCUR, 1
+set_int_param FP_ZCUR, 1

+set_int_param FP_A, 10
+set_int_param FP_B, 28
+set_int_param FP_C, 8

+set_int_param FP_SCALE_Y, 25
+set_int_param FP_OFFSET_X, 160

; initialize FP_C = 8/3
+set_int_param FP_TEMP, 3
lda#< FP_TEMP
ldy#> FP_TEMP
jsr MOVFM
lda#< FP_C
ldy#> FP_C
jsr FDIV
ldx#< FP_C
ldy#> FP_C
jsr MOVMF


; normally we would multiply delta-time (dt, e.g. 0.01)
; to our differential equation results to get the next
; X/Y/Z values. We can save a multiplication here if we
; assume that we approximate 0.01 by shifting the float
; exponent to the right.
;
; Some approximate values:
; - 1/(2**8) = 0.0039..
; - 1/(2**7) = 0.0078..
; - 1/(2**6) = 0.0156..
;
; So we just define the shift amount to set our dt.
!set dt_shift = 6

!macro multiply_dt_to_fac1 {
    clc
    lda $61
    sbc #dt_shift
    sta $61
}


; testing operation order of FSUB
;
;+set_int_param FP_TEMP, 100
;+float_to_fac1 FP_A
;lda #< FP_TEMP
;ldy #> FP_TEMP
;jsr FSUB
;+movmf FP_TEMP ; expect 90 in FP_TEMP




main
    lda #$20 - 5
    sta $FA
draw_loop
    jsr xyz_step
    lda INT_X
    sta $FB
    lda INT_X + 1
    sta $FC
    ldy INT_Y

    jsr blit_xy

    dec $FA
    ;bne draw_loop
    jmp draw_loop




hang
   jmp hang




xyz_step
    ; X_new = a * (Y_cur - X_cur)
    ; X_cur = X_cur + X_new * dt
    ;
    ; 1. Y_cur - X_cur
    +float_to_fac1 FP_XCUR
    lda#< FP_YCUR
    ldy#> FP_YCUR
    jsr FSUB
    ; 2. a * FAC1
    lda#< FP_A
    ldy#> FP_A
    jsr FMULT
    ; 3. FAC1 * dt
    +multiply_dt_to_fac1
    ; 4. FAC1 + X_cur
    lda#< FP_XCUR
    ldy#> FP_XCUR
    jsr FADD
    ; 4. X_cur = FAC1
    ldx#< FP_XCUR
    ldy#> FP_XCUR
    jsr MOVMF



    ; store int(fp_x) as X coordinate
    ;
    ; we're multiplying X_CUR by 8 but
    ; we don't really multiply, we just
    ; add 3 to the float's exponent.
    ;
    ; original code:
    ;!+set_int_param FP_SCALE_X, 8
    ;[...]
    ;lda #< FP_SCALE_X
    ;ldy #> FP_SCALE_X
    ;jsr FMULT
    clc
    lda #3
    adc $61
    sta $61
    lda #< FP_OFFSET_X
    ldy #> FP_OFFSET_X
    jsr FADD
    +fac1_to_int16 INT_X


    ; Y_new = X_cur * (b - Z_cur) - Y_cur
    ; Y_cur = Y_cur + Y_new * dt

    ; (b - Z_cur)
    +float_to_fac1 FP_ZCUR
    +fsub FP_B
    ; FAC1 * X_cur
    +fmult FP_XCUR
    ; FAC1 = ARG - FAC1
    ;  ARG = FAC1 (= X_cur * (b - Z_cur))
    ;  FAC1 = Y_cur
    jsr MOVFA                ; ARG = FAC1
    +float_to_fac1 FP_YCUR
    jsr FSUBT
    ; FAC1 * dt
    +multiply_dt_to_fac1
    ; FAC1 + Y_cur
    +fadd FP_YCUR
    ; Y_cur = FAC1
    +movmf FP_YCUR




    ; Z_new = X_cur * Y_cur - c * Z_cur
    ; Z_cur = Z_cur + Z_new * dt
    ;
    ; 1. temp=(c * Z_cur)
    +float_to_fac1 FP_C
    +fmult FP_ZCUR
    +movmf FP_TEMP
    ; 2. (X_cur * Y_cur)
    +float_to_fac1 FP_XCUR
    +fmult FP_YCUR
    ; 3. FAC1 - temp
    jsr MOVFA
    +float_to_fac1 FP_TEMP
    jsr FSUBT
    ; 4. FAC1 * dt
    +multiply_dt_to_fac1
    ; 5. FAC1 + Z_cur
    +fadd FP_ZCUR
    ; 6. Z_cur = FAC1
    +movmf FP_ZCUR



    ; compute Y addr.
    ; u = (y + z * 10)
    ; y_px = int((u * 25)) >> 6
    ;
    ; store int(y + z*10) as Y coordinate
    ; note that FAC1 contains Z_CUR at this point in time.
    +fmult FP_A ; XXX abuses the fact that A=10
    +fadd FP_YCUR
    +fmult FP_SCALE_Y
    ; normally we'd use +fac1_to_int16 but it assumes signed
    ; integer (which we don't expect) and therefore values >32k
    ; will break. instead we'll use QINT and take the lowest
    ; two bytes.
    jsr QINT
    lda $64 ; HB of 16 bit int
    ldy $65 ; LB of 16 bit int
    sta INT_Y+1
    sty INT_Y
    +rshift_16bit INT_Y+1, INT_Y
    +rshift_16bit INT_Y+1, INT_Y
    +rshift_16bit INT_Y+1, INT_Y
    +rshift_16bit INT_Y+1, INT_Y
    +rshift_16bit INT_Y+1, INT_Y
    +rshift_16bit INT_Y+1, INT_Y

    ; from here on we will only use INT_Y since the possible Y
    ; range is from 0 to 200 anyway.
    ; we need to invert it so the image is not flipped, though.
    clc
    lda #200
    sbc INT_Y
    sta INT_Y


    rts




blit_xy
    ; parameters: x (16 bit), y (8 bit)
    ; 0 <= x < 320, 0 <= y < 200
    ;
    ; assume x is in $FC $FB
    ; assume y is in Y
    ;
    ; clobbers SCREEN_ADDR global, FC/FB and FD/FE.

    ; this is a quest to resolve x/y coordinates into an
    ; screen buffer address. we're assuming 0x2000 as base
    ; address.
    ;
    ; since we have 40x25 byte (8 pixel each, giving 320x200 pixel)
    ; we have a global adressing (byte-level) and a local adressing
    ; (bit-level).

    ; assume x is in $FC $FB
    ; assume y is in Y

    ; intitialize addr. variable to 0x2000
    lda#$00
    sta SCREEN_ADDR
    lda#$20
    sta SCREEN_ADDR + 1

    ; compute pixel mask to OR on the region; this will set the pixel bit
    ; in the byte for which we're currently computing the address of.
_screen_mask
    lda $FB
    and #7
    eor #7
    tax

    ; we round the X offset to a power of 8 since we have
    ; 8 pixel for each adressable byte (pixels are bits, remember).
    ;
    ; addr = addr + (x & 0xF8)
    ;                ^^^^^^^^ -> x.LB = (x.LB & 8)
_x_shift
    clc
    lda $FB
    and #$F8
    adc SCREEN_ADDR
    sta SCREEN_ADDR
    lda $FC
    adc SCREEN_ADDR + 1
    sta SCREEN_ADDR + 1

_y_shift_global
    ;    yoff_row = (y >> 3) * 40 * 8
    ;    yoff_row = (y & 0xF8) * 40
    ;    u = y & 0xF8
    ;    y_off_row = u * 40
    ;    y_off_row = u * ((1 << 5) + (1 << 3))
    ;    y_off_row = (u << 5) + (u << 3)
    ;
    ; clear high byte of (FC,FB) and (FE,FD)
    lda#$0
    sta$fc
    sta$fe
    ; init low bytes to y * 0xF8 (u = y & 0xF8)
    tya
    and #$f8
    sta $fb
    sta $fd
    ; y1 = u << 5
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    ; y2 = u << 3
    +lshift_16bit $FE, $FD
    +lshift_16bit $FE, $FD
    +lshift_16bit $FE, $FD
    ; y_off_row = y1 + y2
    clc
    lda $fd
    adc $fb
    sta $fb
    lda $fe
    adc $fc
    sta $fc

    ; add y_off_row (FE/FD) to screen addr.
    clc
    lda SCREEN_ADDR
    adc $fb
    sta SCREEN_ADDR
    lda SCREEN_ADDR + 1
    adc $fc
    sta SCREEN_ADDR + 1

    ; add yoff_local to screen_addr
_y_shift_local
    clc
    tya
    and #7
    adc SCREEN_ADDR
    sta SCREEN_ADDR
    lda#0
    adc SCREEN_ADDR+1
    sta SCREEN_ADDR+1

    ; load addr., mask pattern, store again
    lda SCREEN_ADDR
    sta $FB
    lda SCREEN_ADDR+1
    sta $FC
    ldy #0
    lda ($FB), Y
    ora SCREEN_MASK_0, X
    sta ($FB), Y

    rts





; vim:ft=acme
