*=$0801

sysline:
    !byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00 ;= SYS 2061

* = $080d ;=2061

!macro SetBorderColor .color {
	lda #.color
	sta $d020
}

!macro bzs .label {
    ; branch if zero flag set
    beq .label
}

!macro bzc .label {
    ; branch if zero flag clear
    bne .label
}

!macro dbgblt .addr, .val {
    ; usage:
    ; +dbgblt $200, $ff   draws a straight line in the first row
    lda #.val
    sta .addr + $2000
}


start
    +SetBorderColor 0
    sta$d021        ; infill color to black as well

    lda#$02
    sta$0400        ; draw 'B'

    sei                ; disable interrupts

    ; setup sound
    jsr init_sid

    lda #<vic_rst_irq
    sta $0314          ;
    lda #>vic_rst_irq  ;
    sta $0315          ; fill interrupt table entry for VIC-II RST interrupt
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
    cli             ; enable interrupts


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

   lda#$00

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

; ZERO PAGE LAYOUT
!addr KEY_PRESS_TIMER = $87
!addr SND_TURN_SOUND_ON = $89
!addr RNG_STATE_LO = $90
!addr RNG_STATE_HI = $91
!addr SND_TURNED_OFF = $92
!addr FOO1 = $93
!addr FOO2 = $94
!addr FREQ_LO_VOICE1_BUF = $95
!addr FREQ_HI_VOICE1_BUF = $96
!addr PLAY_COUNTER = $97


; MEMORY LAYOUT

; Sound output
!addr SID_MEMORY_START = $d400

!addr FREQ_LO_VOICE1 = $d400
!addr FREQ_HI_VOICE1 = $d401
!addr CONTROL_VOICE1 = $d404
!addr ATTACK_DECAY_VOICE1 = $d405
!addr SUSTAIN_RELEASE_VOICE1 = $d406
!addr WAV_DUTY_LO_VOICE1 = $d402
!addr WAV_DUTY_HI_VOICE1 = $d403

!addr FREQ_LO_VOICE2 = $d407
!addr FREQ_HI_VOICE2 = $d408
!addr WAV_DUTY_LO_VOICE2 = $d409
!addr WAV_DUTY_HI_VOICE2 = $d40a
!addr CONTROL_VOICE2 = $d40b
!addr ATTACK_DECAY_VOICE2 = $d40c
!addr SUSTAIN_RELEASE_VOICE2 = $d40d

!addr FREQ_LO_VOICE3 = $d40e
!addr FREQ_HI_VOICE3 = $d40f
!addr WAV_DUTY_LO_VOICE3 = $d410
!addr WAV_DUTY_HI_VOICE3 = $d411
!addr CONTROL_VOICE3 = $d412
!addr ATTACK_DECAY_VOICE3 = $d413
!addr SUSTAIN_RELEASE_VOICE3 = $d414

!addr FILTER_CUTOFF_HI = $d416
!addr FILTER_CUTOFF_LO = $d415

!addr SID_MAIN_CONTROL = $d418

; Attractor computation + drawing
!addr FP_A  = $C400
!addr FP_B  = $C430
!addr FP_C  = $C460

!addr FP_XCUR = $C4C0 ; 5 byte per float
!addr FP_YCUR = $C4F0
!addr FP_ZCUR = $C520

!addr FP_TEMP     = $C550
!addr FP_TEMP2    = $C555
!addr FP_10       = $C55A
!addr FP_SCALE_Y  = $C560
!addr FP_OFFSET_X = $C570

!addr INT_X = $C600
!addr INT_Y = $C602
!addr INT_M = $C604
!addr INT_X_DT = $C606
!addr INT_Y_DT = $C608
!addr INT_Z_DT = $C60A

!addr SCREEN_ADDR = $C630

; X offset where the attractor is visually split in two halves;
; used for implementing the left and right sound switching
!set X_SEPARATOR = 164

; LUT for ORing patterns
!addr SCREEN_MASK_OR_0 = $C640
!addr SCREEN_MASK_OR_1 = $C641
!addr SCREEN_MASK_OR_2 = $C642
!addr SCREEN_MASK_OR_3 = $C643
!addr SCREEN_MASK_OR_4 = $C644
!addr SCREEN_MASK_OR_5 = $C645
!addr SCREEN_MASK_OR_6 = $C646
!addr SCREEN_MASK_OR_7 = $C647


lda#0b00000001
sta SCREEN_MASK_OR_0
lda#0b00000010
sta SCREEN_MASK_OR_1
lda#0b00000100
sta SCREEN_MASK_OR_2
lda#0b00001000
sta SCREEN_MASK_OR_3
lda#0b00010000
sta SCREEN_MASK_OR_4
lda#0b00100000
sta SCREEN_MASK_OR_5
lda#0b01000000
sta SCREEN_MASK_OR_6
lda#0b10000000
sta SCREEN_MASK_OR_7

; LUT for ANDing patterns
!addr SCREEN_MASK_AND_0 = $C648
!addr SCREEN_MASK_AND_1 = $C649
!addr SCREEN_MASK_AND_2 = $C64A
!addr SCREEN_MASK_AND_3 = $C64B
!addr SCREEN_MASK_AND_4 = $C64C
!addr SCREEN_MASK_AND_5 = $C64D
!addr SCREEN_MASK_AND_6 = $C64E
!addr SCREEN_MASK_AND_7 = $C64F


lda#0b11111110
sta SCREEN_MASK_AND_0
lda#0b11111101
sta SCREEN_MASK_AND_1
lda#0b11111011
sta SCREEN_MASK_AND_2
lda#0b11110111
sta SCREEN_MASK_AND_3
lda#0b11101111
sta SCREEN_MASK_AND_4
lda#0b11011111
sta SCREEN_MASK_AND_5
lda#0b10111111
sta SCREEN_MASK_AND_6
lda#0b01111111
sta SCREEN_MASK_AND_7


jsr rng_seed


lda #0
sta KEY_PRESS_TIMER
sta SND_TURN_SOUND_ON



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
; Subtract FAC - number in RAM (A=Addr.LB, Y=Addr.HB) FAC = Mem - FAC
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
; Subtract ARG from FAC1 FAC = ARG - FAC
!addr FSUBT = $B853
; Convert FAC1 to 32 bit integer
!addr QINT = $BC9B
; Fill ARG with number from memory (A=Adr.LB, Y=Adr.HB). Then, in preparation
; for subsequent operations, compares the signs of ARG and FAC and writes the
; result to address $6F ($00: if signs are the same, $80: if signs are
; different), and loads the exponent from FAC to A (sets zero flag when
; FAC equals zero). The routines FADDT , FDIVT , FMULTT and FPWRT
; require this preparation.
!addr CONUPK = $BA8C


; Set to 1 to use fast mult.
!addr USE_FAST_MULT = $C800


; initialize float with addr .name to 16 bit int value .value
!macro set_int_param .name, .value {
    ldy#.value
    lda#0
    jsr GIVAYF
    ldx #< .name
    ldy #> .name
    jsr MOVMF
}

!macro mem_to_fac1 .name {
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
    pha
    lda #1
    cmp USE_FAST_MULT
    beq +
    pla
    jsr FMULT
    jmp ++
+
    pla
    jsr fast_mult
++
}

; store float in fac1 in .other
!macro movmf .other {
    ldx#< .other
    ldy#> .other
    jsr MOVMF
}

!macro fac1_to_mem .other {
    +movmf .other
}

!macro fac1_to_int16 .location {
    jsr FACINX
    sty .location
    sta .location + 1
}

; calculate .f1 = .f1 / .f2
!macro div2 .f1, .f2 {
    lda #<.f2
    ldy #>.f2
    jsr MOVFM
    lda #<.f1
    ldy #>.f1
    jsr FDIV
    ldx #<.f1
    ldy #>.f1
    jsr MOVMF
}

+set_int_param FP_XCUR, 2
+set_int_param FP_YCUR, 1
+set_int_param FP_ZCUR, 1

+set_int_param FP_A, 10
+set_int_param FP_B, 28
+set_int_param FP_C, 8

+set_int_param FP_SCALE_Y, 25
+set_int_param FP_OFFSET_X, 160
+set_int_param FP_10, 10

; initialize FP_C = 8/3
+set_int_param FP_TEMP, 3
; when doing fast mult we're going to sink into the first attractor
; due to error accumulation, so we need to shift C just a tiny bit
; so that we don't :)
lda USE_FAST_MULT
beq +
+set_int_param FP_TEMP2, 3
+div2 FP_TEMP2, FP_10
+mem_to_fac1 FP_TEMP
+fadd FP_TEMP2
+fac1_to_mem FP_TEMP
+
+div2 FP_C, FP_TEMP




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
    sec
    lda $61
    sbc #dt_shift
    sta $61
    clc
}

!macro save_int_gradient_to .target {
    ; save int(grad/8) to .target
    jsr MOVFA
    clc
    lda $61
    sbc #3
    sta $61
    +fac1_to_int16 .target
    jsr MOVEF
}

; testing operation order of FSUB
;
;+set_int_param FP_TEMP, 100
;+mem_to_fac1 FP_A
;lda #< FP_TEMP
;ldy #> FP_TEMP
;jsr FSUB
;+movmf FP_TEMP ; expect 90 in FP_TEMP



lda #0
sta USE_FAST_MULT

; Comment the following jump to reach the fast mult test drawing code.
jmp main



; ================== TEST CODE ====================

; testing fast float multiplication
;
; FP_A = 3.1415
; ['0x82', '0x49', '0xe', '0x56', '0x0']
lda #0x82
sta FP_A+0
lda #0x49
sta FP_A+1
lda #0x0e
sta FP_A+2
lda #0x56
sta FP_A+3
lda #0x00
sta FP_A+4
+mem_to_fac1 FP_A
; FP_B = -10
; ['0x84', '0xa0', '0x0', '0x0', '0x0']
lda #0x84
sta FP_B+0
lda #0xa0
sta FP_B+1
lda #0x00
sta FP_B+2
lda #0x00
sta FP_B+3
lda #0x00
sta FP_B+4
+fmult FP_B
+movmf FP_C


; code for multiplying 3.1415 and 100
; inspect $c48a for result
; expected for approx mult: 290.11199951171875
lda #$82
sta $c480
lda #$49
sta $c481
lda #$0e
sta $c482
lda #$56
sta $c483
lda #$00
sta $c484
+mem_to_fac1 $c480
lda #$87
sta $c485
lda #$48
sta $c486
lda #$00
sta $c487
lda #$00
sta $c488
lda #$00
sta $c489
+fmult $c485
+movmf $c48a

; code for multiplying 3.1415 and 1000
; inspect $c48a for result
; expected for approx mult: 3120.89599609375
lda #$82
sta $c480
lda #$49
sta $c481
lda #$0e
sta $c482
lda #$56
sta $c483
lda #$00
sta $c484
+mem_to_fac1 $c480
lda #$8a
sta $c485
lda #$7a
sta $c486
lda #$00
sta $c487
lda #$00
sta $c488
lda #$00
sta $c489
+fmult $c485
+movmf $c48a


; plot y = FP_A * x + FP_B
; for x in [0; 100]
!macro drawloop {
    ; plot y = FP_A / FP_TEMP * x + FP_C / FP_TEMP
    +set_int_param FP_XCUR, 0
    +set_int_param FP_B, 40
    +set_int_param FP_A, 3
    +set_int_param FP_TEMP, 2
    +div2 FP_A, FP_TEMP
    ; set sign of FP_A
    ;lda #$ff
    ;sta $66
    ;+movmf FP_A
    +set_int_param FP_C, 5
    +set_int_param FP_TEMP, 10
    +div2 FP_C, FP_TEMP

-
    +mem_to_fac1 FP_A
    +fmult FP_XCUR
    +fadd FP_B
    +movmf FP_YCUR

    +fac1_to_int16 INT_Y

    ; increase x by step
    +mem_to_fac1 FP_XCUR
    +fadd FP_C
    +movmf FP_XCUR
    +fac1_to_int16 INT_X

    lda INT_X
    sta $FB
    lda INT_X + 1
    sta $FC
    ldy INT_Y
    jsr blit_xy

    lda INT_X
    cmp #100
    bcc -
}

lda #0
sta USE_FAST_MULT
+drawloop
lda #1
sta USE_FAST_MULT
+drawloop

jmp hang


; ==================================
; End of fast mult test drawing code
; ==================================



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

    jsr rng_next
    jsr clear_rng_pixel
    jsr rng_next
    jsr clear_rng_pixel

    dec $FA
    ;bne draw_loop
    jmp draw_loop



hang
   jmp hang


!zone fast_mult {
fast_mult
    ; similar interface to FMULT:
    ; Multiplies a number from RAM and FAC (clobbers ARG, A=Addr.LB, Y=Addr.HB)
    ;
    ; ---
    ; FAC1
    ;  exponent is in $61
    ;  mantissa is in $62 $63 $64 $65
    ;  sign is in $66
    ;  possibly clear $70
    ; ARG
    ;  exponent is in $69
    ;  mantissa is in $6A $6B $6C $6D
    ;  sign is in $6E (0 for positive, $FF (-1) for negative)
    ;
    pha
    ; load the 2nd number to ARG using CONUPK
    jsr CONUPK

    ; If one of the exponents is zero, we can return early as the
    ; result will be zero.
    lda $61
    beq +
    lda $69
    bne ++
+
    ; Store zero exponent and keep mantissa intact as it will be
    ; ignored with a zero exponent.
    sta $61
    pla
    clc
    rts
++

    ; Multi-byte addition of the mantissas of ARG and FAC1 from least
    ; to most significant to utilize the carry bit.
    clc
    lda $65
    adc $6D
    sta $65

    lda $64
    adc $6C
    sta $64

    lda $63
    adc $6B
    sta $63

    ; Both bytes will have the MSB set for normalization, so it will
    ; always overflow, no matter if that is 'necessary' or not. Therefore
    ; we will determine the overflow by masking the MSB and checking bit 7.
    lda $62
    and #$7f
    sta $fb
    lda $6a
    and #$7f
    adc $fb
    ; if the MSB is set (i.e. not "positive") we know that we overflowed
    ; to the 2**-1 (0.5) position, i.e. we're adding another 0.5 (we're
    ; always adding 0.5, so that makes 0.5+0.5=1). Therefore we need to
    ; set bit 0 of the exponent (2**0=1) in that case (i.e., set the carry).
    clc
    bpl +
    sec
+
    ; make sure that the MSB mantissa bit is always 1 (normalization)
    ora #$80
    sta $62

    ; This adds the two exponents, they'll most likely overflow.
    ; we'll correct this by subtracting 128+ from the exponent below.
    ; We're also adding the carry from the mantissa if there's one
    ; implicitly (since the carry flag will be set if needed).
    lda $61
    adc $69
    sta $61

    ; Check if the addition carried over to the high bit. If it did not carry
    ; over, we suspect that the exponent is < 128 which means that if we
    ; subtract 128, we're underflowing the exponent value. In that case we
    ; should simply set the exponent to 0 since it is the smallest value
    ; we can use in a case of an underflow (2**(130 - 128)
    bcs .no_underflow

    ; to make sure, let's check if the 8 bit portion is also < 128, then
    ; we're sure there's an underflow.
    lda #128
    cmp $61  ; C = A >= M = 128 >= M
    bcc .no_underflow; no underflow, exponent is >= 129
.underflow
    lda #0
    sta $61
    jmp .fi__
.no_underflow
    ; subtract 129 from the added exponents
    lda $61
    sec
    sbc #129
    sta $61
.fi__

    ; sign bit handling; XOR sign byte
    lda $66
    eor $6e
    sta $66

    lda #0
    sta $70

    pla
    clc
    rts
}


!zone xyz_step {
xyz_step
    ; X_new = a * (Y_cur - X_cur)
    ; X_cur = X_cur + X_new * dt
    ;
    ; 1. Y_cur - X_cur
    +mem_to_fac1 FP_XCUR
    lda#< FP_YCUR
    ldy#> FP_YCUR
    jsr FSUB
    ; 2. a * FAC1
    lda#< FP_A
    ldy#> FP_A
    ;jsr FMULT
    jsr fast_mult
    +save_int_gradient_to INT_X_DT
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
    +mem_to_fac1 FP_ZCUR
    +fsub FP_B
    ; FAC1 * X_cur
    +fmult FP_XCUR
    ; FAC1 = ARG - FAC1
    ;  ARG = FAC1 (= X_cur * (b - Z_cur))
    ;  FAC1 = Y_cur
    jsr MOVFA                ; ARG = FAC1
    +mem_to_fac1 FP_YCUR
    jsr FSUBT
    +save_int_gradient_to INT_Y_DT
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
    +mem_to_fac1 FP_C
    +fmult FP_ZCUR
    +movmf FP_TEMP
    ; 2. (X_cur * Y_cur)
    +mem_to_fac1 FP_XCUR
    +fmult FP_YCUR
    ; 3. FAC1 - temp
    jsr MOVFA
    +mem_to_fac1 FP_TEMP
    jsr FSUBT
    +save_int_gradient_to INT_Z_DT
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
    +fmult FP_10
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

    ; compute l2 norm L2(int(dx), int(dy)+2int(dz)) to get the current
    ; curve's magnitude. note that we only scale dz by 2 as simulations
    ; deemed this sufficient and safes us some computation.
    ;
    ; * we use an L2 approximation (max(x,y) + 1/2*min(x,y)) to avoid
    ;   sqrt and squaring :)
    ; * we scale both dx and dy+2dz by 1/4 to get a good value range
    ;   for our purporses (math. correctness to hell)
    ;
    ;
    lda INT_Y_DT
    clc
    adc INT_Z_DT
    adc INT_Z_DT
    sta INT_M     ; INT_M = A = dy + 2dz
    cmp INT_X_DT

    bcs .x_is_max_y_is_min
    jmp .y_is_max_x_is_min

.x_is_max_y_is_min
    lda INT_M
    lsr           ;
    lsr           ; scale dy+2dz value by 4 (we do x/4 later)
    lsr           ; scale INT_M by 2 since it contains the minimum
    sta INT_M
    lda INT_X_DT
    lsr
    lsr
    clc
    adc INT_M     ; INT_M = dx/4 (max) + ((dy + 2dz)/4)/2
    sta INT_M
    jmp .l2_ready

.y_is_max_x_is_min
    lda INT_M
    lsr
    lsr           ; INT_M = (dy+2dz)/4 (max)
    sta INT_M
    lda INT_X_DT
    lsr
    lsr           ; scale dx by 4
    lsr           ; scale dx by 2 because its the minimum
    clc
    adc INT_M
    sta INT_M

.l2_ready
    rts
}


!zone rng {
    ; linear congruential generator
    ; X(n+1) = (a * X(n) + c) mod m
    ; a = 5
    ; c = 1
    ; m = 65536 (2**16)
rng_seed
    ; read current horizontal scanline position from VIC-II
    lda $d012
    sta RNG_STATE_LO
    lda #$00
    sta RNG_STATE_HI
    rts

rng_next
    ; X(n+1) = (5 * X(n) + 1) mod 65536
    ; compute 5 * X(n) as (4 + 1) * X(n)
    lda RNG_STATE_LO
    sta $fb
    lda RNG_STATE_HI
    sta $fc

    ; X(n) << 2
    +lshift_16bit $fc, $fb
    +lshift_16bit $fc, $fb

    ; add X(n)
    clc
    lda RNG_STATE_LO
    adc $fb
    sta RNG_STATE_LO
    lda RNG_STATE_HI
    adc $fc
    sta RNG_STATE_HI

    ; add 1
    clc
    lda RNG_STATE_LO
    adc #1
    sta RNG_STATE_LO
    lda RNG_STATE_HI
    adc #0
    sta RNG_STATE_HI

    rts

}


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
    ora SCREEN_MASK_OR_0, X
    sta ($FB), Y

    rts



clear_rng_pixel
    ; parameters: RNG_STATE_LO, RNG_STATE_HI as linear screen memory offset
    ;
    ; clobbers SCREEN_ADDR global, FC/FB and FD/FE.


    ; load rng state into screen addr
    lda RNG_STATE_LO
    sta SCREEN_ADDR
    lda RNG_STATE_HI
    sta SCREEN_ADDR+1
    ; divide by 8 to convert from pixel offset to byte offset
    lsr SCREEN_ADDR+1
    ror SCREEN_ADDR
    lsr SCREEN_ADDR+1
    ror SCREEN_ADDR
    lsr SCREEN_ADDR+1
    ror SCREEN_ADDR
    ; add baseline screen address 0x2000
    lda #$20
    adc SCREEN_ADDR+1
    sta SCREEN_ADDR+1

    lda RNG_STATE_LO
    and #7
    tax

    ; load addr., mask pattern, store again
    lda SCREEN_ADDR
    sta $FB
    lda SCREEN_ADDR+1
    sta $FC

    ldy #0
    lda ($FB), Y
    and SCREEN_MASK_AND_0, X
    sta ($FB), Y

    rts




; initial pulse wave duty cycles for each voice
;
init_values_pulse
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
init_values_wave
    !byte $08,$08,$08



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
init_values_sid
    !byte $00,$f4,$1f


; this code initializes the SID memory starting at $d400.
;
; SID has 3 configurable voices which are initialized here.
;
!zone init_sid {
init_sid
    lda #1
    sta SND_TURNED_OFF

    ldy #$18
    lda #$00
.loop1
    sta SID_MEMORY_START,y
    dey
    bpl .loop1      ; clear the SID memory with zeroes

    ; populate voice settings with sensible values
    ;
    ldy #$0e        ; y = voice offset in SID memory
    ldx #$02        ; x = voice index
.loop2
    lda init_values_pulse,x
    sta WAV_DUTY_HI_VOICE1,y

    lda init_values_wave,x
    sta CONTROL_VOICE1,y

    lda #$00
    sta ATTACK_DUR_VOICE1,y

    lda init_values_sid,x
    sta FILTER_CUTOFF_HI,x  ; set filter cutoff, resonance and mode / main volume
                            ; abuses x for writing several bytes but does not
                            ; depend on a voice (is a global setting)
    tya
    sec
    sbc #$07
    tay
    dex
    bpl .loop2


    ; TEST CODE PLS REMOVE THX

    lda #0b01000001
    sta CONTROL_VOICE1

    ldy #29   ; D + 35c
    lda freqlo,y
    sta FREQ_LO_VOICE1
    lda freqhi,y
    sta FREQ_HI_VOICE1

    ; wav duty is 12 bit =)
    ; fun fact: half of 2^12 is 2^11!
    lda #$ff
    sta WAV_DUTY_LO_VOICE1
    lda #7
    sta WAV_DUTY_HI_VOICE1

    lda #$00
    sta ATTACK_DECAY_VOICE1
    lda #$f0
    sta SUSTAIN_RELEASE_VOICE1

    ; setup voice 2
    lda #0b01000001
    sta CONTROL_VOICE2

    lda #$ff
    sta WAV_DUTY_LO_VOICE2
    lda #7
    sta WAV_DUTY_HI_VOICE2

    lda #$00
    sta ATTACK_DECAY_VOICE2
    lda #$f0
    sta SUSTAIN_RELEASE_VOICE2

    ; setup voice 3
    lda #0b01000001
    sta CONTROL_VOICE3

    lda #$ff
    sta WAV_DUTY_LO_VOICE3
    lda #7
    sta WAV_DUTY_HI_VOICE3

    lda #$00
    sta ATTACK_DECAY_VOICE3
    lda #$f0
    sta SUSTAIN_RELEASE_VOICE3

    lda #0
    sta FOO1
    lda #0
    sta FOO2






    rts
}


; sub-routine to clear the screen and reset the state of the drawing
!zone reset_screen {
reset_screen

    +set_int_param FP_XCUR, 2
    +set_int_param FP_YCUR, 1
    +set_int_param FP_ZCUR, 1

    ldx#32
    ldy#$00

    lda#$00
    sta$fb
    lda#$20
    sta$fc

    lda#0

.clearscr_loop
    sta($fb),y
    dey
    bne .clearscr_loop
    inc$fc
    ldy#$00
    dex
    bne .clearscr_loop

    rts
}

;
;    		CIA 1 Port B ($DC01) 	Joy 2
;   		PB7 	PB6 	PB5 	PB4 	PB3 	PB2 	PB1 	PB0
;   CIA1
;
;   Port A
;   ($DC00)
;   PA7 	STOP 	Q 	C= 	SPACE 	2 	CTRL 	<- 	1
;   PA6 	/ 	^ 	= 	RSHIFT 	HOME 	    ; 	* 	£
;   PA5 	, 	@ 	: 	. 	- 	L 	P 	    +
;   PA4 	N 	O 	K 	M 	0 	J 	I 	    9 	Fire
;   PA3 	V 	U 	H 	B 	8 	G 	Y 	    7 	Right
;   PA2 	X 	T 	F 	C 	6 	D 	R 	    5 	Left
;   PA1 	LSHIFT 	E 	S 	Z 	4 	A 	    W 	3 	Down
;   PA0 	CRSRDN 	F5 	F3 	F1 	F7 	CRSRRT 	RETURN 	DELETE 	Up
;   Joy 1 					Fire 	Right 	Left 	Down 	Up
;
;   https://www.c64-wiki.com/wiki/Keyboard

!zone handle_key_presses {

!macro set_key_press_timer {
    lda #10  ; number of ISR invocations to wait between key press checks
    sta KEY_PRESS_TIMER
}

handle_key_presses
    ; we wait a fixed amount of ISR invocations between key presses
    ; to debounce.
    lda KEY_PRESS_TIMER
    beq .no_wait

    dec KEY_PRESS_TIMER
    rts

.no_wait

    ; default is to turn sound off after reading the keys.
    lda #0
    sta SND_TURN_SOUND_ON

    ; 'Q' key press handler
    ;
    ; activate sequence 1 for left
    lda #0b01111111
    sta $DC00

    lda $DC01
    and #0b01000000
    bne .q_not_pressed

    +set_key_press_timer

    ; TODO handle q
    lda #1
    sta SND_TURN_SOUND_ON

.q_not_pressed

    ; 'A' key press handler
    ;
    ; activate sequence 2 for left
    lda #0b11111101
    sta $DC00

    lda $DC01
    and #0b00000100
    bne .a_not_pressed

    +set_key_press_timer

    ; TODO handle a
    lda #1
    sta SND_TURN_SOUND_ON

.a_not_pressed

    ; 'Z' key press handler

    lda #0b11111101
    sta $DC00

    lda $DC01
    and #0b00010000
    bne .z_not_pressed

    +set_key_press_timer

    ; TODO handle z
    lda #1
    sta SND_TURN_SOUND_ON

.z_not_pressed

    ; we now know if we need to toggle sound on or not.
    ; let's do that now!
    ;
    ; if SND_TURN_SOUND_ON and SND_TURNED_OFF:
    ;   SND_TURNED_OFF = False
    ;
    ; if not SND_TURN_SOUND_ON and not SND_TURNED_OFF:
    ;   SND_TURNED_OFF = True
    +dbgblt $200, $0
    lda SND_TURN_SOUND_ON
    +bzs +
    lda SND_TURNED_OFF
    +bzs ++ ; SND_TURNED_OFF must be 1, therefore we bail if zero is set
    +dbgblt $200, $ff
    lda #0
    sta SND_TURNED_OFF
    jmp ++
+
    ; SND_TURN_SOUND_ON == 0, we just need to check if
    ; SND_TURNED_OFF is already set
    lda SND_TURNED_OFF
    +bzc ++ ; SND_TURNED_OFF must be 0, therefore we bail if zero is clear
    +dbgblt $200, 1
    lda #1
    sta SND_TURNED_OFF
++



    ; 'R' key press handler
    ;
    ; we hijack this interrupt for checking if the user pressed R
    ; to reset the program. lazyness :)
    lda #0b11111011
    sta $DC00

    lda $DC01
    and #0b00000010
    bne .r_not_pressed

    +set_key_press_timer
    jsr reset_screen

.r_not_pressed
    lda #0b11111011

    ; 'F' key press handler
    ;
    sta $DC00
    lda $DC01
    and #0b00100000
    bne .f_not_pressed

    +set_key_press_timer
    ; Toggle fast multiplication when F is pressed
    lda USE_FAST_MULT
    bne +
    lda #1
    sta USE_FAST_MULT
    jmp ++
+
    lda #0
    sta USE_FAST_MULT
++

.f_not_pressed

    rts
}


vic_rst_irq
    asl $d019          ; clear latch bit of RST interrupt
    +SetBorderColor 2

    ; call musicplay while saving the raster line before the call
    ; and subtracting the current raster line after the call to get
    ; the elapsed time (in elapsed raster lines) to plot it on the
    ; screen. as an additional effect we also change the background
    ; color for the 'processing' lines.
    lda $d012
    sta timer
;    jsr play_sounds
    jsr handle_key_presses
    jsr musicplay
    lda $d012
    sec
    sbc timer
    clc
    adc #$30
    cmp $0401          ; read first 'character' of screen memory
    bcc notbigger
    sta $0401
notbigger
    +SetBorderColor 0
    pla
    tay
    pla
    tax
    pla
    rti ; restore Y, X, A and return from interrupt

timer
    !byte 0





musicplay:	jmp play

!set hardrestartcounter=3

hardrestartindex:		;value to put into wave in hardrestartframes (from right to left)
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0







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

!addr FREQ_LO_VOICE1 = $d400
!addr FREQ_HI_VOICE1 = $d401
!addr CONTROL_VOICE1 = $d404
!addr ATTACK_DUR_VOICE1 = $d405
!addr SUSTAIN_REL_VOICE1 = $d406

!addr FREQ_LO_VOICE2 = $d407
!addr FREQ_HI_VOICE2 = $d408
!addr CONTROL_VOICE2 = $d40b
!addr ATTACK_DUR_VOICE2 = $d40c
!addr SUSTAIN_REL_VOICE2 = $d40d

!addr FREQ_LO_VOICE3 = $d40e
!addr FREQ_HI_VOICE3 = $d40f
!addr CONTROL_VOICE3 = $d412
!addr ATTACK_DUR_VOICE3 = $d413
!addr SUSTAIN_REL_VOICE3 = $d414





!zone play {


!zone play_sub {
!addr ACCU = $dc
!addr P0 = $de
!addr P2 = FP_YCUR
!addr P3 = FP_YCUR+1

!addr TEST = $C655

mult_subroutine:
    ;lda #<FREQ_LO_VOICE1
    lda #<FILTER_CUTOFF_LO
    sta $de
    ;lda #>FREQ_HI_VOICE1
    lda #>FILTER_CUTOFF_LO
    sta $df

    lda $d417
    ora #0b11110001
    sta $d417

    !src "mult_subroutine.asm"
}


play:

    lda FP_YCUR+0
    sta $2010
    lda FP_YCUR+1
    sta $2012
    lda FP_YCUR+2
    sta $2014

    jsr mult_subroutine

    rts


    ; filter cutoff; sounds nice!
    lda $d417
    ora #0b11110001
    sta $d417

    lda FP_YCUR+1
    eor #0b10000000
    and #$0f
    asl
    asl
    asl
    asl
    sta FILTER_CUTOFF_LO
    lda FP_YCUR+1
    eor #0b10000000
    and #$f0
    lsr
    lsr
    lsr
    lsr
    sta FILTER_CUTOFF_HI
    rts




    rts


    ; filter cutoff; sounds nice!
    lda $d417
    ora #0b11110001
    sta $d417

    lda FP_YCUR+1
    eor #0b10000000
    and #$0f
    asl
    asl
    asl
    asl
    sta FILTER_CUTOFF_LO
    lda FP_YCUR+1
    eor #0b10000000
    and #$f0
    lsr
    lsr
    lsr
    lsr
    sta FILTER_CUTOFF_HI
    rts



    ; PWM sweep with attractor
    ; sounds nice
    lda FP_YCUR+1
    eor #0b10000000
    and #$0f
    asl
    asl
    asl
    asl
    sta WAV_DUTY_LO_VOICE1
    lda FP_YCUR+1
    eor #0b10000000
    and #$f0
    lsr
    lsr
    lsr
    lsr
    sta WAV_DUTY_HI_VOICE1

    rts


    lda FP_YCUR+1
    eor #0b10000000
    sta FREQ_HI_VOICE1
    lda FP_YCUR+2
    sta FREQ_LO_VOICE1

    lda FP_XCUR+1
    eor #0b10000000
    sta FREQ_HI_VOICE2
    lda FP_XCUR+2
    sta FREQ_LO_VOICE2

    lda FP_ZCUR+1
    eor #0b10000000
    sta FREQ_HI_VOICE3
    lda FP_ZCUR+2
    sta FREQ_LO_VOICE3

    rts


    ; arpeggio attempt

    ldy #29
    lda freqlo,y
    sta FREQ_LO_VOICE1_BUF
    lda freqhi,y
    sta FREQ_HI_VOICE1_BUF

    ; >>> Counter([n >> 5 for n in range(0, 256)])
    ; Counter({0: 32, 1: 32, 2: 32, 3: 32, 4: 32, 5: 32, 6: 32, 7: 32})
    lda PLAY_COUNTER
    lsr
    lsr
    lsr
    lsr
    lsr

    tay
    lda arpeggio,y
    clc
    adc #29
    tay
    sec
    lda freqlo,y
    sbc FREQ_LO_VOICE1_BUF
    sta FOO1
    lda freqhi,y
    sbc FREQ_HI_VOICE1_BUF
    sta FOO2


    ldy #29
    clc
    lda freqlo,y
    adc FOO1
    sta FREQ_LO_VOICE1_BUF
    lda freqhi,y
    adc FOO2
    sta FREQ_HI_VOICE1_BUF


    lda FREQ_LO_VOICE1_BUF
    sta FREQ_LO_VOICE1
    lda FREQ_HI_VOICE1_BUF
    sta FREQ_HI_VOICE1

    lda #16
    clc
    adc PLAY_COUNTER
    sta PLAY_COUNTER




    rts

    ; vibrato testing

    ldy #29
    lda freqlo,y
    sta FREQ_LO_VOICE1_BUF
    lda freqhi,y
    sta FREQ_HI_VOICE1_BUF

    lda PLAY_COUNTER
    cmp #127
    bcs .no_vibrato

    lda #29
    clc
    adc #7
    tay
    sec
    lda freqlo,y
    sbc FREQ_LO_VOICE1_BUF
    sta FOO1
    lda freqhi,y
    sbc FREQ_HI_VOICE1_BUF
    sta FOO2

    ldy #29
    clc
    lda freqlo,y
    adc FOO1
    sta FREQ_LO_VOICE1_BUF
    lda freqhi,y
    adc FOO2
    sta FREQ_HI_VOICE1_BUF
.no_vibrato

    lda FREQ_LO_VOICE1_BUF
    sta FREQ_LO_VOICE1
    lda FREQ_HI_VOICE1_BUF
    sta FREQ_HI_VOICE1

    lda #48
    clc
    adc PLAY_COUNTER
    sta PLAY_COUNTER


    rts

    lda FP_YCUR+1
    eor #0b10000000
    sta FREQ_HI_VOICE1
    lda FP_YCUR+2
    sta FREQ_LO_VOICE1
    lda FP_YCUR
    and #$0f
    asl
    asl
    asl
    asl
    sta WAV_DUTY_LO_VOICE1
    lda FP_YCUR
    and #$f0
    lsr
    lsr
    lsr
    lsr
    sta WAV_DUTY_HI_VOICE1

    rts

    lda FP_XCUR+1
    eor #0b10000000
    sta FREQ_HI_VOICE2
    lda FP_XCUR+2
    sta FREQ_LO_VOICE2
    lda FP_XCUR
    sta WAV_DUTY_LO_VOICE2
    lda #0
    sta WAV_DUTY_HI_VOICE2

    lda FP_ZCUR+1
    eor #0b10000000
    sta FREQ_HI_VOICE3
    lda FP_ZCUR+2
    sta FREQ_LO_VOICE3
    lda FP_ZCUR
    sta WAV_DUTY_LO_VOICE3
    lda #0
    sta WAV_DUTY_HI_VOICE3

    rts
}




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

arpeggio:
!byte   0,4,7,11,7,4,0,4
