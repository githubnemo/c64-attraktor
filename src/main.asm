*=$0801

sysline:
    !byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00 ;= SYS 2061

* = $080d ;=2061

!macro SetBorderColor .color {
	lda #.color
	sta $d020
}


start
    +SetBorderColor 0
    sta$d021        ; infill color to black as well

    lda#$02
    sta$0400        ; draw 'B'

    ; setup sound
    jsr init_sid
    sei                ; disable interrupts
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
!set dt_shift = 7

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



lda #1
sta USE_FAST_MULT

jmp main

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
    +set_int_param FP_A, 5
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






!addr SID_MEMORY_START = $d400

!addr FREQ_LO_VOICE1 = $d400
!addr FREQ_HI_VOICE1 = $d401
!addr CONTROL_VOICE1 = $d404
!addr ATTACK_DUR_VOICE1 = $d405
!addr SUSTAIN_REL_VOICE1 = $d406
!addr WAV_DUTY_LO_VOICE1 = $d402
!addr WAV_DUTY_HI_VOICE1 = $d403

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

!addr FILTER_CUTOFF_HI = $d416
!addr FILTER_CUTOFF_LO = $d415


voiceinit
    !word voice1
    !word voice2
    !word voice3


voiceloop
    !word voice1loop
    !word voice2loop
    !word voice3loop


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

play_durations
play_duration_voice1
    !byte $01
play_duration_voice2
    !byte $01
play_duration_voice3
    !byte $01


off_duration
    !byte $00

vibrato_state
    !byte $00

; zero page variables for pointers to sounds
!addr voice1_ptr = $30
!addr voice2_ptr = $32
!addr voice3_ptr = $34

!addr sound_ptr = $36
!addr sound_idx = $38

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

    lda #$01
    sta play_durations,x    ; reset play durations as well in case of program
                            ; restarts without rebooting the machine

    lda voiceinit,x
    sta voice1_ptr,x
    lda voiceinit+3,x
    sta voice1_ptr+3,x

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

}


!zone play_sounds {
play_sounds

    ; we hijack this interrupt for checking if the user pressed R
    ; to reset the program. lazyness :)
    lda #0b11111011
    sta $DC00

    lda $DC01
    and #0b00000010
    bne .r_not_pressed

    jsr reset_screen

.r_not_pressed
    lda play_duration_voice1
    clc
    cmp #2
    bcs .play_sound         ; if play duration is >1 play that sound

    lda off_duration
    bne .off

    lda #0b11101111
    sta $DC00

    ; check if M key is pressed
    lda $DC01
    and #0b00010000
    bne .addr_no_m

    ; M key handler
    ; init playing of sound
    clc
    lda INT_M
    lsr
    lsr
    lsr
    adc #1
    sta play_duration_voice1
    sta off_duration
    jsr .play_sound_voice1

.play_sound
    dec play_duration_voice1

.addr_no_m
    ; check if N key is pressed
    lda $DC01
    and #0b10000000
    bne .no_key

    ; N key handler
    lda #1
    sta play_duration_voice2
    jsr .play_sound_voice2

    rts
.off
    lda #00
    sta CONTROL_VOICE1
    dec off_duration
    rts
.no_key
    lda #00
    sta CONTROL_VOICE2
    rts

.play_sound_voice1
    lda #$84
    sta SUSTAIN_REL_VOICE1
    lda #$0a
    sta FREQ_HI_VOICE1
    lda #$11
    sta CONTROL_VOICE1
    lda #0
    sta ATTACK_DUR_VOICE1
    rts

.play_sound_voice2
!set freq = $10
    lda #$84
    sta SUSTAIN_REL_VOICE2
    clc
    lda vibrato_state
    beq .sub
    lda #0
    sta vibrato_state
    lda #freq
    adc INT_M
    jmp .freq_modified
.sub
    lda #freq
    sbc INT_M
    lda #1
    sta vibrato_state
.freq_modified
    sta FREQ_HI_VOICE2
    lda #$11
    sta CONTROL_VOICE2
    lda #0
    sta ATTACK_DUR_VOICE2

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
    jsr play_sounds
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


; ------------------------------------------------------------
; instrument data
;
; format voice1 (Drumtrack):
; .byte SR Value
; .byte Freqhi,wave
; .byte Freqhi,wave - if freqhi=0 -> end of sound

basedrum:
!byte $f7,$dd,$81,$0c,$11,$0a,$11,$08,$11,$06,$10,$03,$10,$00

snare:
!byte $f9,$fc,$81,$0e,$41,$5c,$81,$0d,$40,$80,$3c,$0a,$40,$3b,$80,$00

hihat:
!byte $84,$fe,$81,$d0,$80,$a0,$80,$00


; ------------------------------------------------------------
; format voice2 (vibratotrack):
; first frame
; .byte SR Value
; .byte pulsecontrol    =$0 -> pulse off, other -> pulse on
; .word vibratooffset
; following frames
; .byte wave		=$0 -> next byte is loopindex, =$FF -> end
; .byte noteoffset
; if pulse = on
; .byte wave,pulselow,pulsehigh,noteoffset

silence_voice2
    !byte $00,$00
    !word novibrato
    !byte $08,$00,$ff

; ------------------------------------------------------------
; format voice3 (filtertrack):
; first frame
; !byte SR Value
; following frames
; !byte Filterhigh,wave,noteoffset
; note: if filterhigh=$00, next byte is loopindex. if filterhigh=$ff ->end

silence_voice3
    !byte $00
    !byte $fe,$08,$00
    !byte $ff

filterbass
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

; ------------------------------------------------------------
; vibratotable
; .byte addvalue-high,addvalue-low	if highbyte=$80 -> next byte=loopindex

novibrato
    !byte $00,$00,$80,$00


; format .word soundoffset, .byte duration   if soundoffset=0000 then loop
;
voice1
voice1loop
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

    !word $0000 ;
    !byte $00   ; EOL

voice2
voice2loop
    !word silence_voice2
    !byte $0c,$00

voice3
voice3loop
    !word silence_voice3
    !byte $1e,$00




; vim:ft=acme
