;*=$1000
*=$0801


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

;   lda#3
;   sta$d011
;   lda#$08
;   sta$d016
;   lda#$18
;   sta$d018

   ; colors are defined for 8x8 pixels at once, upper nibble for 'on' pixels
   ldx#$00
   lda#0b11110000
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
   ldy#$ff
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
!addr FP_DT = $C490

!addr FP_XCUR = $C4C0 ; 6 byte per float
!addr FP_YCUR = $C4F0
!addr FP_ZCUR = $C520

!addr FP_TEMP     = $C550
!addr FP_SCALE_Y  = $C560
!addr FP_OFFSET_X = $C570
!addr FP_SCALE_X  = $C580

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
lda#00
sta$FC
lda#$02
sta$FB

ldy#$0
jsr blit_xy



lda#00
sta$FC
lda#$04
sta$FB
ldy #0
jsr blit_xy







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
+set_int_param FP_DT, 1

+set_int_param FP_SCALE_Y, 25
+set_int_param FP_OFFSET_X, 160
+set_int_param FP_SCALE_X, 8 ; todo optimize by shifting exp. directly

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



; initialize FP_DT to 0.01
+set_int_param FP_TEMP, 100
lda#< FP_TEMP
ldy#> FP_TEMP
jsr MOVFM
lda#< FP_DT
ldy#> FP_DT
jsr FDIV
ldx#< FP_DT
ldy#> FP_DT
jsr MOVMF


; testing operation order of FSUB
;
;+set_int_param FP_TEMP, 100
;+float_to_fac1 FP_A
;lda #< FP_TEMP
;ldy #> FP_TEMP
;jsr FSUB
;+movmf FP_TEMP ; expect 90 in FP_TEMP



lda$214e
ora#0b1
sta$214e


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
    lda#< FP_DT
    ldy#> FP_DT
    jsr FMULT
    ; 4. FAC1 + X_cur
    lda#< FP_XCUR
    ldy#> FP_XCUR
    jsr FADD
    ; 4. X_cur = FAC1
    ldx#< FP_XCUR
    ldy#> FP_XCUR
    jsr MOVMF




    ; store int(fp_x) as X coordinate
    ;lda $61
    ;adc #3
    ;sta $61
    lda #< FP_SCALE_X
    ldy #> FP_SCALE_X
    jsr FMULT
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
    +fmult FP_DT
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
    +fmult FP_DT
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






;;
;; Example
;;
;
;*=$0801
;!byte $0c,$08,$b5,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00
;jmp main
;
;.hellotext
;    !scr "hello, world!",0
;    !set ofs = 14
;
;main
;    ldy #0
;
;hello
;    lda .hellotext,y
;    beq +
;    sta $400+ofs,y
;    lda #1
;    sta $d800+ofs,y
;    iny
;    jmp hello
;+
;    rts
;
; vim:ft=acme
