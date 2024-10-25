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

   ; address of the bitmap is given by $d018 bit 4, usually $2000
;   lda#0b101
;   !for i, 24, 31 {
;    sta$2000+i
;   }
;

    ;lda#0b1000
    ;sta$2000

    ;lda#0b1000000
    ;sta$2008

    ;lda#0b100000
    ;sta$2078

    ;lda#0b100
    ;sta$2001

    ;lda#0b100000
    ;sta$2009

    ;lda#0b1000
    ;sta$2142



    ;; L
    ;lda$2142
    ;ora#0b1000
    ;sta$2142

    ;lda$2143
    ;ora#0b1000
    ;sta$2143

    ;lda$2144
    ;ora#0b1000
    ;sta$2144

    ;lda$2145
    ;ora#0b1000
    ;sta$2145

    ;lda$2146
    ;ora#0b1000
    ;sta$2146

    ;lda$2145
    ;ora#0b100
    ;sta$2145

    ;lda$2146
    ;ora#0b100
    ;sta$2146

    ;lda$2145
    ;ora#0b10
    ;sta$2145

    ;lda$2146
    ;ora#0b10
    ;sta$2146


    ;; O
    ;lda$214a
    ;ora#0b10000000
    ;sta$214a

    ;lda$214b
    ;ora#0b10000000
    ;sta$214b

    ;lda$214c
    ;ora#0b10000000
    ;sta$214c

    ;lda$214d
    ;ora#0b10000000
    ;sta$214d

    ;lda$214e
    ;ora#0b10000000
    ;sta$214e

    ;lda$214a
    ;ora#0b1000000
    ;sta$214a

    ;lda$214e
    ;ora#0b1000000
    ;sta$214e

    ;lda$214a
    ;ora#0b100000
    ;sta$214a

    ;lda$214e
    ;ora#0b100000
    ;sta$214e

    ;lda$214a
    ;ora#0b10000
    ;sta$214a

    ;lda$214b
    ;ora#0b10000
    ;sta$214b

    ;lda$214c
    ;ora#0b10000
    ;sta$214c

    ;lda$214d
    ;ora#0b10000
    ;sta$214d

    ;lda$214e
    ;ora#0b10000
    ;sta$214e



    ;; L
    ;lda$214a
    ;ora#0b10
    ;sta$214a

    ;lda$214b
    ;ora#0b10
    ;sta$214b

    ;lda$214c
    ;ora#0b10
    ;sta$214c

    ;lda$214d
    ;ora#0b10
    ;sta$214d

    ;lda$214e
    ;ora#0b10
    ;sta$214e

    ;lda$214d
    ;ora#0b1
    ;sta$214d

    ;lda$214e
    ;ora#0b1
    ;sta$214e

    ;lda$2155
    ;ora#0b10000000
    ;sta$2155

    ;lda$2156
    ;ora#0b10000000
    ;sta$2156



    ;;encode(0, 56)
    ;;block: 0, bit: 7
    ;;yoff local: 0, row: 2240
    ;lda$28c0
    ;ora#0b10000000
    ;sta$28c0





!addr FP_A  = $C400
!addr FP_B  = $C430
!addr FP_C  = $C460
!addr FP_DT = $C490

!addr FP_XCUR = $C4C0
!addr FP_YCUR = $C4F0
!addr FP_ZCUR = $C520

!addr FP_TEMP = $C550

!addr INT_X = $C600
!addr INT_Y = $C610
!addr INT_Z = $C620

!addr SCREEN_ADDR = $C630
!addr SCREEN_MASK = $C640


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
lda#$00
sta$FB

ldy#$22
jsr blit_xy








; FIXME remove this
jmp hang


lda#05
sta$FC
lda#$af
sta$FB
ldy#$0a
jsr blit_xy




blit_xy
    ; parameters: x (16 bit), y (8 bit)
    ; 0 <= x < 320, 0 <= y < 200
    ;
    ; assume x is in $FC $FB
    ; assume y is in Y
    ;
    ; clobbers SCREEN_ADDR and SCREEN_MASK global.

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
    sta SCREEN_MASK

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

    ; yoff_row = (y >> 3) * (5 << 6)
    ;
    ; t * (5 << 6) = (t*5)*(1<<6) = (t*4+t)*(1<<6) = (t+t/2**2)*(1<<8)
    ; since t = (y>>3) we have (y/2**3 + y/2**3 * 2**2)*(1<<8)
    ;         = ((y>>3) + (y>>1))*(1<<8)

    ; clear $FB/$FC since we already did all the necessary computation with
    ; the x values.
_y_shift_global
    lda#0
    sta$FB
    sta$FC
    ; $FB=y >> 3
    tya
    lsr
    lsr
    lsr
    sta$fb
    ; A=y >> 1
    tya
    lsr
    ; $FB = $FB + (y>>1)
    adc$fb
    sta$fb
    clc
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB
    +lshift_16bit $FC, $FB

    ; add yoff_row to screen_addr (0x2000)
    clc
    lda$fb
    adc SCREEN_ADDR
    lda$fc
    adc SCREEN_ADDR+1

    ; add yoff_local to screen_addr
_y_shift_local
    clc
    tya
    and #7
    adc SCREEN_ADDR
    lda#0
    adc SCREEN_ADDR+1

    ; load addr., mask pattern, store again
    lda SCREEN_ADDR
    sta $FB
    lda SCREEN_ADDR+1
    sta $FC
    ldy #0
    lda ($FB), Y
    ;ora SCREEN_MASK
    ora #0b1
    sta ($FB), Y

    rts






hang
   jmp hang
   rts
!eof


; store FAC to RAM (X=Addr.LB, Y=Addr.HB)
!addr MOVMF = $BBD4
; load FAC from RAM (A=Addr.LB, Y=Addr.HB)
!addr MOVFM = $BBA2
; FAC to 16-bit signed int (Y=Addr.LB, A=Addr.HB)
!addr FACINX = $B1AA
; Add FAC + number in RAM (A=Addr.LB, Y=Addr.HB)
!addr FADD = $B867
; Subtract FAC - number in RAM (A=Addr.LB, Y=Addr.HB)
!addr FSUB = $B850
; Divide number in RAM by FAC (A=Addr.LB, Y=Addr.HB)
!addr FDIV = $BB0F
; Multiply number from RAM * FAC (clobbers ARG, A=Addr.LB, Y=Addr.HB)
!addr FMULT = $BA28
; Convert 16-bit signed to float in FAC (Y=LB, A=HB)
!addr GIVAYF = $B391

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

!macro fac1_to_int .location {
    jsr FACINX
    sta > .location
    sty < .location
}




+set_int_param FP_XCUR, 2
+set_int_param FP_YCUR, 1
+set_int_param FP_ZCUR, 1

+set_int_param FP_A, 10
+set_int_param FP_B, 28
+set_int_param FP_C, 8

; initialize FP_C = 8/3
+set_int_param FP_TEMP, 3
lda#< FP_C
ldy#> FP_C
jsr MOVFM
lda#< FP_TEMP
ldy#> FP_TEMP
jsr FDIV
ldx#< FP_C
ldy#> FP_C
jsr MOVMF

; initialize FP_DT to 0.01
+set_int_param FP_TEMP, 10
lda#< FP_DT
ldy#> FP_DT
jsr MOVFM
lda#< FP_TEMP
ldy#> FP_TEMP
jsr FDIV
ldx#< FP_DT
ldy#> FP_DT
jsr MOVMF

xyz_step
    ; X_new = a * (Y_cur - X_cur)
    ; X_cur = X_cur + X_new * dt
    ;
    ; 1. Y_cur - X_cur
    +float_to_fac1 FP_YCUR
    lda#< FP_XCUR
    ldy#> FP_XCUR
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
    +fac1_to_int INT_X


    ; Y_new = X_cur * (b - Z_cuR) - Y_cur
    ; Y_cur = Y_cur + Y_new * dt

    ; (b - Z_cur)
    +float_to_fac1 FP_B
    +fsub FP_ZCUR
    ; FAC1 * X_cur
    +fmult FP_XCUR
    ; FAC1 - Y_cur
    +fsub FP_YCUR
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
    +fsub FP_TEMP
    ; 4. FAC1 * dt
    +fmult FP_DT
    ; 5. FAC1 + Z_cur
    +fadd FP_ZCUR
    ; 6. Z_cur = FAC1
    +movmf FP_ZCUR


    ; store int(y + z*10) as Y coordinate
    +fmult FP_A ; XXX abuses the fact that A=10
    +fadd FP_YCUR
    +fac1_to_int INT_Y










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
