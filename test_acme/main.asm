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

    lda#0b1000
    sta$2000

    lda#0b1000000
    sta$2008

    lda#0b100000
    sta$2078

    lda#0b100
    sta$2001

    lda#0b100000
    sta$2009

    lda#0b1000
    sta$2142



    ; L
    lda$2142
    ora#0b1000
    sta$2142

    lda$2143
    ora#0b1000
    sta$2143

    lda$2144
    ora#0b1000
    sta$2144

    lda$2145
    ora#0b1000
    sta$2145

    lda$2146
    ora#0b1000
    sta$2146

    lda$2145
    ora#0b100
    sta$2145

    lda$2146
    ora#0b100
    sta$2146

    lda$2145
    ora#0b10
    sta$2145

    lda$2146
    ora#0b10
    sta$2146


    ; O
    lda$214a
    ora#0b10000000
    sta$214a

    lda$214b
    ora#0b10000000
    sta$214b

    lda$214c
    ora#0b10000000
    sta$214c

    lda$214d
    ora#0b10000000
    sta$214d

    lda$214e
    ora#0b10000000
    sta$214e

    lda$214a
    ora#0b1000000
    sta$214a

    lda$214e
    ora#0b1000000
    sta$214e

    lda$214a
    ora#0b100000
    sta$214a

    lda$214e
    ora#0b100000
    sta$214e

    lda$214a
    ora#0b10000
    sta$214a

    lda$214b
    ora#0b10000
    sta$214b

    lda$214c
    ora#0b10000
    sta$214c

    lda$214d
    ora#0b10000
    sta$214d

    lda$214e
    ora#0b10000
    sta$214e



    ; L
    lda$214a
    ora#0b10
    sta$214a

    lda$214b
    ora#0b10
    sta$214b

    lda$214c
    ora#0b10
    sta$214c

    lda$214d
    ora#0b10
    sta$214d

    lda$214e
    ora#0b10
    sta$214e

    lda$214d
    ora#0b1
    sta$214d

    lda$214e
    ora#0b1
    sta$214e

    lda$2155
    ora#0b10000000
    sta$2155

    lda$2156
    ora#0b10000000
    sta$2156



    ;encode(0, 56)
    ;block: 0, bit: 7
    ;yoff local: 0, row: 2240
    lda$28c0
    ora#0b10000000
    sta$28c0


    lda#$00
    sta$fb
    lda#$20
    sta$fc
    asl$fb
    rol$fc



hang
   jmp hang
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
