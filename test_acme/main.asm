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


   ; address of the bitmap is given by $d018 bit 4, usually $2000
   lda#0b1010
   !for i, 0, 20 {
    sta$2000+i
   }

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
