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

!addr FP_XCUR = $C4C0 ; 6 byte per float
!addr FP_YCUR = $C4F0
!addr FP_ZCUR = $C520

!addr FP_TEMP     = $C550
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



!zone xyz_step {
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

!macro m_key_is_pressed .addr_no {
    ; fall through if yes, jump to .addr_no if not
    lda #$ff
    sta $DC02
    lda #$00
    sta $DC03

    lda #0b11101111
    sta $DC00

    lda $DC01
    and #0b00010000
    bne .addr_no         ; quick hack to only play sound on 'M' key hold
}


!zone play_sounds {
play_sounds
    lda play_duration_voice1
    clc
    cmp #2
    bcs .play_sound         ; if play duration is >1 play that sound

    lda off_duration
    bne .off

    +m_key_is_pressed .no_key

    ; init playing of sound
    ;lda #$0a
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

    .return_advance_loop_voice1
    rts


.off
    lda #00
    sta CONTROL_VOICE1
    dec off_duration
.no_key
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

.play_sound_voice1_looped ; commented out
    ldy sound_idx

    lda (sound_ptr), y
    sta FREQ_HI_VOICE1
    iny
    lda (sound_ptr), y
    sta CONTROL_VOICE1
    iny
    sty sound_idx
    rts

.advance_loop_voice1
    ldy #0
    lda (voice1_ptr), y
    sta sound_ptr, y
    iny
    lda (voice1_ptr), y
    sta sound_ptr, y
    iny
    lda (voice1_ptr), y
    sta play_duration_voice1
    sta off_duration

    inc voice1_ptr
    inc voice1_ptr
    inc voice1_ptr ; move pointer to next entry in loop list

    ; read first byte as sustain/release value
    ldy #0
    lda (sound_ptr), y
    sta SUSTAIN_REL_VOICE1
    sty ATTACK_DUR_VOICE1
    iny
    sty CONTROL_VOICE1
    sty sound_idx
    jmp .return_advance_loop_voice1


.restart:
    ldx #$02
.restart_loop:
    lda #$01
    sta play_durations,x

    lda voiceinit,x
    sta voice1_ptr,x
    lda voiceinit+3,x
    sta voice1_ptr+3,x

    dex
    bpl .restart_loop
    lda #$08
    sta CONTROL_VOICE1
    sta CONTROL_VOICE2
    sta CONTROL_VOICE3  ; set all voices to 'test'?
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
