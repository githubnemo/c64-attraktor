.s4:
LDA P2 ; (exp + 0)
BEQ .s14: ; (foo.s14 + 0)
.s5:
SEC
SBC #$80
STA ACCU + 0 
LDA #$00
SBC #$00
STA ACCU + 1 
LDA ACCU + 0 
BMI .s6: ; (foo.s6 + 0)
.s15:
CMP #$09
BCC .s6: ; (foo.s6 + 0)
.s13:
LDA #$ff
.s14:
LDY #$00
STA (P0),y ; (target + 0)
INY
.s3:
STA (P0),y ; (target + 0)
RTS
.s6:
SEC
LDA #$00
SBC ACCU + 0 
TAX
LDA #$00
SBC ACCU + 1 
BMI .s7: ; (foo.s7 + 0)
.s12:
BNE .s10: ; (foo.s10 + 0)
.s11:
CPX #$08
BCC .s7: ; (foo.s7 + 0)
.s10:
LDA #$00
BEQ .s14: ; (foo.s14 + 0)
.s7:
TXA
CLC
ADC #$08
TAX
LDA P4 ; (m12 + 1)
ORA #$80
CPX #$00
BEQ .s9: ; (foo.s9 + 0)
.l8:
LSR
ROR P3 ; (m12 + 0)
DEX
BNE .l8: ; (foo.l8 + 0)
.s9:
LDY #$01
STA (P0),y ; (target + 0)
LDA P3 ; (m12 + 0)
DEY
BEQ .s3: ; (foo.s3 + 0)
