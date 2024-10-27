

def encode_v1(x, y):
    print(f';encode({x}, {y})')
    block = (x >> 3) << 3

    bit = 8 - (x & 7) - 1

    yoff_local = y & 7
    yoff_row = (y >> 3) * 40 * 8

    print(f';block: {block}, bit: {bit}')
    print(f';yoff local: {yoff_local}, row: {yoff_row}')

    pattern = 1 << bit
    address = 0x2000 + block + yoff_local + yoff_row
    print(f'lda${hex(address)[2:]}')
    print(f'ora#{bin(pattern)}')
    print(f'sta${hex(address)[2:]}')
    print()



# Recipe for shifting a 16 bit value in memory
#
# Left:
#
# # fb = low byte, fc = high byte
# asl$fb
# rol$fc
#
# Right:
#
# lsr$fc
# ror$fb

# x is 16 bit (0 - 320)
# y is  8 bit (0 - 200)
def encode(x, y):
    print(f';encode({x}, {y})')
    #block = (x >> 3) << 3
    block = x & 0xF8

    bit = 8 - (x & 7) - 1
    # in asm this would be ~(x & 7)

    yoff_local = y & 7
    yoff_row = (y >> 3) * (5 << 6)

    yoff_row = (y & 0xF8) * 40
    yoff_row = (y & 0xF8) * (1 << 5 + 1 << 3)
    u = y & 0xF8
    (u << 5 + u << 3)

    # ; assume x is in $FC $FB
    # ; assume y is in Y
    #
    # ; intitialize addr. variable to 0x2000
    # lda#$20
    # sta >SCREEN_ADDR
    # lda#$00
    # sta <SCREEN_ADDR
    #
    # ; compute pixel mask
    # lda $FB
    # and #7
    # eor #7
    # sta $SCREEN_MASK
    #
    # ; add x offset to address
    # ; x & 0xF8 => x.lb & 8
    # clc
    # lda $FB
    # and #8
    # sta SCREEN_ADDR
    # lda $FC
    # sta SCREEN_ADDR + 1
    #

    # ;    yoff_row = (y & 0xF8) * 40
    # ; <> yoff_row = (y & 0xF8) * (1 << 5 + 1 << 3)
    # ; <> u = y & 0xF8
    # ;    yoff_row = (u << 5 + u << 3)
    #
    # ; clear high bytes of (FE,FD) and (FC,FB)
    # lda#0
    # sta$FC
    # sta$FD
    # ; init low bytes to y * 0xF8 (u = y & 0xF8)
    # ldy
    # and #$f8
    # sta $fb
    # sta $fd
    # ; y1 = u << 5
    # +lshift_16_bit $FC $FB
    # +lshift_16_bit $FC $FB
    # +lshift_16_bit $FC $FB
    # +lshift_16_bit $FC $FB
    # +lshift_16_bit $FC $FB
    # ; y2 = u << 3
    # +lshift_16_bit $FE $FD
    # +lshift_16_bit $FE $FD
    # +lshift_16_bit $FE $FD
    # ; y_off_row = y1 + y2
    # clc
    # lda $fb
    # adc $fd
    # sta $fd
    # lda $fc
    # adc $fe
    # sta $fe
    # ; add y_off_row (FE/FD) to screen addr.
    # lda SCREEN_ADDR
    # adc $fd
    # sta SCREEN_ADDR
    # lda SCREEN_ADDR + 1
    # adc $fe
    # sta SCREEN_ADDR + 1
    #
    #
    # ; add yoff_local to screen_addr
    # clc
    # ldy
    # and #7
    # adc SCREEN_ADDR
    # sta SCREEN_ADDR
    # lda#0
    # adc SCREEN_ADDR+1
    # sta SCREEN_ADDR+1
    #
    # ; load addr., mask pattern, store again
    # lda SCREEN_ADDR
    # sta $FB
    # lda SCREEN_ADDR+1
    # sta $FC
    # ldy #0
    # lda ($FB), Y
    # ora SCREEN_MASK
    # sta ($FB), Y


    print(f';block: {block}, bit: {bit}')
    print(f';yoff local: {yoff_local}, row: {yoff_row}')

    pattern = 1 << bit
    address = 0x2000 + block + yoff_local + yoff_row

    print('lda#$00')
    print('sta$fb')
    print('lda#$20')

    print(f'lda${hex(address)[2:]}')
    print(f'ora#{bin(pattern)}')
    print(f'sta${hex(address)[2:]}')
    print()



#encode(5, 0)
#encode(10, 0)
#encode(123, 0)

#encode(5, 1)
#encode(11, 1)
#encode(5, 10)


## LOL
#encode(5, 10)
#encode(5, 11)
#encode(5, 12)
#encode(5, 13)
#encode(5, 14)
#encode(6, 13)
#encode(6, 14)
#encode(7, 13)
#encode(7, 14)

#encode(8, 10)
#encode(8, 11)
#encode(8, 12)
#encode(8, 13)
#encode(8, 14)
#encode(9, 10)
#encode(9, 14)
#encode(10, 10)
#encode(10, 14)
#encode(11, 10)
#encode(11, 11)
#encode(11, 12)
#encode(11, 13)
#encode(11, 14)

#encode(14, 10)
#encode(14, 11)
#encode(14, 12)
#encode(14, 13)
#encode(14, 14)
#encode(15, 13)
#encode(15, 14)
#encode(16, 13)
#encode(16, 14)

#encode(0, 24*8)

encode(0, 0)
