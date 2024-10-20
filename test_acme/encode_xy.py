

def encode(x, y):
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
