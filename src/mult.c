#ifdef __GNUC__
#include <stdio.h>
#endif

#include <stdint.h>

#ifdef __GNUC__
#define NOINLINE
#else
#define NOINLINE __noinline
#endif

NOINLINE void foo(__zeropage uint16_t* target, uint8_t exp, uint16_t m12) {
    if (exp == 0x0) {
        *target = 0;
        return;
    }

    int8_t shift = (exp - 0x80);

    if (shift >= 1+8) {
        *target = 0xffff;
    } else {
        if (-(shift) >= 16-8) {
            *target = 0;
        } else {
            *target = m12 | 0x8000;
            *target >>= -shift+8;
        }
    }
}


uint32_t* fp1 = (uint32_t*)0x10;
uint16_t* CUTOFF = (uint16_t*)0xd415;

int main(void) {

    uint16_t val = 0;

    foo(&val, 0b00000000, 0b00000000);

#ifdef __GNUC__
    for (uint8_t exp=0; exp < 255; exp++) {
        val = 0;
        //foo(&val, exp, 0b10000000);
        foo(&val, exp, 0xffff);
        printf("shift=%d exp=%d (0x%x), target=%d (0x%x)\n", (exp-0x80)-8, exp, exp, val, val);
    }
#endif

    return 0;
}
