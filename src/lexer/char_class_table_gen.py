#!/usr/bin/env python3

# Copyright (C) 2021 xq-Tec GmbH

LETTER = 0x01
DIGIT = 0x02
GRAPHIC_CHAR = 0x04
EOL = 0x08
LETTER_OR_DIGIT = 0x10

table = [0] * 256
for i in range(0, 256):
    # Letters
    if (ord('a') <= i <= ord('z') or ord('A') <= i <= ord('Z') or
        0xc0 <= i <= 0xd6 or 0xd8 <= i <= 0xf6 or 0xf8 <= i <= 0xff):
        table[i] |= LETTER | LETTER_OR_DIGIT

    # Digits
    if ord('0') <= i <= ord('9'):
        table[i] |= DIGIT | LETTER_OR_DIGIT

    # Graphic character: letter, digit, special character, space character,
    # or other special character
    if 0x20 <= i <= 0x7e or 0xa0 <= i <= 0xff:
        table[i] |= GRAPHIC_CHAR

    # End of line: EOF, \n, \r, form feed, vertical tab
    if i in [0x00, 0x0a, 0x0d, 0x0c, 0x0b]:
        table[i] |= EOL

    ## Special character; single and double quotes are excluded here
    #special = ['#', '&', '(', ')', '*', '+', ',', '-', '.', '/', \
        #':', ';', '<', '=', '>', '?', '@', '[', ']', '_', '`', '|']
    #if i in [ord(ch) for ch in special]:
        #table[i] |= SPECIAL

for i in range(0, 256):
    print("0x%02x, " % table[i], end = '\n' if i % 16 == 15 else '')
