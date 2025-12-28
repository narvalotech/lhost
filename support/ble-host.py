import struct
import binascii
import os
import fcntl
import sys
import time


def generate_name(name: str) -> bytearray:
    ad_type = 0x09.to_bytes(1, "big")
    ad_data = name.encode()
    ad_len = (len(ad_data) + 1).to_bytes(1, "big")

    ad = ad_len + ad_type + ad_data

    return ad

def hci_cmd(opcode, data):
    return struct.pack('<BHB',
                       0x01,
                       opcode,
                       len(data)) + data

def advertise(tx, rx, duration_s=1):
    # Initialize controller & advertiser
    tx.write(bytes.fromhex('01 03 0C 00'))
    tx.write(bytes.fromhex('01 02 20 00'))
    tx.write(bytes.fromhex('01 01 0C 08 FF FF FF FF FF FF FF FF'))
    tx.write(bytes.fromhex('01 01 20 08 FF FF FF FF FF FF FF FF'))
    tx.write(bytes.fromhex('01 05 20 06 0A 89 67 45 23 C1'))
    tx.write(bytes.fromhex('01 06 20 0F 3C 00 3C 00 00 01 00 00 00 00 00 00 00 07 00'))

    # tx.write(bytes.fromhex('01 08 20 0F 0E 02 01 01 0A 09 F0 9F 94 B5 2D F0 9F A6 B7'))
    ad = bytes.fromhex("02 01 01") + generate_name("🐍 is 🔥")
    ad = len(ad).to_bytes(1, "big") + ad

    cmd = hci_cmd(0x2008, ad)
    tx.write(cmd)

    # Start, wait, and stop advertiser
    tx.write(bytes.fromhex('01 0A 20 01 01'))
    time.sleep(duration_s)
    tx.write(bytes.fromhex('01 0A 20 01 00'))


def main():
    command_fifo = '/tmp/py/uart.h2c'
    response_fifo = '/tmp/py/uart.c2h'

    with open(command_fifo, 'wb', buffering=0) as tx:
        with open(response_fifo, 'rb', buffering=0) as rx:
            advertise(tx, rx, 1)

if __name__ == "__main__":
    main()
