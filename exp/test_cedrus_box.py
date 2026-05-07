import struct
import time

import serial
from serial.tools import list_ports


def find_cedrus_port():
    for port in list_ports.comports():
        if port.vid == 0x0403 and port.pid == 0x6001:
            return port.device
    return "/dev/ttyUSB0"


port_name = find_cedrus_port()

with serial.Serial(port_name, baudrate=115200, timeout=0.01) as box:
    answer = b""

    for _ in range(10):
        box.reset_input_buffer()
        box.write(b"_c1")
        time.sleep(0.1)
        answer = box.read(64)

        if answer.startswith(b"_xid"):
            break

    if not answer.startswith(b"_xid"):
        print(f"No XID response from {port_name}. Got: {answer!r}")
        raise SystemExit(1)

    box.write(b"e1")
    box.reset_input_buffer()
    buffer = b""

    print(f"Cedrus box on {port_name}. Press buttons; Ctrl+C to quit.")

    try:
        while True:
            buffer += box.read(box.in_waiting or 1)

            while b"k" in buffer:
                start = buffer.find(b"k")
                buffer = buffer[start:]

                if len(buffer) < 6:
                    break

                packet = buffer[:6]
                buffer = buffer[6:]

                info = packet[1]
                button = (info & 0xE0) >> 5 or 8
                direction = "down" if info & 0x10 else "up"
                rt_ms = struct.unpack("<I", packet[2:6])[0]
                print(f"button {button} {direction}, rt={rt_ms} ms")

            time.sleep(0.001)
    except KeyboardInterrupt:
        print("\nDone.")
