#!/usr/bin/env python3

import sys
import subprocess
import tempfile


objdump = 'riscv32-unknown-elf-objdump'

def output(line):
    print(line, flush=True)

for line in sys.stdin:
    hexstring = line.strip()
    instruction = bytes(reversed(bytes.fromhex(hexstring)))
    assert len(instruction) == 4

    with tempfile.NamedTemporaryFile() as f:
        f.write(instruction)
        f.flush()
        proc = subprocess.run([objdump, '-b', 'binary', '-m', 'riscv', '-D', '-M', 'no-aliases', f.name],
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                              universal_newlines=True)

    if proc.returncode != 0:
        output('???')
    else:
        for line in proc.stdout.split('\n'):
            parts = line.split()

            if len(parts) >= 3 and parts[0] == '0:':
                output(' '.join(parts[2:]))
                break
        else:
            output('???')

