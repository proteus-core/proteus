#!/usr/bin/env python3

import vcdvcd
import subprocess
import os


def evaluate():
    vcd = vcdvcd.VCDVCD("sim.vcd", [])
    addresses = vcd["TOP.Core.pipeline.dbus_cmd_payload_address[31:0]"]

    violation = False

    for (_, val) in addresses.tv:
        addr = int(val, 2)
        if str(hex(addr)) == "0xdead0":
            violation = True

    return violation

proteus_bin = os.path.join(os.path.abspath(os.path.dirname(__file__)), '../../sim/build/sim')

test_cases = [
    "secret-before-branch",
    "secret-after-branch",
]

for case in test_cases:
    print(f"TEST {case}:")
    # run test case with secure variant
    subprocess.call(
        ["make", f"{case}.bin"])
    subprocess.call([f"{proteus_bin}", f"{case}.bin"])
    print("SECURE VARIANT:  ", end='\t')
    print("🗲 Secret leaked!" if evaluate() else "✔ Secret did not leak!")

    # run test case with insecure variant:
    # 1. remove fence instructions from code
    with open(f"{case}.s") as source:
        lines = source.readlines()
        stripped = [
            line for line in lines if not line.strip().startswith("fence") and not line.strip().startswith("sfence")]
        with open(f"{case}_stripped.s", 'w') as stripped_file:
            stripped_file.writelines(stripped)

    subprocess.call(["make", f"{case}_stripped.bin"])
    subprocess.call([f"{proteus_bin}", f"{case}_stripped.bin"])
    print("INSECURE VARIANT:", end='\t')
    print("🗲 Secret leaked!" if evaluate() else "✔ Secret did not leak!")
    print()
