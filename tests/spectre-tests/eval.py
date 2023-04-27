#!/usr/bin/env python3

import vcdvcd
import os
import sys

def evaluate():
	vcd = vcdvcd.VCDVCD("sim.vcd", [])
	addresses = vcd["TOP.Core.pipeline.dbus_cmd_payload_address[31:0]"]

	violation = False

	for (_, val) in addresses.tv:
		addr = int(val, 2)
		if str(hex(addr)) == "0xdead0":
			violation = True

	return violation

base_proteus = sys.argv[1]
secure_proteus = sys.argv[2]

test_cases = [
	"secret-before-branch",
	"secret-after-branch",
]

for case in test_cases:
	# run test case with secure variant
	os.system(f"make {case}.bin")
	os.system(f"{secure_proteus} {case}.bin")
	print("SECURE VARIANT:", end=' ')
	print("Secret leaked" if evaluate() else "Secret did not leak")

	# run test case with insecure variant:
	# 1. remove csrrw instructions from code
	with open(f"{case}.s") as source:
		lines = source.readlines()
		stripped = [line for line in lines if not line.strip().startswith("csr")]
		with open(f"{case}_stripped.s", 'w') as stripped_file:
			stripped_file.writelines(stripped)

	os.system(f"make {case}_stripped.bin")
	os.system(f"{secure_proteus} {case}_stripped.bin")
	print("INSECURE VARIANT:", end=' ')
	print("Secret leaked" if evaluate() else "Secret did not leak")
