.globl _start
.data
	public_value: .word 0
	public_address: .word 0
	secret: .word 0xDEAD0

.text
_start:

setup:
	# public_address = &public_value;

	la t0, public_value
	la t1, public_address
	sw t0, (t1)

	# lower_boundary = &secret
	# upper_boundary = &secret + 4

	la t0, secret
	csrrw zero, 0x707, t0
	addi t0, t0, 4
	csrrw zero, 0x708, t0
	
	nop
	nop
	
	# s0 = *secret  // load secret architecturally
	lw s0, secret

condition:
	# t0 = *public_address
	# t0 = *t0
	# if (t0 == 0) { goto finish; }

	la t0, public_address
	lw t0, (t0)
	lw t0, (t0)
	beqz t0, finish

	# ----- BEGIN TRANSIENT BLOCK -----

	# leak(s0);.
	
	lw zero, (s0)
	
	# ----- END TRANSIENT BLOCK -----

finish:
	lui ra,0x10000
	li sp,4
	sb sp,0(ra)

