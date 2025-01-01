.globl _start
.data
	public_value: .word 0
	.space 1024  # padding to avoid caching
	public_address: .word 0
	.space 1024  # padding to avoid caching
	secret: .word 0xDEAD0

.text
_start:

setup:
	# public_address = &public_value;

	fence

	la t0, public_value
	la t1, public_address
	sw t0, (t1)

	la t6, secret

condition:
	# t0 = *public_address
	# t0 = *t0
	# if (t0 == 0) { goto finish; }

	la t0, public_address
	lw t0, (t0)
	lw t0, (t0)
	beqz t0, finish

	# ----- BEGIN TRANSIENT BLOCK -----

	# t6 = *secret;
	lw t6, (t6)

	fence

	# leak(t6);
	lw zero, (t6)

	# ----- END TRANSIENT BLOCK -----

finish:
	lui ra,0x10000
	li sp,4
	sb sp,0(ra)
