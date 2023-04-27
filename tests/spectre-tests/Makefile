%.elf: %.s
	riscv32-unknown-elf-gcc -mabi=ilp32 -march=rv32im_zircs -c -o $@ $<
	riscv32-unknown-elf-gcc -mabi=ilp32 -march=rv32im_zircs -ffreestanding -nostdlib -T ../tests.ld -o $@ $<

%.ihex: %.elf
	riscv32-unknown-elf-objcopy -O ihex $< $@

%.bin: %.elf
	riscv32-unknown-elf-objcopy -O binary $< $@
