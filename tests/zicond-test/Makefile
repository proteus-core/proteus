%.elf: %.s
	riscv32-none-elf-gcc -mabi=ilp32 -march=rv32im_zicsr_zicond -c -o $@ $<
	riscv32-none-elf-gcc -mabi=ilp32 -march=rv32im_zicsr_zicond -ffreestanding -nostdlib -T ../tests.ld -o $@ $<

%.ihex: %.elf
	riscv32-none-elf-objcopy -O ihex $< $@

%.bin: %.elf
	riscv32-none-elf-objcopy -O binary $< $@
