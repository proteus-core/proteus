KERNEL_DIR = ../newlib
KREMLIB_INCLUDE_DIR = ./include/kremlib
KREMLIN_INCLUDE_DIR = ./include

EXTRA_CFLAGS = -I$(KREMLIN_INCLUDE_DIR) -I$(KREMLIB_INCLUDE_DIR)

TARGET ?= specBench


all: $(TARGET).bin $(TARGET).ihex $(TARGET).objdump

$(TARGET).elf: Hacl_Chacha20.o

include $(KERNEL_DIR)/Makefile.include

$(TARGET).objdump: $(TARGET).elf
	$(OBJDUMP) -d $^ > $@

clean:
	make -C $(KERNEL_DIR) clean
	rm -f *.elf *.bin *.ihex *.o
