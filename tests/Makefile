TESTS_DIR = $(abspath .)
BASE_DIR = $(abspath $(TESTS_DIR)/..)
RISCV_TESTS_DIR = $(TESTS_DIR)/riscv-tests
ISA_TESTS_DIR = $(RISCV_TESTS_DIR)/isa
BUILD_DIR = $(TESTS_DIR)/build
SIM_DIR = $(BASE_DIR)/sim
SIM_EXE = $(SIM_DIR)/build/sim
CORE ?= riscv.CoreExtMem
BUILD_CORE ?= 1

RISCV_PREFIX = riscv64-unknown-elf
CC = $(RISCV_PREFIX)-gcc
LD = $(RISCV_PREFIX)-gcc
OBJCOPY = $(RISCV_PREFIX)-objcopy
ARCHFLAGS = -march=rv32im -mabi=ilp32
CFLAGS = $(ARCHFLAGS)

ifeq ($(BUILD_CORE),1)
FORCE = FORCE
else
FORCE =
endif

-include $(CUSTOM_TESTS_DIR)/Makefile.include

TEST_DIRS ?= $(ISA_TESTS_DIR)/rv32ui $(ISA_TESTS_DIR)/rv32mi $(ISA_TESTS_DIR)/rv32um $(CUSTOM_TESTS_DIR)
ALL_ISA_TESTS = $(shell find $(TEST_DIRS) -name '*.S')
UNSUPPORTED_TESTS = %fence_i.S %breakpoint.S
ALL_TESTS = $(filter-out $(UNSUPPORTED_TESTS),$(ALL_ISA_TESTS))
ALL_TARGETS = $(addsuffix .pass,$(basename $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_TESTS)))))

all: $(ALL_TARGETS)

.SECONDARY:

VPATH = $(TEST_DIRS)

$(BUILD_DIR)/%.o: %.S
	mkdir -p $(@D)
	$(CC) $(CFLAGS) -I$(TESTS_DIR) -I$(ISA_TESTS_DIR)/macros/scalar -c -o $@ $^

%.elf: %.o
	$(LD) $(ARCHFLAGS) -ffreestanding -nostdlib -T tests.ld -o $@ $^

%.bin: %.elf
	$(OBJCOPY) -O binary $^ $@

$(SIM_EXE): $(FORCE)
	make -C $(SIM_DIR) CORE=$(CORE)

.PHONY: FORCE

%.pass: %.bin $(SIM_EXE)
	cd $(BASE_DIR); $(SIM_EXE) $<
	touch $@
