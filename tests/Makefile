TESTS_DIR = $(abspath .)
BASE_DIR = $(abspath $(TESTS_DIR)/..)
RISCV_TESTS_DIR = $(TESTS_DIR)/riscv-tests
ISA_TESTS_DIR = $(RISCV_TESTS_DIR)/isa
BUILD_DIR = $(TESTS_DIR)/build
SIM_DIR = $(BASE_DIR)/sim
SIM_EXE = $(SIM_DIR)/build/sim

RISCV_PREFIX = riscv32-unknown-elf
CC ?= $(RISCV_PREFIX)-gcc
LD = $(RISCV_PREFIX)-gcc
OBJCOPY = $(RISCV_PREFIX)-objcopy

TEST_DIRS = $(ISA_TESTS_DIR)/rv32ui $(ISA_TESTS_DIR)/rv32mi $(ISA_TESTS_DIR)/rv32um
ALL_ISA_TESTS = $(shell find $(TEST_DIRS) -name '*.S')
UNSUPPORTED_TESTS = %fence_i.S %breakpoint.S
ALL_TESTS = $(filter-out $(UNSUPPORTED_TESTS),$(ALL_ISA_TESTS))
ALL_TARGETS = $(addsuffix .pass,$(basename $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_TESTS)))))

all: $(ALL_TARGETS)

.SECONDARY:

VPATH = $(TEST_DIRS)

$(BUILD_DIR)/%.o: %.S
	mkdir -p $(@D)
	$(CC) -I$(TESTS_DIR) -I$(ISA_TESTS_DIR)/macros/scalar -c -o $@ $^

%.elf: %.o
	$(LD) -ffreestanding -nostdlib -T tests.ld -o $@ $^

%.bin: %.elf
	$(OBJCOPY) -O binary $^ $@

$(SIM_EXE): FORCE
	make -C $(SIM_DIR)

.PHONY: FORCE

%.pass: %.bin $(SIM_EXE)
	cd $(BASE_DIR); $(SIM_EXE) $<
	touch $@
