TESTS_DIR = $(abspath .)
BASE_DIR = $(abspath $(TESTS_DIR)/..)
RISCV_TESTS_DIR = $(TESTS_DIR)/riscv-tests
ISA_TESTS_DIR = $(RISCV_TESTS_DIR)/isa
BUILD_DIR = $(TESTS_DIR)/build

RISCV_PREFIX = riscv32-unknown-elf
CC = $(RISCV_PREFIX)-gcc
OBJCOPY = $(RISCV_PREFIX)-objcopy

TEST_DIRS = $(ISA_TESTS_DIR)/rv32ui $(ISA_TESTS_DIR)/rv32mi
ALL_ISA_TESTS = $(shell find $(TESTS_DIRS) -name '*.S')
UNSUPPORTED_TESTS = %fence_i.S %breakpoint.S
ALL_TESTS = $(filter-out $(UNSUPPORTED_TESTS),$(ALL_ISA_TESTS))
ALL_TARGETS = $(addsuffix .pass,$(basename $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_TESTS)))))

all: $(ALL_TARGETS)

.SECONDARY:

VPATH = $(TEST_DIRS)

$(BUILD_DIR)/%.elf: %.S
	mkdir -p $(@D)
	$(CC) -ffreestanding -nostdlib -T tests.ld -I$(TESTS_DIR) -I$(ISA_TESTS_DIR)/macros/scalar -o $@ $^

%.ihex: %.elf
	$(OBJCOPY) -O ihex $^ $@

%.pass: %.ihex
	cd $(BASE_DIR); sbt "runMain riscv.CoreTestSim $<"
	touch $@
