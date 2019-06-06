TESTS_DIR = $(abspath .)
BASE_DIR = $(abspath $(TESTS_DIR)/..)
RISCV_TESTS_DIR = $(TESTS_DIR)/riscv-tests
ISA_TESTS_DIR = $(RISCV_TESTS_DIR)/isa
BUILD_DIR = $(TESTS_DIR)/build

RISCV_PREFIX = riscv32-unknown-elf
CC = $(RISCV_PREFIX)-gcc
OBJCOPY = $(RISCV_PREFIX)-objcopy

ALL_ISA_TESTS = $(shell find $(ISA_TESTS_DIR)/rv32ui -name '*.S')
ALL_TESTS = $(filter-out %fence_i.S,$(ALL_ISA_TESTS))
ALL_TARGETS = $(addsuffix .pass,$(basename $(addprefix $(BUILD_DIR)/,$(notdir $(ALL_TESTS)))))

all: $(ALL_TARGETS)

.SECONDARY:

$(BUILD_DIR)/%.elf: $(ISA_TESTS_DIR)/rv32ui/%.S
	mkdir -p $(@D)
	$(CC) -ffreestanding -nostdlib -T tests.ld -I$(TESTS_DIR) -I$(ISA_TESTS_DIR)/macros/scalar -o $@ $^

%.ihex: %.elf
	$(OBJCOPY) -O ihex $^ $@

%.pass: %.ihex
	cd $(BASE_DIR); sbt "runMain riscv.CoreTestSim $<"
	touch $@
