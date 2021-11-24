BASE_DIR = $(shell pwd)
RISCV_DIR = $(BASE_DIR)/..
RISCV_FORMAL_DIR = $(BASE_DIR)/riscv-formal
CORE_DIR = $(RISCV_FORMAL_DIR)/cores/riscv
CORE ?= riscv.CoreFormal

all: formal

$(RISCV_DIR)/Core.v: FORCE
	cd $(RISCV_DIR); sbt "runMain $(CORE)"

setup: $(RISCV_DIR)/Core.v
	mkdir -p $(CORE_DIR)
	cp $< $(CORE_DIR)/riscv.v
	cp checks.cfg wrapper.sv disasm.py $(CORE_DIR)
	cd $(CORE_DIR); python3 $(RISCV_FORMAL_DIR)/checks/genchecks.py

formal: setup
	$(MAKE) -C $(CORE_DIR)/checks
	@echo "Failed tests:"
	find $(CORE_DIR)/checks -name trace.vcd

formal-%: setup
	$(MAKE) -C $(CORE_DIR)/checks $(subst formal-,,$@)_ch0

.PHONY: FORCE

