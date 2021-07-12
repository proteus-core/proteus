#ifndef RISCV_TEST_H
#define RISCV_TEST_H

#include "riscv-test-env/encoding.h"

#define RVTEST_RV64U                                                    \
    .macro init;                                                        \
    .endm

#define RVTEST_RV64M                                                    \
    .macro init;                                                        \
    .endm

#define RVTEST_RV32U                                                    \
    .macro init;                                                        \
    .endm

#define RVTEST_RV32M                                                    \
    .macro init;                                                        \
    .endm

#define RVTEST_CODE_BEGIN                                               \
    .text;                                                              \
    .weak fail;                                                         \
    .globl _start;                                                      \
_start:                                                                 \
    la tp, trap_vector;                                                 \
    csrw mtvec, tp;                                                     \
    j start_tests;                                                      \
trap_vector:                                                            \
    la tp, mtvec_handler;                                               \
    beqz tp, 1f;                                                        \
    jr tp;                                                              \
1:  la tp, fail;                                                        \
    beqz tp, 1f;                                                        \
    jr tp;                                                              \
1:  mret;                                                               \
start_tests:

// HACK LLVM started erring when weak symbols are redefined as global. However,
// globals being redefined as weak only causes a warning.
// https://reviews.llvm.org/D90108
#define RVTEST_CODE_END                                                 \
    .weak mtvec_handler;

#define RVTEST_DATA_BEGIN                                               \
    .data;

#define RVTEST_DATA_END

#define TESTNUM gp

#define TESTDEV 0x30000000

#define RVTEST_FAIL                                                     \
    li tp, TESTDEV;                                                     \
    sw TESTNUM, 0(tp);

#define RVTEST_PASS                                                     \
    li tp, TESTDEV;                                                     \
    sw zero, 0(tp);

#endif
