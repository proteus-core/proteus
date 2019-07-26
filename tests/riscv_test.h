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
    .weak mtvec_handler;                                                \
    .globl _start;                                                      \
_start:                                                                 \
    la t0, trap_vector;                                                 \
    csrw mtvec, t0;                                                     \
    j start_tests;                                                      \
trap_vector:                                                            \
    la t5, mtvec_handler;                                               \
    beqz t5, 1f;                                                        \
    jr t5;                                                              \
1:  mret;                                                               \
start_tests:

#define RVTEST_CODE_END

#define RVTEST_DATA_BEGIN                                               \
    .data;

#define RVTEST_DATA_END

#define TESTNUM gp

#define TESTDEV 0xf0003000

#define RVTEST_FAIL                                                     \
    li x1, TESTDEV;                                                     \
    sw TESTNUM, 0(x1);

#define RVTEST_PASS                                                     \
    li x1, TESTDEV;                                                     \
    sw zero, 0(x1);

#endif
