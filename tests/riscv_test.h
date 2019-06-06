#ifndef RISCV_TEST_H
#define RISCV_TEST_H

#define RVTEST_RV64U                                                    \
    .macro init;                                                        \
    .endm

#define RVTEST_RV32U                                                    \
    .macro init;                                                        \
    .endm

#define RVTEST_CODE_BEGIN                                               \
    .text;                                                              \
    .globl _start;                                                      \
_start:

#define RVTEST_CODE_END

#define RVTEST_DATA_BEGIN                                               \
    .data;

#define RVTEST_DATA_END

#define TESTNUM gp

#define TESTDEV 8192

#define RVTEST_FAIL                                                     \
    li x1, TESTDEV;                                                     \
    sw TESTNUM, 0(x1);

#define RVTEST_PASS                                                     \
    li x1, TESTDEV;                                                     \
    sw zero, 0(x1);

#endif
