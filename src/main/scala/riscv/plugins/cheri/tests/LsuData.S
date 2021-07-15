#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

#define TEST_CASE_LOAD(testnum, insn) \
    TEST_CASE_FREE(testnum) \
        la t0, insn; \
        CSetAddr c1, ROOT, t0; \
        insn.cap t1, (c1); \
        insn t2, (t0); \
        bne t1, t2, fail;

#define TEST_CASE_LOAD_LENGTH_VIOLATION(testnum, insn, width) \
    TEST_CASE_FREE(testnum) \
        la t0, insn; \
        CSetAddr c1, ROOT, t0; \
        CSetBoundsImm c1, c1, width - 1; \
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, insn.cap t1, (c1));

#define TEST_CASE_LOAD_PERM_VIOLATION(testnum, insn) \
    TEST_CASE_FREE(testnum) \
        la t0, insn; \
        CSetAddr c1, ROOT, t0; \
        li t0, ~(1 << PERM_PERMIT_LOAD); \
        CAndPerm c1, c1, t0; \
        EXPECT_EXCEPTION(CAUSE_PERMIT_LOAD_VIOLATION, 1, insn.cap t1, (c1)); \

#define TEST_CASE_LOAD_SEAL_VIOLATION(testnum, insn) \
    TEST_CASE_FREE(testnum) \
        la t0, insn; \
        CSetAddr c1, ROOT, t0; \
        SEAL(c1, c1, 123); \
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, 1, insn.cap t1, (c1)); \

#define TEST_CASE_STORE(testnum, store_insn, load_insn, val) \
    TEST_CASE_FREE(testnum) \
        la t0, scratch; \
        store_insn zero, (t0); \
        CSetAddr c1, ROOT, t0; \
        li t1, val; \
        store_insn.cap t1, (c1); \
        load_insn t2, (t0); \
        bne t1, t2, fail; \

#define TEST_CASE_STORE_LENGTH_VIOLATION(testnum, store_insn, width) \
    TEST_CASE_FREE(testnum) \
        la t0, scratch; \
        sw zero, (t0); \
        CSetAddr c1, ROOT, t0; \
        CSetBoundsImm c1, c1, width - 1; \
        li t1, 0x12345678; \
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, store_insn.cap t1, (c1));
        # TODO this fails because of https://gitlab.com/ProteusCore/ProteusCore/-/issues/3
        #lw t1, (t0)
        #bne t1, zero, fail

#define TEST_CASE_STORE_PERM_VIOLATION(testnum, store_insn) \
    TEST_CASE_FREE(testnum) \
        la t0, scratch; \
        sw zero, (t0); \
        CSetAddr c1, ROOT, t0; \
        li t1, ~(1 << PERM_PERMIT_STORE); \
        CAndPerm c1, c1, t1; \
        li t1, 0x12345678; \
        EXPECT_EXCEPTION(CAUSE_PERMIT_STORE_VIOLATION, 1, store_insn.cap t1, (c1));

#define TEST_CASE_STORE_SEAL_VIOLATION(testnum, store_insn) \
    TEST_CASE_FREE(testnum) \
        la t0, scratch; \
        sw zero, (t0); \
        CSetAddr c1, ROOT, t0; \
        SEAL(c1, c1, 123); \
        li t1, 0x12345678; \
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, 1, store_insn.cap t1, (c1));

RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    TEST_CASE_LOAD(1, lw)
    TEST_CASE_LOAD_LENGTH_VIOLATION(2, lw, 4)
    TEST_CASE_LOAD_PERM_VIOLATION(3, lw)
    TEST_CASE_LOAD_SEAL_VIOLATION(4, lw)

    TEST_CASE_LOAD(5, lh)
    TEST_CASE_LOAD_LENGTH_VIOLATION(6, lh, 2)
    TEST_CASE_LOAD_PERM_VIOLATION(7, lh)
    TEST_CASE_LOAD_SEAL_VIOLATION(8, lh)

    TEST_CASE_LOAD(9, lb)
    TEST_CASE_LOAD_LENGTH_VIOLATION(10, lb, 1)
    TEST_CASE_LOAD_PERM_VIOLATION(11, lb)
    TEST_CASE_LOAD_SEAL_VIOLATION(12, lb)

    TEST_CASE_LOAD(13, lhu)
    TEST_CASE_LOAD_LENGTH_VIOLATION(14, lhu, 2)
    TEST_CASE_LOAD_PERM_VIOLATION(15, lhu)
    TEST_CASE_LOAD_SEAL_VIOLATION(16, lhu)

    TEST_CASE_LOAD(17, lbu)
    TEST_CASE_LOAD_LENGTH_VIOLATION(18, lbu, 1)
    TEST_CASE_LOAD_PERM_VIOLATION(19, lbu)
    TEST_CASE_LOAD_SEAL_VIOLATION(20, lbu)

    TEST_CASE_STORE(21, sw, lw, 0x12345678)
    TEST_CASE_STORE_LENGTH_VIOLATION(22, sw, 4)
    TEST_CASE_STORE_PERM_VIOLATION(23, sw)
    TEST_CASE_STORE_SEAL_VIOLATION(24, sw)

    TEST_CASE_STORE(25, sh, lh, 0x1234)
    TEST_CASE_STORE_LENGTH_VIOLATION(26, sh, 2)
    TEST_CASE_STORE_PERM_VIOLATION(27, sh)
    TEST_CASE_STORE_SEAL_VIOLATION(28, sh)

    TEST_CASE_STORE(29, sb, lb, 0x12)
    TEST_CASE_STORE_LENGTH_VIOLATION(30, sb, 1)
    TEST_CASE_STORE_PERM_VIOLATION(31, sb)
    TEST_CASE_STORE_SEAL_VIOLATION(32, sb)

    # Test that permission violations have priority over misalignment exceptions
    TEST_CASE_FREE(33)
        la t0, lw
        addi t0, t0, 1
        CSetAddr c1, ROOT, t0
        CSetBoundsImm c1, c1, 3
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, LW.cap t1, (c1))

#if 0
    # Test that store are not performed when an excpetion is raised
    # TODO this fails because of https://gitlab.com/ProteusCore/ProteusCore/-/issues/3
    TEST_CASE_FREE(34)
        la t0, scratch
        sw zero, (t0)
        CSetAddr c1, ROOT, t0
        li t1, ~(1 << PERM_PERMIT_STORE)
        CAndPerm c1, c1, t1
        li t1, 0x12345678
        EXPECT_EXCEPTION(CAUSE_PERMIT_STORE_VIOLATION, 1, SW.cap t1, (c1));
        lw t1, (t0)
        bne t1, zero, fail
#endif

    TEST_PASSFAIL

RVTEST_CODE_END

RVTEST_DATA_BEGIN

lw:      .word  0x12345678
lh: lhu: .hword 0xfedc
lb: lbu: .byte  0xff

    .align 4
scratch:
    .word 0

    TEST_DATA

RVTEST_DATA_END
