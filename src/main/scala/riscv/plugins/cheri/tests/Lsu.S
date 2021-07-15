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
    SET_BOUNDS_DCC(512)

    TEST_CASE_FREE(1)
        LC c1, (zero)
        CHECK_TAG(c1, 0)

    TEST_CASE_FREE(2)
        SC ROOT, (zero)
        LC c1, (zero)
        CHECK_CAP_EQ(c1, ROOT)

    TEST_CASE_FREE(3)
        SC ROOT, (zero)
        sb zero, 1(zero)
        LC c1, (zero)
        CHECK_TAG(c1, 0)

    TEST_CASE_DCC_BOUNDS_EXCEPTION(4, 7, CAUSE_LENGTH_VIOLATION, SC ROOT, (zero))

    TEST_CASE_DCC_AND_PERM_EXCEPTION(5, ~(1 << PERM_PERMIT_STORE), CAUSE_PERMIT_STORE_VIOLATION, SC ROOT, (zero))
    TEST_CASE_DCC_AND_PERM_EXCEPTION(6, ~(1 << PERM_PERMIT_LOAD),  CAUSE_PERMIT_LOAD_VIOLATION,  LC c1, (zero))

    TEST_CASE_DCC_AND_PERM_NO_EXCEPTION(7, ~(1 << PERM_PERMIT_STORE), LC c1, (zero))
    TEST_CASE_DCC_AND_PERM_NO_EXCEPTION(8, ~(1 << PERM_PERMIT_LOAD),  SC ROOT, (zero))

    TEST_CASE_FREE(9)
        SET_BOUNDS_DCC(512)
        SEAL_DDC(123)
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, CAP_IDX_DDC, SC ROOT, (zero))

    TEST_CASE_LOAD(10, lw)
    TEST_CASE_LOAD_LENGTH_VIOLATION(11, lw, 4)
    TEST_CASE_LOAD_PERM_VIOLATION(12, lw)
    TEST_CASE_LOAD_SEAL_VIOLATION(13, lw)

    TEST_CASE_LOAD(14, lh)
    TEST_CASE_LOAD_LENGTH_VIOLATION(15, lh, 2)
    TEST_CASE_LOAD_PERM_VIOLATION(16, lh)
    TEST_CASE_LOAD_SEAL_VIOLATION(17, lh)

    TEST_CASE_LOAD(18, lb)
    TEST_CASE_LOAD_LENGTH_VIOLATION(19, lb, 1)
    TEST_CASE_LOAD_PERM_VIOLATION(20, lb)
    TEST_CASE_LOAD_SEAL_VIOLATION(21, lb)

    TEST_CASE_LOAD(22, lhu)
    TEST_CASE_LOAD_LENGTH_VIOLATION(23, lhu, 2)
    TEST_CASE_LOAD_PERM_VIOLATION(24, lhu)
    TEST_CASE_LOAD_SEAL_VIOLATION(25, lhu)

    TEST_CASE_LOAD(26, lbu)
    TEST_CASE_LOAD_LENGTH_VIOLATION(27, lbu, 1)
    TEST_CASE_LOAD_PERM_VIOLATION(28, lbu)
    TEST_CASE_LOAD_SEAL_VIOLATION(29, lbu)

    TEST_CASE_STORE(30, sw, lw, 0x12345678)
    TEST_CASE_STORE_LENGTH_VIOLATION(31, sw, 4)
    TEST_CASE_STORE_PERM_VIOLATION(32, sw)
    TEST_CASE_STORE_SEAL_VIOLATION(33, sw)

    TEST_CASE_STORE(34, sh, lh, 0x1234)
    TEST_CASE_STORE_LENGTH_VIOLATION(35, sh, 2)
    TEST_CASE_STORE_PERM_VIOLATION(36, sh)
    TEST_CASE_STORE_SEAL_VIOLATION(37, sh)

    TEST_CASE_STORE(38, sb, lb, 0x12)
    TEST_CASE_STORE_LENGTH_VIOLATION(39, sb, 1)
    TEST_CASE_STORE_PERM_VIOLATION(40, sb)
    TEST_CASE_STORE_SEAL_VIOLATION(41, sb)

    # Test that permission violations have priority over misalignment exceptions
    TEST_CASE_FREE(42)
        la t0, lw
        addi t0, t0, 1
        CSetAddr c1, ROOT, t0
        CSetBoundsImm c1, c1, 3
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, LW.cap t1, (c1))

#if 0
    # Test that store are not performed when an excpetion is raised
    # TODO this fails because of https://gitlab.com/ProteusCore/ProteusCore/-/issues/3
    TEST_CASE_FREE(43)
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
