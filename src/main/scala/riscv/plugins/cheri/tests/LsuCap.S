#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

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

    TEST_CASE_FREE(10)
        la t0, scratch
        CSetAddr c1, ROOT, t0
        SC.cap ROOT, (c1)
        LC.cap c2, (c1)
        CHECK_CAP_EQ(c2, ROOT)

    TEST_CASE_FREE(11)
        la t0, scratch
        CSetAddr c1, ROOT, t0
        CSetBoundsImm c1, c1, 3
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, SC.cap ROOT, (c1))

    TEST_CASE_FREE(12)
        la t0, scratch
        CSetAddr c1, ROOT, t0
        CSetBoundsImm c1, c1, 3
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, LC.cap ROOT, (c1))

    TEST_CASE_FREE(13)
        la t0, scratch
        CSetAddr c1, ROOT, t0
        li t0, ~(1 << PERM_PERMIT_STORE)
        CAndPerm c1, c1, t0
        EXPECT_EXCEPTION(CAUSE_PERMIT_STORE_VIOLATION, 1, SC.cap ROOT, (c1))

    TEST_CASE_FREE(14)
        la t0, scratch
        CSetAddr c1, ROOT, t0
        li t0, ~(1 << PERM_PERMIT_LOAD)
        CAndPerm c1, c1, t0
        EXPECT_EXCEPTION(CAUSE_PERMIT_LOAD_VIOLATION, 1, LC.cap ROOT, (c1))

    TEST_CASE_FREE(15)
        la t0, scratch
        CSetAddr c1, ROOT, t0
        SEAL(c1, c1, 123)
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, 1, LC.cap ROOT, (c1))
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, 1, SC.cap ROOT, (c1))


    TEST_PASSFAIL

RVTEST_CODE_END

RVTEST_DATA_BEGIN

    .align 8
scratch:
    .fill 8, 1, 0

    TEST_DATA

RVTEST_DATA_END
