#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

#define SET_BOUNDS(cs, rs_val) \
    li x1, rs_val; \
    CSetBounds c31, cs, x1; \

#define TEST_CSetBounds_NO_CHANGE(testnum, cs, rs_val) \
    TEST_CASE_CAP_EQ(testnum, c31, cs, SET_BOUNDS(cs, rs_val))

#define TEST_CSetBounds_NEW_BOUNDS_OFFSET(testnum, cs, rs_val, base, len, offset) \
    TEST_CASE_CAP_NEW_BOUNDS_OFFSET(testnum, c31, cs, base, len, offset, SET_BOUNDS(cs, rs_val))

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_CSetBounds_NO_CHANGE(1, c1, -1)
    TEST_CSetBounds_NEW_BOUNDS_OFFSET(2, c1, 4, 0, 4, 0)

    TEST_EXPECT_EXCEPTION(3, CAUSE_TAG_VIOLATION, 0, CSetBounds c0, c0, zero)

    TEST_CASE_FREE(4)
        CSetBounds c2, c1, zero
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 2, CSetBounds c0, c2, 1)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
