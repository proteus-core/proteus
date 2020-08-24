#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_CASE_FREE(1)
        li t0, 7
        CSetOffset c2, c1, t0
        CHECK_OFFSET(c2, 7)
        CHECK_TAG_EQ(c1, c2)
        CHECK_BOUNDS_EQ(c1, c2)
        CHECK_PERMS_EQ(c1, c2)

    TEST_CASE_FREE(2)
        CSetBounds c2, c1, zero
        li t0, 7
        CSetOffset c3, c2, t0
        CHECK_OFFSET(c3, 7)
        CHECK_TAG_EQ(c2, c3)
        CHECK_BOUNDS_EQ(c2, c3)
        CHECK_PERMS_EQ(c2, c3)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
