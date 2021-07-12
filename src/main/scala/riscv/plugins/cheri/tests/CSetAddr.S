#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    # Normal case
    TEST_CASE_FREE(1)
        li t0, 7
        CSetAddr c2, ROOT, t0
        CHECK_ADDR(c2, 7)
        CHECK_TAG_EQ(ROOT, c2)
        CHECK_PERMS_EQ(ROOT, c2)

    # Address below base
    TEST_CASE_FREE(2)
        li t0, 32
        CSetOffset c2, ROOT, t0
        CSetBounds c2, c2, t0
        li t0, 7
        CSetAddr c3, c2, t0
        CHECK_ADDR(c3, 7)
        CHECK_TAG(c3, 0)
        CHECK_PERMS_EQ(c2, c3)

    # Address above top
    TEST_CASE_FREE(3)
        li t0, 32
        CSetBounds c2, ROOT, t0
        li t0, 64
        CSetAddr c3, c2, t0
        CHECK_ADDR(c3, 64)
        CHECK_TAG(c3, 0)
        CHECK_PERMS_EQ(c2, c3)

    # Sealed source
    TEST_CASE_FREE(4)
        CSeal c2, ROOT, ROOT
        li t0, 64
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, 2, CSetAddr c3, c2, t0)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
