#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_CASE_FREE(1)
        LC c2, (zero)
        CHECK_TAG(c2, 0)

    TEST_CASE_FREE(2)
        SC c1, (zero)
        LC c2, (zero)
        CHECK_CAP_EQ(c1, c2)

    TEST_CASE_FREE(3)
        SC c1, (zero)
        sb zero, 1(zero)
        LC c2, (zero)
        CHECK_TAG(c2, 0)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
