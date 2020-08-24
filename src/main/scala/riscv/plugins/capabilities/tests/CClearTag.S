#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_CASE_FREE(1)
        CClearTag c2, c1
        CHECK_TAG(c2, 0)
        CHECK_BOUNDS_EQ(c1, c2)
        CHECK_OFFSET_EQ(c1, c2)
        CHECK_PERMS_EQ(c1, c2)

    TEST_CASE_FREE(2)
        CClearTag c2, c1
        CClearTag c3, c2
        CHECK_CAP_EQ(c2, c3)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
