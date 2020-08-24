#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

#define TEST_CIncOffsetImm(testnum, cs, inc, result) \
    TEST_CASE_FREE(testnum) \
    CIncOffset c30, cs, inc; \
    CHECK_OFFSET(c30, result); \
    CHECK_TAG_EQ(cs, c30); \
    CHECK_BOUNDS_EQ(cs, c30); \
    CHECK_PERMS_EQ(cs, c30)

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_CIncOffsetImm(1, c1, 7, 7)
    TEST_CIncOffsetImm(2, c1, -1, 0xffffffff)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
