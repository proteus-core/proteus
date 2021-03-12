#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

#define CHECK_CAP_MASKED_PERM(cd, cs, perm_mask) \
    CHECK_TAG_EQ(cd, cs); \
    CHECK_BOUNDS_EQ(cd, cs); \
    CHECK_OFFSET_EQ(cd, cs); \
    CGetPerm t0, cs; \
    li t1, perm_mask; \
    and t0, t0, t1; \
    CGetPerm t1, cd; \
    bne t0, t1, fail;

#define TEST_AND_PERM(testnum, cs, mask) \
    TEST_CASE_FREE(testnum) \
    li t0, mask; \
    CAndPerm c31, cs, t0; \
    CHECK_CAP_MASKED_PERM(c31, cs, mask)

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_AND_PERM(1, c1, 0x0);
    TEST_AND_PERM(2, c1, 0xffffffff);
    TEST_AND_PERM(3, c1, (1 << PERM_PERMIT_LOAD));

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
