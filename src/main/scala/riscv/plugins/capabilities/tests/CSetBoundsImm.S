#include "riscv_test.h"

#include "test_macros_cap.h"

#define SET_BOUNDS(cs, imm) \
    CSetBoundsImm c31, cs, imm; \

#define TEST_CSetBoundsImm_NO_CHANGE(testnum, cs, imm) \
    TEST_CASE_CAP_EQ(testnum, c31, cs, SET_BOUNDS(cs, imm))

#define TEST_CSetBoundsImm_NEW_BOUNDS_OFFSET(testnum, cs, imm, base, len, offset) \
    TEST_CASE_CAP_NEW_BOUNDS_OFFSET(testnum, c31, cs, base, len, offset, SET_BOUNDS(cs, imm))

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_CSetBoundsImm_NEW_BOUNDS_OFFSET(1, c1, 0x4,   0, 0x4,   0)
    TEST_CSetBoundsImm_NEW_BOUNDS_OFFSET(2, c1, 0xfff, 0, 0xfff, 0)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
