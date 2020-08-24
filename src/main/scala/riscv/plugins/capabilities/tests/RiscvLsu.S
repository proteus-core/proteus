#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

#define SET_BOUNDS_DCC(bounds) \
    CSetBounds c1, ROOT, bounds; \
    CSpecialW ddc, c1

#define TEST_CASE_DCC_BOUNDS_EXCEPTION(testnum, bounds, cause, code...) \
    TEST_CASE_START(testnum); \
    SET_BOUNDS_DCC(bounds); \
    EXPECT_EXCEPTION(cause, CAP_IDX_DDC, code)

#define TEST_CASE_DCC_BOUNDS_NO_EXCEPTION(testnum, bounds, code...) \
    TEST_CASE_START(testnum); \
    SET_BOUNDS_DCC(bounds); \
    EXPECT_NO_EXCEPTION(code)

#define AND_PERM_DCC(perm) \
    li t0, perm; \
    CAndPerm c1, ROOT, t0; \
    CSpecialW ddc, c1

#define TEST_CASE_DCC_AND_PERM_EXCEPTION(testnum, perm, cause, code...) \
    TEST_CASE_START(testnum); \
    AND_PERM_DCC(perm); \
    EXPECT_EXCEPTION(cause, CAP_IDX_DDC, code)

#define TEST_CASE_DCC_AND_PERM_NO_EXCEPTION(testnum, perm, code...) \
    TEST_CASE_START(testnum); \
    AND_PERM_DCC(perm); \
    EXPECT_NO_EXCEPTION(code)

RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    TEST_CASE_DCC_BOUNDS_EXCEPTION(1, 0, CAUSE_LENGTH_VIOLATION, sb zero, (zero))
    TEST_CASE_DCC_BOUNDS_EXCEPTION(2, 1, CAUSE_LENGTH_VIOLATION, sh zero, (zero))
    TEST_CASE_DCC_BOUNDS_EXCEPTION(3, 3, CAUSE_LENGTH_VIOLATION, sw zero, (zero))
    TEST_CASE_DCC_BOUNDS_EXCEPTION(4, 0, CAUSE_LENGTH_VIOLATION, lb zero, (zero))
    TEST_CASE_DCC_BOUNDS_EXCEPTION(5, 1, CAUSE_LENGTH_VIOLATION, lh zero, (zero))
    TEST_CASE_DCC_BOUNDS_EXCEPTION(6, 3, CAUSE_LENGTH_VIOLATION, lw zero, (zero))

    TEST_CASE_DCC_BOUNDS_NO_EXCEPTION(7,  1, sb zero, (zero))
    TEST_CASE_DCC_BOUNDS_NO_EXCEPTION(8,  2, sh zero, (zero))
    TEST_CASE_DCC_BOUNDS_NO_EXCEPTION(9,  4, sw zero, (zero))
    TEST_CASE_DCC_BOUNDS_NO_EXCEPTION(10, 1, lb zero, (zero))
    TEST_CASE_DCC_BOUNDS_NO_EXCEPTION(11, 2, lh zero, (zero))
    TEST_CASE_DCC_BOUNDS_NO_EXCEPTION(12, 4, lw zero, (zero))

    TEST_CASE_DCC_AND_PERM_EXCEPTION(13, ~(1 << PERM_PERMIT_STORE), CAUSE_PERMIT_STORE_VIOLATION, sb zero, (zero))
    TEST_CASE_DCC_AND_PERM_EXCEPTION(14, ~(1 << PERM_PERMIT_LOAD),  CAUSE_PERMIT_LOAD_VIOLATION,  lb zero, (zero))

    TEST_CASE_DCC_AND_PERM_NO_EXCEPTION(15, ~(1 << PERM_PERMIT_STORE), lb zero, (zero))
    TEST_CASE_DCC_AND_PERM_NO_EXCEPTION(16, ~(1 << PERM_PERMIT_LOAD),  sb zero, (zero))

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
