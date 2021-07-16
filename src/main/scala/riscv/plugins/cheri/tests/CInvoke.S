#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    TEST_CASE_START(1)
        la t0, test_func
        CSetOffset c1, ROOT, t0
        SEAL(c2, c1, 456)
        li t0, 0x100
        CSetOffset c3, ROOT, t0
        li t0, ~(1 << PERM_PERMIT_EXECUTE)
        CAndPerm c3, c3, t0
        SEAL(c4, c3, 456)
        CInvoke c2, c4
        j fail

    test_func:
        CSpecialR c5, pcc
        CHECK_CAP_EQ(c5, c1)
        CHECK_CAP_EQ(c31, c3)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
