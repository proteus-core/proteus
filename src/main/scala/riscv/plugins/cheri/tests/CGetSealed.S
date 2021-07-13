#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    TEST_CASE_START(1)
        CGetSealed t0, ROOT
        bne t0, zero, fail

    TEST_CASE_START(2)
        SEAL(c1, ROOT, 123)
        CGetSealed t0, c1
        li t1, 1
        bne t0, t1, fail

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
