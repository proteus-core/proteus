#include "riscv_test.h"

#include "test_macros_cap.h"
#include "exceptions.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    TEST_CASE_START(1)

        CSetBounds c1, ROOT, zero
        CSpecialW ddc, c1

        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, CAP_IDX_DDC,
            sb zero, (zero)
        )

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
