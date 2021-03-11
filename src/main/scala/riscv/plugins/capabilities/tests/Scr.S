#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    # c1 contains a root capability during this whole test
    CSpecialR c1, ddc;

    TEST_CASE_CAP_EQ(1, c1, c2,
        CSpecialW mtdc, c1;
        CSpecialR c2, mtdc;
    )

    TEST_CASE_CAP_EQ(2, c1, c2,
        CSpecialW mscratchc, c1;
        CSpecialR c2, mscratchc;
    )

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
