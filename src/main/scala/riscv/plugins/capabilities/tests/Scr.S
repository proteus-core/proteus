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

    # test exception when PERM_ACCESS_SYSTEM_REGISTERS is not set
    TEST_CASE_START(3)
        # to unset PERM_ACCESS_SYSTEM_REGISTERS on PCC, we have to create the
        # new capability and jump to it.
        CSpecialR c2, pcc
        li t0, ~(1 << PERM_ACCESS_SYSTEM_REGISTERS)
        CAndPerm c2, c2, t0
        la t0, 1f
        CSetOffset c2, c2, t0
        CJALR c2
    1:
        EXPECT_EXCEPTION(CAUSE_ACCESS_SYSTEM_REGISTERS_VIOLATION, CAP_IDX_MTDC,
            CSpecialR c3, mtdc
        )

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
