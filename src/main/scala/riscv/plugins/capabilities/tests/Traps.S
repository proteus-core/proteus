#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"


RVTEST_RV32U
RVTEST_CODE_BEGIN

    TEST_CASE_START(1)
        CSpecialR c1, pcc
        la t0, trap_vec
        CSetOffset c1, c1, t0
        CSetBoundsImm c1, c1, 4 * 4
        CSpecialW mtcc, c1

        ecall
        j fail
        j test_end

    trap_vec:
        CSpecialR c2, mepcc
        CIncOffsetImm c2, c2, 8
        CSpecialW mepcc, c2
        mret
    test_end:


    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
