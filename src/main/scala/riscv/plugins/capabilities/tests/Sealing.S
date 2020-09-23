#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    # Check otype is correctly set when sealing
    TEST_CASE_START(1)
        SEAL_ROOT(c1, 100)
        CHECK_CAP_NEW_TYPE(c1, ROOT, 100)

    # Check unsealing a capability results in original capability
    TEST_CASE_START(2)
        SEAL_ROOT(c1, 1234)
        li t0, 1234
        CSetOffset c2, ROOT, t0
        CUnseal c3, c1, c2
        CHECK_CAP_EQ(c3, ROOT)

    # Sealing without permission
    TEST_CASE_START(3)
        li t0, ~(1 << PERM_PERMIT_SEAL)
        CAndPerm c1, ROOT, t0
        EXPECT_EXCEPTION(CAUSE_PERMIT_SEAL_VIOLATION, 1, CSeal c2, ROOT, c1)

    # Unsealing without permission
    TEST_CASE_START(4)
        SEAL_ROOT(c1, 1234)
        li t0, 1234
        CSetOffset c2, ROOT, t0
        li t0, ~(1 << PERM_PERMIT_UNSEAL)
        CAndPerm c2, c2, t0
        EXPECT_EXCEPTION(CAUSE_PERMIT_UNSEAL_VIOLATION, 2, CUnseal c3, c1, c2)

    # Unsealing using wrong otype
    TEST_CASE_START(5)
        SEAL_ROOT(c1, 1234)
        li t0, 5678
        CSetOffset c2, ROOT, t0
        EXPECT_EXCEPTION(CAUSE_TYPE_VIOLATION, 2, CUnseal c3, c1, c2)

    # Unsealing unsealed capability
    TEST_CASE_START(6)
        CMove c1, ROOT
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, 1, CUnseal c2, c1, ROOT)

    TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
