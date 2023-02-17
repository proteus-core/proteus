#include "riscv_test.h"

#include "test_macros_cap.h"
#include "cheri.h"

#define SET_DUMMY_TARGET(cap) \
    la t0, dummy_jump_target; \
    CSetOffset cap, ROOT, t0; \
    CSetBoundsImm cap, cap, 4


RVTEST_RV32U
RVTEST_CODE_BEGIN

    INIT_ROOT_CAP

    # Jump to non-executable capability
    TEST_CASE_START(1)
        SET_DUMMY_TARGET(c1)
        li t0, ~(1 << PERM_PERMIT_EXECUTE)
        CAndPerm c1, c1, t0
        EXPECT_EXCEPTION(CAUSE_PERMIT_EXECUTE_VIOLATION, 1, CJALR c1)

    # Jump to capability with length 0
    TEST_CASE_START(2)
        SET_DUMMY_TARGET(c1)
        CSetBoundsImm c1, c1, 0
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, CJALR c1)

    # Jump to capability with length 3
    TEST_CASE_START(3)
        SET_DUMMY_TARGET(c1)
        CSetBoundsImm c1, c1, 3
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, 1, CJALR c1)

    # Jump to capability with unset tag
    TEST_CASE_START(4)
        SET_DUMMY_TARGET(c1)
        CClearTag c1, c1
        EXPECT_EXCEPTION(CAUSE_TAG_VIOLATION, 1, CJALR c1)

    # Overflow execution out of capability range (4 bytes)
    TEST_CASE_START(5)
        la t0, 1f
        CSetBounds c1, ROOT, t0
        la t0, 2f
        CSetOffset c1, c1, t0
        CJALR c1
    2:
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, CAP_IDX_PCC, 1: j fail)

    # Overflow execution out of capability range (1 byte)
    TEST_CASE_START(6)
        la t0, 1f
        addi t0, t0, 3
        CSetBounds c1, ROOT, t0
        la t0, 2f
        CSetOffset c1, c1, t0
        CJALR c1
    2:
        EXPECT_EXCEPTION(CAUSE_LENGTH_VIOLATION, CAP_IDX_PCC, 1: j fail)

    # mret while mepcc has invalid capability
    TEST_CASE_START(7)
        la t0, 1f
        CSetOffset c1, ROOT, t0
        CClearTag c1, c1
        CSpecialW mepcc, c1

        la t0, 2f
        CSetOffset c2, ROOT, t0
        CSpecialW mtcc, c2

        mret
    1:
        j fail
    2:

    # Jump to sealed capability
    TEST_CASE_START(8)
        SET_DUMMY_TARGET(c1)
        SEAL(c1, c1, 456)
        EXPECT_EXCEPTION(CAUSE_SEAL_VIOLATION, 1, CJALR c1)

    TEST_PASSFAIL

    dummy_jump_target:
        CRET

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
