#ifndef TEST_MACROS_CAP_H
#define TEST_MACROS_CAP_H

#include "test_macros.h"

#define CHECK_GETTER(getter, cr, correct_result) \
    getter x30, cr; \
    li  x29, MASK_XLEN(correct_result); \
    beq x29, x30, 1f; \
    RESTORE_SAFE_STATE; \
    j fail; \
1:

#define CHECK_TAG(cr, tag) \
    CHECK_GETTER(CGetTag, cr, tag);

#define CHECK_BOUNDS(cr, base, len) \
    CHECK_GETTER(CGetBase, cr, base); \
    CHECK_GETTER(CGetLen, cr, len);

#define CHECK_OFFSET(cr, offset) \
    CHECK_GETTER(CGetOffset, cr, offset);

#define CHECK_PERMS(cr, perms) \
    CHECK_GETTER(CGetPerm, cr, perms);

#define CHECK_TYPE(cr, type) \
    CHECK_GETTER(CGetType, cr, type);

#define CHECK_ADDR(cr, addr) \
    CHECK_GETTER(CGetAddr, cr, addr);

#define CHECK_CAP_NO_OFFSET(cr, tag, base, len, perms, type) \
    CHECK_TAG(cr, tag) \
    CHECK_BOUNDS(cr, base, len) \
    CHECK_PERMS(cr, perms) \
    CHECK_TYPE(cr, type)

#define CHECK_CAP(cr, tag, base, len, offset, perms, type) \
    CHECK_CAP_NO_OFFSET(cr, tag, base, len, perms, type) \
    CHECK_OFFSET(cr, offset)

#define CHECK_GETTER_EQ(getter, cd, cs) \
    getter x29, cs; \
    getter x30, cd; \
    beq x29, x30, 1f; \
    RESTORE_SAFE_STATE; \
    j fail; \
1:

#define CHECK_TAG_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetTag, cd, cs);

#define CHECK_BOUNDS_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetBase, cd, cs); \
    CHECK_GETTER_EQ(CGetLen, cd, cs);

#define CHECK_OFFSET_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetOffset, cd, cs);

#define CHECK_PERMS_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetPerm, cd, cs);

#define CHECK_TYPE_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetType, cd, cs);

#define CHECK_CAP_NEW_BOUNDS_OFFSET(cd, cs, base, len, offset) \
    CHECK_BOUNDS(cd, base, len); \
    CHECK_OFFSET(cd, offset); \
    CHECK_TAG_EQ(cd, cs); \
    CHECK_PERMS_EQ(cd, cs); \
    CHECK_TYPE_EQ(cd, cs);

#define CHECK_CAP_NEW_TYPE(cd, cs, type) \
    CHECK_TAG_EQ(cd, cs); \
    CHECK_BOUNDS_EQ(cd, cs); \
    CHECK_OFFSET_EQ(cd, cs); \
    CHECK_PERMS_EQ(cd, cs); \
    CHECK_TYPE(cd, type);

#define CHECK_CAP_EQ(cd, cs) \
    CHECK_TAG_EQ(cd, cs); \
    CHECK_BOUNDS_EQ(cd, cs); \
    CHECK_OFFSET_EQ(cd, cs); \
    CHECK_PERMS_EQ(cd, cs); \
    CHECK_TYPE_EQ(cd, cs);


#define TEST_CASE_CAP_NO_OFFSET(testnum, testcap, correct_tag, correct_base, correct_len, correct_perms, correct_type, code... ) \
test_ ## testnum: \
    code; \
    li  TESTNUM, testnum; \
    CHECK_CAP_NO_OFFSET(testcap, correct_tag, correct_base, correct_len, correct_perms, correct_type)

#define TEST_CASE_CAP(testnum, testcap, correct_tag, correct_base, correct_len, correct_offset, correct_perms, correct_type, code... ) \
test_ ## testnum: \
    code; \
    li  TESTNUM, testnum; \
    CHECK_CAP(testcap, correct_tag, correct_base, correct_len, correct_offset, correct_perms, correct_type)

#define TEST_CASE_NULL_CAP(testnum, testcap, code...) \
    TEST_CASE_CAP(testnum, testcap, 0, 0x0, 0x0, 0x0, 0b0, 0xffffffff, code)

#define TEST_CASE_ROOT_CAP_NO_OFFSET(testnum, testcap, code...) \
    TEST_CASE_CAP_NO_OFFSET(testnum, testcap, 1, 0x0, 0xffffffff, 0b011110111110, 0xffffffff, code)

#define TEST_CASE_ROOT_CAP(testnum, testcap, code...) \
    TEST_CASE_CAP(testnum, testcap, 1, 0x0, 0xffffffff, 0x0, 0b011110111110, 0xffffffff, code)

#define TEST_CASE_CAP_NEW_BOUNDS_OFFSET(testnum, cd, cs, base, len, offset, code...) \
test_ ## testnum: \
    code; \
    li  TESTNUM, testnum; \
    CHECK_CAP_NEW_BOUNDS_OFFSET(cd, cs, base, len, offset)

#define TEST_CASE_CAP_EQ(testnum, cd, cs, code...) \
test_ ## testnum: \
    code; \
    li  TESTNUM, testnum; \
    CHECK_CAP_EQ(cd, cs)

#define TEST_CASE_START(testnum) \
test_ ## testnum: \
    li TESTNUM, testnum; \

#define TEST_CASE_FREE(testnum, code...) \
    TEST_CASE_START(testnum) \
    code;

#define ROOT c30

#define INIT_ROOT_CAP CSpecialR ROOT, ddc

#define RESTORE_SAFE_STATE \
    CSpecialW ddc, ROOT; \
    la tp, trap_vector; \
    csrw mtvec, tp

#define EXPECT_EXCEPTION(cause, capidx, code...) \
    la tp, 999f; \
    csrw mtvec, tp; \
    code; \
    RESTORE_SAFE_STATE; \
    j fail; \
999: \
    RESTORE_SAFE_STATE; \
    csrr tp, mcause; \
    li t6, 10; \
    bne tp, t6, fail; \
    csrr tp, mccsr; \
    li t6, 0x0000ffe0; \
    and tp, tp, t6; \
    li t6, ((capidx << 10) | (cause << 5)); \
    bne tp, t6, fail;


#define TEST_EXPECT_EXCEPTION(testnum, cause, capidx, code...) \
    TEST_CASE_FREE(testnum, EXPECT_EXCEPTION(cause, capidx, code))

#define EXPECT_NO_EXCEPTION(code...) \
    la tp, 1f; \
    csrw mtvec, tp; \
    code; \
    RESTORE_SAFE_STATE; \
    j 2f; \
1: \
    RESTORE_SAFE_STATE; \
    j fail; \
2:

#define SET_BOUNDS_DCC(bounds) \
    li t0, 0x80000000; \
    CSetOffset c1, ROOT, t0; \
    CSetBounds c1, c1, bounds; \
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
    CSpecialR c1, ddc; \
    CAndPerm c1, c1, t0; \
    CSpecialW ddc, c1

#define TEST_CASE_DCC_AND_PERM_EXCEPTION(testnum, perm, cause, code...) \
    TEST_CASE_START(testnum); \
    SET_BOUNDS_DCC(512); \
    AND_PERM_DCC(perm); \
    EXPECT_EXCEPTION(cause, CAP_IDX_DDC, code)

#define TEST_CASE_DCC_AND_PERM_NO_EXCEPTION(testnum, perm, code...) \
    TEST_CASE_START(testnum); \
    SET_BOUNDS_DCC(512); \
    AND_PERM_DCC(perm); \
    EXPECT_NO_EXCEPTION(code)

#define SEAL(cd, cs, type) \
    li t0, type; \
    CSetOffset c29, ROOT, t0; \
    CSeal cd, cs, c29

#define SEAL_DDC(type) \
    CSpecialR c1, ddc; \
    SEAL(c1, c1, type); \
    CSpecialW ddc, c1

#define SEAL_ROOT(cd, type) SEAL(cd, ROOT, type)

#endif
