#ifndef TEST_MACROS_CAP_H
#define TEST_MACROS_CAP_H

#include "test_macros.h"

#define CHECK_GETTER(getter, cr, correct_result) \
    getter x30, cr; \
    li  x29, MASK_XLEN(correct_result); \
    bne x29, x30, fail;

#define CHECK_TAG(cr, tag) \
    CHECK_GETTER(CGetTag, cr, tag);

#define CHECK_BOUNDS(cr, base, len) \
    CHECK_GETTER(CGetBase, cr, base); \
    CHECK_GETTER(CGetLen, cr, len);

#define CHECK_OFFSET(cr, offset) \
    CHECK_GETTER(CGetOffset, cr, offset);

#define CHECK_PERMS(cr, perms) \
    CHECK_GETTER(CGetPerm, cr, perms);

#define CHECK_CAP_NO_OFFSET(cr, tag, base, len, perms) \
    CHECK_TAG(cr, tag) \
    CHECK_BOUNDS(cr, base, len) \
    CHECK_PERMS(cr, perms)

#define CHECK_CAP(cr, tag, base, len, offset, perms) \
    CHECK_CAP_NO_OFFSET(cr, tag, base, len, perms) \
    CHECK_OFFSET(cr, offset)

#define CHECK_GETTER_EQ(getter, cd, cs) \
    getter x29, cs; \
    getter x30, cd; \
    bne x29, x30, fail;

#define CHECK_TAG_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetTag, cd, cs);

#define CHECK_BOUNDS_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetBase, cd, cs); \
    CHECK_GETTER_EQ(CGetLen, cd, cs);

#define CHECK_OFFSET_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetOffset, cd, cs);

#define CHECK_PERMS_EQ(cd, cs) \
    CHECK_GETTER_EQ(CGetPerm, cd, cs);

#define CHECK_CAP_NEW_BOUNDS_OFFSET(cd, cs, base, len, offset) \
    CHECK_BOUNDS(cd, base, len); \
    CHECK_OFFSET(cd, offset); \
    CHECK_TAG_EQ(cd, cs); \
    CHECK_PERMS_EQ(cd, cs);

#define CHECK_CAP_EQ(cd, cs) \
    CHECK_TAG_EQ(cd, cs); \
    CHECK_BOUNDS_EQ(cd, cs); \
    CHECK_OFFSET_EQ(cd, cs); \
    CHECK_PERMS_EQ(cd, cs);


#define TEST_CASE_CAP_NO_OFFSET(testnum, testcap, correct_tag, correct_base, correct_len, correct_perms, code... ) \
test_ ## testnum: \
    code; \
    li  TESTNUM, testnum; \
    CHECK_CAP_NO_OFFSET(testcap, correct_tag, correct_base, correct_len, correct_perms)

#define TEST_CASE_CAP(testnum, testcap, correct_tag, correct_base, correct_len, correct_offset, correct_perms, code... ) \
test_ ## testnum: \
    code; \
    li  TESTNUM, testnum; \
    CHECK_CAP(testcap, correct_tag, correct_base, correct_len, correct_offset, correct_perms)

#define TEST_CASE_NULL_CAP(testnum, testcap, code...) \
    TEST_CASE_CAP(testnum, testcap, 0, 0x0, 0x0, 0x0, 0b0, code)

#define TEST_CASE_ROOT_CAP_NO_OFFSET(testnum, testcap, code...) \
    TEST_CASE_CAP_NO_OFFSET(testnum, testcap, 1, 0x0, 0xffffffff, 0b010000111110, code)

#define TEST_CASE_ROOT_CAP(testnum, testcap, code...) \
    TEST_CASE_CAP(testnum, testcap, 1, 0x0, 0xffffffff, 0x0, 0b010000111110, code)

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

#define ROOT c31

#define INIT_ROOT_CAP CSpecialR ROOT, ddc

#define RESTORE_SAFE_STATE \
    CSpecialW ddc, ROOT; \
    la tp, trap_vector; \
    csrw mtvec, tp

#define EXPECT_EXCEPTION(cause, capidx, code...) \
    la tp, 1f; \
    csrw mtvec, tp; \
    code; \
    RESTORE_SAFE_STATE; \
    j fail; \
1: \
    RESTORE_SAFE_STATE; \
    csrr tp, mcause; \
    li t6, 10; \
    bne tp, t6, fail; \
    csrr tp, mccsr; \
    li t6, 0x0000ffe0; \
    and tp, tp, t6; \
    li t6, ((capidx << 10) | (cause << 5)); \
    bne tp, t6, fail;


#define TEST_EXPECT_EXCEPTION(testnum, cause, code...) \
    TEST_CASE_FREE(testnum, EXPECT_EXCEPTION(cause, code))

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

#endif
