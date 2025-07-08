#include "../riscv_test.h"
#include "../riscv-tests/isa/macros/scalar/test_macros.h"
#define __riscv_xlen 32

RVTEST_RV32U
RVTEST_CODE_BEGIN

	TEST_RR_OP(1, czero.eqz, 0, 9, 0)
	TEST_RR_OP(2, czero.eqz, 8, 8, 1)
	TEST_RR_OP(3, czero.nez, 7, 7, 0)
	TEST_RR_OP(4, czero.nez, 0, 6, 1)

	TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
