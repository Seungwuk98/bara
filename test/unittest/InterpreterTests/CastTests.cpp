#include "InterpreterTestUtils.h"

namespace bara {

TEST(INTERPRETER_TEST_SUITE, CastTests) {
  InterpreterTests tests;

  auto floatToInt = tests.eval("int(1.0)");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatToInt->isa<IntegerValue>());
  ASSERT_EQ(floatToInt->cast<IntegerValue>()->getValue(), 1);

  auto intToFloat = tests.eval("float(1)");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intToFloat->isa<FloatValue>());
  ASSERT_EQ(intToFloat->cast<FloatValue>()->getValue(), APFloat(1.0));

  auto intToBool = tests.eval("bool(1)");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intToBool->isa<BoolValue>());
  ASSERT_EQ(intToBool->cast<BoolValue>()->getValue(), true);

  auto boolToInt = tests.eval("int(true)");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(boolToInt->isa<IntegerValue>());
  ASSERT_EQ(boolToInt->cast<IntegerValue>()->getValue(), 1);

  auto boolToInt2 = tests.eval("int(false)");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(boolToInt2->isa<IntegerValue>());
  ASSERT_EQ(boolToInt2->cast<IntegerValue>()->getValue(), 0);

  auto strToInt = tests.eval("int(\"1\")");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(strToInt->isa<IntegerValue>());
  ASSERT_EQ(strToInt->cast<IntegerValue>()->getValue(), 1);

  auto strToFloat = tests.eval("float(\"1.0\")");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(strToFloat->isa<FloatValue>());
  ASSERT_EQ(strToFloat->cast<FloatValue>()->getValue(), APFloat(1.0));

  auto strToBool = tests.eval("bool(\"true\")");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(strToBool->isa<BoolValue>());
  ASSERT_EQ(strToBool->cast<BoolValue>()->getValue(), true);

  auto strToBool2 = tests.eval("bool(\"false\")");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(strToBool2->isa<BoolValue>());
  ASSERT_EQ(strToBool2->cast<BoolValue>()->getValue(), false);
}

} // namespace bara
