#include "InterpreterTestUtils.h"
#include "bara/interpreter/Value.h"

namespace bara {

TEST(INTERPRETER_TEST_SUITE, AddTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 + 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 3);

  auto floatVal = tests.eval("1.0 + 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<FloatValue>());
  ASSERT_EQ(floatVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "3.0"));

  auto mixedVal = tests.eval("1 + 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<FloatValue>());
  ASSERT_EQ(mixedVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "3.0"));

  auto mixedVal2 = tests.eval("1 + true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<IntegerValue>());
  ASSERT_EQ(mixedVal2->cast<IntegerValue>()->getValue(), 2);

  auto mixedVal3 = tests.eval("1 + false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal3->isa<IntegerValue>());
  ASSERT_EQ(mixedVal3->cast<IntegerValue>()->getValue(), 1);

  auto mixedVal4 = tests.eval("true + true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal4->isa<IntegerValue>());
  ASSERT_EQ(mixedVal4->cast<IntegerValue>()->getValue(), 2);

  auto mixedVal5 = tests.eval("true + false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal5->isa<IntegerValue>());
  ASSERT_EQ(mixedVal5->cast<IntegerValue>()->getValue(), 1);

  auto mixedVal6 = tests.eval("false + false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal6->isa<IntegerValue>());
  ASSERT_EQ(mixedVal6->cast<IntegerValue>()->getValue(), 0);

  auto strVal = tests.eval("\"hello\" + \" world\"");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(strVal->isa<StringValue>());
  ASSERT_EQ(strVal->cast<StringValue>()->getValue().str(), "hello world");

  auto listVal = tests.eval("[1, 2] + [3, 4]");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(listVal->isa<ListValue>());
  auto list = listVal->cast<ListValue>();
  ASSERT_EQ(list->size(), 4);
  ASSERT_TRUE(list->get(0)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(0)->get()->cast<IntegerValue>()->getValue(), 1);
  ASSERT_TRUE(list->get(1)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(1)->get()->cast<IntegerValue>()->getValue(), 2);
  ASSERT_TRUE(list->get(2)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(2)->get()->cast<IntegerValue>()->getValue(), 3);
  ASSERT_TRUE(list->get(3)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(3)->get()->cast<IntegerValue>()->getValue(), 4);
}

TEST(INTERPRETER_TEST_SUITE, SubTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 - 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), -1);

  auto floatVal = tests.eval("1.0 - 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<FloatValue>());
  ASSERT_EQ(floatVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "-1.0"));

  auto mixedVal = tests.eval("1 - 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<FloatValue>());
  ASSERT_EQ(mixedVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "-1.0"));

  auto mixedVal2 = tests.eval("1 - true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<IntegerValue>());
  ASSERT_EQ(mixedVal2->cast<IntegerValue>()->getValue(), 0);

  auto mixedVal3 = tests.eval("1 - false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal3->isa<IntegerValue>());
  ASSERT_EQ(mixedVal3->cast<IntegerValue>()->getValue(), 1);

  auto mixedVal4 = tests.eval("true - true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal4->isa<IntegerValue>());
  ASSERT_EQ(mixedVal4->cast<IntegerValue>()->getValue(), 0);

  auto mixedVal5 = tests.eval("true - false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal5->isa<IntegerValue>());
  ASSERT_EQ(mixedVal5->cast<IntegerValue>()->getValue(), 1);

  auto mixedVal6 = tests.eval("false - false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal6->isa<IntegerValue>());
  ASSERT_EQ(mixedVal6->cast<IntegerValue>()->getValue(), 0);
}

TEST(INTERPRETER_TEST_SUITE, MulTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 * 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 2);

  auto floatVal = tests.eval("1.0 * 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<FloatValue>());
  ASSERT_EQ(floatVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "2.0"));

  auto mixedVal = tests.eval("1 * 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<FloatValue>());
  ASSERT_EQ(mixedVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "2.0"));

  auto mixedVal2 = tests.eval("1 * true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<IntegerValue>());
  ASSERT_EQ(mixedVal2->cast<IntegerValue>()->getValue(), 1);

  auto mixedVal3 = tests.eval("1 * false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal3->isa<IntegerValue>());
  ASSERT_EQ(mixedVal3->cast<IntegerValue>()->getValue(), 0);

  auto mixedVal4 = tests.eval("true * true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal4->isa<IntegerValue>());
  ASSERT_EQ(mixedVal4->cast<IntegerValue>()->getValue(), 1);

  auto mixedVal5 = tests.eval("true * false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal5->isa<IntegerValue>());
  ASSERT_EQ(mixedVal5->cast<IntegerValue>()->getValue(), 0);

  auto mixedVal6 = tests.eval("false * false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal6->isa<IntegerValue>());
  ASSERT_EQ(mixedVal6->cast<IntegerValue>()->getValue(), 0);

  auto strVal = tests.eval("\"hello\" * 3");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(strVal->isa<StringValue>());
  ASSERT_EQ(strVal->cast<StringValue>()->getValue().str(), "hellohellohello");

  auto listVal = tests.eval("[1, 2] * 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(listVal->isa<ListValue>());
  auto list = listVal->cast<ListValue>();
  ASSERT_EQ(list->size(), 4);
  ASSERT_TRUE(list->get(0)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(0)->get()->cast<IntegerValue>()->getValue(), 1);
  ASSERT_TRUE(list->get(1)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(1)->get()->cast<IntegerValue>()->getValue(), 2);
  ASSERT_TRUE(list->get(2)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(2)->get()->cast<IntegerValue>()->getValue(), 1);
  ASSERT_TRUE(list->get(3)->get()->isa<IntegerValue>());
  ASSERT_EQ(list->get(3)->get()->cast<IntegerValue>()->getValue(), 2);
}

TEST(INTERPRETER_TEST_SUITE, DivTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 / 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 0);

  auto floatVal = tests.eval("1.0 / 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<FloatValue>());
  ASSERT_EQ(floatVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "0.5"));

  auto mixedVal = tests.eval("1 / 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<FloatValue>());
  ASSERT_EQ(mixedVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "0.5"));

  auto mixedVal2 = tests.eval("1.0 / 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<FloatValue>());
  ASSERT_EQ(mixedVal2->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "0.5"));
}

TEST(INTERPRETER_TEST_SUITE, ModTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 % 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 1);

  auto intVal2 = tests.eval("-1 % 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<IntegerValue>());
  ASSERT_EQ(intVal2->cast<IntegerValue>()->getValue(), -1);
}

TEST(INTERPRETER_TEST_SUITE, LtTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 < 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<BoolValue>());
  ASSERT_EQ(intVal->cast<BoolValue>()->getValue(), true);

  auto intVal2 = tests.eval("1 < 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<BoolValue>());
  ASSERT_EQ(intVal2->cast<BoolValue>()->getValue(), false);

  auto intVal3 = tests.eval("1 < 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<BoolValue>());
  ASSERT_EQ(intVal3->cast<BoolValue>()->getValue(), false);

  auto intVal4 = tests.eval("-2 > -1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<BoolValue>());
  ASSERT_EQ(intVal4->cast<BoolValue>()->getValue(), false);

  auto floatVal = tests.eval("1.0 < 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<BoolValue>());
  ASSERT_EQ(floatVal->cast<BoolValue>()->getValue(), true);

  auto floatVal2 = tests.eval("1.0 < 1.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal2->isa<BoolValue>());
  ASSERT_EQ(floatVal2->cast<BoolValue>()->getValue(), false);

  auto floatVal3 = tests.eval("1.0 < 0.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal3->isa<BoolValue>());
  ASSERT_EQ(floatVal3->cast<BoolValue>()->getValue(), false);

  auto mixedVal = tests.eval("1 < 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<BoolValue>());
  ASSERT_EQ(mixedVal->cast<BoolValue>()->getValue(), true);

  auto mixedVal2 = tests.eval("1.0 < 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<BoolValue>());
  ASSERT_EQ(mixedVal2->cast<BoolValue>()->getValue(), true);
}

TEST(INTERPRETER_TEST_SUITE, LeTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 <= 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<BoolValue>());
  ASSERT_EQ(intVal->cast<BoolValue>()->getValue(), true);

  auto intVal2 = tests.eval("1 <= 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<BoolValue>());
  ASSERT_EQ(intVal2->cast<BoolValue>()->getValue(), true);

  auto intVal3 = tests.eval("1 <= 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<BoolValue>());
  ASSERT_EQ(intVal3->cast<BoolValue>()->getValue(), false);

  auto intVal4 = tests.eval("-2 <= -1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<BoolValue>());
  ASSERT_EQ(intVal4->cast<BoolValue>()->getValue(), true);

  auto floatVal = tests.eval("1.0 <= 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<BoolValue>());
  ASSERT_EQ(floatVal->cast<BoolValue>()->getValue(), true);

  auto floatVal2 = tests.eval("1.0 <= 1.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal2->isa<BoolValue>());
  ASSERT_EQ(floatVal2->cast<BoolValue>()->getValue(), true);

  auto floatVal3 = tests.eval("1.0 <= 0.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal3->isa<BoolValue>());
  ASSERT_EQ(floatVal3->cast<BoolValue>()->getValue(), false);

  auto mixedVal = tests.eval("1 <= 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<BoolValue>());
  ASSERT_EQ(mixedVal->cast<BoolValue>()->getValue(), true);

  auto mixedVal2 = tests.eval("1.0 <= 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<BoolValue>());
  ASSERT_EQ(mixedVal2->cast<BoolValue>()->getValue(), true);

  auto mixedVal3 = tests.eval("1 <= 1.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal3->isa<BoolValue>());
  ASSERT_EQ(mixedVal3->cast<BoolValue>()->getValue(), true);
}

TEST(INTERPRETER_TEST_SUITE, GtTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 > 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<BoolValue>());
  ASSERT_EQ(intVal->cast<BoolValue>()->getValue(), false);

  auto intVal2 = tests.eval("1 > 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<BoolValue>());
  ASSERT_EQ(intVal2->cast<BoolValue>()->getValue(), false);

  auto intVal3 = tests.eval("1 > 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<BoolValue>());
  ASSERT_EQ(intVal3->cast<BoolValue>()->getValue(), true);

  auto intVal4 = tests.eval("-2 > -1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<BoolValue>());
  ASSERT_EQ(intVal4->cast<BoolValue>()->getValue(), false);

  auto floatVal = tests.eval("1.0 > 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<BoolValue>());
  ASSERT_EQ(floatVal->cast<BoolValue>()->getValue(), false);

  auto floatVal2 = tests.eval("1.0 > 1.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal2->isa<BoolValue>());
  ASSERT_EQ(floatVal2->cast<BoolValue>()->getValue(), false);

  auto floatVal3 = tests.eval("1.0 > 0.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal3->isa<BoolValue>());
  ASSERT_EQ(floatVal3->cast<BoolValue>()->getValue(), true);

  auto mixedVal = tests.eval("1 > 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<BoolValue>());
  ASSERT_EQ(mixedVal->cast<BoolValue>()->getValue(), false);

  auto mixedVal2 = tests.eval("1.0 > 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<BoolValue>());
  ASSERT_EQ(mixedVal2->cast<BoolValue>()->getValue(), false);
}

TEST(INTERPRETER_TEST_SUITE, GeTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 >= 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<BoolValue>());
  ASSERT_EQ(intVal->cast<BoolValue>()->getValue(), false);

  auto intVal2 = tests.eval("1 >= 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<BoolValue>());
  ASSERT_EQ(intVal2->cast<BoolValue>()->getValue(), true);

  auto intVal3 = tests.eval("1 >= 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<BoolValue>());
  ASSERT_EQ(intVal3->cast<BoolValue>()->getValue(), true);

  auto intVal4 = tests.eval("-2 >= -1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<BoolValue>());
  ASSERT_EQ(intVal4->cast<BoolValue>()->getValue(), false);

  auto floatVal = tests.eval("1.0 >= 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<BoolValue>());
  ASSERT_EQ(floatVal->cast<BoolValue>()->getValue(), false);

  auto floatVal2 = tests.eval("1.0 >= 1.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal2->isa<BoolValue>());
  ASSERT_EQ(floatVal2->cast<BoolValue>()->getValue(), true);

  auto floatVal3 = tests.eval("1.0 >= 0.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal3->isa<BoolValue>());
  ASSERT_EQ(floatVal3->cast<BoolValue>()->getValue(), true);

  auto mixedVal = tests.eval("1 >= 2.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<BoolValue>());
  ASSERT_EQ(mixedVal->cast<BoolValue>()->getValue(), false);

  auto mixedVal2 = tests.eval("1.0 >= 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<BoolValue>());
  ASSERT_EQ(mixedVal2->cast<BoolValue>()->getValue(), false);

  auto mixedVal3 = tests.eval("1 >= 1.0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal3->isa<BoolValue>());
  ASSERT_EQ(mixedVal3->cast<BoolValue>()->getValue(), true);
}

TEST(INTERPRETER_TEST_SUITE, BitAndTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 & 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 0);

  auto intVal2 = tests.eval("1 & 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<IntegerValue>());
  ASSERT_EQ(intVal2->cast<IntegerValue>()->getValue(), 1);

  auto intVal3 = tests.eval("1 & 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<IntegerValue>());
  ASSERT_EQ(intVal3->cast<IntegerValue>()->getValue(), 0);

  auto intVal4 = tests.eval("-2 & -1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<IntegerValue>());
  ASSERT_EQ(intVal4->cast<IntegerValue>()->getValue(), -2);

  auto intVal5 = tests.eval("1 & 3");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal5->isa<IntegerValue>());
  ASSERT_EQ(intVal5->cast<IntegerValue>()->getValue(), 1);
}

TEST(INTERPRETER_TEST_SUITE, BitOrTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 | 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 3);

  auto intVal2 = tests.eval("1 | 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<IntegerValue>());
  ASSERT_EQ(intVal2->cast<IntegerValue>()->getValue(), 1);

  auto intVal3 = tests.eval("1 | 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<IntegerValue>());
  ASSERT_EQ(intVal3->cast<IntegerValue>()->getValue(), 1);

  auto intVal4 = tests.eval("-2 | -1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<IntegerValue>());
  ASSERT_EQ(intVal4->cast<IntegerValue>()->getValue(), -1);

  auto intVal5 = tests.eval("1 | 3");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal5->isa<IntegerValue>());
  ASSERT_EQ(intVal5->cast<IntegerValue>()->getValue(), 3);

  auto intVal6 = tests.eval("1 | 4");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal6->isa<IntegerValue>());
  ASSERT_EQ(intVal6->cast<IntegerValue>()->getValue(), 5);
}

TEST(INTERPRETER_TEST_SUITE, BitXorTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 ^ 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 3);

  auto intVal2 = tests.eval("1 ^ 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<IntegerValue>());
  ASSERT_EQ(intVal2->cast<IntegerValue>()->getValue(), 0);

  auto intVal3 = tests.eval("1 ^ 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<IntegerValue>());
  ASSERT_EQ(intVal3->cast<IntegerValue>()->getValue(), 1);

  auto intVal4 = tests.eval("-2 ^ -1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<IntegerValue>());
  ASSERT_EQ(intVal4->cast<IntegerValue>()->getValue(), 1);

  auto intVal5 = tests.eval("1 ^ 3");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal5->isa<IntegerValue>());
  ASSERT_EQ(intVal5->cast<IntegerValue>()->getValue(), 2);

  auto intVal6 = tests.eval("1 ^ 4");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal6->isa<IntegerValue>());
  ASSERT_EQ(intVal6->cast<IntegerValue>()->getValue(), 5);
}

TEST(INTERPRETER_TEST_SUITE, ShlTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("1 << 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 4);

  auto intVal2 = tests.eval("1 << 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<IntegerValue>());
  ASSERT_EQ(intVal2->cast<IntegerValue>()->getValue(), 2);

  auto intVal3 = tests.eval("1 << 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<IntegerValue>());
  ASSERT_EQ(intVal3->cast<IntegerValue>()->getValue(), 1);

  auto intVal4 = tests.eval("-2 << 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<IntegerValue>());
  ASSERT_EQ(intVal4->cast<IntegerValue>()->getValue(), -4);

  auto intVal5 = tests.eval("1 << 3");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal5->isa<IntegerValue>());
  ASSERT_EQ(intVal5->cast<IntegerValue>()->getValue(), 8);

  auto intVal6 = tests.eval("1 << 4");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal6->isa<IntegerValue>());
  ASSERT_EQ(intVal6->cast<IntegerValue>()->getValue(), 16);

  auto mixedVal = tests.eval("1 << true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<IntegerValue>());
  ASSERT_EQ(mixedVal->cast<IntegerValue>()->getValue(), 2);

  auto mixedVal2 = tests.eval("1 << false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<IntegerValue>());
  ASSERT_EQ(mixedVal2->cast<IntegerValue>()->getValue(), 1);
}

TEST(INTERPRETER_TEST_SUITE, ShrTest) {
  InterpreterTests tests;

  auto intVal = tests.eval("4 >> 2");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 1);

  auto intVal2 = tests.eval("2 >> 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal2->isa<IntegerValue>());
  ASSERT_EQ(intVal2->cast<IntegerValue>()->getValue(), 1);

  auto intVal3 = tests.eval("1 >> 0");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal3->isa<IntegerValue>());
  ASSERT_EQ(intVal3->cast<IntegerValue>()->getValue(), 1);

  auto intVal4 = tests.eval("-4 >> 1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal4->isa<IntegerValue>());
  ASSERT_EQ(intVal4->cast<IntegerValue>()->getValue(), -2);

  auto intVal5 = tests.eval("8 >> 3");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal5->isa<IntegerValue>());
  ASSERT_EQ(intVal5->cast<IntegerValue>()->getValue(), 1);

  auto intVal6 = tests.eval("16 >> 4");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal6->isa<IntegerValue>());
  ASSERT_EQ(intVal6->cast<IntegerValue>()->getValue(), 1);

  auto mixedVal = tests.eval("4 >> true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal->isa<IntegerValue>());
  ASSERT_EQ(mixedVal->cast<IntegerValue>()->getValue(), 2);

  auto mixedVal2 = tests.eval("4 >> false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(mixedVal2->isa<IntegerValue>());
  ASSERT_EQ(mixedVal2->cast<IntegerValue>()->getValue(), 4);
}

} // namespace bara
