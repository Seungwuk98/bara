#include "InterpreterTestUtils.h"

namespace bara {

TEST(INTERPRETER_TEST_SUITE, LiteralTests) {
  InterpreterTests tests;

  auto intVal = tests.eval("1");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(intVal->isa<IntegerValue>());
  ASSERT_EQ(intVal->cast<IntegerValue>()->getValue(), 1);

  auto floatVal = tests.eval("1.000");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(floatVal->isa<FloatValue>());
  ASSERT_EQ(floatVal->cast<FloatValue>()->getValue(),
            APFloat(APFloat::IEEEdouble(), "1.000"));

  auto trueVal = tests.eval("true");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(trueVal->isa<BoolValue>());
  ASSERT_EQ(trueVal->cast<BoolValue>()->getValue(), true);

  auto falseVal = tests.eval("false");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(falseVal->isa<BoolValue>());
  ASSERT_EQ(falseVal->cast<BoolValue>()->getValue(), false);

  auto stringVal = tests.eval("\"the string\"");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(stringVal->isa<StringValue>());
  ASSERT_EQ(stringVal->cast<StringValue>()->getValue().str(), "the string");

  auto stringValWithEscape = tests.eval("\"newline:\\n\"");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(stringValWithEscape->isa<StringValue>());
  ASSERT_EQ(stringValWithEscape->cast<StringValue>()->getValue().str(),
            "newline:\n");

  auto nilValue = tests.eval("nil");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(nilValue->isa<NilValue>());

  auto tupleValue = tests.eval("(1, 2)");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(tupleValue->isa<TupleValue>());

  auto tupleView = tupleValue->cast<TupleValue>();
  ASSERT_EQ(tupleView->size(), 2);
  ASSERT_TRUE(tupleView->getElement(0)->view()->isa<IntegerValue>());
  ASSERT_EQ(tupleView->getElement(0)->view()->cast<IntegerValue>()->getValue(),
            1);

  ASSERT_TRUE(tupleView->getElement(1)->view()->isa<IntegerValue>());
  ASSERT_EQ(tupleView->getElement(1)->view()->cast<IntegerValue>()->getValue(),
            2);

  auto listValue = tests.eval("[1, 2]");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(listValue->isa<ListValue>());

  auto listView = tupleValue->cast<TupleValue>();
  ASSERT_EQ(listView->size(), 2);
  ASSERT_TRUE(listView->getElement(0)->view()->isa<IntegerValue>());
  ASSERT_EQ(listView->getElement(0)->view()->cast<IntegerValue>()->getValue(),
            1);

  ASSERT_TRUE(listView->getElement(1)->view()->isa<IntegerValue>());
  ASSERT_EQ(listView->getElement(1)->view()->cast<IntegerValue>()->getValue(),
            2);

  auto lambdaValue = tests.eval(R"(\a => 1)");
  ASSERT_FALSE(tests.hasError());
  ASSERT_TRUE(lambdaValue->isa<LambdaValue>());

  auto lambdaView = lambdaValue->cast<LambdaValue>();
  auto pattern_a = IdentifierPattern::create({}, tests.getASTContext(), "a");
  auto integer_1 = IntegerLiteral::create({}, tests.getASTContext(), 1);
  auto lambda_a_1 = LambdaExpression::create({}, tests.getASTContext(),
                                             {pattern_a}, integer_1);
  ASSERT_TRUE(lambdaView->getExpression()->isEqual(lambda_a_1));
}

} // namespace bara
