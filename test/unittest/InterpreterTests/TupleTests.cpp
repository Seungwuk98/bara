#include "InterpreterTestUtils.h"

namespace bara {

TEST(INTERPRETER_TEST_SUITE, TupleTests) {
  InterpreterTests tests;

  auto testProgram = InterpreterTests::TestProgram(&tests);

  auto tupleValue = testProgram.statement("var a = (1, 2);").eval("a");
  ASSERT_FALSE(testProgram.hasError());

  ASSERT_TRUE(tupleValue->isa<TupleValue>());
  auto tupleView = tupleValue->cast<TupleValue>();

  ASSERT_EQ(tupleView->size(), 2);
  ASSERT_TRUE(tupleView->getElement(0)->isa<IntegerValue>());
  ASSERT_EQ(tupleView->getElement(0)->cast<IntegerValue>()->getValue(), 1);

  ASSERT_TRUE(tupleView->getElement(1)->isa<IntegerValue>());
  ASSERT_EQ(tupleView->getElement(1)->cast<IntegerValue>()->getValue(), 2);

  auto tupleValue3 = testProgram.statement("var b = a;").eval("b");
  ASSERT_FALSE(testProgram.hasError());

  ASSERT_TRUE(tupleValue3->isa<TupleValue>());
  auto tupleView3 = tupleValue3->cast<TupleValue>();

  ASSERT_EQ(tupleView3->size(), 2);
  ASSERT_TRUE(tupleView3->getElement(0)->isa<IntegerValue>());
  ASSERT_EQ(tupleView3->getElement(0)->cast<IntegerValue>()->getValue(), 1);

  ASSERT_TRUE(tupleView3->getElement(1)->isa<IntegerValue>());
  ASSERT_EQ(tupleView3->getElement(1)->cast<IntegerValue>()->getValue(), 2);

  auto tupleValue4 = testProgram.statement("var (x, y) = a;").eval("x");
  ASSERT_FALSE(testProgram.hasError());

  ASSERT_TRUE(tupleValue4->isa<IntegerValue>());
  ASSERT_EQ(tupleValue4->cast<IntegerValue>()->getValue(), 1);

  auto tupleValue5 = testProgram.eval("a[0]");
  ASSERT_FALSE(testProgram.hasError());

  ASSERT_TRUE(tupleValue5->isa<IntegerValue>());
  ASSERT_EQ(tupleValue5->cast<IntegerValue>()->getValue(), 1);
}

} // namespace bara
