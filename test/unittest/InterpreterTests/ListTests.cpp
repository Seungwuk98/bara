#include "InterpreterTestUtils.h"

namespace bara {

TEST(INTERPRETER_TEST_SUITE, ListRefTests) {
  InterpreterTests tests;

  auto testProgram = InterpreterTests::TestProgram(&tests);

  /// list reference test
  auto listValue = testProgram.statement("var a = [1, 2];")
                       .statement("var b = a;")
                       .statement("push(a, 3);")
                       .eval("b");

  ASSERT_FALSE(testProgram.hasError());
  ASSERT_TRUE(listValue->isa<ListValue>());

  auto listView = listValue->cast<ListValue>();
  ASSERT_EQ(listView->size(), 3);

  ASSERT_TRUE(listView->getElement(0)->view()->isa<IntegerValue>());
  ASSERT_EQ(listView->getElement(0)->view()->cast<IntegerValue>()->getValue(),
            1);

  ASSERT_TRUE(listView->getElement(1)->view()->isa<IntegerValue>());
  ASSERT_EQ(listView->getElement(1)->view()->cast<IntegerValue>()->getValue(),
            2);

  ASSERT_TRUE(listView->getElement(2)->view()->isa<IntegerValue>());
  ASSERT_EQ(listView->getElement(2)->view()->cast<IntegerValue>()->getValue(),
            3);

  auto listValue2 = testProgram.statement("b[0] = 10;").eval("a[0]");
  ASSERT_FALSE(testProgram.hasError());

  ASSERT_TRUE(listValue2->isa<IntegerValue>());
  ASSERT_EQ(listValue2->cast<IntegerValue>()->getValue(), 10);

  auto listValue3 = testProgram.eval("pop(a)");
  ASSERT_FALSE(testProgram.hasError());

  ASSERT_TRUE(listValue3->isa<IntegerValue>());
  ASSERT_EQ(listValue3->cast<IntegerValue>()->getValue(), 3);

  auto listValue4 = testProgram.eval("b");
  ASSERT_FALSE(testProgram.hasError());
  ASSERT_TRUE(listValue4->isa<ListValue>());
  auto listView4 = listValue4->cast<ListValue>();
  ASSERT_EQ(listView4->size(), 2);

  ASSERT_TRUE(listView4->getElement(0)->view()->isa<IntegerValue>());
  ASSERT_EQ(listView4->getElement(0)->view()->cast<IntegerValue>()->getValue(),
            10);

  ASSERT_TRUE(listView4->getElement(1)->view()->isa<IntegerValue>());
  ASSERT_EQ(listView4->getElement(1)->view()->cast<IntegerValue>()->getValue(),
            2);
}
} // namespace bara