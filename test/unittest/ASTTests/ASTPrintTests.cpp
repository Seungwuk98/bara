#include "bara/ast/AST.h"
#include "bara/context/ASTContext.h"
#include "gtest/gtest.h"

namespace bara {

TEST(ASTPrintTests, Identifier) {
  ASTContext context;
  auto *identifier = IdentifierExpression::create({}, &context, "foo");

  EXPECT_STREQ(identifier->toString().c_str(), "foo");
}

TEST(ASTPrintTests, TupleLiteral) {
  ASTContext context;
  auto *tuple1 = TupleExpression::create(
      {}, &context,
      {IdentifierExpression::create({}, &context, "foo"),
       IdentifierExpression::create({}, &context, "bar"),
       IdentifierExpression::create({}, &context, "baz")});

  EXPECT_STREQ(tuple1->toString().c_str(), "(foo, bar, baz)");

  auto *tuple2 = TupleExpression::create({}, &context, {});

  EXPECT_STREQ(tuple2->toString().c_str(), "(,)");

  auto *tuple3 = TupleExpression::create({}, &context,
                                         {
                                             tuple1,
                                         });

  EXPECT_STREQ(tuple3->toString().c_str(), "((foo, bar, baz),)");
}

TEST(ASTPrintTests, ArrayExpression) {
  ASTContext context;
  auto *list1 = ArrayExpression::create(
      {}, &context,
      {IdentifierExpression::create({}, &context, "foo"),
       IdentifierExpression::create({}, &context, "bar"),
       IdentifierExpression::create({}, &context, "baz")});

  EXPECT_STREQ(list1->toString().c_str(), "[foo, bar, baz]");
}

TEST(ASTPrintTests, BinaryExpression) {
  ASTContext context;
  auto *binary1 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::Plus, IdentifierExpression::create({}, &context, "bar"));

  EXPECT_STREQ(binary1->toString().c_str(), "foo + bar");

  auto *binary2 = BinaryExpression::create(
      {}, &context,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "foo"),
          Operator::Minus, IdentifierExpression::create({}, &context, "bar")),
      Operator::Plus, IdentifierExpression::create({}, &context, "baz"));

  EXPECT_STREQ(binary2->toString().c_str(), "foo - bar + baz");

  auto *binary3 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::BitAnd,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "bar"),
          Operator::BitOr, IdentifierExpression::create({}, &context, "baz")));

  EXPECT_STREQ(binary3->toString().c_str(), "foo & bar | baz");

  auto *binary4 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::Eq,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "bar"),
          Operator::Ne, IdentifierExpression::create({}, &context, "baz")));

  EXPECT_STREQ(binary4->toString().c_str(), "foo == bar != baz");

  auto *binary5 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::Lt,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "bar"),
          Operator::Le, IdentifierExpression::create({}, &context, "baz")));

  EXPECT_STREQ(binary5->toString().c_str(), "foo < bar <= baz");

  auto *binary6 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::Gt,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "bar"),
          Operator::Ge, IdentifierExpression::create({}, &context, "baz")));

  EXPECT_STREQ(binary6->toString().c_str(), "foo > bar >= baz");

  auto *binary7 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::Mul,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "bar"),
          Operator::Div, IdentifierExpression::create({}, &context, "baz")));

  EXPECT_STREQ(binary7->toString().c_str(), "foo * bar / baz");

  auto *binary8 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::Mod,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "bar"),
          Operator::Shl, IdentifierExpression::create({}, &context, "baz")));

  EXPECT_STREQ(binary8->toString().c_str(), "foo % bar << baz");

  auto *binary9 = BinaryExpression::create(
      {}, &context, IdentifierExpression::create({}, &context, "foo"),
      Operator::Shr,
      BinaryExpression::create(
          {}, &context, IdentifierExpression::create({}, &context, "bar"),
          Operator::And, IdentifierExpression::create({}, &context, "baz")));

  EXPECT_STREQ(binary9->toString().c_str(), "foo >> bar && baz");
}

TEST(ASTPrintTests, UnaryExpression) {
  ASTContext context;
  auto *unary1 = UnaryExpression::create(
      {}, &context, Operator::Minus,
      IdentifierExpression::create({}, &context, "foo"));

  EXPECT_STREQ(unary1->toString().c_str(), "-foo");

  auto *unary2 = UnaryExpression::create(
      {}, &context, Operator::Not,
      IdentifierExpression::create({}, &context, "foo"));

  EXPECT_STREQ(unary2->toString().c_str(), "!foo");

  auto *unary3 = UnaryExpression::create(
      {}, &context, Operator::BitNot,
      IdentifierExpression::create({}, &context, "foo"));

  EXPECT_STREQ(unary3->toString().c_str(), "~foo");
}

} // namespace bara
