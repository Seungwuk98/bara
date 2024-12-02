#include "bara/context/ASTContext.h"
#include "bara/ast/AST.h"

namespace bara {

ASTContext::ASTContext() {
  breakStmt = make<BreakStatement>();
  continueStmt = make<ContinueStatement>();
}

} // namespace bara
