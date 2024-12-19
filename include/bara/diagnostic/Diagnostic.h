#ifndef BARA_DIAGNOSTIC_H
#define BARA_DIAGNOSTIC_H

#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"
#include "llvm/Support/SourceMgr.h"

namespace bara {

class Diagnostic {
public:
  Diagnostic(llvm::SourceMgr &srcMgr, raw_ostream &os = errs())
      : errorCnt(0), srcMgr(srcMgr), os(os) {}
  size_t getErrorCount() const { return errorCnt; }
  bool hasError() const { return errorCnt; }

  raw_ostream &getOS() { return os; }

  llvm::SourceMgr &getSourceMgr() { return srcMgr; }

  void report(SMRange range, llvm::SourceMgr::DiagKind kind,
              StringRef message) {
    srcMgr.PrintMessage(os, range.Start, kind, message, {range});
    errorCnt += kind == llvm::SourceMgr::DK_Error;
  }

  void reset() { errorCnt = 0; }

private:
  size_t errorCnt;
  llvm::SourceMgr &srcMgr;
  raw_ostream &os;
};

} // namespace bara

#endif // BARA_DIAGNOSTIC_H
