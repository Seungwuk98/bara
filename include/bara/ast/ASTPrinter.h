#ifndef BARA_AST_PRINTER_H
#define BARA_AST_PRINTER_H

#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"

namespace bara {
class ASTPrinter {
public:
  ASTPrinter(raw_ostream &os) : os(os) {}

  raw_ostream &getOS() { return os; }
  ASTPrinter &ln() {
    os << '\n' << string(indentLevel, ' ');
    return *this;
  }

  struct AddIndentScope {
    AddIndentScope(ASTPrinter &printer, size_t cnt = 2)
        : printer(printer), savedIndent(printer.indentLevel) {
      printer.indentLevel += cnt;
    }

    ~AddIndentScope() { printer.indentLevel = savedIndent; }

  private:
    ASTPrinter &printer;
    size_t savedIndent;
  };

private:
  raw_ostream &os;
  size_t indentLevel = 0;
};

template <typename T>
ASTPrinter &operator<<(ASTPrinter &os, const T &value) {
  os.getOS() << value;
  return os;
}

} // namespace bara

#endif // BARA_AST_PRINTER_H
