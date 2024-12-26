#include "bara/interpreter/ValueDeleter.h"
#include "bara/interpreter/Value.h"

namespace bara {

class ValueEraser : public ValueVisitorBase<ValueEraser> {
public:
  template <typename T>
  void visit(T &value) {
    value.~T();
  }

  template <typename T>
    requires std::is_same_v<T, TupleValue>
  void visit(T &value) {
    auto *begin = value.template getTrailingObjects<UniqueValue<Value>>();
    auto *end = begin + value.size();
    for (; begin != end; ++begin) {
      begin->reset();
    }
  }
};

static ValueEraser valueEraser;

void ValueDeleter::operator()(Value *value) const {
  value->accept(valueEraser);
  free(value);
}

} // namespace bara
