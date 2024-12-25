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
    for (auto *begin = value.template getTrailingObjects<UniqueValue<Value>>(),
              *end = begin + value.size();
         begin != end; ++begin) {
      begin->reset();
    }
  }

  static ValueEraser &get() {
    static ValueEraser valueEraser;
    return valueEraser;
  }
};

void ValueDeleter::operator()(Value *value) const {
  value->accept(ValueEraser::get());
  free(value);
}

} // namespace bara
