#ifndef BARA_VALUE_ERASER_H
#define BARA_VALUE_ERASER_H

#include <memory>

namespace bara {

class Value;

class ValueDeleter {
public:
  void operator()(Value *value) const;
};

using std::unique_ptr;

template <typename T>
using UniqueValue = unique_ptr<T, ValueDeleter>;

} // namespace bara

#endif // BARA_VALUE_ERASER_H
