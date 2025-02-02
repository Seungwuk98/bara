#ifndef BARA_LLVM_H
#define BARA_LLVM_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/TrailingObjects.h"
#include "llvm/Support/raw_ostream.h"

namespace bara {

using llvm::ArrayRef;
using llvm::DenseMap;
using llvm::DenseSet;
using llvm::MapVector;
using llvm::MutableArrayRef;
using llvm::SmallVector;

using llvm::errs;
using llvm::outs;
using llvm::raw_ostream;
using llvm::raw_string_ostream;
using llvm::SMLoc;
using llvm::SMRange;
using llvm::StringRef;
using llvm::TrailingObjects;

using llvm::cast;
using llvm::dyn_cast;
using llvm::isa;

} // namespace bara

#endif // BARA_LLVM_H
