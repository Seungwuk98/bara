#include "testlib.h"
#include <utility>
#include <vector>

int main(int argc, char *argv[]) {
  registerGen(argc, argv, 1);
  constexpr auto MAX_N = 1'000;
  constexpr auto MIN_N = 3;

  constexpr auto MAX_D = 1'000'000'000;
  constexpr auto MIN_D = -1'000'000'000;

  auto max_n = opt<int>("max_n");
  auto min_n = opt<int>("min_n");

  auto max_d = opt<int>("max_d");
  auto min_d = opt<int>("min_d");

  ensuref(MIN_N <= max_n && max_n <= MAX_N,
          "max_n must be in a range [%d, %d].", MIN_N, MAX_N);
  ensuref(MIN_N <= min_n && min_n <= MAX_N,
          "min_n must be in a range [%d, %d].", MIN_N, MAX_N);
  ensuref(min_n <= max_n, "min_n must be less than or equal to max_n");

  ensuref(MIN_D <= max_d && max_d <= MAX_D,
          "max_d must be in a range [%d, %d].", MIN_D, MAX_D);
  ensuref(MIN_D <= min_d && min_d <= MAX_D,
          "min_d must be in a range [%d, %d].", MIN_D, MAX_D);
  ensuref(min_d <= max_d, "min_d must be less than or equal to max_d");

  auto N = rnd.next(min_n, max_n);
  println(N);
  std::vector<int> arr(N);
  for (auto i = 1; i <= N; ++i) {
    auto p = rnd.next(min_d, max_d);
    arr.push_back(p);
  }
  std::sort(arr.begin(), arr.end());
  for (auto i = 0; i < N; ++i) {
    println(arr[i]);
  }
}
