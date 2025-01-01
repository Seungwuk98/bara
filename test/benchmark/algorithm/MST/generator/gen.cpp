#include "testlib.h"
#include <iostream>
#include <set>
#include <vector>

int pr[101010];
int rk[101010];

int find(int x) { return pr[x] == x ? x : pr[x] = find(pr[x]); }

bool uni(int x, int y) {
  int rx = find(x), ry = find(y);
  if (rx == ry)
    return false;
  if (rk[rx] == rk[ry])
    rk[rx]++;
  if (rk[rx] > rk[ry])
    pr[ry] = rx;
  else
    pr[rx] = ry;
  return true;
}

int main(int argc, char *argv[]) {
  registerGen(argc, argv, 1);

  const int MAX_N = 100'000;
  const int MIN_N = 3;

  const int MAX_K = 1'000;
  const int MIN_K = 1;

  int max_n = opt<int>("max_n");
  int min_n = opt<int>("min_n");

  ensuref(MIN_N <= min_n && max_n <= MAX_N,
          "min_n, max_n should be in [%d, %d]", MIN_N, MAX_N);
  ensuref(min_n <= max_n, "min_n : %d  max_n : %d", min_n, max_n);

  int max_k = opt<int>("max_k");
  int min_k = opt<int>("min_k");

  ensuref(MIN_K <= min_k && max_k <= MAX_K,
          "min_k, max_k should be in [%d, %d]", MIN_K, MAX_K);
  ensuref(min_k <= max_k, "min_k : %d  max_k : %d", min_k, max_k);

  int N = rnd.next(min_n, max_n);

  for (int i = 1; i <= N; ++i)
    pr[i] = i;
  std::vector<std::vector<int>> g(N + 1);
  std::set<std::pair<int, int>> S;
  int E = 0;
  int M = 0;
  while (E != N - 1) {
    int a = rnd.next(1, N);
    int b = rnd.next(1, N);
    if (a > b)
      std::swap(a, b);
    while (a == b || S.find({a, b}) != S.end()) {
      a = rnd.next(1, N);
      b = rnd.next(1, N);
      if (a > b)
        std::swap(a, b);
    }
    S.insert({a, b});
    if (uni(a, b)) {
      E++;
    }
    M++;
  }

  println(N, M);
  for (auto [a, b] : S) {
    println(a, b, rnd.next(min_k, max_k));
  }
}
