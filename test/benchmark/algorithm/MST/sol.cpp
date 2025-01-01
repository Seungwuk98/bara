#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;
using piii = tuple<int, int, int>;

int pr[101010];
int rk[101010];

int find(int x) { return pr[x] == x ? x : pr[x] = find(pr[x]); }

int uni(int x, int y) {
  int rx = find(x), ry = find(y);
  if (rx == ry)
    return false;
  if (rk[rx] == rk[ry])
    rk[rx]++;
  if (rk[rx] < rk[ry])
    pr[rx] = ry;
  else
    pr[ry] = rx;
  return true;
}

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);
  cout.tie(0);
  int N, M;

  cin >> N >> M;
  vector<piii> E(M);
  for (int i = 0; i < M; ++i) {
    int a, b, c;
    cin >> a >> b >> c;
    E[i] = {c, a, b};
  }

  sort(E.begin(), E.end());
  for (int i = 1; i <= N; ++i)
    pr[i] = i;
  int ans = 0;
  for (auto [c, a, b] : E) {
    if (uni(a, b))
      ans += c;
  }
  cout << ans;
}
