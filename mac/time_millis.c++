#include <chrono>
#include <cstdio>

// this is similar to `date +%s.%N` on linux, but mac doesn't have `%N`.

using namespace std::chrono;
int main() {
  milliseconds ms = duration_cast<milliseconds>(system_clock::now().time_since_epoch());
  uint64_t u_ms = ms.count();
  printf("%llu.%llu\n", u_ms/1000, u_ms%1000);
}
