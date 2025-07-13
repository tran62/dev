// Iterative Euclid algorithm
int
gcd_iter(int u, int v) {
  if (u < 0) u = -u;
  if (v < 0) v = -v;
  if (v) while ((u %= v) && (v %= u));
  return (u + v);
}

// Recursive Euclid algorithm
int gcd(int u, int v) {
return (v != 0)?gcd(v, u%v):u;
}

// Iterative binary algorithm
int gcd_bin(int u, int v) {
  int t, k;

  u = u < 0 ? -u : u; /* abs(u) */
  v = v < 0 ? -v : v; 
  if (u < v) {
    t = u;
    u = v;
    v = t;
  }
  if (v == 0)
    return u;

  k = 1;
  while ((u & 1) == 0 && (v & 1) == 0) { /* u, v - even */
    u >>= 1; v >>= 1;
    k <<= 1;
  }



  t = (u & 1) ? -v : u;
  while (t) {
    while ((t & 1) == 0) 
      t >>= 1;

    if (t > 0)
      u = t;
    else
      v = -t;

    t = u - v;
  }
  return u * k;        
}

// tests

#include <stdio.h>
int main(){
	printf("gcd_iter(57,38): %d \n",gcd_iter(57,38));
    printf("gcd(57,38): %d \n",gcd(57,38));
    printf("gcd_bin(57,38): %d \n",gcd_bin(57,38));
	return 0;
}
