#include <bits/stdc++.h>
using namespace std;

int ham_sinh(int n) {
  int a1 = 4, a2 = 12; // khởi tạo hai số Fibonacci đầu tiên
  if (n == 1)
    return 4;
  if (n==2)
    return 12;
  int i = 3, a;
  while (i <= n) { 
    a = a2 + 2*a1 + pow(2,n); 
    a1 = a2; 
    a2 = a; 
    i++; 
  }
  return a; 
}

int main() {
  int n;
  cout << "nhap n: ";
  cin >> n;
  cout << ham_sinh(n);
  return 0;
}