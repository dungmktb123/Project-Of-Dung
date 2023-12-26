#include<bits/stdc++.h>
using namespace std;
int de_quy(int n)
{
    int S;
    if(n<1)
    {
        return 0;
    }
    if(n==1)
    {
        return 6;
    }
    if(n==2)
    {
        return 42;
    }

    else
    {
        S = de_quy(n-1) + 6*de_quy(n-2) + 6*n;
        return S;
    }
    
}
int main()
{
    int n,S;
    cin>>n;
    cout<<de_quy(n);
}