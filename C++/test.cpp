#include<bits/stdc++.h>
using namespace std;
int main ()
{
    stack <char> st;
    int n;
    cin>>n;
    
    while(n!=0)
    {
        st.push(n%2 +'0');
        n /= 2;
    }
    while(!st.empty())
    {
        cout<<st.top();
        st.pop();
    }

}