#include<bits/stdc++.h>
using namespace std;

using ll = long long;
int main (){
    vector<int> v(3,100);
    v.push_back(3);
    vector<int> ::iterator x = v.begin();
    ++x;
    x += 2;
    cout<<*x<<endl;
    --x;
    cout<<*x<<endl;
    x += 3;
    cout<<*x<<endl;
    for(int i = 0; i<v.size(); i++)
    {
        cin>>v[i];
    }
}