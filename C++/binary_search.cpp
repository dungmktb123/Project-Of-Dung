#include<bits/stdc++.h>
using namespace std;
bool binarySearch (int a[], int n, int x)
{
    int left = 0, right = n-1;
    while(left <- right)
    {
        int mid = (left + right)/2;
        if(a[mid] == x)
            return true;
        else if(a[mid] <x)
            //tìm kiếm ở bên phải
            left = mid+ 1;
        else 
            //tìm kiếm bên trái
            right = mid - 1;
    }
    return false;
}
//ý tưởng là nếu phần tử cần tìm kiếm bằng phần tử ở vị trí mid thì kết luận là tìm thấy, nếu không ta có thể giảm 1 nửa đoạn tìm kiếm xuống và tiếp tục tìm kiếm ở bên trái và bên phải mid
//Điều kiện áp dụng: Mảng đã được sắp xếp
int main (){
    int n;
    cin>>n;
    vector<int> a(n);
    for(int i=0;i<n;i++)
    {
        cin>>a[i];
    }
    if(binary_search(a.begin(),a.end(),9) )
        cout<<"Found";
    else
        cout<<"Not found";
}