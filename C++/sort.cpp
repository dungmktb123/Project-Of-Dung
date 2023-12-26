#include<iostream>
#include<algorithm>
using namespace std;
int tong(int n)
{
    int ans = 0;
    while(n){
        ans += n%10;
        n /= 10;
    }
    return ans;
}
bool cmp(int a, int b)
{
    // if(a<b)
    //     return true;
    // else
    //     return false;
    // return a>b;
    if(tong(a) != tong(b))
    {
        return tong(a) < tong(b);
    }
    else
        return a>b;
}
int main()
{
    ios::sync_with_stdio(false);
//sync_with_stdio là là một chức năng trong C++ vô hiệu hóa việc đồng bộ hóa giữa luồng tiêu chuẩn C và C++. Theo mặc định, tất cả các luồng tiêu chuẩn 
//đều được đồng bộ hóa, trong thực tế cho phép bạn kết hợp I/O kiểu C và C+± và nhận được kết quả hợp lý và mong đợi. Nếu bạn tắt tính năng đồng bộ hóa 
//thì các luồng C++ sẽ được phép có bộ đệm độc lập riêng, điều này khiến việc trộn I/O kiểu C và C+± trở thành một cuộc phiêu lưu. Ngoài ra, hãy nhớ rằng
// các luồng C++ được đồng bộ hóa là an toàn theo luồng (đầu ra từ các luồng khác nhau có thể xen kẽ, nhưng bạn không nhận được các cuộc đua dữ liệu)
//.123 ios::sync_with_stdio(false) được sử dụng để tăng tốc thời gian thực thi của một chương trình bằng cách cho phép Các luồng C++ để đệm I/O một cách
// độc lập, có thể nhanh hơn đáng kể trong một số trường hợp.3 Tuy nhiên, điều quan trọng là phải hiểu chức năng của từng luồng, hiểu hậu quả và sau đó 
//quyết định xem bạn có thực sự muốn hoặc cần tác dụng phụ có thể xảy ra hay không cải thiện tốc độ.12
    cin.tie(nullptr);
//cin.tie là một hàm trong C++ trả về hoặc đặt luồng đầu ra được gắn với luồng đầu vào. Luồng bị ràng buộc là luồng đầu ra được tự động xóa trước mỗi 
//thao tác đầu vào/đầu ra trên luồng đầu vào. Theo mặc định, cin được gắn với cout, có nghĩa là cout bị xóa trước khi bất kỳ đầu vào nào được đọc từ cin.
// Điều này đảm bảo sự tương tác hợp lý của người dùng, nhưng nó cũng làm chậm chương trình đối với I/O lớn. Nếu bạn muốn tăng tốc thời gian thực hiện 
//chương trình, bạn có thể gỡ bỏ cin khỏi cout bằng cách gọi cin.tie(NULL). Điều này có nghĩa là cout sẽ không được xóa tự động trước khi đọc từ cin, 
//vì vậy bạn phải xóa nó theo cách thủ công bất cứ khi nào bạn muốn hiển thị nội dung nào đó trước khi mong đợi dữ liệu đầu vào. 
//Ví dụ: // cin và cout được ràng buộc theo mặc định std::cout << “Nhập tên:”; // cout được xóa trước khi đọc từ cin std::cin >> name; 
// cin và cout được cởi trói std::cout << “Nhập tên:”; cin.tie(NULL); // cởi trói cin khỏi cout std::cin >> name; 
// cout không bị xóa nên thông báo có thể không hiển thị // cin và cout không được ràng buộc std::cout << “Enter name:” << std::flush; 
// tuôn ra cout thủ công cin.tie(NULL); // cởi trói cin khỏi cout std::cin >> name; // cout bị xóa nên thông báo hiển thị
    int n; cin>>n;
    int a[n];
    for(int i=0;i<n;i++)
    {
        cin>>a[i];
    }
    sort(a,a+n,cmp); // 0(NlogN) = intro sort : quick sort + heap sort
    for(int x:a)
    {
        cout<<x<<" ";
    }
}