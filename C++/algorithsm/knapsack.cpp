// Nhập các thư viện cần thiết
#include<bits/stdc++.h>
using namespace std;
struct Node {
    int level, profit, bound, weight;
};

// Một hàm để tính cận trên của nút u bằng phương pháp tham lam
int bound(Node u, int n, int W, int wt[], int val[]) {
    // nếu trọng lượng vượt quá khả năng chứa của túi, trả về 0 là cận trên mong đợi
    if (u.weight >= W)
        return 0;

    // khởi tạo cận trên về giá trị bằng giá trị hiện tại
    int profit_bound = u.profit;

    // bắt đầu thêm các đồ vật từ chỉ số 1 lớn hơn chỉ số của đồ vật hiện tại
    int j = u.level + 1;
    int totweight = u.weight;

    // kiểm tra điều kiện về chỉ số và điều kiện về khả năng chứa của túi
    while ((j < n) && (totweight + wt[j] <= W)) {
        totweight += wt[j];
        profit_bound += val[j];
        j++;
    }

    // Nếu j không phải là n, thì thêm một phần của đồ vật cuối cùng để tính cận trên về giá trị
    if (j < n)
        profit_bound += (W - totweight) * val[j] / wt[j];

    return profit_bound;
}

// Một hàm để giải bài toán cái túi bằng phương pháp nhánh cận
void knapsack(int n, int W, int wt[], int val[]) {
    // Tạo một hàng đợi ưu tiên để lưu trữ các nút sống của cây tìm kiếm
    priority_queue<Node> Q;
    // Tạo một nút giả ở đầu
    Node u, v;
    u.level = -1;
    u.profit = u.weight = 0;
    Q.push(u);

    // Khởi tạo giá trị lợi nhuận cuối cùng
    int maxProfit = 0;

    // Lần lượt lấy ra một đồ vật từ cây quyết định
    // tính giá trị lợi nhuận của tất cả các nút con của đồ vật được lấy ra
    // và cập nhật giá trị lợi nhuận lớn nhất
    while (!Q.empty()) {
        // Lấy ra một nút
        u = Q.top();
        Q.pop();

        // Nếu đó là nút đầu tiên, gán cấp độ bằng 0
        if (u.level == -1)
            v.level = 0;

        // Nếu không có gì ở cấp độ tiếp theo
        if (u.level == n - 1)
            continue;

        // Nếu không phải là nút cuối cùng, thì tăng cấp độ lên 1, và tính giá trị lợi nhuận của các nút con
        v.level = u.level + 1;

        // Lấy đồ vật ở cấp độ hiện tại, cộng trọng lượng và giá trị của đồ vật đó vào trọng lượng và giá trị của nút u
        v.weight = u.weight + wt[v.level];
        v.profit = u.profit + val[v.level];

        // Nếu trọng lượng tích lũy nhỏ hơn W và giá trị lợi nhuận lớn hơn giá trị lợi nhuận trước đó, cập nhật giá trị lợi nhuận lớn nhất
        if (v.weight <= W && v.profit > maxProfit)
            maxProfit = v.profit;

        // Tính cận trên cho nút v để quyết định có thêm v vào Q hay không
        v.bound = bound(v, n, W, wt, val);

        // Nếu cận trên lớn hơn giá trị lợi nhuận, thì mới thêm vào hàng đợi để xem xét tiếp
        if (v.bound > maxProfit)
            Q.push(v);

        // Làm tương tự, nhưng không lấy đồ vật ở cấp độ hiện tại vào túi
        v.weight = u.weight;
        v.profit = u.profit;
        v.bound = bound(v, n, W, wt, val);
        if (v.bound > maxProfit)
            Q.push(v);
    }

    // In ra giá trị lợi nhuận lớn nhất
    cout << "Giá trị lớn nhất có thể đạt được là " << maxProfit << endl;

    // In ra các đồ vật được chọn vào túi
    cout << "Các đồ vật được chọn vào túi là:" << endl;
    while (v.level != -1) {
        if (v.profit == maxProfit) {
            cout << v.level + 1 << " ";
            maxProfit -= val[v.level];
        }
        v.level--;
    }
    cout << endl;
}

// Hàm chính
int main() {
    int n; // Số lượng đồ vật
    int W; // Khả năng chứa tối đa của túi
    cin >> n >> W;
    int val[n]; // Giá trị của các đồ vật
    int wt[n]; // Trọng lượng của các đồ vật
    for (int i = 0; i < n; i++) {
        cin >> wt[i] >> val[i];
    }
    knapsack(n, W, wt, val);
    return 0;
}
