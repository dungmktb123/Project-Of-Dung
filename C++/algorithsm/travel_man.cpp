#include<stdio.h>
#include<conio.h>
#include<iostream>
using namespace std;
int c = 0,cost = 999999;
int graph[5][5] = { {0, 55, 87, 70, 76},
                    {91, 0, 81, 57, 39},
                    {80, 77, 0, 81, 58},
                    {99, 83, 64, 0, 89},
                    {56, 91, 82, 88, 0},
                  };
int path[5] = {0, 1, 2, 3, 4}; // declare and initialize the path array
void swap (int *x, int *y)
{
    int temp;
    temp = *x;
    *x = *y;
    *y = temp;
}
void copy_array(int *a, int n)
{
    int i, sum = 0;
    for(i = 0; i <= n; i++)
    {
        sum += graph[a[i % 5]][a[(i + 1) % 5]];
    }
    if (cost > sum)
    {
        cost = sum;
        for(i = 0; i <= n; i++) // update the path array with the optimal permutation
        {
            path[i] = a[i];
        }
    }
}  
void permute(int *a, int i, int n) 
{
   int j, k; 
   if (i == n)
   {
        copy_array(a, n);
   }
   else
   {
        for (j = i; j <= n; j++)
        {
            swap((a + i), (a + j));
            permute(a, i + 1, n);
            swap((a + i), (a + j));
        }
    }
} 
int main()
{
   int i, j;
   int a[] = {0, 1, 2, 3, 4};  
   permute(a, 0, 4);
   cout<<"minimum cost:"<<cost<<endl;
   cout<<"optimal path:"; // print the path array
   for(i = 0; i < 5; i++)
   {
       cout<<path[i]<<" ";
   }
   cout<<endl;
   getch();
}
