#include <iostream>

using namespace std;

int success = 0;
/******************寻找路径函数*******************************/
int findpath(int i,int j ,int endRow,int endCol,int** maze,int n ,int m);
/****************走迷宫maze:N*M*********************************/

void showMaze(int startRow,int startCol,int endRow,int endCol,int** maze,int n,int m){
    cout<<"迷宫示意图如下所示："<<endl;
    for(int i=0;i<n;i++){
        for(int j=0;j<m;j++){
            if(maze[i][j]==1){
                cout<<"█";
            }else{
                cout<<"  ";
            }
        }
        cout<<endl;
    }
    if (findpath(startRow,startCol,endRow,endCol,maze,n,m)==0){
        cout<<"没有出口"<<endl;
    }else {
        cout<<""<<endl;
        for (int i=0;i<n;i++){
            for(int j=0;j<m;j++){
                if(maze[i][j]==1){
                    cout<<"█";
                }else if(maze[i][j]==2){
                    cout<<"◇";
                }else{
                    cout<<"  ";
                }
            }
            cout<<endl;
        }
    }
}


/******************i，j当前位置startRow,startCol起点坐标，endRow，endCol终点位置*******************************/

/*********************展示迷宫函数showMaze****************************/
 int findpath(int i,int j,int endRow,int endCol,int** maze ,int n,int m){
     maze[i][j]=2;
     if (i==endRow && j==endCol){
        success=1;
        return success;
     }else {
/***************************寻找出口路径findPath**********************/

/*****************其中,j表示当前位置坐标，endRow,endCol表示出口路径********************************/


/************************递归检查当前格子的相邻四个方向上的格子是否能走*************************/
         if (i<n-1 && j<m-2 && maze[i][j+1] == 0&& success!=1)
            findpath(i,j+1,endRow,endCol,maze,n,m);//right
         if (i<n-2 && j<m-1 && maze[i+1][j] == 0&& success!=1)
            findpath(i+1,j,endRow,endCol,maze,n,m);//down
         if (i<n-1 && j>1 && maze[i][j-1] == 0&& success!=1)
            findpath(i,j-1,endRow,endCol,maze,n,m);//left
         if (i>1 && j<m-1 && maze[i-1][j] == 0&& success!=1)
            findpath(i-1,j,endRow,endCol,maze,n,m); //up
     if(success !=1)
        maze[i][j]=0;//0没走过，1不能走，2走过了
     return success;
     }
 }


/*
请输入迷宫的行数N和列数M

4 4
请输入迷宫，1表示墙壁，0表示行走的通道
1 1 1 1
1 0 0 1
1 1 0 1
1 1 1 1
请输入迷宫的入口坐标：
1 1
请输入迷宫的出口坐标:
2 2
迷宫示意图如下所示：
 #  #  #  #
 #        #
 #  #     #
 #  #  #  #

 #  #  #  #
 #  @  @  #
 #  #  @  #
 #  #  #  #


 请输入迷宫的行数N和列数M

11 10
请输入迷宫，1表示墙壁，0表示行走的通道
1 1 1 1 1 1 1 1 1 1
1 0 0 1 0 0 0 1 0 1
1 0 0 1 0 0 0 1 0 1
1 0 0 1 0 1 1 0 1 1
1 0 1 1 1 0 0 1 0 1
1 0 0 0 1 0 0 0 0 1
1 0 1 0 0 0 1 0 1 1
1 0 1 1 1 1 0 0 1 1
1 1 1 0 0 0 1 0 1 1
1 1 1 0 0 0 0 0 0 1
1 1 1 1 1 1 1 1 1 1
请输入迷宫的入口坐标：
1 1
请输入迷宫的出口坐标:
9 8
注：这里PDF是8 9 行列式M行N列，和一般习惯的不一样，编程总忘记，看着很难受，我写的时候把行列换回来。
迷宫示意图如下所示：
 #  #  #  #  #  #  #  #  #  #
 #        #           #     #
 #        #           #     #
 #        #     #  #     #  #
 #     #  #  #        #     #
 #           #              #
 #     #           #     #  #
 #     #  #  #  #        #  #
 #  #  #           #     #  #
 #  #  #                    #
 #  #  #  #  #  #  #  #  #  #

 #  #  #  #  #  #  #  #  #  #
 #  @  @  #           #     #
 #     @  #           #     #
 #  @  @  #     #  #     #  #
 #  @  #  #  #        #     #
 #  @  @  @  #  @  @  @     #
 #     #  @  @  @  #  @  #  #
 #     #  #  #  #     @  #  #
 #  #  #           #  @  #  #
 #  #  #              @  @  #
 #  #  #  #  #  #  #  #  #  #

*/



/*********************测试主函数main****************************/


int main()
{
    cout << "请输入迷宫的行数N和列数M\n" << endl;
    int n,m;
    cin>>n>>m;
    int** maze = new int* [n];
    for(int i=0;i<n;i++){
        maze[i]=new int[m];
    }
    cout<<"请输入迷宫，1表示墙壁，0表示行走的通道"<<endl;
    for(int i=0;i<n;i++){
        for(int j=0;j<m;j++){
            cin>>maze[i][j];
        }
    }
    int startRow,startCol,endRow,endCol;
    cout<<"请输入迷宫的入口坐标："<<endl;
    cin>>startRow>>startCol;
    cout<<"请输入迷宫的出口坐标:"<<endl;
    cin>>endRow>>endCol;
    showMaze(startRow,startCol,endRow,endCol,maze,n,m);
    return 0;

}


