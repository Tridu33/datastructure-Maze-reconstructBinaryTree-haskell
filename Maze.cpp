#include <iostream>

using namespace std;

int success = 0;
/******************Ѱ��·������*******************************/
int findpath(int i,int j ,int endRow,int endCol,int** maze,int n ,int m);
/****************���Թ�maze:N*M*********************************/

void showMaze(int startRow,int startCol,int endRow,int endCol,int** maze,int n,int m){
    cout<<"�Թ�ʾ��ͼ������ʾ��"<<endl;
    for(int i=0;i<n;i++){
        for(int j=0;j<m;j++){
            if(maze[i][j]==1){
                cout<<"��";
            }else{
                cout<<"  ";
            }
        }
        cout<<endl;
    }
    if (findpath(startRow,startCol,endRow,endCol,maze,n,m)==0){
        cout<<"û�г���"<<endl;
    }else {
        cout<<""<<endl;
        for (int i=0;i<n;i++){
            for(int j=0;j<m;j++){
                if(maze[i][j]==1){
                    cout<<"��";
                }else if(maze[i][j]==2){
                    cout<<"��";
                }else{
                    cout<<"  ";
                }
            }
            cout<<endl;
        }
    }
}


/******************i��j��ǰλ��startRow,startCol������꣬endRow��endCol�յ�λ��*******************************/

/*********************չʾ�Թ�����showMaze****************************/
 int findpath(int i,int j,int endRow,int endCol,int** maze ,int n,int m){
     maze[i][j]=2;
     if (i==endRow && j==endCol){
        success=1;
        return success;
     }else {
/***************************Ѱ�ҳ���·��findPath**********************/

/*****************����,j��ʾ��ǰλ�����꣬endRow,endCol��ʾ����·��********************************/


/************************�ݹ��鵱ǰ���ӵ������ĸ������ϵĸ����Ƿ�����*************************/
         if (i<n-1 && j<m-2 && maze[i][j+1] == 0&& success!=1)
            findpath(i,j+1,endRow,endCol,maze,n,m);//right
         if (i<n-2 && j<m-1 && maze[i+1][j] == 0&& success!=1)
            findpath(i+1,j,endRow,endCol,maze,n,m);//down
         if (i<n-1 && j>1 && maze[i][j-1] == 0&& success!=1)
            findpath(i,j-1,endRow,endCol,maze,n,m);//left
         if (i>1 && j<m-1 && maze[i-1][j] == 0&& success!=1)
            findpath(i-1,j,endRow,endCol,maze,n,m); //up
     if(success !=1)
        maze[i][j]=0;//0û�߹���1�����ߣ�2�߹���
     return success;
     }
 }


/*
�������Թ�������N������M

4 4
�������Թ���1��ʾǽ�ڣ�0��ʾ���ߵ�ͨ��
1 1 1 1
1 0 0 1
1 1 0 1
1 1 1 1
�������Թ���������꣺
1 1
�������Թ��ĳ�������:
2 2
�Թ�ʾ��ͼ������ʾ��
 #  #  #  #
 #        #
 #  #     #
 #  #  #  #

 #  #  #  #
 #  @  @  #
 #  #  @  #
 #  #  #  #


 �������Թ�������N������M

11 10
�������Թ���1��ʾǽ�ڣ�0��ʾ���ߵ�ͨ��
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
�������Թ���������꣺
1 1
�������Թ��ĳ�������:
9 8
ע������PDF��8 9 ����ʽM��N�У���һ��ϰ�ߵĲ�һ������������ǣ����ź����ܣ���д��ʱ������л�������
�Թ�ʾ��ͼ������ʾ��
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



/*********************����������main****************************/


int main()
{
    cout << "�������Թ�������N������M\n" << endl;
    int n,m;
    cin>>n>>m;
    int** maze = new int* [n];
    for(int i=0;i<n;i++){
        maze[i]=new int[m];
    }
    cout<<"�������Թ���1��ʾǽ�ڣ�0��ʾ���ߵ�ͨ��"<<endl;
    for(int i=0;i<n;i++){
        for(int j=0;j<m;j++){
            cin>>maze[i][j];
        }
    }
    int startRow,startCol,endRow,endCol;
    cout<<"�������Թ���������꣺"<<endl;
    cin>>startRow>>startCol;
    cout<<"�������Թ��ĳ�������:"<<endl;
    cin>>endRow>>endCol;
    showMaze(startRow,startCol,endRow,endCol,maze,n,m);
    return 0;

}


