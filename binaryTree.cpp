/*
*已知前序遍历序列和中序遍历序列建立二叉树
*并求其后序遍历序列和层序遍历序列
*/
#include <iostream>
#include <string>
#include <queue>
#include <string.h>
#include <cstdlib>
#include <math.h>

//二叉树节点结构定义
struct BIN_NODE {
    char data;
    BIN_NODE *leftChild, *rightChild;
};


#define MAX(a,b) (((a)>(b))?(a):(b))

using namespace std;

int PrintTree_h_height;
char PrintTree_h_buffer[6][128];
int PrintTree_h_x;


int PrintTree_h_treeHeight(BIN_NODE* tree){
	if (tree == NULL) return 0;
	int heightLeft = PrintTree_h_treeHeight(tree->leftChild);
	int heightRight = PrintTree_h_treeHeight(tree->rightChild);
	return MAX(heightLeft, heightRight) + 1;
}

void PrintTree_h_corePrintTree(BIN_NODE* tree, int level){
	if (tree == NULL){
		PrintTree_h_x += (pow(2, PrintTree_h_height - level) - 1);
		return;
	}
	char(*a)[128] = PrintTree_h_buffer;
	PrintTree_h_corePrintTree(tree->leftChild, level + 1);
	a[level][PrintTree_h_x++] = tree->data;
	PrintTree_h_corePrintTree(tree->rightChild, level + 1);
}

#define INF 127

void printTree(BIN_NODE* tree){
	if (tree == NULL) return;
	char(*a)[128] = PrintTree_h_buffer;
	for (int i = 0; i<6; i++){
		for (int j = 0; j<128; j++){
			a[i][j] = INF;
		}
	}
	//先获取树高度
	PrintTree_h_height = PrintTree_h_treeHeight(tree);
	if (PrintTree_h_height > 6){
		cout << "树超过6层，无法打印" << endl;
		return;
	}
	PrintTree_h_corePrintTree(tree, 0);
	for (int i = 0; i < 6; i++){
		for (int j = 0; j < 128; j++){
			if (a[i][j] == INF) cout << " ";
			else cout << a[i][j];
		}
		cout << endl;
	}
}

/*


*/

//序列存储变量
//用户输入字符串测试
//char preSequence[100];
//char inSequence[100];
//指定字符串测试
//char *preSequence = "GDAFEMHZ";
//char* inSequence = "ADEFGHMZ";
char *preSequence = "ABDFCEGH";
char* inSequence = "BFDAGEHC";
//树根定义
BIN_NODE *tree;
//存储下一层节点的队列
std::queue<BIN_NODE *> memoryNextLevel;

//函数声明
void PrintBinTreeByPostOrder(BIN_NODE *subTree);        //后序打印二叉树数据域
void PrintBinTreeByLevelOrder(BIN_NODE *subTree);       //层序打印二叉树数据域
//通过前序、中序序列建立二叉树
void postorder(BIN_NODE*  t);
void traverse_level(BIN_NODE *root);
BIN_NODE * CreateBinTreeByPreInOrder(char* preSeq, char* InSeq, int subStrLen);

int main()
{
    system("color 0f");//改变控制台前景，背景颜色
	system("title BinaryTree");
    int groupsAmount = 1;
    //std::cin >> groupsAmount;
    while (groupsAmount--) {
        //std::cin >> preSequence >> inSequence;
        tree = CreateBinTreeByPreInOrder(preSequence, inSequence, strlen(inSequence));
        postorder(tree);
        std::cout << std::endl;
        traverse_level(tree);//层级输出
        std::cout << std::endl;
        printTree(tree);

    }
    return 0;
}
//层次遍历
void traverse_level(BIN_NODE *root)
{
    if (root == 0) {
        return;
    }
    std::queue<BIN_NODE*>  qnodes;
    std::vector<int>       num_nodes; // num_nodes[i] :　第ｉ层结点总数

    num_nodes.push_back(1);     // 第0层结点个数
    num_nodes.push_back(0);

    int n = 0, depth = 0;
    for (qnodes.push(root); !qnodes.empty(); qnodes.pop()) {
        BIN_NODE*temp = qnodes.front();

        std::cout << temp->data << " ";

        if (temp->leftChild) {
            qnodes.push(temp->leftChild);
            num_nodes[depth+1]++;
        }

        if (temp->rightChild) {
            qnodes.push(temp->rightChild);
            num_nodes[depth+1]++;
        }

        // 当前层最后一个节点
        if (++n == num_nodes[depth]) {
            n = 0;
            depth++;
            num_nodes.push_back(0);
            std::cout << std::endl;
        }
    }
}

//二叉树的后序遍历
void postorder(BIN_NODE*  t)
{
    if(t)
    {
        postorder(t->leftChild);
        postorder(t->rightChild);
        std::cout<<t->data<<std::endl;
    }
}

BIN_NODE * CreateBinTreeByPreInOrder(char* preSeq, char* InSeq, int subStrLen) {
    if (0 == subStrLen) {
        return NULL;
    }
    BIN_NODE *node = new BIN_NODE;
    if (node == NULL) {
        std::cerr << "error" << std::endl;
        exit(1);
    }
    node->data = *preSeq;
    //前序相应元素在中序中的下标索引值
    int rootIndex = 0;
    //求解这个索引值
    for (; rootIndex < subStrLen; rootIndex ++) {
        if (InSeq[rootIndex] == *preSeq) {
            break;
        }
    }
    node->leftChild = CreateBinTreeByPreInOrder(preSeq + 1, InSeq, rootIndex);
    node->rightChild = CreateBinTreeByPreInOrder(preSeq + rootIndex + 1,
        InSeq + rootIndex + 1, subStrLen - (rootIndex + 1));
    return node;
}

