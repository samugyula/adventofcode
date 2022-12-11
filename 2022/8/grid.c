#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#define SIZE 1024

int maxInRow(int, int, int, int**);
int maxInCol(int, int, int, int**);
bool isVisible(int , int , int,  int**);
int scoreInRow(int, int, int, int**);
int scoreInCol(int, int, int, int**);
int totalScore(int, int, int, int**);

int main(){

    char buffer[SIZE];
    FILE* fin;
    char filename[] = "data.txt";
    fin = fopen(filename,"r");

    if(fin == NULL){ printf("File not found: %s\n", filename); return -1; }

    fgets(buffer, SIZE, fin);
    buffer[strlen(buffer)-1]='\0';
    int size = strlen(buffer)*sizeof(char);
    fclose(fin);
    
    int l = sizeof(int *)*size + sizeof(int)*size*size;
    int **grid = (int **)malloc(l);
    int *pt = (int *)(grid + size);
    for(int i = 0; i < size; i++)
        grid[i] = (pt + size * i);

    fin = fopen(filename,"r");
    int ii = 0;
    while( fgets(buffer, SIZE, fin)){
        for(int i=0; i<size; i++) grid[ii][i] = buffer[i] - '0';
        ii++;
    }
    fclose(fin);

    int count = 0;
    int mScore=0;
    for(int i=0; i < size; i++){
        for(int j=0; j < size; j++){
            if(isVisible(i,j,size,grid)) count++;
            int score = totalScore(i,j,size,grid);
            if(score > mScore) mScore = score;
        }
    }

    printf("%d\n",count);
    printf("%d\n",mScore);

    free(grid);
    
    return 0;

}

bool isVisible(int i, int j, int size, int** grid){
    if (i == 0 || j == 0 || i == (size-1) || j == (size-1)) return true;
    int mUp    = maxInCol(0,i,j,grid);
    int mDown  = maxInCol(i+1,size,j,grid);
    int mLeft  = maxInRow(0,j,i,grid);
    int mRight = maxInRow(j+1,size,i,grid);
    int val = grid[i][j];
    return (mUp < val || mDown < val || mLeft < val || mRight < val);
}

int maxInCol(int a, int b, int c, int** grid){
    int max = grid[a][c];
    for(int i = a; i<b; i++){
        if (max < grid[i][c]) max = grid[i][c];
    }
    return max;
}
    

int maxInRow(int a, int b, int c, int** grid){
    int max = grid[c][a];
    for(int i = a; i<b; i++){
        if (max < grid[c][i]) max = grid[c][i];
    }
    return max;
}

int scoreInCol(int i, int j, int size, int** grid){
    int a = i;
    int pt1 = 0;
    while(a > 0 && grid[i][j] > grid[a-1][j]){
        pt1++;
        a--;
    }
    if(a > 0) pt1++;
    a = i;
    int pt2 = 0;
    while(a < (size-1) && grid[i][j] > grid[a+1][j]){
        pt2++;
        a++;
    }
    if(a < (size-1)) pt2++;
    return pt1*pt2;
}

int scoreInRow(int i, int j, int size, int** grid){
    int a = j;
    int pt1 = 0;
    while(a > 0 && grid[i][j] > grid[i][a-1]){
        pt1++;
        a--;
    }
    if(a > 0) pt1++;
    a = j;
    int pt2 = 0;
    while((a < (size-1)) && (grid[i][j] > grid[i][a+1])){
        pt2++;
        a++;
    }
    if(a < (size-1)) pt2++;
    return pt1*pt2;
}

int totalScore(int i, int j, int size, int** grid){
    return scoreInRow(i,j,size,grid)*scoreInCol(i,j,size,grid);
}
