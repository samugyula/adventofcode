#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "read.h"

bool isNeighbor(int, int, int, int, int**);
bool isConnected(int, int, int, int, int**);
int** adjMat(int, int, int**);
long unsigned int shortestDist(int, int, int, int**);

int main(){

    int* sizes = getDim();
    int s1 = sizes[0];
    int s2 = sizes[1];
    char** cmap = readIn(s1,s2);
    int** imap = convertToInt(s1,s2,'0' + 49,cmap);
    free(cmap);

    int** adjM = adjMat(s1,s2,imap);

/*
    for(int x=0; x<s1*s2; x++){
        for(int y=0; y<s1*s2; y++){
            printf("%d",adjM[x][y]);
        }
        printf("\n");
    }
*/
/*
    short int*** memo = initIntArr3D(s1*s2,s1*s2,s1*s2);
    free(imap);
    free(adjM);
    free(memo);
    free(sizes);
    return 0;
*/

    printf("%lu\n",shortestDist(20*173+1,20*173+149,s1*s2,adjM));

    free(adjM);
    free(sizes);
    free(imap);

    return 0;

}

long unsigned int shortestDist(int i, int j, int k, int** g){
    if(k==0){
        if(g[i-1][j-1] == 1){
            return 1;
        } else {
            return 1e6;
        }
    } else {
        long unsigned int d1 = shortestDist(i,j,k-1,g); // (1,4,3) // 
        long unsigned int d2 = shortestDist(i,k,k-1,g); // (1,4,3) //
        long unsigned int d3 = shortestDist(k,j,k-1,g); // (4,4,3) //
        if (d1 < (d2 + d3)) {
            return d1;
        } else {
            return d2 + d3;
        }
    }
} 
    

int** adjMat(int s1, int s2, int** map){

    int** res = initIntArr(s1*s2,s1*s2);

    for(int x1=0; x1<s1; x1++){
        for(int y1=0; y1<s2; y1++){
            int ind1 = x1*s1 + y1;
            for(int x2=x1-1; x2<x1+2; x2++){
                if(x2 < 0 || x2 >= s1) continue; 
                for(int y2=y1-1; y2<y1+2; y2++){
                    if(y2 < 0 || y2 >= s2) continue; 
                    if(x1 == x2 && y1 == y2) continue;
                    if(x1 != x2 && y1 != y2) continue;
                    int ind2 = x2*s1 + y2;
                        if(isConnected(x1,y1,x2,y2,map)){
                            res[ind1][ind2] = 1;
                        } else {
                            res[ind1][ind2] = 0;
                        }
                }
            }
        }
    }

    return res;

}

bool isConnected(int x, int y, int x1, int y1, int** map){

    bool l = (map[x1][y1] + 1) >= map[x][y];

    return l;
}
