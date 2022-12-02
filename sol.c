#define BUFFER 1024
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char* argv[]){

    FILE* fin;

    char input[BUFFER];

    int nnums = 1;

    fin = fopen(argv[1],"r");
    while( fgets(input, BUFFER, fin) ){
        if(strcmp(input,"\n") == 0) nnums += 1;
    }
    fclose(fin);

    int* arr = malloc(sizeof(int)*nnums);

    fin = fopen(argv[1],"r");
    int res = 0;
    int ind = 0;
    while( fgets(input, BUFFER, fin) ){
        if(strcmp(input,"\n") == 0) {
            arr[ind++] = res; 
            res = 0;
        } else {
            res += atoi(input);
        }
    }
    arr[ind++] = res; 
    
    fclose(fin);

    int max=arr[0];
    for(int i=1; i < nnums; i++){
        if(arr[i] > max) max = arr[i];
    }

    printf("%d\n",max);

    free(arr);

}
