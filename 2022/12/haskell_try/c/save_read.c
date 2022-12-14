#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#define SIZE 1024

char** initArr(int s1, int s2){

    int len1, len2;
    len1 = s1 * sizeof(char *);
    len2 = s1 * s2 * sizeof(char);
    char **arr = malloc(len1 + len2); 
    if (arr == NULL) { printf("malloc ERROR\n"); return arr; }

    char *ptr = (char *)(arr + s1);

    for(int i = 0; i<s1; ++i) 
        arr[i] = ptr + i * s2 ;
    
    return arr;

}

short int*** initIntArr3D(int s1, int s2, int s3){

    int len1, len2, len3;
    len1 = s1 * sizeof(short int **);
    len2 = s1 * s2 * sizeof(short int *);
    len3 = s1 * s2 * s3 * sizeof(short int );
    short int ***arr = malloc(len1 + len2 + len3); 
    if (arr == NULL) { printf("malloc ERROR\n"); return arr; }

    short int **ptr1 = (short int **)(arr + s1);
    short int *ptr2 = (short int *)(arr + s1 + s1*s2 );

    for(int i = 0; i < s1; ++i) {
        arr[i] = ptr1 + i * s2 ;
        for(int j = 0; j < s2; j++){
            arr[i][j] = ptr2 + i*s2*s3 + j*s3;
            for(int k = 0; k < s3; k++){
                arr[i][j][k] = -1;
            }
        }
    }

    return arr;
}

int** initIntArr(int s1, int s2){

    int len1, len2;
    len1 = s1 * sizeof(int *);
    len2 = s1 * s2 * sizeof(int);
    int **arr = malloc(len1 + len2); 
    if (arr == NULL) { printf("malloc ERROR\n"); return arr; }

    int *ptr = (int *)(arr + s1);

    for(int i = 0; i < s1; ++i) {
        arr[i] = ptr + i * s2 ;
        for(int j = 0; j < s2; j++){
            arr[i][j] = 0;
        }
    }

    
    return arr;

}

int** convertToInt(int s1, int s2, int shift, char** arr){

    int** res = initIntArr(s1,s2);

    for(int i = 0; i < s1; i++){
        for(int j = 0; j < s2; j++){
            res[i][j] = arr[i][j] - shift;
        }
    }

    return res;

}

int* getDim(){
    int* res = malloc(sizeof(int)*2);

    FILE* fin;
    char filename[] = "data.txt";
    fin = fopen(filename,"r");
    if(fin == NULL){ printf("File not found: %s\n", filename); return NULL; }


    char buffer[SIZE+1];
    fgets(buffer,SIZE,fin);

    int s2 = strlen(buffer) - 1;
    int s1 = 1;
    while( fgets(buffer, SIZE, fin) ){
        s1++;
    }

    fclose(fin);

    res[0] = s1;
    res[1] = s2;
    return res;
}

char** readIn(){


    FILE* fin;
    char filename[] = "data.txt";
    fin = fopen(filename,"r");
    if(fin == NULL){ printf("File not found: %s\n", filename); return NULL; }


    char buffer[SIZE+1];
    fgets(buffer,SIZE,fin);

    int s2 = strlen(buffer) - 1;
    int s1 = 1;
    while( fgets(buffer, SIZE, fin) ){
        s1++;
    }


    fclose(fin);

    char** arr = initArr(s1,s2);

    fin = fopen(filename,"r");
    int i = 0;
    while( fgets(buffer, SIZE+1, fin) ){
        buffer[strlen(buffer)-1] = '\0';
        strcpy(arr[i++],buffer);
    }

    fclose(fin);

    return arr;
}
