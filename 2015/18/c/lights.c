#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#define SIZE 100

char** initArr();
char** readIn();
int countNeighbors(char, int, int, char **);
char** changeLights(char **, bool);
int countLights(char **);

int main(){

    char** lights = readIn();
    if(lights==NULL) {return -1;}

    for(int i=0; i<100; i++) lights = changeLights(lights, false);
    printf("%d\n",countLights(lights));

    free(lights);

    lights = readIn();
    if(lights==NULL) {return -1;}
    lights[0][0]  ='#';
    lights[0][99] ='#';
    lights[99][0] ='#';
    lights[99][99]='#';

    for(int i=0; i<100; i++) lights = changeLights(lights, true);
    printf("%d\n",countLights(lights));

    return 0;
}

int countLights(char **lights){
    int count = 0;
    for(int i=0; i<SIZE; i++){
        for(int j=0; j<SIZE; j++){
            if(lights[i][j] == '#') count++;
        }
    }
    return count;
}

char** changeLights(char **lights, bool edgeStays){
    char** newLights = initArr();
    for(int i=0; i<SIZE; i++){
        for(int j=0; j<SIZE; j++){
            if(edgeStays && (i == 0 && j == 0 || i == 99 && j == 0 || i == 0 && j == 99 || i == 99 && j == 99)){
                newLights[i][j] = '#';
                continue;
            }
            char light = lights[i][j];
            int nearLights = countNeighbors('#', i, j, lights);
            if(light == '#'){
                if(nearLights < 2 || nearLights > 3){
                     newLights[i][j] = '.';
                } else {
                     newLights[i][j] = '#';
                }
            } else if(light == '.'){
                if(nearLights == 3){
                    newLights[i][j] = '#';
                } else {
                    newLights[i][j] = '.';
                }
            }
        }
    }
    free(lights);
    return newLights;
}
    

int countNeighbors(char ch, int x, int y, char** lights){
    int count = 0;
    for(int i=x-1; i <= x+1; i++){
        for(int j=y-1; j <= y+1; j++){
            if(i < 0 || i > SIZE-1 || j < 0 || j > SIZE-1){
                if(ch == '.') count++;
                continue;
            }
            if(x == i && y == j) continue;
            if(lights[i][j] == ch) count ++;
        }
    }
    return count;       
}

char** initArr(){

    int len1, len2;
    len1 = SIZE * sizeof(char *);
    len2 = SIZE * SIZE * sizeof(char *);
    char **arr = malloc(len1 + len2); 
    if (arr == NULL) { printf("malloc ERROR\n"); return arr; }

    char *ptr = (char *)(arr + SIZE);

    for(int i = 0; i<SIZE; ++i) 
        arr[i] = ptr + i * SIZE ;
    
    return arr;

}

char** readIn(){

    char** arr = initArr();

    FILE* fin;
    char filename[] = "data.txt";
    fin = fopen(filename,"r");
    if(fin == NULL){ printf("File not found: %s\n", filename); return arr; }


    char buffer[SIZE+1];

    int i = 0;
    while( fgets(buffer, SIZE+1, fin) ){
        int ii = 0;
        while(buffer[ii] != '\0'){
            ii++;
        }
        if(ii == 100){
            strcpy(arr[i++],buffer);
        }
    }

    fclose(fin);

    return arr;
}
