#include <unistd.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#define SIZE 25

char** initArr();
char** readIn();
char** randomLights();
int countNeighbors(char, int, int, char **);
char** changeLights(char **, bool);
int countLights(char **);
void res(char **, bool);
void printTable(char **);
void animate(char **, bool);

int main(){

//  char** lights = readIn();
    char** lights = randomLights();
    if(lights==NULL) {return -1;}

//  res(lights,false);
    animate(lights,false);
    if(lights==NULL) {return -1;}
    return 0;

    lights = readIn();
    if(lights==NULL) {return -1;}
    lights[0][0]  ='#';
    lights[0][SIZE - 1] ='#';
    lights[SIZE - 1][0] ='#';
    lights[SIZE - 1][SIZE - 1]='#';

    res(lights,true);
    if(lights==NULL) {return -1;}

    return 0;
}

void res(char **lights, bool edgeStays){
    for(int i=0; i<100; i++) {
        lights = changeLights(lights, edgeStays);
        if(lights == NULL) {return;};
    }
    printf("%d\n",countLights(lights));
    printTable(lights);
    free(lights);
}

void printTable(char **lights){
    for(int i = 0; i < SIZE; i++){
        for(int j = 0; j < SIZE; j++){
            printf("%c ",lights[i][j]);
        }
        printf("\n");
    }
    for(int i=0; i<5; i++) printf("\n");
}

void animate(char **lights, bool edgeStays){
    while(1){
        lights = changeLights(lights, edgeStays);
        if(lights == NULL) {return;};
        printTable(lights);
        usleep(5e5);
    }
    free(lights);
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
    if(newLights==NULL) {return NULL;}
    for(int i=0; i<SIZE; i++){
        for(int j=0; j<SIZE; j++){
            if(edgeStays && ((i == 0 && j == 0) || 
                             (i == (SIZE - 1) && j == 0) || 
                             (i == 0 && j == (SIZE - 1)) || 
                             (i == (SIZE - 1) && j == (SIZE - 1)))){
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
    char filename[] = "data2.txt";
    fin = fopen(filename,"r");
    if(fin == NULL){ printf("File not found: %s\n", filename); return NULL; }


    char buffer[SIZE+1];

    int i = 0;
    while( fgets(buffer, SIZE+1, fin) ){
        int ii = 0;
        while(buffer[ii] != '\0'){
            ii++;
        }
        if(ii == SIZE){
            strcpy(arr[i++],buffer);
        }
    }

    fclose(fin);

    return arr;
}

char** randomLights(){
    char** arr = initArr();

    srand(time(NULL));

    for(int i = 0; i < SIZE; i++){
        for(int j = 0; j < SIZE; j++){
            if((rand() % 100 + 1) > 50){
                arr[i][j] = '#';
            } else {
                arr[i][j] = '.';
            }
        }
    }

    return arr;
}

    
