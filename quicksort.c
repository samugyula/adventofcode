#include <stdlib.h>
#include "quicksort.h"

void sort_part(int* srt, int* part, int len, int shift){
    if(len > 0) {
        int* res = sort(part,len);
        for(int i=0; i < len; i++) srt[i+shift] = res[i];
        free(res);
    }
}

int* sort(int* unsorted, int len){

    int* srt = malloc(sizeof(int)*len);

    if(len == 1){
        srt[0] = unsorted[0];
        return srt;
    }

    int pivot = unsorted[0];
    int* smaller = malloc(sizeof(int)*len);
    int* bigger  = malloc(sizeof(int)*len);
    int i = 0, j = 0;
    for(int ii = 1; ii < len; ii++){
        if(unsorted[ii] >= pivot) {
            bigger[i++]  = unsorted[ii];
        } else {
            smaller[j++] = unsorted[ii];
        }
    }

    sort_part(srt, smaller, j, 0);

    srt[j] = pivot;

    sort_part(srt, bigger, i, j + 1);

    free(smaller);
    free(bigger);

    return srt;
}
