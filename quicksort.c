#ifndef QUICKSORT
#define QUICKSORT
#include <stdlib.h>
#include <stdio.h>

int* sorted(int* unsorted, int len){

    int* srt = malloc(sizeof(int)*len);

    if(len==1){
        srt[0] = unsorted[0];
        return srt;
    }

    int pivot = unsorted[0];
    int* smaller = malloc(sizeof(int)*len);
    int* bigger  = malloc(sizeof(int)*len);
    int i = 0, j = 0;
    for(int ii=1;ii<len;ii++){
        if(unsorted[ii] >= pivot) bigger[i++]  = unsorted[ii];
        if(unsorted[ii] <  pivot) smaller[j++] = unsorted[ii];
    }

    if(j>0) {
        int* res_smaller = sorted(smaller,j);
        for(int jj=0; jj<j; jj++) srt[jj] = res_smaller[jj];
        free(res_smaller);
    }

    srt[j] = pivot;

    if(i>0) {
        int* res_bigger =  sorted(bigger, i);
        for(int ii=0; ii<i; ii++) srt[ii+j+1] = res_bigger[ii];
        free(res_bigger);
    }

    free(smaller);
    free(bigger);

    return srt;
}

#endif
