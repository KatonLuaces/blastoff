#include <stdlib.h>
#include <stdio.h>


typedef struct matrix_placeholder {
    int testData;
} matrix;

matrix* matmul(matrix* a, matrix* b){
    return a;
}

#ifdef RUN_TEST
int main(int argc, char** argv){
    printf("Testing\n");
}
#endif
