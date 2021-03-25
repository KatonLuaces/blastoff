#include <stdlib.h>
#include <stdio.h>

struct matrix {
    int nrows;
    int ncols;
    int **data;
};

struct matrix *matrix_create(int nrows, int ncols)
{
    struct matrix *m;
    int i;
    
    m = malloc(sizeof(struct matrix));

    m->nrows = nrows;
    m->ncols = ncols;
    m->data = malloc(m->nrows * sizeof(*m->data));
    for (i = 0; i < m->nrows; i++)
        m->data[i] = calloc(m->ncols, sizeof(**m->data));
    
    return m;
}

int matrix_print(struct matrix *m)
{
    int r, c;

    for (r = 0; r < m->nrows; r++) {
        for (c = 0; c < m->ncols; c++)
            printf("%d ", m->data[r][c]);
        printf("\n");
    }

    return 1;
}

#ifdef RUN_TEST
int main(int argc, char** argv){
    struct matrix *m = matrix_create(3, 5);
    matrix_print(m);
    return 0;
}
#endif
