#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <GraphBLAS.h>

struct matrix {
    GrB_Matrix mat;
};

static void die(const char *msg)
{
    if (errno)
        perror(msg);
    else
        fprintf(stderr, "%s\n", msg);
    exit(1);
}

#define GrB_die(msg, object)            \
do {                                    \
    const char *GrB_msg;                \
    GrB_error(&GrB_msg, object);        \
    fprintf(stderr, "%s\n", GrB_msg);   \
    die(msg);                           \
} while (0)

static int GrB_ok(GrB_Info info)
{
    if (info == GrB_SUCCESS || info == GrB_NO_VALUE) {
        return 1;
    } else {
        fprintf(stderr, "GrB_ok saw error code: %d\n", info);
        return 0;
    }
}

void GrB_print(GrB_Matrix mat)
{
    if (!GrB_ok(GxB_Matrix_fprint(mat, NULL, GxB_COMPLETE_VERBOSE, stdout)))
        die("GxB_Matrix_fprint");
}

void GrB_size(GrB_Matrix mat, GrB_Index *nrows, GrB_Index *ncols)
{
    if (!GrB_ok(GrB_Matrix_nrows(nrows, mat)))
        GrB_die("GrB_Matrix_nrows", mat);

    if (!GrB_ok(GrB_Matrix_ncols(ncols, mat)))
        GrB_die("GrB_Matrix_ncols", mat);
}

/* automatically called before main() */
__attribute__((constructor))
static void matrix_lib_init(void) {
    if (!GrB_ok(GrB_init(GrB_BLOCKING)))
        die("GrB_init");
}

/* automatically called after main() */
__attribute__((destructor))
void matrix_lib_finalize(void)
{
    if (!GrB_ok(GrB_finalize()))
        die("GrB_finalize");
}

/* BELOW: Functions used externally */

struct matrix *matrix_create(int nrows, int ncols)
{
    struct matrix *A;
    if (!(A = malloc(sizeof *A)))
        die("malloc failed");

    if (!GrB_ok(GrB_Matrix_new(&A->mat, GrB_INT32, nrows, ncols)))
        GrB_die("GrB_Matrix_new", A->mat);
    
    return A;
}

int matrix_getelem(struct matrix *A, int row, int col)
{
    int32_t elem = 0;

    if (!GrB_ok(GrB_Matrix_extractElement(&elem, A->mat, row, col)))
        GrB_die("GrB_Matrix_extractElement", A->mat);
    
    return elem;
}

void matrix_setelem(struct matrix *A, int val, int row, int col)
{
    // 0 is the implicit value; storing it explicitly would waste space
    if (val == 0)
        return;

    if (!GrB_ok(GrB_Matrix_setElement(A->mat, val, row, col)))
        GrB_die("GrB_Matrix_setElement", A->mat);
}

void matrix_print(struct matrix *A)
{
    GrB_Index nrows, ncols, i;
    int elem;

    GrB_size(A->mat, &nrows, &ncols);
    if (ncols != 1)
        die("Tried to print string with more than 1 col");

    for (i = 0; i < nrows && (elem = matrix_getelem(A, i, 0)) != 0; i++)
        putchar(elem);
}

struct matrix *matrix_tostring(struct matrix *A)
{
    struct matrix *B;
    GrB_Index nrows, ncols, i, j, k;
    char buf[1000], *b;

    GrB_size(A->mat, &nrows, &ncols);
    B = matrix_create(nrows * (ncols + 1) * 20, 1);

    k = 0;
    for (i = 0; i < nrows; i++) {
        for (j = 0; j < ncols; j++) {
            snprintf(buf, sizeof(buf), "%d ", matrix_getelem(A, i, j));
            for (b = buf; *b; b++)
                matrix_setelem(B, *b, k++, 0);
        }
        matrix_setelem(B, '\n', k++, 0);
    }
    matrix_setelem(B, 0, k, 0);

    return B;
}


struct matrix *matrix_mul(struct matrix *A, struct matrix *B)
{
    struct matrix *C;
    GrB_Info info;
    GrB_Index nrows, ncols;
    
    if (!GrB_ok(GrB_Matrix_nrows(&nrows, A->mat)))
        GrB_die("GrB_Matrix_nrows", A->mat);

    if (!GrB_ok(GrB_Matrix_ncols(&ncols, B->mat)))
        GrB_die("GrB_Matrix_ncols", B->mat);

    C = matrix_create(nrows, ncols);

    info = GrB_mxm(C->mat,
                   GrB_NULL,
                   GrB_NULL,
                   GrB_PLUS_TIMES_SEMIRING_INT32,
                   A->mat,
                   B->mat,
                   GrB_NULL);

    if (!GrB_ok(info))
        GrB_die("GrB_mxm", A->mat);
    
    return C;
}

struct matrix *matrix_conv(struct matrix *A, struct matrix *B)
{
    struct matrix *C;
    GrB_Info info;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols, C_nrows, C_ncols;
    int32_t sum;
    int i, j, v, w;

    GrB_size(A->mat, &A_nrows, &A_ncols);
    GrB_size(B->mat, &B_nrows, &B_ncols);

    if (A_nrows < B_nrows || A_ncols < B_ncols)
        die("matrix_conv bad dimensions");

    C_nrows = A_nrows - B_nrows + 1;
    C_ncols = A_ncols - B_ncols + 1;
    C = matrix_create(C_nrows, C_ncols);

    for (i = 0; i < C_nrows; i++) {
        for (j = 0; j < C_ncols; j++) {
            sum = 0;
            for (v = 0; v < B_nrows; v++) {
                for (w = 0; w < B_ncols; w++) {
                    sum += matrix_getelem(B, v, w) 
                           * matrix_getelem(A, i+v, j+w);
                }
            }
            matrix_setelem(C, sum, i, j);
        }
    }

    return C;
}


#ifdef RUN_TEST
int main(int argc, char** argv){
    struct matrix *A, *B, *C;
    
    A = matrix_create(3, 4);
    matrix_setelem(A, 1, 0, 0);
    matrix_setelem(A, 2, 1, 2);
    matrix_setelem(A, 2, 2, 2);

    B = matrix_create(4, 3);
    matrix_setelem(B, 3, 0, 0);
    matrix_setelem(B, 4, 1, 2);
    matrix_setelem(B, 5, 2, 2);

    C = matrix_mul(A, B);
    matrix_print(matrix_tostring(A));
    printf("\n");
    matrix_print(matrix_tostring(B));
    printf("\n");
    matrix_print(matrix_tostring(C));
    printf("\n");
}
#endif
