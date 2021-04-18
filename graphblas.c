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

void matrix_print(struct matrix *A)
{
    GrB_Index nrows, ncols, i;
    int32_t elem;
    
    if (!GrB_ok(GrB_Matrix_nrows(&nrows, A->mat)))
        GrB_die("GrB_Matrix_nrows", A->mat);

    if (!GrB_ok(GrB_Matrix_ncols(&ncols, A->mat)))
        GrB_die("GrB_Matrix_ncols", A->mat);

    if (ncols != 1)
        die("Tried to print string with more than 1 col");

    for (i = 0; i < nrows; i++) {
        if (!GrB_ok(GrB_Matrix_extractElement(&elem, A->mat, i, 0)))
            GrB_die("GrB_Matrix_extractElement", A->mat);
        putchar(elem);
    }
}

void matrix_setelem(struct matrix *A, int val, int row, int col)
{
    // 0 is the implicit value; storing it explicitly would waste space
    if (val == 0)
        return;
    
    if (!GrB_ok(GrB_Matrix_setElement(A->mat, val, row, col)))
        GrB_die("GrB_Matrix_setElement", A->mat);
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

    matrix_print(A);
    matrix_print(B);
    matrix_print(C);
}
#endif
