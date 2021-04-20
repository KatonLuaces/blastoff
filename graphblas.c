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
    if (nrows && !GrB_ok(GrB_Matrix_nrows(nrows, mat)))
        GrB_die("GrB_Matrix_nrows", mat);

    if (ncols && !GrB_ok(GrB_Matrix_ncols(ncols, mat)))
        GrB_die("GrB_Matrix_ncols", mat);
}

int32_t GrB_scalar(GrB_Matrix mat)
{
    GrB_Index nrows, ncols;
    int32_t elem;

    GrB_size(mat, &nrows, &ncols);
    if (nrows != 1 || ncols != 1)
        die("GrB_scalar mat dims bad");

    if (!GrB_ok(GrB_Matrix_extractElement(&elem, mat, 0, 0)))
        GrB_die("GrB_Matrix_extractElement", mat);

    return elem;
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
    int32_t unused;
    if (val == 0 &&
            GrB_Matrix_extractElement(&unused, A->mat, row, col) == GrB_NO_VALUE)
        return;

    if (!GrB_ok(GrB_Matrix_setElement(A->mat, val, row, col)))
        GrB_die("GrB_Matrix_setElement", A->mat);
}

struct matrix *matrix_create(int nrows, int ncols)
{
    struct matrix *A;
    if (!(A = malloc(sizeof *A)))
        die("malloc failed");

    if (!GrB_ok(GrB_Matrix_new(&A->mat, GrB_INT32, nrows, ncols)))
        GrB_die("GrB_Matrix_new", A->mat);

    return A;
}

struct matrix *matrix_create_zero(struct matrix *dims)
{
    GrB_Index dim_nrows, dim_ncols, nrows, ncols;

    GrB_size(dims->mat, &dim_nrows, &dim_ncols);
    if ((dim_nrows != 1 && dim_nrows != 2) || dim_ncols != 1)
        die("matrix_create_zero invalid dims arg");

    nrows = matrix_getelem(dims, 0, 0);
    ncols = dim_nrows == 2 ? matrix_getelem(dims, 1, 0) : nrows;

    return matrix_create(nrows, ncols);
}

struct matrix *matrix_create_identity(struct matrix *N_scalar)
{
    struct matrix *A;
    GrB_Index i, n;

    n = GrB_scalar(N_scalar->mat);
    A = matrix_create(n, n);
    for (i = 0; i < n; i++)
        matrix_setelem(A, 1, i, i);

    return A;
}

struct matrix *matrix_create_range(struct matrix *range)
{
    struct matrix *A;
    int32_t lo, hi;
    GrB_Index i, range_nrows, range_ncols;

    GrB_size(range->mat, &range_nrows, &range_ncols);
    if (range_nrows == 1 && range_ncols == 1) {
        lo = 0;
        hi = matrix_getelem(range, 0, 0);
    } else if (range_nrows == 2 && range_ncols == 1) {
        lo = matrix_getelem(range, 0, 0);
        hi = matrix_getelem(range, 1, 0);
    } else {
        die("matrix_create_range invalid range arg");
    }

    if (lo > hi)
        return matrix_create(0, 1);

    A = matrix_create(hi - lo + 1, 1);
    i = 0;
    while (lo <= hi)
        matrix_setelem(A, lo++, i++, 0);

    return A;
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

    if (nrows == 0 || ncols == 0)
        return B;

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

    GrB_size(A->mat, &nrows, NULL);
    GrB_size(B->mat, NULL, &ncols);

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

struct matrix *matrix_elmul(struct matrix *A, struct matrix *B)
{
    struct matrix *C;
    GrB_Info info;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols;

    GrB_size(A->mat, &A_nrows, &A_ncols);
    GrB_size(B->mat, &B_nrows, &B_ncols);

    if (A_nrows != B_nrows || A_ncols != B_ncols)
        die("matrix_elmul bad dimensions");

    C = matrix_create(A_nrows, A_ncols);

    info = GrB_Matrix_eWiseMult_Semiring(C->mat,
                                         GrB_NULL,
                                         GrB_NULL,
                                         GrB_PLUS_TIMES_SEMIRING_INT32,
                                         A->mat,
                                         B->mat,
                                         GrB_NULL);

    if (!GrB_ok(info))
        GrB_die("GrB_Matrix_eWiseMult_Semiring", A->mat);

    return C;
}

struct matrix *matrix_eladd(struct matrix *A, struct matrix *B)
{
    struct matrix *C;
    GrB_Info info;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols;

    GrB_size(A->mat, &A_nrows, &A_ncols);
    GrB_size(B->mat, &B_nrows, &B_ncols);

    if (A_nrows != B_nrows || A_ncols != B_ncols)
        die("matrix_eladd bad dimensions");

    C = matrix_create(A_nrows, A_ncols);

    info = GrB_Matrix_eWiseAdd_Semiring(C->mat,
                                        GrB_NULL,
                                        GrB_NULL,
                                        GrB_PLUS_TIMES_SEMIRING_INT32,
                                        A->mat,
                                        B->mat,
                                        GrB_NULL);

    if (!GrB_ok(info))
        GrB_die("GrB_Matrix_eWiseAdd_Semiring", A->mat);

    return C;
}

struct matrix *matrix_conv(struct matrix *A, struct matrix *B)
{
    struct matrix *C;
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

struct matrix *matrix_extract(struct matrix *M, struct matrix *A, struct matrix *B, struct matrix *C, struct matrix *D)
{
    struct matrix *R;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols, C_nrows, C_ncols, D_nrows, D_ncols, R_nrows, R_ncols;
    int i, j, v, w;


    // verify that A, B, C, D are all integer matrices??

    //veryify that A, B are column vectors and that C, D are 1x1

    GrB_size(A->mat, &A_nrows, &A_ncols);
    GrB_size(B->mat, &B_nrows, &B_ncols);
    GrB_size(C->mat, &C_nrows, &C_ncols);
    GrB_size(D->mat, &D_nrows, &D_ncols);

    if (A_ncols != 1 || B_ncols != 1 || C_ncols != 1 || C_nrows != 1 || D_nrows != 1 || D_ncols != 1)
        die("matrix_extract bad dimensions");

    int cval = matrix_getelem(C, 0, 0);
    int dval = matrix_getelem(D, 0, 0);
    R = matrix_create(A_ncols*cval, B_ncols*dval);

    //(A[i], B[j]) is top-left corner in form (cols, rows)
    //(A[i]+v, B[j]+w) is what we iterate through
    //(i*cval+v, j*dval+w) is where we store
    int outi = 0;
    for (i = 0; i < A_nrows; i++){
      for (j = 0; j < B_nrows; j++){
        int Ai = matrix_getelem(A, i, 0);
        int Bj = matrix_getelem(B, j, 0);
        for (v = 0; v < cval; v++){
          for (w = 0; w < dval; w++){
              matrix_setelem(R, matrix_getelem(M, Ai, Bj), i*cval+v, j*dval+w);
          }
        }
      }
    }

    return R;
}


#ifdef RUN_TEST
int main(int argc, char** argv){
    struct matrix *A, *B, *C;

    A = matrix_create(2, 1);
    matrix_setelem(A, 3, 0, 0);
    matrix_setelem(A, 6, 1, 0);

    matrix_print(matrix_tostring(A));
    printf("\n");

    B = matrix_create_range(A);

    matrix_print(matrix_tostring(B));
    printf("\n");
}
#endif
