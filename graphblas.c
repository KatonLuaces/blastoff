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

// begin ring_* functions //

// stack of rings, implemented as intrusive linked list
struct ring {
    GrB_Semiring ring;
    struct ring *prev;
};

struct ring *curr_ring = NULL;

void ring_push()
{
    struct ring *r = malloc(sizeof(*r));
    r->ring = GrB_PLUS_TIMES_SEMIRING_INT32;
    r->prev = curr_ring;
    curr_ring = r;
}

void ring_pop()
{
    struct ring *prev;

    if (!curr_ring)
        die("ring_change: curr_ring is NULL");

    prev = curr_ring->prev;
    free(curr_ring);
    curr_ring = prev;
}

void ring_change(int which)
{
    if (!curr_ring)
        die("ring_change: curr_ring is NULL");

    if (which == 0) {
        if (!curr_ring->prev)
            die("ring_change to #_ but curr_ring->prev is NULL");
        curr_ring->ring = curr_ring->prev->ring;
    } else if (which == 1) {
        curr_ring->ring = GrB_PLUS_TIMES_SEMIRING_INT32;
    } else if (which == 2) {
        curr_ring->ring = GrB_LAND_LOR_SEMIRING_BOOL;
    } else if (which == 3) {
        curr_ring->ring = GrB_MAX_MIN_SEMIRING_INT32;
    } else {
        die("ring_change: unknown semiring");
    }
}

// end ring_* functions //

// begin matrix_* functions //

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

    A = matrix_create(hi - lo, 1);
    i = 0;
    while (lo < hi)
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
    GrB_Index nrows, ncols, eq1, eq2;

    GrB_size(A->mat, &nrows, &eq1);
    GrB_size(B->mat, &eq2, &ncols);
    if (eq1 != eq2)
        die("matrix_mul bad dimensions");

    C = matrix_create(nrows, ncols);

    info = GrB_mxm(C->mat,
                   GrB_NULL,
                   GrB_NULL,
                   curr_ring->ring,
                   A->mat,
                   B->mat,
                   GrB_NULL);

    if (!GrB_ok(info))
        GrB_die("GrB_mxm", A->mat);

    return C;
}

struct matrix *matrix_exp(struct matrix *A, struct matrix *N_scalar)
{
    struct matrix *B;
    int n;
    GrB_Index i, nrows, ncols;

    GrB_size(A->mat, &nrows, &ncols);
    if (nrows != ncols)
        die("matrix_exp mat not square");

    n = GrB_scalar(N_scalar->mat);
    if (n < 1)
        die("matrix_exp needs positive exponent");
    
    B = A;
    for (i = 0; i < n - 1; i++) {
        B = matrix_mul(A, B);
    }

    return B;
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
                                         curr_ring->ring,
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
                                        curr_ring->ring,
                                        A->mat,
                                        B->mat,
                                        GrB_NULL);

    if (!GrB_ok(info))
        GrB_die("GrB_Matrix_eWiseAdd_Semiring", A->mat);

    return C;
}

struct matrix *matrix_extract(struct matrix *M, struct matrix *A, struct matrix *B, struct matrix *C, struct matrix *D)
{
    struct matrix *R;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols, C_nrows, C_ncols, D_nrows, D_ncols;
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
    R = matrix_create(A_nrows*cval, B_nrows*dval);

    //(A[i], B[j]) is top-left corner in form (cols, rows)
    //(A[i]+v, B[j]+w) is what we iterate through
    //(i*cval+v, j*dval+w) is where we store
    for (i = 0; i < A_nrows; i++){
      for (j = 0; j < B_nrows; j++){
        int Ai = matrix_getelem(A, i, 0);
        int Bj = matrix_getelem(B, j, 0);
        for (v = 0; v < cval; v++){
          for (w = 0; w < dval; w++){
              matrix_setelem(R, matrix_getelem(M, Ai+v, Bj+w), i*cval+v, j*dval+w);
          }
        }
      }
    }

    return R;
}

struct matrix *matrix_insert(struct matrix *M, struct matrix *N, struct matrix *A, struct matrix *B, struct matrix *C, struct matrix *D)
{
    //Syntax is like M[A,B,C,D] = N;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols, C_nrows, C_ncols, D_nrows, D_ncols, N_nrows, N_ncols;
    int i, j, v, w;

    // verify that A, B, C, D are all integer matrices??

    //veryify that A, B are column vectors and that C, D are 1x1

    GrB_size(A->mat, &A_nrows, &A_ncols);
    GrB_size(B->mat, &B_nrows, &B_ncols);
    GrB_size(C->mat, &C_nrows, &C_ncols);
    GrB_size(D->mat, &D_nrows, &D_ncols);
    GrB_size(N->mat, &N_nrows, &N_ncols);

    if (A_ncols != 1 || B_ncols != 1 || C_ncols != 1 || C_nrows != 1 || D_nrows != 1 || D_ncols != 1)
        die("matrix_extract bad dimensions");

    int cval = matrix_getelem(C, 0, 0);
    int dval = matrix_getelem(D, 0, 0);

    if ((N_nrows != cval) | (N_ncols != dval))
        die("matrix_extract size mismatch");

    for (i = 0; i < A_nrows; i++){
      for (j = 0; j < B_nrows; j++){
        int Ai = matrix_getelem(A, i, 0);
        int Bj = matrix_getelem(B, j, 0);
        for (v = 0; v < cval; v++){
          for (w = 0; w < dval; w++){
              matrix_setelem(M, matrix_getelem(N, v, w), Ai+v, Bj+w);
          }
        }
      }
    }

    return N;
}

struct matrix *matrix_size(struct matrix *A)
{
    struct matrix *S;
    GrB_Index nrows, ncols;
    GrB_size(A->mat, &nrows, &ncols);

    S = matrix_create(2,1);

    matrix_setelem(S, nrows, 0, 0);
    matrix_setelem(S, ncols, 1, 0);

    return S;
}

struct matrix *matrix_reduce(struct matrix *A, int mult_flag)
{
    struct matrix *R;
    GrB_Index nrows;
    GrB_size(A->mat, &nrows, NULL);

    GrB_Vector v;
    GrB_Vector_new(&v, GrB_INT32, nrows) ;

    GrB_Monoid op;

    if(mult_flag){
      GrB_BinaryOp mult;
      GxB_Semiring_multiply(&mult, curr_ring->ring);
      // TODO: Find a better way of doing empty product
      GrB_Monoid_new_INT32(&op, mult, 1);
    } else {
      GxB_Semiring_add(&op, curr_ring->ring);
    }

    GrB_Matrix_reduce_Monoid(v, GrB_NULL, GrB_NULL, op, A->mat, GrB_NULL);

    R = matrix_create(nrows,1);
    GrB_Col_assign(R->mat, GrB_NULL, GrB_NULL, v, GrB_ALL, nrows, 0, GrB_NULL);

    return R;
}

struct matrix *matrix_transpose(struct matrix *A)
{
    struct matrix *T;
    GrB_Index nrows, ncols;
    GrB_size(A->mat, &nrows, &ncols);

    T = matrix_create(ncols, nrows);
    GrB_transpose(T->mat, GrB_NULL, GrB_NULL, A->mat, GrB_NULL);

    return T;
}

struct matrix *matrix_negate(struct matrix *A)
{
    struct matrix *R;
    GrB_Index nrows, ncols;
    int i,j;
    GrB_size(A->mat, &nrows, &ncols);

    R = matrix_create(ncols, nrows);

    for (i = 0; i < nrows; i++) {
        for (j = 0; j < ncols; j++) {
          matrix_setelem(R, matrix_getelem(A, i, j) == 0, i, j);
        }
    }

    return R;
}

struct matrix *matrix_conv(struct matrix *A, struct matrix *B)
{
    struct matrix *C;
    struct matrix *E;
    struct matrix *f;
    struct matrix *g;
    struct matrix *h;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols, C_nrows, C_ncols;
    int i, j;

    GrB_size(A->mat, &A_nrows, &A_ncols);
    GrB_size(B->mat, &B_nrows, &B_ncols);

    if (A_nrows < B_nrows || A_ncols < B_ncols)
        die("matrix_conv bad dimensions");

    // lots of memory leaked here!

    GrB_Index *row_indices, *col_indices;
    if (!(row_indices = malloc(B_nrows * sizeof(int)))) die("malloc failed");
    if (!(col_indices = malloc(B_ncols * sizeof(int)))) die("malloc failed");

    C_nrows = A_nrows - B_nrows + 1;
    C_ncols = A_ncols - B_ncols + 1;
    C = matrix_create(C_nrows, C_ncols);
    E = matrix_create(B_nrows, B_ncols);
    f = matrix_create(B_nrows, 1);
    g = matrix_create(1, B_nrows);
    h = matrix_create(1, 1);

    for (i = 0; i < C_nrows; i++) {
        for (j = 0; j < C_ncols; j++) {
          int k;
          for (k = 0; k < B_nrows; k++) row_indices[k] = i+k;
          for (k = 0; k < B_ncols; k++) col_indices[k] = j+k;
          GrB_extract(E->mat, GrB_NULL, GrB_NULL, A->mat, row_indices, B_nrows, col_indices, B_ncols, GrB_NULL);
          E = matrix_elmul(E, B);
          f = matrix_reduce(E, 0);
          g = matrix_transpose(f);
          h = matrix_reduce(g, 0);
          matrix_setelem(C, matrix_getelem(h, 0, 0), i, j);
        }
    }

    return C;
}

struct matrix *matrix_concat(struct matrix *A, struct matrix *B)
{
    struct matrix *C;
    GrB_Info info;
    GrB_Index A_nrows, A_ncols, B_nrows, B_ncols;
    int i;

    GrB_size(A->mat, &A_nrows, &A_ncols);
    GrB_size(B->mat, &B_nrows, &B_ncols);

    if (A_ncols != B_ncols)
        die("matrix_concat bad dimensions");

    GrB_Index *A_row_indices, *B_row_indices, *col_indices;
    if (!(A_row_indices = malloc(A_nrows * sizeof(int)))) die("malloc failed");
    if (!(B_row_indices = malloc(B_nrows * sizeof(int)))) die("malloc failed");
    if (!(col_indices = malloc(A_ncols * sizeof(int)))) die("malloc failed");

    for (i = 0; i < A_nrows; i++) A_row_indices[i] = i;
    for (i = A_nrows; i < A_nrows + B_nrows; i++) B_row_indices[i - A_nrows] = i;
    for (i = 0; i < A_ncols; i++) col_indices[i] = i;

    C = matrix_create(A_nrows + B_nrows, A_ncols);

    info = GrB_assign(C->mat,
                      GrB_NULL,
                      GrB_NULL,
                      A->mat,
                      A_row_indices,
                      A_nrows,
                      GrB_ALL,
                      A_ncols,
                      GrB_NULL);

    info = GrB_assign(C->mat,
                      GrB_NULL,
                      GrB_NULL,
                      B->mat,
                      B_row_indices,
                      B_nrows,
                      GrB_ALL,
                      B_ncols,
                      GrB_NULL);

    if (!GrB_ok(info))
        GrB_die("GrB_Matrix_eWiseAdd_Semiring", A->mat);

    return C;
}

// Comparison operators

struct matrix *matrix_elcompare(struct matrix *A, struct matrix *B, int op_index)
{
    struct matrix *C;
    int i, j;
    int a, b, comp_val;

    GrB_Index nrows, ncols, nrowsB, ncolsB;
    GrB_size(A->mat, &nrows, &ncols);
    GrB_size(B->mat, &nrowsB, &ncolsB);

    /*
    printf("dims of A: %d %d\n", (int) nrows, (int) ncols);
    matrix_print(matrix_tostring(A));
    printf("dims of B: %d %d\n", (int) nrowsB, (int) ncolsB);
    matrix_print(matrix_tostring(B));
    */

    C = matrix_create(1, 1);

    if (nrows != nrowsB || ncols != ncolsB)
        die("Can't compare two matrices that are different dimensions");

    for (i = 0; i < nrows; i++) {
        for (j = 0; j < ncols; j++) {
            a = matrix_getelem(A, i, j);
            b = matrix_getelem(B, i, j);
            switch (op_index) {
                case 0: comp_val = a == b; break;
                case 1: comp_val = a != b; break;
                case 2: comp_val = a <= b; break;
                case 3: comp_val = a < b; break;
                case 4: comp_val = a >= b; break;
                case 5: comp_val = a > b; break;
                default: die("Unknown comparison operator");
            }
            if (!comp_val) return C;
        }
    }
    matrix_setelem(C, 1, 0, 0);
    return C;
}

struct matrix *matrix_eq(struct matrix *A, struct matrix *B) { return matrix_elcompare(A, B, 0); }
struct matrix *matrix_neq(struct matrix *A, struct matrix *B) { return matrix_elcompare(A, B, 1); }
struct matrix *matrix_leq(struct matrix *A, struct matrix *B) { return matrix_elcompare(A, B, 2); }
struct matrix *matrix_less(struct matrix *A, struct matrix *B) { return matrix_elcompare(A, B, 3); }
struct matrix *matrix_geq(struct matrix *A, struct matrix *B) { return matrix_elcompare(A, B, 4); }
struct matrix *matrix_greater(struct matrix *A, struct matrix *B) { return matrix_elcompare(A, B, 5); }

// "The truth value of an expr is equivalent to expr > 0" (Jake, 2021)
int matrix_truthy(struct matrix *A)
{
    struct matrix *C;
    struct matrix *B;
    GrB_Index nrows, ncols;
    GrB_size(A->mat, &nrows, &ncols);

    B = matrix_create(nrows, ncols);
    C = matrix_greater(A, B);

    return matrix_getelem(C, 0, 0) > 0;
}

// end matrix_* functions //

#ifdef RUN_TEST
int main(int argc, char **argv){
    struct matrix *A, *B, *C;

    ring_push();

    A = matrix_create(2, 2);
    B = matrix_create(2, 2);
    // B = matrix_create(1, 1);
    matrix_setelem(A, 2, 0, 0);
    matrix_setelem(A, 2, 0, 1);
    matrix_setelem(A, 2, 1, 0);
    matrix_setelem(A, 2, 1, 1);
    matrix_setelem(B, 2, 0, 0);
    matrix_setelem(B, 2, 0, 1);
    matrix_setelem(B, 2, 1, 0);
    matrix_setelem(B, 2, 1, 1);
    matrix_print(matrix_tostring(A));
    matrix_print(matrix_tostring(B));

    C = matrix_mul(A, B);
    matrix_print(matrix_tostring(C));
}
#endif
