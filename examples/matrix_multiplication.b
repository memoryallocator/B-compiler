get_matrix(rows, cols, ival) {
    auto matrix; matrix = getvec(rows - 1);
    auto i; i = 0;
    while (i < rows) {
        matrix[i] = getvec(cols - 1);
        auto j; j = 0;
        while (j < cols) {
            matrix[i][j] = ival;
            ++j;
        }
        ++i;
    }
    return ( matrix );
}

matrix_multiply(A, a_rows, a_cols, B, b_rows, b_cols) {
    if (a_cols != b_rows) {
        return ( 0 );  /* nullptr */
    }
    auto res; res = get_matrix(a_rows, b_cols, 0);
    auto i; i = 0;
    while (i < a_rows) {
        auto j; j = 0;
        while (j < b_cols) {
            auto k; k = 0;
            while (k < a_cols) {
                res[i][j] =+ A[i][k] * B[k][j];
                ++k;
            }
            ++j;
        }
        ++i;
    }
    return ( res );
}

print_num(c) {
    if (c == 0) {
        putchar('0');
        return;
    }
    auto d[19];
    auto i; i = 0;
    while (1) {
        d[i] = c % 10;
        ++i;
        c =/ 10;
        if (c == 0) {
            break;
        }
    }
    --i;
    while (i >= 0) {
        putchar('0' + d[i]);
        --i;
    }
}

print_matrix(M, rows, cols) {
    auto i; i = 0;
    while (i < rows) {
        print_num(M[i][0]);
        auto j; j = 1;
        while (j < cols) {
            putchar(' ');
            print_num(M[i][j]);
            ++j;
        }
        putchar('*n');
        ++i;
    }
}

free_matrix(M, rows) {
    auto i; i = 0;
    while (i < rows) {
        rlsevec(M[i], -1);
        ++i;
    }
    rlsevec(M, rows - 1);
}

main() {
    auto A;
    auto a_rows; a_rows = 1;
    auto a_cols; a_cols = 2;
    A = get_matrix(a_rows, a_cols, 0);
    A[0][0] = 5; A[0][1] = 4;

    auto B;
    auto b_rows; b_rows = 2;
    auto b_cols; b_cols = 3;
    B = get_matrix(b_rows, b_cols, 0);
    B[0][0] = 1; B[0][1] = 2; B[0][2] = 3;
    B[1][0] = 9; B[1][1] = 24; B[1][2] = 31;

    auto C; C = matrix_multiply(A, a_rows, a_cols, B, b_rows, b_cols);
    free_matrix(A, a_rows);
    free_matrix(B, b_rows);

    print_matrix(C, a_rows, b_cols);
    free_matrix(C, a_rows);
    getchar();
}