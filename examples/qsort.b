DEBUG_NMB 0;

swap(a, b, sz) {
    while (sz-- > 0) {
        auto tmp; tmp = a[sz];
        a[sz] = b[sz];
        b[sz] = tmp;
    }
}

get(a, i, sz) {
    return ( a + i * sz );
}

partition(arr, n, sz, cmp) {
    if (n <= 1) {
        return ( 0 );
    }
    auto pivot; pivot = get(arr, n - 1, sz);
    auto border; border = -1;
    auto scan; scan = 0;
    while (scan < n - 1) {
        auto curr; curr = get(arr, scan, sz);
        if (cmp(curr, pivot) <= 0) {
            border =+ 1;
            if (scan != border) {
                swap(curr, get(arr, border, sz), sz);
            }
        }
        ++scan;
    }

    ++border;
    if (border != n - 1) {
        swap(get(arr, border, sz), pivot, sz);
    }
    return ( border );
}

qsort(arr, n, sz, cmp) {
    extrn DEBUG_NMB;
    if (n <= 1) {
        return;
    }
    if (DEBUG_NMB != 0 & sz == 1) {
        extrn print_nmb;
        print_arr(arr, n, sz, print_nmb);
    }

    auto pivot_pos; pivot_pos = partition(arr, n, sz, cmp);
    qsort(arr, pivot_pos, sz, cmp);

    ++pivot_pos;
    qsort(arr + pivot_pos * sz, n - pivot_pos, sz, cmp);
}

cmp_nmb(x, y) {
    x = *x;
    y = *y;
    if (x == y) {
        return ( 0 );
    }
    return ( x < y ? -1 : 1 );
}

print_nmb(x) {
    printf("%d", *x);
}

print_arr(a, n, sz, printer) {
    printf("[*n");
    auto i; i = 0;
    while (i < n) {
        printf("  ");
        printer(a + i * sz);
        printf("*n");
        ++i;
    }
    printf("]*n");
}

print_pair(x) {
    printf("(%d, %d)", x[0], x[1]);
}

cmp_pair(x, y) {
    extrn cmp_nmb;
    if (x[0] == y[0]) {
        return ( cmp_nmb(x + 1, y + 1) );
    }
    return ( cmp_nmb(x, y) );
}

main() {
    {
        extrn cmp_nmb;
        extrn print_nmb;

        {
            auto arr[5];
            arr[0] = 5;
            arr[1] = 40;
            arr[2] = 3;
            arr[3] = 20;
            arr[4] = 1;
            arr[5] = 60;

            qsort(arr, 6, 1, cmp_nmb);
            print_arr(arr, 6, 1, print_nmb);
        }

        {
            auto arr[5];
            arr[0] = 0;
            arr[1] = 1;
            arr[2] = 2;
            arr[3] = 3;
            arr[4] = 4;
            arr[5] = 5;

            qsort(arr, 6, 1, cmp_nmb);
            print_arr(arr, 6, 1, print_nmb);
        }
    }

    {
        extrn cmp_pair;
        extrn print_pair;
        auto arr[9];
        arr[0] = 5; arr[1] = 10;
        arr[2] = 5; arr[3] = -300;
        arr[4] = 4; arr[5] = 500;
        arr[6] = 4; arr[7] = -800;
        arr[8] = 8; arr[9] = 9;
        qsort(arr, 5, 2, cmp_pair);
        print_arr(arr, 5, 2, print_pair);
    }
}