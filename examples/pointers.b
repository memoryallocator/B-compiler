VAL 5;

...() {
    return ( "..." );
}

panic() {
    putstr("*nsmth went wrong...*n");
    exit();
}

main() {
    {
        extrn VAL;
        auto x;
        auto y; y = &x;
        *y = VAL;
        printf("%d is %d*n", x, VAL);
    }
    {
        auto ptr; ptr = &ptr;
        printf("prev ptr is %d*n", ptr);
        auto ptr_to_ptr; ptr_to_ptr = &ptr;
        ptr_to_ptr = &ptr;
        auto v;
        *ptr_to_ptr = &v;
        printf("ptr is %d*n", ptr);
    }
    {
        extrn ...;
        auto ellipsis; ellipsis = ...;
        putstr(ellipsis());

        if (... != &...) {
            panic();
        }
    }
    {
        auto a, b;
        if ((&a[b] != &*(a + b)) | (&a[b] != a + b)) {
            panic();
        }
    }
    putstr("*nOK*n");
}