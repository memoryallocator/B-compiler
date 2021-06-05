main() {
    auto i; i = 10;
    loop:
        --i;
        printf("i = %d*n", i);
        if (i > 0) {
            goto loop;
        }
    return ( i );
}