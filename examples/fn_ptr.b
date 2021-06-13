verbose_malloc(n) {
    auto m;
    printf("Attempt to allocate %d words of memory\n", n);
    m = getvec(n);
    if (m == 0) {
        printf("Allocation failed\n", n);
        return ( 0 );
    }
    printf("Allocation is successful\n", n);
    return ( m );
}

getvec verbose_malloc;

main() {
    auto q; q = 'AAAA';
    auto v;
    v = getvec(5);
}