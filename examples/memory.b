SIZE 5;

main() {
    extrn SIZE;
    auto v;
    v = getvec(SIZE);
    printf("allocated*n");

    printf("Please type not less than %d symbols.*n", SIZE+1);
    auto i; i = 0;
    while (i <= SIZE) {
        v[i] = getchar();
        ++i;
    }

    printf("The symbols you typed are:*n");
    i = 0;
    while (i <= SIZE) {
        putchar(v[i]);
        ++i;
    }
    rlsevec(v, SIZE);
    printf("*ndone*n");
}