main() {
    printf("trivial*n");
    printf("%s", "the next line is empty:*n");
    printf("");
    printf("%s", "");

    auto end; end = "end";
    auto of; of = "of";
    auto line; line = "line";
    printf("*n%s %s %s*n*n", end, of, line);

    auto a; a = 2;
    auto b; b = 2;
    auto c; c = 2;
    printf("%d + %d * %d = %d", a, b, c, a + b * c);

    printf("*n%s", "*nThe alphabet:*n");
    auto ch; ch = 'A';
    while (ch <= 'Z') {
        printf("%c", ch);
        ++ch;
    }
    printf("%c*n", '*n');

    printf("An example from the reference (<<printf(*"%%d + %%o is %%s or %%c**n*", 1,-1,*"zero*",'0');>>):*n");
    printf("%d + %o is %s or %c*n", 1, -1, "zero", '0');

    printf("*nDid everything work out?..");
}