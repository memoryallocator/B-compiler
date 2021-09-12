main() {
    printf("trivial*n");
    printf("%s", "the next line is empty:*n");
    printf("");
    printf("%s", "");

    auto end; end = "end";
    auto of; of = "of";
    auto line; line = "line";
    printf("*n%s %s %s*n*n", end, of, line);

    printf("%s %s %s %c%c%c%c%c%c%c%c%c%c", "a", "lot", "of", 'a', 'r', 'g', 'u', 'm', 'e', 'n', 't', 's', '*n');
    printf("*n");

    printf("percent sign (%%) %s*n*n", "test");

    auto min_word; min_word = -9223372036854775808;
    printf("min_word = %d*n", min_word);
    printf("min_word + 1 = %d*n", min_word + 1);
    printf("min_word - 1 = %d*n", min_word - 1);
    printf("%s", "*n");

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