main() {
    auto i; i = 10;
loop:
    --i;
    putchar('0' + i);
    putchar('*n');
    if (i > 0) {
        goto loop;
    }

    auto goodbye; goodbye = "Thank you for your attention! Press Enter to exit";
    auto c;
    i = 0;
    while ((c = char(goodbye, i)) != '*e') {
        putchar(c);
        ++i;
    }
    getchar();
    return ( i );
}