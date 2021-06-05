/* see the first comment in ascii_file_copy.b */

f(x) {
    return ( x % 3 + 1 );
}

g(x) {
    return ( x );
}

main() {
    auto x;
    auto xy;

    auto i;
    i = 10;

    while (i--) {
        auto .while;
        auto j; j = f(i);
        switch (j) {
        auto .switch;
        case 1:
            x = 5;
            break;
        case 2:
        case 3:
            x = g(j);
        }
    labl:
        if (j < 0) {
            auto .if;
            xy = 3;
            break;
        }
        xy = j/2;
    }
    more:
    printf("j = %d, xy = %d*n", j, xy);
}