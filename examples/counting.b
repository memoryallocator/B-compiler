greet_user() {
    printf("Hello, user!*n");
}

main() {
    greet_user();
    count();
}

EXIT_CHAR 'q';

count() {
    extrn EXIT_CHAR;
    auto skip;
    skip = 0;
    auto i;
    i = 0;

    while (1) {
        if (!skip) {
            printf("i = %d*n", i);
            if (i % 10 == 0) {
                printf("Type '%c' (without quotes) to exit.*n", EXIT_CHAR);
            }
        }
        skip = 0;

        auto char;
        char = getchar();
        if (char == 13) {
            while (getchar() != '*n');
            skip = 1;
        }
        if (skip) {
            continue;
        }
        if (char == EXIT_CHAR) {
            printf("Bye!*n");
            exit();
        }

        ++i;
    }
}