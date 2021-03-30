greet_user() {
    printf("Hello, user!*n");
}

main() {
    greet_user();
    count();
}

count() {
    auto i;

    while (1) {
        printf("i = %d*n", i);

        if (i % 10 == 0) {
            printf("Type 'q' (without quotes) to exit.*n'");
        }

        auto char;
        char = getchar();
        if (char == "q*n") {
            printf("Bye!*n");
            exit();
        }

        ++i;
    }
}