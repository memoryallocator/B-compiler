a "Import me!";

main() {
    auto a;
    {
        {
            extrn a;
            extrn a, a;
            extrn a;
            a;
        }
        {
            auto a;
            printf("a = %d*n", a);
        }
        auto a;
        printf("a = %d*n", a);
        b:
            a;
    }
}