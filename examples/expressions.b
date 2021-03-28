main() {
    auto a, b, c;
    /* a = -1;
    b = 1; */
    {
        ;
    }
    auto x;
    return ( (a == 0 & a == b & b == c ? 0 : 1)
            | (x = 1 ? 100 ? x = 200 : 300 : 400)
            | f() + g(get(), set(-1)) * h()[0][1]
            | (a & b & c ? +900 : -1) ) ;
}