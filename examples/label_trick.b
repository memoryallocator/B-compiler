main() {
    goto first;

seems_unreachable:
    printf("reachable*n");
    return ( 101 );

    auto i, j;
first:
    j = i = 1;
    seems_unreachable();

out:
    return ( 0 * printf("correct*n") );
}