/* copy-pasted from
USERS' REFERENCE TO B ON MH-TSS
S. C. Johnson
Bell Laboratories
Murray Hill, New Jersey */

/* The following complete B program, if compiled and put on your
file "hstar", will act as an ascii file copy routine; the command
at "SYSTEM?" level:
/hstar file1 file2
will copy file1 to file2. */
main () {
    auto j,s[20],t[20];
    reread(); /* get command line */
    getstr(s); /* put into s */
    j = getarg(t,s,0); /* skip H* name */
    j = getarg(t,s,j); /* filel */
    openr( 5,t );
    getarg(t,s,j); /* file2 */
    openw( 6,t );
    while( putchar( getchar() ) != '*e' ) ; /* copy contents */
}

/* This function is called with a string s of the form nnn, nnn,
nnn, . . . , where the nnn are integers. The values are placed
in successive locations in a vector v. The number of integers
converted is returned as a function value. This program
provides a simple illustration of the switch and case statements. */

convert(s,v) {

    auto m,i,j,c,sign;

    i = O; /* vector index */
    j =-1; /* character index */

    init: /* initialize to convert an integer */
        m = 0; /* the integer value */
        sign = 0; /* sign = 1 if the integer is negative */

    loop: /* convert an integer */

        switch (C = char(s,++j)){

        case '-':
            if(sign) goto syntax;
            s = 1;
        case ' ':
            goto loop;
        case '*e':
        case ',': /* delimiter . . . store converted value */
            v[i++] = sign?(-m):m;
            if( c == '*e' ) return(i);
            goto init;
    }

/* none of the above cases . . . if a digit, add to m */

        if ( '0' <= c & c <= '9' ){
            m = 10*m + c- '0';
            goto loop;
        }

    /* syntax error . . . print message and return -1 */

    syntax:
        printf("bad syntax*n");
        return(-1 );
    }


/* This function replaces each upper case character in the input
string s by its lower case equivalent. It uses the fact that
the ascii alphabetic characters are contiguous. */

lower(s) {

    auto c,i;
    i = -1 ;
    while( (c=char(s,++i)) != '*e' )
    if( c >= 'A' & c <= 'Z' ) lchar(s,i,c-'A'+'a');
    }

/* This function prints out an unsympathetic error message on the
terminal for each integer value of errno from O to 5 */

snide(errno) {
    extrn wr.unit, mess;
    auto u; /* temporary storage for the unit number */

    u = wr.unit ; wr.unit = 1;

    printf("error number %d, %s*n",errno,mess[errno]);

    wr.unit = u;
}

mess [5] "too bad", "tough luck", "sorry, Charlie", "that's the breaks",
"what a shame", "some days you can't win";