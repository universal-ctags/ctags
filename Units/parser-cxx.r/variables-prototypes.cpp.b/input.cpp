int  main( int  argc, char ** argv )
{
    // 1. Angle brackets handling bug
    // As of now a2, a3 and a4 will be skipped

    int  a1 = i < 7 ? i : 7;
    int  a2 = 8;
    int  a3 = 9;
    int  a4 = p->data;
    int  a5 = 8;


    // 2. Function pointer decl bug
    // As of now a6 will be skipped

    int ( *a6 )( int, char ** ) = &main;


    // 3. Wrong prototype will be written
    // As of now prototype will be ( SomeType ( p )( int ) )

    int  a7( short ( *p )( int ) );


    // 4. DECL_IGNORE bug
    // As of now a8 will be skipped

    if ( argc > 0 )
        return 0;

    short a8 = i;


    // 5. Local variables support
	// As of now all the identifiers below are either reported as prototypes,
	// or not recognized at all
	// The comments on the right specify which kind of tag should be generated

    const char *b1( "uuu" );                //variable
    int b2( int ooo, const char* o );       //prototype
    int b3( int( 7 ) );                     //variable
    int c1( a >> ooo );                     //variable
    int c2( int * h );                      //prototype
    int c3( std::map<int>( 9 ) );           //variable
    int c4( std::map<int> a );              //prototype
    int c5( map<int>  & a );                //prototype
    int c6( ooo & a );                      //prototype (ambiguous)
    int c7( int & a );                      //prototype
    int c8( map<int> a );                   //prototype
    int c9( int a );                        //prototype
    ind d1( a * 2 );                        //variable
    ind d2( "a" * 2 );                      //variable
    int d3( *j );                           //variable
    int f1( SomeType (*p)( int ) );         //prototype
    SomeType (*f2)( int o );                //variable
    int f3( std::map<int> & a );            //prototype
    int g1( SomeType a );                   //prototype
	// As of now, the signature of h1 will be "( int a = R )",
	// 'R' is to be replaced by something clearer to denote a
	// string literal
    char h1( const char *a = "a" );         //prototype
    int h2();                               //prototype
    char h3( double );                      //prototype
    int h4( h u );                          //prototype
    int (*p1)( int o );                     //variable
    int v4( iii( 'o' ) );                   //variable
    int v5( iii( o ) );                     //variable
    int v6( int (*p)( int ) );              //prototype
    int v7( 89 );                           //variable
    int v8( ooo );                          //variable
    // __ARGS(x) is a compatibility macro which optionally
	// expands to x when prototypes are supported, so
	// w1 should be parsed as a prototype
    int w1 __ARGS (( int a ));              //prototype
    int w2 (( a - 5 ) / 6 );                //variable
    int x1( a.b );                          //variable
    int x2( a->b );                         //variable
    int x3( NS::a  b );                     //prototype
    int x4( a ^ b );                        //variable 
    int x5( a ^ 6 );                        //variable
    int x6( a | b );                        //variable
	// As of now, x7 is not recognized at all; it should
	// give a variable tag but it's tricky to get it
	// right because of '&' which suggests a prototype,
	// so for now let's just ignore it
    //int x7( a & b & c );                  //variable
    int x8( a & b & 2 );                    //variable
    int x9( a && b );                       //variable
}

/* 
 * Below is a copy/paste of the local identifiers above made uppercase,
 * but with a global scope instead.
 */

// 1. Angle brackets handling bug
int  A1 = i < 7 ? i : 7;
int  A2 = 8;
int  A3 = 9;
int  A4 = p->data;
int  A5 = 8;


// 2. Function pointer decl bug
int ( *A6 )( int, char ** ) = &main;


// 3. Wrong prototype will be written
int  A7( short ( *p )( int ) );


// 4. Global variables support
const char *B1( "uuu" );                //variable
int B2( int ooo, const char* o );       //prototype
int B3( int( 7 ) );                     //variable
int C1( a >> ooo );                     //variable
int C2( int * h );                      //prototype
int C3( std::map<int>( 9 ) );           //variable
int C4( std::map<int> a );              //prototype
int C5( map<int>  & a );                //prototype
int C6( ooo & a );                      //prototype (ambiguous)
int C7( int & a );                      //prototype
int C8( map<int> a );                   //prototype
int C9( int a );                        //prototype
ind D1( a * 2 );                        //variable
ind D2( "a" * 2 );                      //variable
int D3( *j );                           //variable
int F1( SomeType (*p)( int ) );         //prototype
SomeType (*F2)( int o );                //variable
int F3( std::map<int> & a );            //prototype
int G1( SomeType a );                   //prototype
char H1( const char *a = "a" );         //prototype
int H2();                               //prototype
char H3( double );                      //prototype
int H4( h u );                          //prototype
int (*P1)( int o );                     //variable
int V4( iii( 'o' ) );                   //variable
int V5( iii( o ) );                     //variable (ambiguous)
int V6( int (*p)( int ) );              //prototype
int V7( 89 );                           //variable
int V8( ooo );                          //variable (ambiguous)
int W1 __ARGS (( int a )){}             //function
int W2 (( a - 5 ) / 6 );                //variable
int X1( a.b );                          //variable
int X2( a->b );                         //variable
int X3( NS::a  b );                     //prototype
int X4( a ^ b );                        //variable 
int X5( a ^ 6 );                        //variable
int X6( a | b );                        //variable
//int X7( a & b & c );                  //variable
int X8( a & b & 2 );                    //variable
int X9( a && b );                       //variable (ambiguous in C++)
