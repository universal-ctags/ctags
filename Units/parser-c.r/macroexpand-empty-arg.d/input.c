#define X
#define m0(Q,W,E,R) int Q
#define m1(Q,W,E,R) int W
#define m2(Q,W,E,R) int E
#define m3(Q,W,E,R) int R

m0(a,b,c,d);
m0(a,X,c,d);
m0(a,b,X,d);
m0(a,b,c,X);
m0(a,X,X,d);
m0(a,b,X,X);
m0(a,X,c,X);
m0(a,X,X,X);

m1(a,b,c,d);
m1(X,b,c,d);
m1(a,b,X,d);
m1(a,b,c,X);
m1(X,b,X,d);
m1(a,b,X,X);
m1(X,b,c,X);
m1(X,b,X,X);

m2(a,b,c,d);
m2(X,b,c,d);
m2(a,X,c,d);
m2(a,b,c,X);
m2(X,X,c,d);
m2(a,X,c,X);
m2(X,b,c,X);
m2(X,X,c,X);

m3(a,b,c,d);
m3(X,b,c,d);
m3(a,X,c,d);
m3(a,b,X,d);
m3(X,X,c,d);
m3(a,X,X,d);
m3(X,b,X,d);
m3(X,X,X,d);

m0(a,b,c,d);
m0(a,,c,d);
m0(a,b,,d);
m0(a,b,c,);
m0(a,,,d);
m0(a,b,,);
m0(a,,c,);
m0(a,,,);

m1(a,b,c,d);
m1(,b,c,d);
m1(a,b,,d);
m1(a,b,c,);
m1(,b,,d);
m1(a,b,,);
m1(,b,c,);
m1(,b,,);

m2(a,b,c,d);
m2(,b,c,d);
m2(a,,c,d);
m2(a,b,c,);
m2(,,c,d);
m2(a,,c,);
m2(,b,c,);
m2(,,c,);

m3(a,b,c,d);
m3(,b,c,d);
m3(a,,c,d);
m3(a,b,,d);
m3(,,c,d);
m3(a,,,d);
m3(,b,,d);
m3(,,,d);
