void f00(a00) int  a00; {}
void f01(a01) int *a01; {}

/* struct */
void f10(a10) struct file *a10; {}
void f11(a11, a12) struct file *a11; int a12; {}
void f12(a13, a14) int a13; struct file *a14; {}

void f13(a15) register struct file *a15; {}
void f14(a16, a17) register struct file *a16; int a17; {}
void f15(a18, a19) register int a18; struct file *a19; {}

void f16(a19_1) const struct file *a19_1; {}
void f17(a19_2, a19_3) const struct file *a19_2; int a19_3; {}
void f18(a19_4, a19_5) const int a19_4; struct file *a19_5; {}

/* union */
void f20(a20) union file *a20; {}
void f21(a21, a22) union file *a21; int a22; {}
void f22(a23, a24) int a23; union file *a24; {}

/* enum */
void f30(a30) enum file *a30; {}
void f31(a31, a32) enum file *a31; int a32; {}
void f32(a33, a34) int a33; enum file *a34; {}

/* struct union enum */
void f40 (a41, a42, a43) struct file a41;  union file a42; enum file a43; {}
