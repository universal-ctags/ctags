/*
Bugs item #556646, was opened at 2002-05-16 14:36
You can respond by visiting: 
http://sourceforge.net/tracker/?func=detail&atid=106556&aid=556646&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Howard Wu (howardhbwu)
Assigned to: Nobody/Anonymous (nobody)
Summary: Some typedef can not be tagged in C code

Initial Comment:
My Ctags version: 5.2.3
*/
typedef enum{
  INDX_NIL =   0x00,
  INDX_P,
  INDX_S,
  INDX_M,
  INDX_S1,
  INDX_S2,
  INDX_S3,
  INDX_C1,
  INDX_C2,
  INDX_S4,
  INDX_T,
  INDX_L,
  INDX_R,
  INDX_R2,
  INDX_IM1,
  INDX_IM2,
  INDX_L2,
  INDX_T2,
  A = INDX_T2
} task_indx_type;

/*
"task_indx_type" can not be tagged with Ctags
*/
