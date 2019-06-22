/* Taken from u-boot */
#define IX_GET_BIT_FIELD16(					  \
                            arg_PackedData16, \
                            arg_FieldLSBBit, \
                            arg_FieldMSBBit \
                          ) \
                          (((ix_uint16)(arg_PackedData16) & IX_BIT_FIELD_MASK16(arg_FieldLSBBit, arg_FieldMSBBit)) >> \
                             arg_FieldLSBBit)

#define M0() 1
#define M1(X) 1 + X
#define M2(X,Y) 1 + X + Y
#define M3(X,Y,Z) 1 + X + Y + Z
#define Mn(X,Y,Z,...) 1 + X + Y + Z
