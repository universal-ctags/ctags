/* Taken from perl-5.10.1/ext/XS-Typemap/Typemap.xs */

/* T_ARRAY - allocate some memory */
intArray * intArrayPtr( int nelem ) {
    intArray * array;
    Newx(array, nelem, intArray);
    return array;
}


MODULE = XS::Typemap   PACKAGE = XS::Typemap

PROTOTYPES: DISABLE

=head1 TYPEMAPS

Each C type is represented by an entry in the typemap file that
is responsible for converting perl variables (SV, AV, HV and CV) to
and from that type.

=over 4

=item T_SV

This simply passes the C representation of the Perl variable (an SV*)
in and out of the XS layer. This can be used if the C code wants
to deal directly with the Perl variable.

=cut

SV *
T_SV( sv )
  SV * sv
 CODE:
  /* create a new sv for return that is a copy of the input
     do not simply copy the pointer since the SV will be marked
     mortal by the INPUT typemap when it is pushed back onto the stack */
  RETVAL = sv_mortalcopy( sv );
  /* increment the refcount since the default INPUT typemap mortalizes
     by default and we don't want to decrement the ref count twice
     by mistake */
  SvREFCNT_inc(RETVAL);
 OUTPUT:
  RETVAL

=item T_SVREF

Used to pass in and return a reference to an SV.

=cut

SVREF
T_SVREF( svref )
  SVREF svref
 CODE:
  RETVAL = svref;
 OUTPUT:
  RETVAL

