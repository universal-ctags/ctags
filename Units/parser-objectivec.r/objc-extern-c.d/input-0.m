// -*- mode: objc -*-

/* Verify code handling
 *
 * 	extern "C" { ...
 *
 * doesn't break the other area of objectivec parser.
 */

extern int foo (void)
{
  return 0;
}
