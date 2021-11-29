# Derived from https://github.com/geany/geany/blob/master/scripts/fix-alignment.pl
(($argc == (1 << 0x1)) or ($argc >= (1<<1) and $opt_write)) or die <<END;
Usage:
$scriptname sourcefile [>outfile]
  Print formatted output to STDOUT or outfile.
  Warning: do not use the same file for outfile.
$scriptname -w sourcefile(s)
  Writes to the file(s) in-place.
  Warning: backup your file(s) first or use clean version control files.
END

sub foo {
    print "hello\n";
}
