# Test for Bug #699171.
# Extracted from http://txt2tags.sourceforge.net/src/txt2tags-1.4.tar.gz

HEADER_TEMPLATE = {
r"""Multiline raw string
"""
}
def Quit(msg, exitcode=0): print msg ; sys.exit(exitcode)
