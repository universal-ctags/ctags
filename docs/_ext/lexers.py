from ctags_optlib_highlighter import CtagsOptlibLexer
from pygments.lexers.special import TextLexer
import sphinx

def install(app, h, name):
    v = sphinx.__version__.split('.')
    if int(v[0]) > 3:
        c = h
    elif (int(v[0]) == 3) and (int(v[1]) > 3):
        c = h
    else:
        c = h()
    app.add_lexer(name, c)

def setup(app):
    install(app, CtagsOptlibLexer, 'ctags')
    # Setup stubs
    for h in ['git', 'yacc', 'EmacsLisp', 'tags', 'Autoconf', 'SystemTap', 'RMarkdown']:
        install(app, TextLexer, h)
