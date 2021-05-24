from ctags_optlib_highlighter import CtagsOptlibLexer
import sphinx

def setup(app):
    v = sphinx.__version__.split('.')
    if int(v[0]) > 3:
        c = CtagsOptlibLexer
    else:
        if (int(v[1]) > 3):
            c = CtagsOptlibLexer
        else:
            c = CtagsOptlibLexer()
    app.add_lexer('ctags', c)
