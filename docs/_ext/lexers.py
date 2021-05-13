from ctags_optlib_highlighter import CtagsOtplibLexer
import sphinx

def setup(app):
    v = sphinx.__version__.split('.')
    if int(v[0]) > 3:
        c = CtagsOtplibLexer
    else:
        if (int(v[1]) > 3):
            c = CtagsOtplibLexer
        else:
            c = CtagsOtplibLexer()
    app.add_lexer('ctags', c)
