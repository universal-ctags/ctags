from ctags_optlib_highlighter import CtagsOtplibLexer

def setup(app):
    app.add_lexer('ctags', CtagsOtplibLexer())

