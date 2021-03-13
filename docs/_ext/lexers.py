from ctags_optlib_highlighter import CtagsLexer

def setup(app):
    app.add_lexer('ctags', CtagsLexer)

