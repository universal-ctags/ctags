from ctags_highlighter import CtagsLexer

def setup(app):
    app.add_lexer('ctags', CtagsLexer)

