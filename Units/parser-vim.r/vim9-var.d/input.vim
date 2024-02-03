vim9script

# Taken from vim/runtime/makemenu.vim

var cur_menu_name = ""
var cur_menu_nr = 0
var cur_menu_item: number = 0
var cur_menu_char = ""

# Taken from vim/runtime/import/dist/vimhighlight.vim

var language_section: list<string> =<< trim END

    Highlighting groups for language syntaxes
    -----------------------------------------

END
