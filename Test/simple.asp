' "_descriptions.asp"
' Documentation for my variables

Dim lang_start      '(htmlct.inc) char-index, use for multilanguage-strings
DIM lang_length     '(htmlct.inc) num.chars to get, i.e. mid(str, lang_start, lang_length)
function HtmlCutout(lang, str)   'dig-out a piece of text from a multilang. string str

dim lang_swe(230)   'translation-table for pagetitles, buttons, etc.
Function f_translate_9data(id,defaulttext,data)   'lookup lang_???(id) and insert data at $0,$1,...

sub js_erroralert(txt)           'print inline-script to generate popup-window with txt

