; Taken from https://nsis.sourceforge.io/Docs/Chapter4.html#sections
SectionGroup "some stuff"
Section "a section"
SectionEnd
Section "another section"
SectionEnd
SectionGroupEnd

SectionGroup "some item"
Section "b section"
SectionEnd
Section "alt section"
SectionEnd
SectionGroupEnd

SectionGroup "the rest (その他)"
Section "c section"
SectionEnd
Section "d section"
SectionEnd
SectionGroupEnd

SectionGroup "${SG0}" SG0
Section "e section"
SectionEnd
Section "f section"
SectionEnd
SectionGroupEnd

SectionGroup "$(SG1)" SG1
Section "g section"
SectionEnd
Section "h section"
SectionEnd
SectionGroupEnd

SectionGroup "" EMPTY0
Section "i section"
SectionEnd
Section "j section"
SectionEnd
SectionGroupEnd

SectionGroup "x$\"y" XQY
Section "k section"
SectionEnd
Section "l section"
SectionEnd
SectionGroupEnd
