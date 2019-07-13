# Taken from https://nsis.sourceforge.io/Reference/Section
Section "-hidden section"
SectionEnd

Section # hidden section
SectionEnd

Section "!bold section"
SectionEnd

Section /o "optional"
SectionEnd

Section "install something" SEC_IDX
SectionEnd

Section "インストール (install in Japanese)" SEC_J_IDX
SectionEnd

Section "${SOMETHING0}" SEC_S0_IDX
SectionEnd

Section "$(SOMETHING1)" SEC_S1_IDX
SectionEnd

Section "" EMPTY_IDX
SectionEnd
