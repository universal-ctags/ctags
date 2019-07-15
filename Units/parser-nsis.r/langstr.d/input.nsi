;; https://nsis.sourceforge.io/Docs/Chapter4.html#langstring

LangString message ${LANG_ENGLISH} "English message"
LangString message ${LANG_FRENCH} "French message"
LangString message ${LANG_KOREAN} "Korean message"

MessageBox MB_OK "A translated message: $(message)"

;; https://nsis.sourceforge.io/Docs/Chapter4.html#licenselangstring
LicenseLangString license ${LANG_ENGLISH} license-english.txt
LicenseLangString license ${LANG_FRENCH} license-french.txt
LicenseLangString license ${LANG_GERMAN} license-german.txt

LicenseData $(license)

;; Taken from vim/nsis/lang/english.nsi

# Overwrite the default translation.
# These strings should be always English.  Otherwise dosinst.c fails.
LangString ^SetupCaption     ${LANG_ENGLISH} \
        "$(^Name) Setup"
LangString ^UninstallCaption ${LANG_ENGLISH} \
        "$(^Name) Uninstall"

