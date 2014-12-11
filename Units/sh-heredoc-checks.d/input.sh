
f1()
{
  cat<<EOF
EOF
};

f2(){ :; }
cat<<EOF
bug(){ :; }
EOF

f3(){ :; }
cat<<"EOF"
bug(){ :; }
EOF

f4(){ :; }
cat<<'EOF'
bug(){ :; }
EOF

f5(){ :; }
cat<<'End Of File'
bug(){ :; }
End Of File

f6(){ :; }
cat<<EOF 
this isn't terminated
EOF 
EOF#test
 EOF

bug(){ :; }
EOF

f7(){ :; }
cat<<"End Of \"File\""
bug(){ :; }
End Of "File"

f8(){ :; }
# this is valid, the heredoc starts on the next line after the <<DELIM
cat<<EOF; fancy() { echo hello; }
bug(){ :; }
EOF

f9(){ :; }
cat<<-EOF
	bug(){ :; }
  this isn't the end, only tabulations are stripped, not spaces
  EOF
	EOF

f10(){ :; }
cat << EOF
bug(){ :; }
EOF

f11(){ :; }
cat << 	 	EOF
bug(){ :; }
EOF

f12(){ :; }

fancy
