
f1()
{
  cat<<EOF
EOF
};

f2(){ :; }
cat<<EOF
bug2(){ :; }
EOF

f3(){ :; }
cat<<"EOF"
bug3(){ :; }
EOF

f4(){ :; }
cat<<'EOF'
bug4(){ :; }
EOF

f5(){ :; }
cat<<'End Of File'
bug5(){ :; }
End Of File

f6(){ :; }
cat<<EOF 
this isn't terminated

EOF#test
 EOF

bug6(){ :; }
EOF 

f7(){ :; }
cat<<"End Of \"File\""
bug7(){ :; }
End Of "File"

f8(){ :; }
# this is valid, the heredoc starts on the next line after the <<DELIM
cat<<EOF; fancy() { echo hello; }
bug8(){ :; }
EOF

f9(){ :; }
cat<<-EOF
	bug9(){ :; }
  this isn't the end, only tabulations are stripped, not spaces
  EOF
	EOF

f10(){ :; }
cat << EOF
bug10(){ :; }
EOF

f11(){ :; }
cat << 	 	EOF
bug11(){ :; }
EOF

f12(){ :; }
# this is not an indented here-document but a "-EOF" delimiter
cat << -EOF
bug12(){ :; }
-EOF

f13(){ :; }
# quoted empty delimiters are valid
cat <<''
bug13(){ :; }


f14(){ :; }
cat <<""
bug14(){ :; }


f15(){ :; }
cat <<-""
bug15(){ :; }
	

f16(){ :; }

fancy
