f()
{
	:
}

cat > foo.xml <<EOFA
<input xmlns="https://ctags.io/root"
       xmlns:abc="https://ctags.io/nosuchthing"
       xmlns:unused="https://ctags.io/unused"
       >
<abc:UnKnownTag id="a" xmlns:efg="https://ctags.io/nosuchthing2">
  <efg:UnKnownTag2 id="b"/>
</abc:UnKnownTag>
</input>
EOFA

g()
{
	:
}
