# Test format tag generation.

format XYZ =
@<<< @>>>
"1", "a"
.

format =
    @>>>
24
.

write;
$~ = XYZ;
write;
