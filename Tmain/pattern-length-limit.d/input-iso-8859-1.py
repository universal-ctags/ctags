# this is made to pass the `(c & 0xC0) == 0x80` UTF-8 sub-byte check to make
# sure we have a working hard limit in case of malicious input.
a='€‚ƒ€‚ƒ'
