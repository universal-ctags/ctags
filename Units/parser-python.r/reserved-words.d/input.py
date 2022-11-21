if True:
    x = 1
else:
    x = 2

def f(text, pat):
    if "." in text:
        return []
    try: regexp = pat
