autoload func
autoload -z kfun
autoload -k zfun
autoload f0 f1 f2
autoload /path0/A/pf0 /path1/B/pf1
autoload /path3/pf3 f3

foo()
{
    autoload -X
}

autoload +x g0 g1
