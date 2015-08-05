my class JSONPrettyActions {
    method TOP($/) {
        make $/.values.[0].ast;
    };
    method object($/) {
        make $<pairlist>.ast.hash.item;
    }

    method pairlist($/) {
        make $<pair>>>.ast.flat;
    }

    method pair($/) {
        make $<string>.ast => $<value>.ast;
    }

    method array($/) {
        make $<arraylist>.ast.item;
    }

    method arraylist($/) {
        make [$<value>>>.ast];
    }

    method string($/) {
        make $0.elems == 1
            ?? ($0[0].<str> || $0[0].<str_escape>).ast
            !! join '', $0.list.map({ (.<str> || .<str_escape>).ast });
    }
    method value:sym<number>($/) { make +$/.Str }
    method value:sym<string>($/) { make $<string>.ast }
    method value:sym<true>($/)   { make Bool::True  }
    method value:sym<false>($/)  { make Bool::False }
    method value:sym<null>($/)   { make Any }
    method value:sym<object>($/) { make $<object>.ast }
    method value:sym<array>($/)  { make $<array>.ast }

    method str($/)               { make ~$/ }

    my %esc = '\\' => "\\",
              '/'  => "/",
              'b'  => "\b",
              'n'  => "\n",
              't'  => "\t",
              'f'  => "\f",
              'r'  => "\r",
              '"'  => "\"";
    method str_escape($/) {
        make $<xdigit> ?? chr(:16($<xdigit>.join)) !! %esc.AT-KEY(~$/);
    }
}

my grammar JSONPrettyGrammar {
    token TOP       { ^ \s* [ <object> | <array> ] \s* $ }
    rule object     { '{' ~ '}' <pairlist>     }
    rule pairlist   { <pair> * % \,            }
    rule pair       { <string> ':' <value>     }
    rule array      { '[' ~ ']' <arraylist>    }
    rule arraylist  {  <value> * % [ \, ]        }

    proto token value {*};
    token value:sym<number> {
        '-'?
        [ 0 | <[1..9]> <[0..9]>* ]
        [ \. <[0..9]>+ ]?
        [ <[eE]> [\+|\-]? <[0..9]>+ ]?
    }
    token value:sym<true>    { <sym>    };
    token value:sym<false>   { <sym>    };
    token value:sym<null>    { <sym>    };
    token value:sym<object>  { <object> };
    token value:sym<array>   { <array>  };
    token value:sym<string>  { <string> }

    token string {
        \" ~ \" ( <str> | \\ <str_escape> )*
    }

    token str {
        <-["\\\t\n]>+
    }

    token str_escape {
        <["\\/bfnrt]> | u <xdigit>**4
    }
}

proto sub to-json($, :$indent = 0, :$first = 0) {*}

multi sub to-json(Version:D $v, :$indent = 0, :$first = 0) { to-json(~$v, :$indent, :$first) }
multi sub to-json(Real:D $d, :$indent = 0, :$first = 0) { (' ' x $first) ~ ~$d }
multi sub to-json(Bool:D $d, :$indent = 0, :$first = 0) { (' ' x $first) ~ ($d ?? 'true' !! 'false') }
multi sub to-json(Str:D $d, :$indent = 0, :$first = 0) {
    (' ' x $first) ~ '"'
    ~ $d.trans(['"', '\\', "\b", "\f", "\n", "\r", "\t"]
            => ['\"', '\\\\', '\b', '\f', '\n', '\r', '\t'])\
            .subst(/<-[\c32..\c126]>/, { ord(~$_).fmt('\u%04x') }, :g)
    ~ '"'
}
multi sub to-json(Positional:D $d, :$indent = 0, :$first = 0) {
    (' ' x $first) ~ "\["
            ~ ($d ?? $d.map({ "\n" ~ to-json($_, :indent($indent + 2), :first($indent + 2)) }).join(",") ~ "\n" ~ (' ' x $indent) !! ' ')
            ~ ']';
}
multi sub to-json(Associative:D $d, :$indent = 0, :$first = 0) {
    (' ' x $first) ~ "\{"
            ~ ($d ?? $d.map({ "\n" ~ to-json(.key, :first($indent + 2)) ~ ' : ' ~ to-json(.value, :indent($indent + 2)) }).join(",") ~ "\n" ~ (' ' x $indent) !! ' ')
            ~ '}';
}

multi sub to-json(Mu:U $, :$indent = 0, :$first = 0) { 'null' }
multi sub to-json(Mu:D $s, :$indent = 0, :$first = 0) {
    die "Can't serialize an object of type " ~ $s.WHAT.perl
}

sub from-json($text) {
    my $a = JSONPrettyActions.new();
    my $o = JSONPrettyGrammar.parse($text, :actions($a));
    $o.ast;
}
