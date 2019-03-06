When defining a *function* or macro, Elixir uses the following syntax:

```elixir
def <functionname> <params separated by spaces> do
    <code of the function>
end

def sum a b do
    a + b
end
```

But when defining (or overriding) an *operator*, Elixir follows this syntax:

```elixir
def <left_variable> <operator> <right_variable> do
    <code of the operator>
end

def a < b do
  :erlang.<(a,b)
end
```

This makes creating a regex that differenciates between function and operator
particulary annoying, specially when you consider on the edge cases (a function
with zero params, or a name with special characters, like `assert!`)

The solution my uneducated mind could think of was to determine all the
operators that Elixir could parse, and then reduce them to their basic
characters:

The following is a list of all the operators that Elixir is capable of
parsing, but that are not used by default (separated by a comma):

`|, |||, &&&, <<<, >>>, <<~, ~>>, <~, ~>, <~>, <|>, ^^^, \~\~\~`

They could be expressed in a regex atom `(&&&|<~|<~>|etc...)` but I think
it can be reduced (I don't know the difference in efficiency) to:

`[\|\^&<>~]{1,3}`

If we add the characters for the regular operators that can be overriden, we're
left with: `[\|\^\/&<>~.=!*+-]{1,3}`

Now we're left with these regex:

```perl
--regex-Elixir=/^[ \t]*def(p?)[ \t]+([a-z_][a-zA-Z0-9_?!]*)(.[^\|\^\/&<>~.=!*+-]+)/\2/f/
--regex-Elixir=/^[ \t]*defmacro(p?)[ \t]+([a-z_][a-zA-Z0-9_?!]*)(.[^\|\^\/&<>~.=!*+-]+)/\2/a/
```

Which aren't so bad but are not fine grained enough, because they're missing
these important operators:

* and
* or
* not
* in
* when

My solution isn't good enough to notice the keywords above are actually
operators (and even more: default operators).

I tried with a negative lookahead: `(?![\|\^\/&<>~.=!*+-]{1,3}|and|or|not|in|when)`

But since the regex engine used in universal-ctags can't do lookahead, I'm
not gonna bother to try and make the operator regex exhaustive...

Good luck and godspeed to the person brave enough to tackle this.
