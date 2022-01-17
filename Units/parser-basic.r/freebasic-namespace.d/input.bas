namespace first 'first defines something'
  sub first_func
  end sub

  namespace second 'second defines something'
    sub first_func  'oh a second first_func
    end sub

    sub second_func
    end sub
  end namespace 'ignored'
end namespace

sub first_func 'oh another first_func
end sub

' vim: bs=2 sw=2 ss=2 ts=2 nu et ft=basic
