function e
{
    $i = 1
    $heredoc0 =
@'
    Write-Host $heredoc
}
function inherestr
{
    Write-Host d
}
c
d
pause

'"' $i
'@
}
