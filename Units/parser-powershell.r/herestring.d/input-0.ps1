function c
{
    $i = 1
    $heredoc0 =
@'
'"' $i
'@
    Write-Host $heredoc
}
function d
{
    Write-Host d
}
c
d
pause
