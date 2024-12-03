function a
{
    $i = 1
    $heredoc =
@"
""" $i
"@
    Write-Host $heredoc
}
function b
{
    Write-Host b
}
a
b
pause
