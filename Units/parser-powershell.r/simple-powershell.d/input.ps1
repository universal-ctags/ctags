# pseudo #!/PowerShell :)
#
# test file for the CTags/Geany PowerShell tag parser
# based on real world code but simplified for automated tests

<#
multiline comment including a function and variable, both should be ignored:

$IgnoreThisVaribale = "Stop"

function IgnoreThisFunction($arg1)  {
    Write-Host "dummy"
}
#>

# immediately stop the script if an errors occurs
$ErrorActionPreference = "Stop"

# a global scoped variable
$Global:Settings = $null

# a local scoped variable
$Local:ALocalVar = $null

# a usual variable
$BasePath = split-path -parent $Global:MyInvocation.InvocationName


FUNCTION Read-Configuration-File() {
    $Hostname = [System.Environment]::MachineName
    $ConfigurationFileName = $BasePath + "\script-${Hostname}.conf"
    LogMessage "Read configuration '${ConfigurationFileName}'"

    $ConfigFileContent = Get-Content -raw $ConfigurationFileName
    $Global:Settings = Convertfrom-Stringdata $ConfigFileContent
}

Function LogMessageOK()
{
    $x = [Console]::WindowWidth - 6
    $y = [Console]::CursorTop
    Try {
        [Console]::setcursorposition($x, $y)
    } Catch [system.exception] {
        # intentionally left empty for redirect of outputs to file
    }

    Write-Host -foregroundcolor "green" "[ok]"
}

function LogMessage() {
    param(
        [Parameter(Mandatory=$false)][switch] $NoNewLine,
        [Parameter(Mandatory=$true)][string] $Message
    )
    $Date = Get-Date -UFormat "%Y-%m-%d %T: "
    Write-Host -foregroundcolor "yellow" -NoNewLine $Date

    if ($NoNewLine) {
        Write-Host -foregroundcolor "green" -NoNewLine $Message
    } else {
        Write-Host -foregroundcolor "green" $Message
    }
}

function global:A-Global-Scope-Function() {
    Write-Host "dummy"
}

filter Script:MyFilter {
    filter-something
}

Filter Private:MyPrivateFilter {
    filter-something
}

function LoadTemplate($template) {
    # woah, this is real magic,
    # see http://stackoverflow.com/questions/10754582/string-interpolation-of-hashtable-values-in-powershell

    # Set all unbound variables (@args) in the local context
    while ($args)
    {
        ($key, $val, $args) = $args
        Set-Variable -Name $key.SubString(1, $key.Length-2) -Value $val
    }
    $ExecutionContext.InvokeCommand.ExpandString($template)
}

function TopLevelFunction() {
    function SecondLevelNestedFunction() {
        function ThirdLevelNestedFunction() {
            doSomething()
        }
        
        ThirdLevelNestedFunction
    }
    
    SecondLevelNestedFunction
}

function Main() {
    Read-Configuration-File
    LogMessage $("Working on Environment '{0}'" -f $Settings["EnvironmentName"])

    LogMessage "do something ..."
    Stripped-Down-Code
    LogMessageOK
}

Main
