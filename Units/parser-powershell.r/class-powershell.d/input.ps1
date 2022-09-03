class MyException : Exception {
    MyException([String]$Message) : base([String]$Message) {
        Write-Host "dummy"
    }
}

class Foo {
    $Property1
    $Property2 = 20
    Method($Arg1) {
        $LocalVar1 = 100
        Write-Host "dummy"
    }
}

function GetBar {
    $LocalVar2 = 200
    Write-Host "dummy"
}

function GetBaz() {
    Write-Host "dummy"
}
