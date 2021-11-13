function Invoke-DotNet {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Command,
        [string[]]$Arguments
    )

    & "dotnet" $Command $Arguments
}

function Invoke-Interpreter {
    param (
        [Parameter(Mandatory=$true)]
        [IO.FileInfo]$Interpreter,
        [Parameter(Mandatory=$true)]
        [IO.FileInfo]$Module,
        [string]$Configuration,
        [IO.DirectoryInfo[]]$ImportedDirectories,
        [switch]$LaunchInterpreterDebugger,
        [switch]$Trace,
        [string[]]$ApplicationArguments
    )

    $arguments = @("--project", $Interpreter)
    if (-not [string]::IsNullOrEmpty($Configuration)) { $arguments += @("--configuration", $Configuration) }
    $arguments += @("--", "--program", $Module)

    foreach ($dir in $ImportedDirectories) { $arguments += @("--import-directory", $dir) }
    if ($LaunchInterpreterDebugger) { $arguments += "--launch-interpreter-debugger" }
    if ($Trace) { $arguments += @("--trace", [IO.Path]::ChangeExtension($Module, ".speedscope.json")) }
    if ($ApplicationArguments.Length -ge 0) { $arguments += @("--") + $ApplicationArguments }

    Invoke-DotNet -Command "run" -Arguments $arguments
}

Export-ModuleMember -Function Invoke-Interpreter
