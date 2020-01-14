Set-StrictMode -Version Latest
$ErrorActionPreference = "stop"

$BASE_PATH = Split-Path $MyInvocation.MyCommand.Path
$PROBLEMS_DIR = "${BASE_PATH}\Problems"

if (Test-Path ${PROBLEMS_DIR}) {
    $res = (Read-Host "Info: ${PROBLEMS_DIR} is already exists. Remake that? [y/n] > ")
    if (${res} -ne "y") {
        Write-Output "Info: stop process."
        exit 0
    }
    Remove-Item ${PROBLEMS_DIR} -Force -Recurse
}

New-Item ${PROBLEMS_DIR} -ItemType Directory -Force

$p_names = @("a", "b", "c", "d", "e", "f") 

${p_names} | % { Copy-Item ${BASE_PATH}\template\template.fsx ${PROBLEMS_DIR}\${_}.fsx }

Write-Output "Making ${PROBLEMS_DIR} dir have done. Go For It!"