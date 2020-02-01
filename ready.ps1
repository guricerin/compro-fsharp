Set-StrictMode -Version Latest
$ErrorActionPreference = "stop"

Set-Variable -Name BASE_PATH -Value (Split-Path $MyInvocation.MyCommand.Path) -Option Constant
Set-Variable -Name PROBLEMS_DIR -Value "${BASE_PATH}\Problems" -Option Constant

if (Test-Path ${PROBLEMS_DIR}) {
    Write-Host "Info: ${PROBLEMS_DIR} is already exists. Remake that?" -ForegroundColor Cyan
    $res = (Read-Host "[y/n] (Default: n) ")
    if (${res} -ne "y") {
        Write-Host "Info: stop process." -ForegroundColor Cyan
        exit 0
    }
    Write-Host ""
    Remove-Item ${PROBLEMS_DIR} -Force -Recurse
}

New-Item ${PROBLEMS_DIR} -ItemType Directory -Force

$p_names = @("a", "b", "c", "d", "e", "f") 

${p_names} | % { Copy-Item ${BASE_PATH}\template\template.fsx ${PROBLEMS_DIR}\${_}.fsx }

Write-Host ""
Write-Host "Making ${PROBLEMS_DIR} dir have done. Go For It!" -ForegroundColor Green