Set-StrictMode -Version Latest
$ErrorActionPreference = "stop"

if (${args}.Length -ne 1) {
    Write-Host "Usage: .\test.ps1 <a ~ f>" -ForegroundColor Cyan
    exit 1
}

Set-Variable -Name BASE_PATH -Value (Split-Path $MyInvocation.MyCommand.Path) -Option Constant
Set-Variable -Name PROBLEMS_DIR -Value "${BASE_PATH}\Problems" -Option Constant
Set-Variable -Name P_NAME -Value ${args}[0] -Option Constant
Set-Variable -Name TEST_DIR -Value "${PROBLEMS_DIR}\${P_NAME}-test" -Option Constant

if (!(Test-Path ${TEST_DIR})) {
    Write-Host "Info: input ${P_NAME}-problem url." -ForegroundColor Cyan
    $url = (Read-Host " ")
    New-Item ${TEST_DIR} -ItemType Directory -Force
    Set-Location ${TEST_DIR}
    oj dl ${url}
}

Set-Location ${TEST_DIR}
Copy-Item ${PROBLEMS_DIR}\${P_NAME}.fsx .
oj test -c "dotnet fsi ${P_NAME}.fsx --exec"
Set-Location ${BASE_PATH}
