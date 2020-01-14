Set-StrictMode -Version Latest
$ErrorActionPreference = "stop"

if (${args}.Length -ne 1) {
    Write-Output "Usage: .\test.ps1 (a - f)"
    exit 1
}

$BASE_PATH = Split-Path $MyInvocation.MyCommand.Path
$PROBLEMS_DIR = "${BASE_PATH}\Problems"
$P_NAME = ${args}[0]
$TEST_DIR = "${PROBLEMS_DIR}\${P_NAME}-test"

if (!(Test-Path ${TEST_DIR})) {
    $url = (Read-Host "Info: input problem url. > ")
    New-Item ${TEST_DIR} -ItemType Directory -Force
    Set-Location ${TEST_DIR}
    oj dl ${url}
}

Set-Location ${TEST_DIR}
Copy-Item ${PROBLEMS_DIR}\${P_NAME}.fsx .
oj test -c "dotnet fsi ${P_NAME}.fsx --exec"
Set-Location ${BASE_PATH}
