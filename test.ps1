if (${args}.Length -ne 1) {
    Write-Output "Error: expected argument is 1, but actual is $(${args}.Length)."
    exit 1
}

$BASE_PATH = Split-Path $MyInvocation.MyCommand.Path
$SOLUTIONS_DIR = "${BASE_PATH}\src\Solutions"
$P_NAME = ${args}[0]
$TEST_DIR = "${SOLUTIONS_DIR}\${P_NAME}-test"

if (!(Test-Path ${TEST_DIR})) {
    $url = (Read-Host "Info: input problem url. > ")
    New-Item ${TEST_DIR} -ItemType Directory -Force
    Set-Location ${TEST_DIR}
    oj dl ${url}
}

Set-Location ${TEST_DIR}
Copy-Item ${SOLUTIONS_DIR}\${P_NAME}.fsx .
oj test -c "dotnet fsi ${P_NAME}.fsx --exec"
Set-Location ${BASE_PATH}
