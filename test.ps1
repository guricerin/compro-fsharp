if (${args}.Length -ne 1) {
    Write-Output "Error: expected argument is 1, but actual is $(${args}.Length)."
    exit 1
}

$BASE_PATH = Split-Path $MyInvocation.MyCommand.Path
$SOLUTIONS_DIR = "${BASE_PATH}\src\Solution\Solutions"
$P_NAME = ${args}[0]
$TEST_DIR = "${SOLUTIONS_DIR}\${P_NAME}-test"

if (!(Test-Path ${TEST_DIR})) {
    $url = (Read-Host "Info: input problem url. > ")
    New-Item ${TEST_DIR} -ItemType Directory -Force
    cd ${TEST_DIR}
    oj dl ${url}
}

cd ${TEST_DIR}
cp ${SOLUTIONS_DIR}\${P_NAME}.fs .
fsc.exe ${P_NAME}.fs
oj test -c ".\\${p_name}.exe"
cd ${BASE_PATH}