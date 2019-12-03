$BASE_PATH = Split-Path $MyInvocation.MyCommand.Path
$SRC_PATH = "${BASE_PATH}\src\Solution"
$SOLUTIONS_DIR = "${SRC_PATH}\Solutions"

if (Test-Path ${SOLUTIONS_DIR}) {
    $res = (Read-Host "Info: ${SOLUTIONS_DIR} is already exists. Remake that? [y/n] > ")
    if (${res} -ne "y") {
        Write-Output "Info: stop process."
        exit 1
    }
}

New-Item ${SOLUTIONS_DIR} -ItemType Directory -Force

$p_names = @("a", "b", "c", "d", "e", "f") 

${p_names} | % { Copy-Item ${BASE_PATH}\template\template.fsx ${SOLUTIONS_DIR}\${_}.fs }

Write-Output "Making ${SOLUTIONS_DIR} dir have done. Go For It!"