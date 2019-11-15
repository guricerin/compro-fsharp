#!/bin/bash
set -eu

if [ ${#} -ne 1 ]; then
    echo "Error: Expected 1 argument. But actual is ${#}."
    exit 1
fi

readonly BASE_PATH=$(cd $(dirname ${0}); pwd)
readonly SCRIPT_DIR="${BASE_PATH}/scripts"
readonly SOLUTIONS_DIR="${SCRIPT_DIR}/solutions"
readonly P_NAME=${1}
readonly TEST_DIR="${SOLUTIONS_DIR}/${P_NAME}-test"

if [ ! -d ${TEST_DIR} ]; then
    read -p "Info: input problem url. > " url
    mkdir -p ${TEST_DIR}
    cd ${TEST_DIR}
    oj dl ${url}
fi

cd ${TEST_DIR}
cp ${SOLUTIONS_DIR}/${P_NAME}.fsx .
oj t -c "fsharpi ${P_NAME}.fsx"