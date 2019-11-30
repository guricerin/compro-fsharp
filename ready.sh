#!/bin/bash
set -eu

readonly BASE_PATH=$(cd $(dirname ${0}); pwd)
readonly SRC_PATH="${BASE_PATH}/src/Solution"
readonly SOLUTIONS_DIR="${SRC_PATH}/Solutions"

if [ -d ${SOLUTIONS_DIR} ]; then
    read -p "Info: ${SOLUTIONS_DIR} is already exists. Remake that? [y/n] > " res
    if [ ${res} != "y" ]; then
        echo "Info: stop process."
        exit 0
    fi
fi

rm -rf ${SOLUTIONS_DIR}
mkdir -p ${SOLUTIONS_DIR}
for p_name in {a..f}; do
    cp ${BASE_PATH}/template/template.fsx ${SOLUTIONS_DIR}/${p_name}.fs
done

echo "Making ${SOLUTIONS_DIR} dir have done. Go For It!"