#!/bin/bash
set -eu

readonly BASE_PATH=$(cd $(dirname ${0}); pwd)
readonly SCRIPT_DIR="${BASE_PATH}/scripts"
readonly SOLUTIONS_DIR="${SCRIPT_DIR}/solutions"

if [ -d ${SOLUTIONS_DIR} ]; then
    read -p "Info: ${SOLUTIONS_DIR} is already exists. Remake that? [y/n] > " res
    if [ ${res} != "y" ]; then
        echo "Info: stop process."
        exit 0
    fi
fi

mkdir -p ${SOLUTIONS_DIR}
for p_name in {a..f}; do
    cp ${SCRIPT_DIR}/template.fsx ${SOLUTIONS_DIR}/${p_name}.fsx
done

echo "Making ${SOLUTIONS_DIR} dir have done. Go For It!"