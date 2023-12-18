#! /bin/bash

for file in test/*; do
    if [ -f "$file" ]; then
        # 파일이 regular file인 경우에만 처리
        echo "Running test for file: $file"
        ./run "$file"
        echo "--------------------------------------------------------------------------------------------------------------------------------------------------------"
    fi
done
