#!/bin/bash

TEST_DIR=$(pwd)
cd ..

FAILURES=0
FAILED_TESTS=""

for f in "$TEST_DIR"/t_*.lisp; do
    BASENAME=$(basename "$f")
    NAME_NO_EXT="${BASENAME%.lisp}"
    
    echo "--- Compiling $BASENAME ---"
    
    # Prepend the standard library before passing it to the compiler
    cat stdlib.lisp "$f" | ./boot3 > "$TEST_DIR/$NAME_NO_EXT.fasm"
    
    fasm "$TEST_DIR/$NAME_NO_EXT.fasm"
    FASM_CODE=$?
    
    if [ $FASM_CODE -ne 0 ]; then
        echo ">>> COMPILATION ERROR: $BASENAME failed to assemble <<<"
        FAILURES=$((FAILURES + 1))
        FAILED_TESTS="$FAILED_TESTS\n  - $BASENAME (Assembly Failed)"
        echo ""
        continue
    fi
    
    echo "--- Running $NAME_NO_EXT ---"
    
    "$TEST_DIR/$NAME_NO_EXT"
    
    EXIT_CODE=$?
    if [ $EXIT_CODE -ne 0 ]; then
        echo ">>> CRASH: $BASENAME failed with exit code $EXIT_CODE <<<"
        FAILURES=$((FAILURES + 1))
        FAILED_TESTS="$FAILED_TESTS\n  - $BASENAME (Exit code: $EXIT_CODE)"
    fi
    echo ""
done

echo "======================================"
if [ $FAILURES -eq 0 ]; then
    echo "ALL TESTS EXECUTED SUCCESSFULLY."
    exit 0
else
    echo -e "FAILED TESTS: $FAILURES$FAILED_TESTS"
    exit 1
fi
