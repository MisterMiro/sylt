
echo building release...
gcc src/sylt.c -o sylt -Wall -Wextra -std=c99 -O3 -lm -D_POSIX_C_SOURCE=199309L -DRELEASE

echo building debug...
gcc src/sylt.c -o sylt_dbg -Wall -Wextra -std=c99 -O3 -lm -D_POSIX_C_SOURCE=199309L -DDEBUG
