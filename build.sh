gcc src/sylt.c -o bin/sylt_0.1 -Wall -Wextra -Werror -std=c99 -O3 -lm -D_POSIX_C_SOURCE=199309L -DRELEASE
gcc src/sylt.c -o bin/sylt_0.1_debug -Wall -Wextra -Werror -std=c99 -lm -D_POSIX_C_SOURCE=199309L -DDEBUG
