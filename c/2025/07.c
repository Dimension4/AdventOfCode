#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ROWS 256
#define COLS 256

static char grid[ROWS][COLS];
static int grid_rows;
static int grid_cols;

void load_file(const char* path) {
    FILE* f = fopen(path, "r");
    assert(f && "file doesn't exist");
    while (!feof(f) && grid_rows < ROWS)
        fgets(grid[grid_rows++], COLS, f);
    --grid_rows;
    while (!isspace(grid[0][grid_cols++])) {}
    assert(feof(f) && "buffer too small");
    assert(!ferror(f));
    fclose(f);
}

void trace(int row, int col, int* split_count) {
    if (grid[row][col] == '|')
        return;
    for (; row < grid_rows; ++row) {
        switch (grid[row][col]) {
        case '^':
            *split_count += 1;
            trace(row, col - 1, split_count);
            trace(row, col + 1, split_count);
            return;
        case '|':
            return;
        default:
            grid[row][col] = '|';
        }
    }
}

int64_t backtrace(int target_col) {
    static int64_t cache[COLS];
    for (int i = 0; i < COLS; ++i)
        cache[i] = 1;
    for (int row = grid_rows - 1; row > 0; --row) {
        for (int col = 0; col < grid_cols; ++col) {
            if (grid[row][col] == '^') {
                cache[col] = cache[col - 1] + cache[col + 1];
            }
        }
    }
    return cache[target_col];
}

int main(int argc, char** argv) {
    assert(argc == 2 && "pass input file path");
    load_file(argv[1]);

    int start = strchr(grid[0], 'S') - grid[0];
    int splits_part1 = 0;
    trace(1, start, &splits_part1);
    int64_t splits_part2 = backtrace(start);

    printf("part 1: %d\npart 2: %lld\n", splits_part1, splits_part2);
}
