#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define GRID_CAP 256

typedef char(Grid_Row)[GRID_CAP];

static char grids[2][GRID_CAP][GRID_CAP] = {0};
static int grid_size = 0;

int neighbour_sum(Grid_Row* grid, int row, int col) {
    return grid[row - 1][col - 1] + grid[row - 1][col - 0] + grid[row - 1][col + 1] + grid[row - 0][col - 1] +
           grid[row - 0][col + 1] + grid[row + 1][col - 1] + grid[row + 1][col - 0] + grid[row + 1][col + 1];
}

int main(int argc, char** argv) {
    assert(argc == 2 && "pass input file path");
    FILE* f = fopen(argv[1], "r");
    assert(f && "file doesn't exist");
    for (int i = 1; i < GRID_CAP && !feof(f); ++i) {
        fgets(&grids[0][i][1], GRID_CAP - 1, f);
        assert(!ferror(f));
    }
    assert(feof(f) && "buffer too small");
    fclose(f);

    grid_size = strlen(&grids[0][1][1]) + 2 - 1;
    for (int row = 0; row < grid_size; ++row)
        for (int col = 0; col < grid_size; ++col)
            grids[0][row][col] = grids[0][row][col] == '@';

    int number_of_rolls_part1 = 0, number_of_rolls_part2 = 0;

    for (int grid_idx = 0;; grid_idx ^= 1) {
        Grid_Row* cur = grids[grid_idx];
        Grid_Row* next = grids[grid_idx ^ 1];
        memcpy(next, cur, GRID_CAP * GRID_CAP);
        int newly_added_rolls = 0;
        for (int row = 1; row < grid_size - 1; ++row) {
            for (int col = 1; col < grid_size - 1; ++col) {
                if (cur[row][col] && neighbour_sum(cur, row, col) < 4) {
                    ++newly_added_rolls;
                    next[row][col] = 0;
                }
            }
        }
        if (number_of_rolls_part1 == 0)
            number_of_rolls_part1 = newly_added_rolls;
        number_of_rolls_part2 += newly_added_rolls;
        if (newly_added_rolls == 0)
            break;
    }

    printf("part 1: %d\npart 2: %d\n", number_of_rolls_part1, number_of_rolls_part2);
}
