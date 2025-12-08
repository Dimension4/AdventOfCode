#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static char input[1000 * 1000];
static int input_rows;
static int input_cols;

static int64_t numbers[10 * 1000];
static char ops[1000];
static int numbers_count;
static int numbers_per_row;

void load_file(const char* path) {
    FILE* f = fopen(path, "r");
    assert(f && "file doesn't exist");
    int read = fread(input, 1, sizeof(input), f);
    while (input[input_cols++] != '\n') {}
    input_rows = read / input_cols;
    assert(read % input_cols == 0 && "no grid layout");
    assert(feof(f) && "buffer too small");
    assert(!ferror(f));
    fclose(f);
}

void transpose_input(void) {
    for (int r = 0; r < input_rows; ++r) {
        for (int c = 0; c < input_cols; ++c) {
            char tmp = input[r * input_cols + c];
            input[r * input_cols + c] = input[c * input_rows + r];
            input[c * input_rows + r] = tmp;
        }
    }
    int tmp = input_rows;
    input_rows = input_cols;
    input_cols = tmp;
    numbers_per_row = numbers_count / numbers_per_row;
}

void parse_ops(void) {
    numbers_per_row = 0;
    char* line = input + (input_rows - 1) * input_cols;
    while (*line) {
        int off = 0;
        int read = sscanf(line, " %c %n", &ops[numbers_per_row++], &off);
        line += off;
        assert(read == 1);
    }
    assert(*line == 0 && "Not at end of string");
}

void parse_numbers(void) {
    numbers_count = 0;
    char* s = input;
    while (*s) {
        char* prev = s;
        numbers[numbers_count] = strtoll(s, &s, 10);
        if (prev == s)
            ++s;
        else
            ++numbers_count;
    }
}

int64_t do_the_math(void) {
    int cols = numbers_per_row;
    int rows = numbers_count / numbers_per_row;
    int64_t res = 0;
    for (int c = 0; c < cols; ++c) {
        char op = ops[c];
        int64_t col_total = op == '*' ? 1 : 0;
        for (int r = 0; r < rows; ++r) {
            if (op == '*')
                col_total *= numbers[r * cols + c];
            else
                col_total += numbers[r * cols + c];
        }
        // printf("%lld\n", col_total);
        res += col_total;
    }
    return res;
}

int main(int argc, char** argv) {
    assert(argc == 2 && "pass input file path");
    load_file(argv[1]);
    parse_ops();
    parse_numbers();

    puts(input);
    int64_t grand_total_part1 = do_the_math();
    transpose_input();
    parse_numbers();
    int64_t grand_total_part2 = do_the_math();

    puts(input);
    // for (int i = 0; i < numbers_count; ++i) {
    //     if (i % numbers_per_row == 0)
    //         puts("");
    //     printf("%4lld ", numbers[i]);
    // }
    // puts("");
    // for (int i = 0; i < numbers_per_row; ++i)
    //     printf("%4c ", ops[i]);
    // puts("");

    printf("part 1: %lld\npart 2: %lld\n", grand_total_part1, grand_total_part2);
}
