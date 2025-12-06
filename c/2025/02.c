#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static char input_buffer[2 * 1000 * 1000];

int to_reverse_digits(uint64_t x, char *buffer) {
    int i = 0;
    while (x != 0) {
        buffer[i++] = x % 10;
        x /= 10;
    }

    return i;
}

int digits_repetition_count(const char *digits, int number_of_digits) {
    for (int segment_count = 2; segment_count <= number_of_digits; ++segment_count) {
        if (number_of_digits % segment_count != 0)
            continue;
        int segment_size = number_of_digits / segment_count;
        for (int offset = segment_size; offset < number_of_digits; offset += segment_size) {
            if (memcmp(digits, digits + offset, segment_size) != 0)
                goto outer;
        }
        return segment_count;
    outer:;
    }
    return 1;
}

int main(int argc, char **argv) {
    assert(argc == 2 && "pass input file path");
    FILE *f = fopen(argv[1], "r");
    assert(f != NULL && "file doesn't exist");
    int64_t read = fread(input_buffer, 1, sizeof(input_buffer), f);
    assert(feof(f) && "buffer too small");
    assert(read < sizeof(input_buffer) && "buffer too small by 1");
    while (isspace(input_buffer[read - 1]))
        --read;
    input_buffer[read] = 0;
    fclose(f);

    const char *src = input_buffer;
    char digits[32];
    uint64_t invalid_sum_part1 = 0, invalid_sum_part2 = 0;

    while (*src) {
        uint64_t low, high;
        int read;
        int parsed = sscanf(src, "%llu-%llu%n", &low, &high, &read);
        assert(parsed == 2);
        assert(0 < read && read < 100);
        src += read;
        if (*src == ',')
            ++src;

        for (uint64_t x = low; x <= high; ++x) {
            int digit_count = to_reverse_digits(x, digits);
            int repetitions = digits_repetition_count(digits, digit_count);
            if (repetitions == 2)
                invalid_sum_part1 += x;
            if (repetitions > 1)
                invalid_sum_part2 += x;
        }
    }

    printf("part 1: %llu\npart 2: %llu\n", invalid_sum_part1, invalid_sum_part2);
}
