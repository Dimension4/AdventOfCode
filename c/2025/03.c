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

int parse_line(char **src) {
    char *s = *src;
    while (isdigit(*s))
        ++s;
    int count = s - *src;
    while (isspace(*s))
        ++s;
    *src = s;
    return count;
}

void max_digit_sequence(char *begin, char *end, int *digits, int number_of_digits) {
    if (number_of_digits <= 0)
        return;

    for (char *s = begin; s != end - number_of_digits + 1; ++s) {
        int digit = *s - '0';
        if (digit >= digits[0]) {
            if (digit > digits[0])
                digits[1] = 0;
            digits[0] = digit;
            max_digit_sequence(s + 1, end, digits + 1, number_of_digits - 1);
        }
    }
}

uint64_t sum_digits(int *digits, int number_of_digits) {
    uint64_t sum = 0;
    for (int i = 0; i < number_of_digits; ++i)
        sum = sum * 10 + digits[i];
    return sum;
}

int main(int argc, char **argv) {
    assert(argc == 2 && "pass input file path");
    FILE *f = fopen(argv[1], "r");
    assert(f && "file doesn't exist");
    int64_t read = fread(input_buffer, 1, sizeof(input_buffer), f);
    assert(feof(f) && "buffer too small");
    assert(read < sizeof(input_buffer) && "buffer too small by 1");
    input_buffer[read] = 0;
    fclose(f);

    char *src = input_buffer;
    uint64_t total_sum_part1 = 0, total_sum_part2 = 0;

    while (*src) {
        char *begin = src;
        char *end = begin + parse_line(&src);

        int digits[12] = {0};
        max_digit_sequence(begin, end, digits, 2);
        total_sum_part1 += sum_digits(digits, 2);

        memset(digits, 0, sizeof(digits));
        max_digit_sequence(begin, end, digits, 12);
        total_sum_part2 += sum_digits(digits, 12);
    }

    printf("part 1: %llu\npart 2: %llu\n", total_sum_part1, total_sum_part2);
}
