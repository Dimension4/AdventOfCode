#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CAP 1000

typedef struct {
    int64_t low, high;
} Interval;

Interval intervals[CAP];
int intervals_count;
int64_t ingredients[CAP];
int ingredients_count;

void merge_intervals(void) {
    for (int i = 0; i < intervals_count;) {
        Interval a = intervals[i];
        for (int j = i + 1; j < intervals_count;) {
            Interval b = intervals[j];
            Interval merged = {a.low < b.low ? a.low : b.low, a.high > b.high ? a.high : b.high};
            if ((merged.high - merged.low) <= (a.high - a.low) + (b.high - b.low)) {
                memcpy(&a, &merged, sizeof(a));
                memcpy(&intervals[j], &intervals[intervals_count - 1], sizeof(intervals[j]));
                --intervals_count;
            } else {
                ++j;
            }
        }
        if (memcmp(&a, &intervals[i], sizeof(a)) == 0)
            ++i;
        else
            memcpy(&intervals[i], &a, sizeof(a));
    }
}

int main(int argc, char** argv) {
    assert(argc == 2 && "pass input file path");
    FILE* f = fopen(argv[1], "r");
    assert(f && "file doesn't exist");
    for (; intervals_count < CAP && !feof(f); ++intervals_count) {
        int read = fscanf(f, "%lld-%lld ", &intervals[intervals_count].low, &intervals[intervals_count].high);
        if (read == 1) {
            ingredients[0] = intervals[intervals_count].low;
            ingredients_count = 1;
            break;
        }
        assert(read == 2);
        assert(!ferror(f));
    }
    for (; ingredients_count < CAP && !feof(f); ++ingredients_count) {
        fscanf(f, "%lld ", &ingredients[ingredients_count]);
        assert(!ferror(f));
    }
    assert(feof(f) && "buffer too small");
    fclose(f);

    merge_intervals();

    int fresh_ingredients_part1 = 0;
    for (int i = 0; i < ingredients_count; ++i) {
        for (int j = 0; j < intervals_count; ++j) {
            if (intervals[j].low <= ingredients[i] && ingredients[i] <= intervals[j].high) {
                ++fresh_ingredients_part1;
                break;
            }
        }
    }

    int64_t fresh_ingredients_part2 = 0;
    for (int i = 0; i < intervals_count; ++i)
        fresh_ingredients_part2 += intervals[i].high - intervals[i].low + 1;

    printf("part 1: %d\npart 2: %lld\n", fresh_ingredients_part1, fresh_ingredients_part2);
}
