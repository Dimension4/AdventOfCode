#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CAP 1024

typedef struct {
    int64_t x, y;
} Vec;

static Vec points[CAP];
static int points_count;

void load_file(const char *path) {
    FILE *f = fopen(path, "r");
    assert(f && "file doesn't exist");
    while (!feof(f) && points_count < CAP) {
        fscanf(f, "%lld,%lld ", &points[points_count].x, &points[points_count].y);
        ++points_count;
    }
    assert(feof(f) && "buffer too small");
    assert(!ferror(f));
    fclose(f);
}

Vec vec_sub(Vec a, Vec b) {
    return (Vec){a.x - b.x, a.y - b.y};
}

int64_t rect_area(Vec rect_min, Vec rect_max) {
    Vec area = vec_sub(rect_max, rect_min);
    ++area.x;
    ++area.y;
    return llabs(area.x * area.y);
}

void vec_min_max(Vec *a, Vec *b) {
    int64_t t;
    if (a->x > b->x) {
        t = a->x;
        a->x = b->x;
        b->x = t;
    }
    if (a->y > b->y) {
        t = a->y;
        a->y = b->y;
        b->y = t;
    }
}

bool rect_intersects_line(Vec rect_min, Vec rect_max, Vec line_min, Vec line_max) {
    return (line_min.x < rect_max.x && line_max.x > rect_min.x && rect_min.y < line_min.y && line_min.y < rect_max.y) ||
           (line_min.y < rect_max.y && line_max.y > rect_min.y && rect_min.x < line_min.x && line_min.x < rect_max.x);
}

int main(int argc, char **argv) {
    assert(argc == 2 && "pass input file path");
    load_file(argv[1]);

    int64_t max_area_part1 = 0;
    for (int i = 0; i < points_count - 1; ++i) {
        for (int j = i + 1; j < points_count; ++j) {
            Vec a = points[i], b = points[j];
            vec_min_max(&a, &b);
            int64_t area = rect_area(a, b);
            if (area > max_area_part1)
                max_area_part1 = area;
        }
    }

    int64_t max_area_part2 = 0;
    for (int i = 0; i < points_count - 1; ++i) {
        for (int j = i + 1; j < points_count; ++j) {
            Vec a = points[i], b = points[j];
            vec_min_max(&a, &b);
            for (int k = 0; k < points_count; ++k) {
                int l = (k + 1) % points_count;
                Vec c = points[k], d = points[l];
                vec_min_max(&c, &d);
                if (rect_intersects_line(a, b, c, d))
                    goto skip;
            }
            int64_t area = rect_area(a, b);
            if (area > max_area_part2)
                max_area_part2 = area;
        skip:;
        }
    }

    printf("part 1: %lld\n", max_area_part1);
    printf("part 2: %lld\n", max_area_part2);
}
