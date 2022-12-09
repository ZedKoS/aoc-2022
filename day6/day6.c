#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// #define NCHARS 4 // part 1
#define NCHARS 14 // part2

typedef uint8_t bool;
#define false 0
#define true 1


char *read_to_string(const char *filename, size_t *contents_size);
bool repeating(const char*, int);

int main(void) {
    size_t n;
    char *input = read_to_string("input.txt", &n);

    if (input == NULL) {
        return errno;
    }

    const char* chunk;
    int found = -1;

    for (int i = NCHARS - 1; i < n; i++) {
        chunk = input + i - (NCHARS - 1);
        bool r = repeating(chunk, NCHARS);

        if (!r) {
            found = i;
            break;
        }
    }

    if (found == -1) {
        printf("marker sequence not found :(\n");
    } else {
        printf("found marker sequence at index: %d\n", found);
        printf("ANSWER (characters read): %d\n", found + 1);
    }

    free(input);
    return 0;
}

bool repeating(const char* chunk, int n) {
    for (int c = 0; c < n; c++) {
        for (int o = 0; o < n; o++) {
            if (c != o && chunk[c] == chunk[o]) {
                return true;
            }
        }
    }
    return false;
}

char *read_to_string(const char *filename, size_t *contents_size) {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        fprintf(stderr, "Could not open file %s\n", filename);
        errno = 1;
        return NULL;
    }

    size_t size;
    fseek(file, 0, SEEK_END);
    size = ftell(file);
    rewind(file);

    char *contents = malloc(size * sizeof(*contents));
    size_t read_count = fread(contents, sizeof(*contents), size, file);

    if (read_count != size) {
        fprintf(stderr, "Could not read contents of file %s\n", filename);
        errno = 2;
        return NULL;
    }

    *contents_size = size;
    return contents;
}