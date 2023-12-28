#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>


int main(int nargs, char* argv[]) {
    if (nargs != 2) {
        fprintf(stderr, "Incorrect args.\n");
        return 1;
    }
    FILE* file = fopen(argv[1], "r");

    int buffer_size = 256;
    char line[buffer_size];
    long long seeds[buffer_size];
    int seeds_len = 0;

    char* pos = line;
    // First line contains seeds after "seeds:"
    fgets(line, buffer_size, file);
    pos += 6; // strlen("seeds:")
    while (*pos != '\0') {
        pos += 1; // white space
        char digits[buffer_size];
        int digits_len = 0;
        while ( isdigit(*pos) ){
            digits[digits_len] = *pos;
            digits_len++;
            pos++;
        }
        if (digits_len > 0) {
            digits[digits_len] = '\0';
            seeds[seeds_len] = atoll(digits);
            seeds_len++;
        }
    }

    int previous_alpha = 0;
    int seed_transformed[seeds_len];
    while (fgets(line, buffer_size, file)) {
        if (isalpha(*line)) {
            previous_alpha = 1;
            for (int i=0; i<seeds_len; i++)
                seed_transformed[i] = 0;
            continue;
        }
        if (!previous_alpha || !isdigit(*line))
            continue;

        size_t from_start, to_start, range;
        sscanf(line, "%lu %lu %lu", &to_start, &from_start, &range);
        for (int i=0; i<seeds_len; i++) {
            long long shift = seeds[i]-from_start;
            if (shift >= 0 && shift<range && !seed_transformed[i]) {
                seeds[i] = to_start + shift;
                seed_transformed[i] = 1;
            }
        }
    }
    long long min;
    for (int i=0; i<seeds_len; i++) {
        if (i==0 || seeds[i] < min)
            min = seeds[i];
    }
    printf("Lowest location is %lli.\n", min);

    fclose(file);
    return 0;
}
