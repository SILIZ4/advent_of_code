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
    long long seeds[4*buffer_size][2];
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
            seeds[seeds_len/2][seeds_len%2] = atoll(digits);
            seeds_len++;
        }
    }
    seeds_len /= 2;

    int previous_alpha = 0;
    int seed_transformed[buffer_size];
    while (fgets(line, buffer_size, file)) {
        if (isalpha(*line)) {
            previous_alpha = 1;
            for (int i=0; i<seeds_len; i++)
                seed_transformed[i] = 0;
            continue;
        }
        if (!previous_alpha || !isdigit(*line))
            continue;

        long long from_start, to_start, range;
        sscanf(line, "%lli %lli %lli", &to_start, &from_start, &range);
        long long from_end = from_start+range;

        for (int i=0; i<seeds_len; i++) {
            if (seed_transformed[i])
                continue;

            long long seed_end = seeds[i][0] + seeds[i][1];
            // No seed is transformed
            if (! (seed_end > from_start && seeds[i][0] < from_end) ) {
                continue;
            }

            long long min_in_range, max_in_range;
            if (seeds[i][0]<from_start) {
                min_in_range = from_start;

                seeds[seeds_len][0] = seeds[i][0];
                seeds[seeds_len][1] = from_start-seeds[i][0];
                seed_transformed[seeds_len] = 0;
                seeds_len++;
            } else {
                min_in_range = seeds[i][0];
            }

            if (seed_end>from_end) {
                max_in_range = from_end;

                seeds[seeds_len][0] = from_end;
                seeds[seeds_len][1] = seed_end-from_end;
                seed_transformed[seeds_len] = 0;
                seeds_len++;
            } else {
                max_in_range = seed_end;
            }

            seeds[i][0] = to_start + min_in_range - from_start;
            seeds[i][1] = max_in_range - min_in_range;
            seed_transformed[i] = 1;
        }
    }

    long long min;
    for (int i=0; i<seeds_len; i++) {
        if (i==0 || seeds[i][0] < min)
            min = seeds[i][0];
    }
    printf("Lowest location is %lli.\n", min);

    fclose(file);
    return 0;
}
