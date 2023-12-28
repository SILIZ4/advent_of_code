#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int is_symbol(const char c) {
    return !isdigit(c) && c!='.';
}

// max cols is excluded
int has_symbol_around(const char scheme[], const size_t min_col, const size_t max_col, const size_t cols, const size_t row, const size_t rows) {

    if (min_col>0 && is_symbol(scheme[row*rows+min_col])) // left
        return 1;
    if (max_col<=cols && is_symbol(scheme[row*rows+max_col-1])) // right
        return 1;

    // also covers diagonals
    for (size_t j=min_col; j<max_col; j++) {
        if (row>0 && is_symbol(scheme[(row-1)*rows+j])) // up
            return 1;
        if (row+1<rows && is_symbol(scheme[(row+1)*rows+j])) // down
            return 1;
    }
    return 0;
}


int main(int nargs, char *argv[]) {
    if (nargs != 2) {
        fprintf(stderr, "Missing args.");
        return 1;
    }
    FILE* file = fopen(argv[1], "r");

    fseek(file, 0L, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    char scheme[size];
    size_t cols=0;
    size_t rows=0;

    int buffer_size = 256;
    {
        char buffer[buffer_size];
        size_t i=0;
        while (fgets(buffer, buffer_size, file)){
            if (cols==0) {
                cols = strlen(buffer) - 1; // removing newlines
            }
            for (char* c=buffer; *c!='\n' && *c!='\0'; c++) {
                scheme[i] = *c;
                i++;
            }
            rows++;
        }
    }

    char digits[buffer_size];
    size_t digits_len = 0;
    size_t sum = 0;
    for (size_t i=0; i<rows; i++) {
        for (size_t j=0; j<cols; j++) {
            char c = scheme[i*rows+j];
            if (isdigit(c)) {
                digits[digits_len++] = c;
            } else if (digits_len > 0) {
                size_t start_col = j-digits_len;
                size_t min_col = start_col>0 ? start_col-1 : 0;
                size_t max_col = j+1<cols ? j+1 : cols; // +1 because excluded
                if (has_symbol_around(scheme, min_col, max_col, cols, i, rows)) {
                    digits[digits_len] = '\0';
                    sum += atoi(digits);
                }
                digits_len = 0;
            }
        }
        if (digits_len > 0) {
            size_t start_col = cols-digits_len;
            size_t min_col = start_col>0 ? start_col-1 : 0;
            if (has_symbol_around(scheme, min_col, cols, cols, i, rows)) {
                digits[digits_len] = '\0';
                sum += atoi(digits);
            }
            digits_len = 0;
        }
    }
    printf("Sum is %lu\n", sum);

    fclose(file);
    return 0;
}
