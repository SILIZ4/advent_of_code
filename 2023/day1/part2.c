#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char text_to_digit(const char str[]) {
    if (!strncmp(str, "one", 3)) {
        return '1';
    } else if (!strncmp(str, "two", 3)) {
        return '2';
    } else if (!strncmp(str, "three", 5)) {
        return '3';
    } else if (!strncmp(str, "four", 4)) {
        return '4';
    } else if (!strncmp(str, "five", 4)) {
        return '5';
    } else if (!strncmp(str, "six", 3)) {
        return '6';
    } else if (!strncmp(str, "seven", 5)) {
        return '7';
    } else if (!strncmp(str, "eight", 5)) {
        return '8';
    } else if (!strncmp(str, "nine", 4)) {
        return '9';
    } else if (!strncmp(str, "zero", 4)) {
        return '0';
    }
    return ' ';
}

char first_digit(const char str[], const size_t maxlen){
    size_t len = strnlen(str, maxlen);
    for (size_t i=0; i<len; i++) {
        if (isdigit(str[i])) {
            return str[i];
        }
        int digit = text_to_digit(str+i);
        if (digit != ' ') {
            return digit;
        }
    }
    return maxlen;
}

char last_digit(const char str[], const size_t maxlen){
    size_t len = strnlen(str, maxlen);
    for (size_t i=len-1; i>=0; i--) {
        if (isdigit(str[i])) {
            return str[i];
        }
        int digit = text_to_digit(str+i);
        if (digit != ' ') {
            return digit;
        }
    }
    return maxlen;
}


int main(int nargs, char* argv[]) {
    if (nargs!=2) {
        fprintf(stderr, "Missing file name.\n");
        return 1;
    }
    char const* const fileName = argv[1];
    FILE* file = fopen(fileName, "r");

    if (file == NULL) {
        fprintf(stderr, "Couldn't open file.\n");
        return 1;
    }

    size_t sum = 0;
    size_t buff_size = 256;
    char line[buff_size];
    while (fgets(line, buff_size, file)) {
        char digits[2] = {first_digit(line, buff_size), last_digit(line, buff_size)};
        sum += atoi(digits);
    }
    printf("Total is %lu.\n", sum);
    fclose(file);

    return 0;
}
