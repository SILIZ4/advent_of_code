#include <stdio.h>
#include <stdlib.h>
#include <string.h>


size_t reverse_search_str(const char str[], const size_t maxlen, const char chars[]){
    size_t len = strnlen(str, maxlen);
    for (size_t i=len-1; i>=0; i--) {
        for (size_t j=0; j<strlen(chars); j++) {
            if (chars[j] == str[i]) {
                return i;
            }
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
        size_t first = strcspn(line, "1234567890");
        size_t last = reverse_search_str(line, buff_size, "1234567890");
        char code[2] = { line[first], line[last] };
        sum += atoi(code);
    }
    printf("Total is %lu.\n", sum);
    fclose(file);

    return 0;
}
