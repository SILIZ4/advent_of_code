#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main(int nargs, char *argv[]) {
    if (nargs != 2) {
        fprintf(stderr, "Missing args.");
        return 1;
    }
    FILE* file = fopen(argv[1], "r");

    int buffer_size = 256;
    char line[buffer_size];
    int elf_card[buffer_size];
    int own_card[buffer_size];

    size_t points = 0;
    while (fgets(line, buffer_size, file)) {
        int elf_size = 0;
        int power_of_two = -1;

        char* after_colon = strchr(line, ':') + 1;
        int separator_pos = strchr(line, '|')-line;

        char* digit_start = after_colon;
        while (digit_start-line < separator_pos) {
            int len_white_spaces = strspn(digit_start, " ");
            int digits_len = 3-len_white_spaces;
            char digits[digits_len+1];
            for (int i=0; i<digits_len; i++) {
                digits[i] = *(digit_start+len_white_spaces+i);
            }
            digits[digits_len] = '\0';
            elf_card[elf_size] = atoi(digits);
            elf_size += 1;
            digit_start += 3;
        }

        int len = strlen(line)-1; // exclude newline
        digit_start = line+separator_pos+1;
        while (digit_start - line < len || *digit_start == '\0') {
            int len_white_spaces = strspn(digit_start, " ");
            int digits_len = 3-len_white_spaces;
            char digits[digits_len+1];
            for (int i=0; i<digits_len; i++) {
                digits[i] = *(digit_start+len_white_spaces+i);
            }
            digits[digits_len] = '\0';
            int n = atoi(digits);

            for (int i=0; i<elf_size; i++) {
                if (elf_card[i] == n) {
                    power_of_two++;
                    break;
                }
            }
            digit_start += 3;
        }
        if (power_of_two >= 0) {
            points += (size_t) 1 << power_of_two;
        }
    }
    printf("Points: %lu\n", points);

    fclose(file);
    return 0;
}
