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
    int card_copies[buffer_size];
    for (int i=0; i<buffer_size; i++)
        card_copies[i] = 1;

    int card_id = 0;
    while (fgets(line, buffer_size, file)) {
        int elf_size = 0;
        int winning_cards = 0;

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
                    winning_cards++;
                    break;
                }
            }
            digit_start += 3;
        }
        if (winning_cards > 0) {
            int max_card = card_id+winning_cards+1;
            for (int j=card_id+1; j< (max_card<buffer_size? max_card : buffer_size); j++) {
                card_copies[j] += card_copies[card_id];
            }
        }
        card_id++;
    }

    size_t total_copies = 0;
    for (int i=0; i<card_id; i++)
        total_copies += card_copies[i];
    printf("Total copies: %lu\n", total_copies);

    fclose(file);
    return 0;
}
