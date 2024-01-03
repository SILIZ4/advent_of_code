#include <stdio.h>
#include <stdlib.h>
#include <string.h>


size_t search_chars_first_pos(const char str[], const char chars[]){
    size_t i=0;
    size_t len = strlen(str);
    for (; i<len; i++) {
        for (size_t j=0; j<strlen(chars); j++) {
            if (chars[j] == str[i]) {
                return i;
            }
        }
    }
    return strlen(str);
}


int main(int nargs, char *argv[]){
    if (nargs != 2) {
        fprintf(stderr, "Missing args.\n");
        return 1;
    }
    FILE* file = fopen(argv[1], "r");
    /*const size_t n_red = atoi(argv[2]);*/
    /*const size_t n_blue = atoi(argv[4]);*/
    /*const size_t n_green = atoi(argv[3]);*/
    const size_t n_red = 12;
    const size_t n_blue = 13;
    const size_t n_green = 14;

    const size_t buff_size=256;
    char buff[buff_size];
    size_t sum = 0;
    while (fgets(buff, buff_size, file)) {
        // first digit after strlen("Game ") = 5
        char *first_digit = buff+5;
        const size_t colon_pos = strchr(first_digit, ':')-first_digit;
        char game_digits[buff_size];
        strncpy(game_digits, first_digit, colon_pos);
        const int game_id = atoi(game_digits);

        char *draws_begin = first_digit+colon_pos+1;
        const size_t len = strlen(first_digit);
        int game_valid = 1;
        while (draws_begin-first_digit < len) {
            size_t draws_end_pos = search_chars_first_pos(draws_begin, ";");
            char *beg = draws_begin;
            while (beg-draws_begin < draws_end_pos) {
                size_t next_item_beg_pos = search_chars_first_pos(beg, ",;");
                char str_to_parse[buff_size];
                strncpy(str_to_parse, beg, next_item_beg_pos);
                str_to_parse[next_item_beg_pos] = '\0';

                int n_balls, comparison;
                char color[buff_size];
                sscanf(str_to_parse, " %d %s", &n_balls, color);
                if (!strcmp(color, "red")) {
                    comparison = n_red;
                } else if (!strcmp(color, "blue")) {
                    comparison = n_blue;
                } else if (!strcmp(color, "green")) {
                    comparison = n_green;
                } else {
                    fprintf(stderr, "Couldn't determine color.");
                    return 1;
                }

                if (n_balls > comparison) {
                    game_valid = 0;
                    break;
                }
                beg += next_item_beg_pos+1;
            }
            if (!game_valid) {
                break;
            }
            draws_begin += draws_end_pos+1;
        }
        if (game_valid) {
            sum += game_id;
        }
    }
    printf("Valid game sum = %lu\n", sum);
    fclose(file);
    return 0;
}
