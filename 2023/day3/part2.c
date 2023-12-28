#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


struct Node {
    struct Node* next;
    size_t v;
};

struct Node* append(struct Node* last, size_t value) {
    struct Node* new_node = (struct Node*) malloc(sizeof(struct Node));
    new_node->v = value;
    new_node->next = NULL;
    if (last != NULL) {
        last->next = new_node;
    }
    return new_node;
}

void destroy_list(struct Node* first) {
    struct Node* p = first;
    while (p!=NULL) {
        struct Node* tmp_next = p->next;
        free(p);
        p = tmp_next;
    }
}

int extract_int_from_pos(char scheme[], const int row, const int rows, const int col, const int cols) {
    int start = col;
    char c;

    while (1) {
        if (start-1<0)
            break;
        c = scheme[row*rows+ --start];
        if (!isdigit(c)) {
            start++;
            break;
        }
    }

    int end = col;
    while (1) {
        if (end+1 == cols) {
            end++;
            break;
        }
        c = scheme[row*rows+ ++end];
        if (!isdigit(c))
            break;
    }
    char digits[256];
    int len = end-start;
    strncpy(digits, scheme+row*rows+start, len);
    digits[len] = '\0';

    return atoi(digits);
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
    struct Node* first_star = NULL;
    struct Node* last_star = NULL;
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
                if (*c == '*') {
                    last_star = append(last_star, i);
                    if (first_star == NULL)
                        first_star = last_star;
                }
                i++;
            }
            rows++;
        }
    }

    size_t sum = 0;
    struct Node* star = first_star;
    while (star != NULL) {
        size_t pos = star->v;
        int i = pos/cols;
        int j = pos-i*rows;

        int min_col = j>0 ? 0 : 1;
        int max_col = j+1<cols ? 2 : 1;
        int min_row = i>0 ? 0 : 1;
        int max_row = i+1<rows ? 2 : 1;

        int candidate_neighbours[3][3] = {{0,0,0},{0,0,0},{0,0,0}};
        int n_canditates = 0;
        for (int r=min_row; r<max_row+1; r++) {
            int previous_is_digit = 0;
            for (int s=min_col; s<max_col+1; s++) {
                if (isdigit(scheme[(i+r-1)*rows + j+s-1])) {
                    if (!previous_is_digit) {
                        candidate_neighbours[r][s] = 1;
                        n_canditates += 1;
                        previous_is_digit = 1;
                    }
                } else {
                    previous_is_digit = 0;
                }
            }
        }

        if (n_canditates>1) {
            printf("(%d, %d) is valid\n", i, j);
            size_t prod = 1;
            for (int r=min_row; r<max_row+1; r++) {
                for (int s=min_col; s<max_col+1; s++) {
                    if (candidate_neighbours[r][s] == 1) {
                        prod *= extract_int_from_pos(scheme, i+r-1, rows, j+s-1, cols);
                    }
                }
            }
            sum += prod;
        }

        star = star->next;
    }
    printf("Sum is %lu\n", sum);

    destroy_list(first_star);
    fclose(file);
    return 0;
}
