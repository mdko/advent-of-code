#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_INS 1000

typedef enum bool {
    FALSE = 0,
    TRUE = 1,
} bool_t;

typedef enum operation {
    JMP,
    ACC,
    NOP,
} op_t;

typedef struct instruction {
    op_t operation;
    int argument;
} inst_t;

inst_t parse_instruction(char *line) {
    if (strncmp(line, "jmp", 3) == 0) {
        int arg = strtol(line, NULL, 10);
        inst_t inst = { .operation = JMP, .argument = arg };
        return inst;
    } else if (strncmp(line, "acc", 3) == 0) {
        int arg = strtol(line, NULL, 10);
        inst_t inst = { .operation = ACC, .argument = arg };
        return inst;
    } else if (strncmp(line, "nop", 3) == 0) {
        inst_t inst = { .operation = NOP, .argument = 0 };
        return inst;
    } else {
        printf("Illegal instruction: %s\n", line);
        exit(-1);
    }
}

int simulate_until_repeat(inst_t *ins, int n_ins)
{
    int accumulator = 0;
    int pc = 0;
    bool_t visited[MAX_INS] = {FALSE};

    while (pc < n_ins) {
        if (visited[pc]) {
            break;
        }
        visited[pc] = TRUE;
        switch (ins[pc].operation) {
            case JMP:
                pc += ins[pc].argument;
                break;
            case ACC:
                accumulator += ins[pc].argument;
                // Explicit fallthrough :)
            case NOP:
                pc += 1;
        }
    }

    return accumulator;
}

int main()
{
    FILE *file;
    char *line;
    ssize_t linelen;

    inst_t ins[MAX_INS];
    int count = 0;
    int final_accum_value;

    file = fopen("input", "w");
    while ((linelen = getline(&line, NULL, file)) > 0) {
        ins[count] = parse_instruction(line);
        count++;
        if (count > MAX_INS) {
            printf("Unexpected number of instructions: %d\n", count);
        }
    }

    final_accum_value = simulate_until_repeat(ins, count);
    printf("%d\n", final_accum_value);
}