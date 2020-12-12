#define _GNU_SOURCE
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

typedef struct result {
    bool_t repeated;
    int accumulator;
} result_t;

int parse_argument(char *line) {
    char *c = strchr(line, ' ');
    if (!c) {
        printf("Couldn't read argument for line %s\n", line);
        exit(-1);
    }
    int arg = strtol(c, NULL, 10);
    return arg;
}

inst_t parse_instruction(char *line) {
    if (strncmp(line, "jmp", 3) == 0) {
        int arg = parse_argument(line);
        inst_t inst = { .operation = JMP, .argument = arg };
        return inst;
    } else if (strncmp(line, "acc", 3) == 0) {
        int arg = parse_argument(line);
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

result_t simulate_until_repeat_or_done(inst_t *ins, int n_ins)
{
    int pc = 0;
    bool_t visited[MAX_INS] = {FALSE};
    result_t res = { .accumulator = 0, .repeated = FALSE };

    while (pc < n_ins) {
        if (visited[pc]) {
            res.repeated = TRUE;
            break;
        }
        visited[pc] = TRUE;
        switch (ins[pc].operation) {
            case JMP:
                pc += ins[pc].argument;
                break;
            case ACC:
                res.accumulator += ins[pc].argument;
                // Explicit fallthrough :)
            case NOP:
                pc += 1;
        }
    }
    return res;
}

void part1(inst_t *ins, int n_ins)
{
    result_t res = simulate_until_repeat_or_done(ins, n_ins);
    printf("%d\n", res.accumulator);
}

void part2(inst_t *ins, int n_ins)
{
    int i;
    result_t res = {};
    for (i = 0; i < n_ins; i++) {
        op_t prev_op;    
        switch (ins[i].operation) {
            case JMP:
                prev_op = JMP;
                ins[i].operation = NOP;
                break;
            case NOP:
                prev_op = NOP;
                ins[i].operation = JMP;
                break;
            case ACC:
                prev_op = ACC;
                break;
        }
        res = simulate_until_repeat_or_done(ins, n_ins);
        if (!res.repeated) {
            break;
        }
        ins[i].operation = prev_op;
    }
    printf("%d\n", res.accumulator);
}

int main()
{
    FILE *file;
    char *line = NULL;
    size_t len = 0;
    size_t nread;
    inst_t ins[MAX_INS];
    int count = 0;

    file = fopen("input", "r");
    while ((nread = getline(&line, &len, file)) != -1) {
        ins[count] = parse_instruction(line);
        count++;
        if (count > MAX_INS) {
            printf("Unexpected number of instructions: %d\n", count);
            exit(-1);
        }
    }
    free(line);

    part1(ins, count);
    part2(ins, count);
}
