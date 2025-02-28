#include <stdio.h>
#include <ctype.h>

#define ROWS 140
#define COLS 140

typedef struct {
  char c;
  int acc;
} cell_t;

cell_t grid[ROWS][COLS] = {};

// the goal of my attempt is to make this algorithm
// O(N), ie do it all in a single pass

int main() {
  FILE *f = fopen("./input.txt", "r");
  int c, i = 0, j = 0;

  memset(grid, 0, sizeof(cell_t) * ROWS * COLS);

  while ((c = fgetc(f)) != EOF) {
    if (c == '\n') {
      i++;
      j = 0;
      continue;
    }
    grid[i][j].c = c;

    if (c == '.') continue;

    if (isdigit(c)) {
      int d = atoi(c);
      if (j == 0) c.acc = d;
      else grid[i][j].acc = d + grid[i][j-1].acc;
    } else {
      // get all adjacent cells that are the last numbers
      // in their sequence and add them to the running sum.
      // also mark them as recorded.
    }


    // when we hit a non-numeric, non-'.' symbol#include <stdio.h>
    // inform our connected cells. if a number and connected (horizontally)
    // numbers are all done, figure out number and add it so global.
    // then mark cell as visited/done.

    j++;
  }
}
