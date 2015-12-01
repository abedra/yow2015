#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

char *caesar(int shift, char *input) {
  char *output = malloc(strlen(input));
  memset(output, '\0', strlen(input));

  for (int x = 0; x <= strlen(input); x++) {
    if (isalpha(input[x])) {
      int c = toupper(input[x]);
      c = (((c - 65) + shift) % 26) + 65;
      output[x] = c;
    } else {
      output[x] = input[x];
    }
  }

  return output;
}
