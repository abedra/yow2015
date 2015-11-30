#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

uint32_t ROL(uint32_t val, uint32_t shift) {
  shift &= 0x1f;
  val = (val >> (0x20 - shift)) | (val << shift);
  return val;
}

void sha1_calcHashBuf(const char *input, size_t length, uint32_t *result) {
  void *dataptr = malloc(1024);
  memset(dataptr, 0, 1024);
  uint32_t *data = (uint32_t *) dataptr;
  memcpy(data, input, length);

  for (int i = 16; i < 80; i++) {
    data[i] = ROL(1, (int) (data[i-16] ^ data[i-8] ^ data[i-14] ^ data[i-3]) % 32);
  }

  uint32_t A = 0x67452301;
  uint32_t B = 0xefcdab89;
  uint32_t C = 0x98badcfe;
  uint32_t D = 0x10325476;
  uint32_t E = 0xc3d2e1f0;

  uint32_t temp = 0;
  for (int i = 0; i < 20; i++) {
    temp = *data++ + ROL(A, 5) + E + ((B & C) | (~B & D)) + 0x5a827999;
    E = D;
    D = C;
    C = ROL(B, 30);
    B = A;
    A = temp;
  }

  for (int i = 0; i < 20; i++) {
    temp = (D ^ C ^ B) + E + ROL(temp, 5) + *data++ + 0x6ed9eba1;
    E = D; D = C; C = ROL(B, 30); B = A; A = temp;
  }

  for (int i = 0; i < 20; i++) {
    temp = *data++ + ROL(temp, 5) + E + ((C & B) | (D & C) | (D & B)) - 0x70E44324;
    E = D; D = C; C = ROL(B, 30); B = A; A = temp;
  }

  for (int i = 0; i < 20; i++) {
    temp = (D ^ C ^ B) + E + ROL(temp, 5) + *data++ - 0x359d3e2a;
    E = D; D = C; C = ROL(B, 30); B = A; A = temp;
  }

  result[0] = A + 0x67452301;
  result[1] = B + 0xefcdab89;
  result[2] = C + 0x98badcfe;
  result[3] = D + 0x10325476;
  result[4] = E + 0xc3d2e1f0;

  free(dataptr);
}

char *sha1(char *input) {
  uint32_t result[5];
  sha1_calcHashBuf(input, strlen(input), (uint32_t *)result);
  char *output = malloc(40);
  sprintf(output, "%08x%08x%08x%08x%08x", result[0], result[1], result[2], result[3], result[4]);
  return output;
}

int main(int argc, char** argv) {
  char *input = argv[1];
  char *output = sha1(input);
  printf("SHA1 %s: %s\n", input, output);
  return 0;
}
