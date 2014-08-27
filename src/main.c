
#include <stdio.h>

int len = 16;

char** code1 = new char*[len];
char** code2 = new char*[len];
int codes_len = 2;

char*** codes = new char**[codes_len];

void initialize() {
  for (int i = 0; i < len; i++) {
    code1[i] = new char[8];
    code2[i] = new char[8];
    sprintf(code1[i], "1:%d", i);
    sprintf(code2[i], "2:%d", i);
  }
  sprintf(code1[len-1], "Fin");
  sprintf(code2[len-1], "Fin");
  codes[0] = code1;
  codes[1] = code2;
}

void run_codes(int codes_len, char*** codes) {
  int* ips = new int[codes_len];
  ips[0] = 0;
  ips[1] = 0;
  int current_code = 0;
  int finished = 0;
  while (!finished) {
    char** code = codes[current_code];
    int ip = ips[current_code]; 
    char* line;
    int limit = ip + 5;
    for (;(ip < limit) && (ip < len); ip++) {
      line = code[ip];
      printf("%s\n", line);
    }
    //update ip of current program
    ips[current_code] = ip;
    //change of running code
    current_code = (current_code + 1) % 2;
    //check if the codes have finished
    if ((ips[0] == len - 1) && (ips[1] == len - 1)) {
      finished = 1;
    }
  }
}

int main(int argc, char* argv[]) {
  initialize();
  run_codes(codes_len, codes);
  return 0;
}
