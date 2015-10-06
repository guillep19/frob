
#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include "FrobDefinitions.h"

/*
 *  * 'open_port()' - Open serial port 1.
 *  *
 *  * Returns the file descriptor on success or -1 on error.
 *  */

int open_port(void) {
  int fd; /* File descriptor for the port */
  //fd = open("/dev/ttyACM0", O_RDWR | O_NOCTTY | O_NDELAY);
  fd = open("/dev/stdout", O_RDWR | O_NOCTTY | O_NDELAY);
  if (fd == -1) {
    perror("open_port: Unable to open /dev/ttyf1 - ");
  } else {
    fcntl(fd, F_SETFL, 0);
  }

  return (fd);
}

int main() {
  int fd = open_port();
  if (fd == -1) {
    return 1;
  }

  struct termios options;
  /* Get the current options for the port...*/
  tcgetattr(fd, &options);
  /* Set the baud rates to 19200...*/
  cfsetispeed(&options, B9600);
  cfsetospeed(&options, B9600);

  /* Enable the receiver and set local mode...*/
  options.c_cflag |= (CLOCAL | CREAD);
  
  /* Set the new options for the port...*/
  tcsetattr(fd, TCSANOW, &options);

  int n = write(fd, "ALF\r", 4);
  if (n < 0) {
    fputs("write() of 2 bytes failed!\n", stderr);
  }

  FILE * fp;
  char * line = NULL;
  size_t len = 0;
  ssize_t read;

  fp = fopen("example2.alf", "r");
  if (fp == NULL) return 2;

  int readd;
  WORD val;
  int v;
  while ((read = getline(&line, &len, fp)) != -1) {
    readd = sscanf(line, "%s %s %s", s1, s2, s3);
    if (readd == 3) {
      WORD opcode;
      sscanf(s3, "%d", &v);
      if (strcmp(s2, "t_halt") == 0) {
        opcode = t_halt;
      } else if (strcmp(s2, "t_call") == 0) {
        opcode = t_call;
      } else if (strcmp(s2, "t_ret") == 0) {
        opcode = t_ret;
      } else if (strcmp(s2, "t_load_param") == 0) {
        opcode = t_load_param;
      } else if (strcmp(s2, "t_lift") == 0) {
        opcode = t_lift;
      } else if (strcmp(s2, "t_lift2") == 0) {
        opcode = t_lift2;
      } else if (strcmp(s2, "t_folds") == 0) {
        opcode = t_folds;
      } else if (strcmp(s2, "t_read") == 0) {
        opcode = t_read;
      } else if (strcmp(s2, "t_write") == 0) {
        opcode = t_write;
      } else if (strcmp(s2, "t_jump") == 0) {
        opcode = t_jump;
      } else if (strcmp(s2, "t_jump_false") == 0) {
        opcode = t_jump_false;
      } else if (strcmp(s2, "t_cmp_eq") == 0) {
        opcode = t_cmp_eq;
      } else if (strcmp(s2, "t_cmp_neq") == 0) {
        opcode = t_cmp_neq;
      } else if (strcmp(s2, "t_cmp_gt") == 0) {
        opcode = t_cmp_gt;
      } else if (strcmp(s2, "t_cmp_lt") == 0) {
        opcode = t_cmp_lt;
      } else if (strcmp(s2, "t_add") == 0) {
        opcode = t_add;
      } else if (strcmp(s2, "t_sub") == 0) {
        opcode = t_sub;
      } else if (strcmp(s2, "t_div") == 0) {
        opcode = t_div;
      } else if (strcmp(s2, "t_mul") == 0) {
        opcode = t_mul;
      } else if (strcmp(s2, "t_op_and") == 0) {
        opcode = t_op_and;
      } else if (strcmp(s2, "t_op_or") == 0) {
        opcode = t_op_or;
      } else if (strcmp(s2, "t_op_not") == 0) {
        opcode = t_op_not;
      } else if (strcmp(s2, "t_push") == 0) {
        opcode = t_push;
      } else if (strcmp(s2, "t_pop") == 0) {
        opcode = t_pop;
      } else if (strcmp(s2, "t_dup") == 0) {
        opcode = t_dup;
      } else if (strcmp(s2, "t_store") == 0) {
        opcode = t_store;
      } else if (strcmp(s2, "t_load") == 0) {
        opcode = t_load;
      }
      val = opcode << 8 | v;
    } else {
      sscanf(s2, "%d", &v);
      val = (WORD) v;
    }
    printf("%.4x %s", val, line);
  }

  printf("Read %d strings", readd);

  WORD* buff = new WORD[4];
  buff[0] = 'a';
  buff[1] = 'l';
  buff[2] = 'f';
  buff[3] = '\n';
  n = write(fd, buff, 8);
  if (n < 0) {
    fputs("write() of 8 bytes failed!\n", stderr);
  }

  /* Close resources. */
  int cl = close(fd);
  if (cl == -1) {
    perror("Unable to close port.");
    return 1;
  }
  return 0;
}
