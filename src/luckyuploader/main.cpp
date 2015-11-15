
#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include "FrobDefinitions.h"

/*
 * 'open_port()' - Open serial port.
 * Returns the file descriptor on success or -1 on error.
 */

int open_port(void) {
  int fd; /* File descriptor for the port */
  //fd = open("/dev/ttyACM0", O_RDWR | O_NOCTTY | O_NDELAY);
  fd = open("/dev/ttyACM0", O_RDWR | O_NOCTTY | O_NDELAY);
  if (fd == -1) {
    perror("open_port: Unable to open /dev/stdout - ");
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
    fputs("write() of 4 bytes failed!\n", stderr);
  }

  FILE * fp;
  char * line = NULL;
  size_t len = 0;
  ssize_t read;

  fp = fopen("example.hex", "r");
  if (fp == NULL) return 2;

  WORD* buffer = new WORD[1024];
  WORD word;
  int count;
  for (count = 0; (read = getline(&line, &len, fp)) != -1; count++) {
    sscanf(line, "%x", &word);
    buff[count] = word;
  }

  int index = 0;
  for (index = 0; index < count; index++) {
      int n = write(fd, buffer+index, 2);
      if (n != 2) {
        fputs("write() of 2 bytes failed!\n", stderr);
      }
      printf("%.4x %s", val, line);
  }

  /* Close resources. */
  int cl = close(fd);
  if (cl == -1) {
    perror("Unable to close port.");
    return 1;
  }
  return 0;
}
