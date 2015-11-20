#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h>

#define BUFFER_SIZE 128

int main(int argc, char* argv[])
{
	if(argc != 2){
		printf("Program expects one argument - path to binary file.\n");
		return -1;
	}
	// open UART: -----------------------------------
	int uart0_filestream = -1;

	uart0_filestream = open("/dev/ttyUSB0", O_RDWR | O_NOCTTY);
	if (uart0_filestream == -1)
	{
		printf("Error -- Unable to open UART.\n");
		return 1;
	}

	struct termios options;
	tcgetattr(uart0_filestream, &options);
	options.c_cflag = B115200 | CS8 | CLOCAL | CREAD;  // baudrate, char size
	options.c_iflag = IGNPAR; // no parity check
	options.c_oflag = 0;
	options.c_lflag = 0;
	
	tcflush(uart0_filestream, TCIFLUSH);
	tcsetattr(uart0_filestream, TCSANOW, &options);

	int pfd;
    if( (pfd = open(argv[1], O_RDONLY)) == -1 ){
		printf("Unable to open file %s!\n", argv[1]);
		return 1;
	}

	char tosend[BUFFER_SIZE];
	int size;
	if( (size=read(pfd, tosend, BUFFER_SIZE)) < 0){
		printf("Reading binary file failure.\n");
		return 1;
	}
	int cnt = write(uart0_filestream, tosend, size);
     if (cnt < 0)
	{
		printf("UART TX error\n");
		return 1;
	}

	int reclen;
	unsigned char received;
    do{
        reclen = read(uart0_filestream, &received,  1);
        if (reclen < 0){
        	printf("Error while receiving.\n");
        	return 1;
        }
        printf("%u\n", received);
    }while(reclen > 0);
        // close UART: ----------------------------------
	koniec:
	close(uart0_filestream);
	printf("%s\n", "Kuniec transmisji.");
	return 0;
}
