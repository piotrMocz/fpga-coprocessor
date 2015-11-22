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
	unsigned char received[256];
        unsigned char reversed[256];
        memset(reversed, 0, 256);
        memset(received, 0, 256);
        int rec_idx = 0;
    
    while (1) {
        reclen = read(uart0_filestream, received + rec_idx,  1);
        if (reclen < 0){
        	printf("Error while receiving.\n");
        	return 1;
        }

        if (reclen == 0 || rec_idx == 255) break; // shouldn't really happen but still...
        if (received[rec_idx] == 255) { 
            received[rec_idx] = 0;
            break;
        }

        rec_idx++;
        // printf("%u\n", received[rec_idx-1]);
    }

    int idx = 0;
    int start = (rec_idx % 8 == 0) ? (rec_idx / 8 - 1) : (rec_idx / 8);
    for (int i = start; i >= 0; i--)
        for (int j = 0; j < 8; j++)
            reversed[idx++] = received[i*8 + j];

    printf("[");        
    for (int i = 0; i < idx; i++) {
        printf("%u", reversed[i]);
        if (i != idx-1) printf(", ");
    }

    printf("]\n");

        // close UART: ----------------------------------
	koniec:
	close(uart0_filestream);
	printf("%s\n", "Kuniec transmisji.");
	return 0;
}
