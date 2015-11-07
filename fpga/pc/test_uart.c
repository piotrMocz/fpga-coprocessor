#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h>

int main()
{
	// open UART: -----------------------------------
	int uart0_filestream = -1;

	uart0_filestream = open("/dev/ttyUSB0", O_RDWR | O_NOCTTY);
	if (uart0_filestream == -1)
	{
		printf("Error -- Unable to open UART.\n");
		return 1;
	}

	//printf("Deskryptor: %d\n", uart0_filestream);

	// configure UART: ------------------------------
	struct termios options;
	tcgetattr(uart0_filestream, &options);
	options.c_cflag = B9600 | CS8 | CLOCAL | CREAD;  // baudrate, char size
	options.c_iflag = IGNPAR; // no parity check
	options.c_oflag = 0;
	options.c_lflag = 0;
	
	tcflush(uart0_filestream, TCIFLUSH);
	tcsetattr(uart0_filestream, TCSANOW, &options);

	// transmit some bytes: -------------------------

        unsigned char tosend[32];
        tosend[0] = 45;
        tosend[1] = 46;
        tosend[2] = 47;
        tosend[3] = 255;
        tosend[4] = 0;
	//int cnt = write(uart0_filestream, tx_buffer, p_tx_buffer - tx_buffer); 
	int cnt = write(uart0_filestream, tosend, 32);
        printf("Wyslano: %s\n", tosend); 
        if (cnt < 0)
	{
		printf("UART TX error\n");
		return 1;
	}

	printf("Odebrano:\n");
        unsigned char received[32], result[8];
        memset(received, 0, sizeof(received));

        int reclen = read(uart0_filestream, (void *) received,  2);
        if (reclen < 0) printf("Klops 1.");
        result[0] = received[0];
        //reclen = read(uart0_filestream, (void *) received+1, 1);
        //reclen = read(uart0_filestream, (void *) received+2, 8);
        //printf("Reclen: %d\n", reclen);

        
        //else {
        //    printf("%c %u\n", received[0], received[0]);
        //    printf("%c %u\n", received[1], received[1]);
        //    printf("%c %u\n", received[2], received[2]);
        //}

        reclen = read(uart0_filestream, (void *) received,  2);
        if (reclen < 0) printf("Klops 2.");
        result[1] = received[0];
        //else {
        //    printf("%c %u\n", received[0], received[0]);
        //    printf("%c %u\n", received[1], received[1]);
        //    printf("%c %u\n", received[2], received[2]);
        //}

        reclen = read(uart0_filestream, (void *) received+2,  2);
        if (reclen < 0) printf("Klops 3.");
        result[2] = received[2];        

        printf("Received:\n");
        printf("%c %u\n", result[0], result[0]);
        printf("%c %u\n", result[1], result[1]);
        printf("%c %u\n", result[2], result[2]);

	// close UART: ----------------------------------
	koniec:
	close(uart0_filestream);
	printf("%s\n", "Kuniec transmisji.");
	return 0;
}
