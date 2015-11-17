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
	options.c_cflag = B115200 | CS8 | CLOCAL | CREAD;  // baudrate, char size
	options.c_iflag = IGNPAR; // no parity check
	options.c_oflag = 0;
	options.c_lflag = 0;
	
	tcflush(uart0_filestream, TCIFLUSH);
	tcsetattr(uart0_filestream, TCSANOW, &options);

	// transmit some bytes: -------------------------

        unsigned char tosend[64];
        tosend[0] = 66;  
        tosend[1] = 255;  
        tosend[2] = 25;  
        tosend[3] = 49;
        tosend[4] = 50;
        tosend[5] = 51;
        tosend[6] = 52;
        tosend[7] = 53;
        tosend[8] = 54;
        tosend[9] = 55;
        // first chunk:
        tosend[10] = 20;
        tosend[11] = 21;
        tosend[12] = 22;
        tosend[13] = 23;
        tosend[14] = 24;
        tosend[15] = 25;
        tosend[16] = 26;
        tosend[17] = 27;
        // second chunk:
        tosend[18] = 28;
        tosend[19] = 29;
        tosend[20] = 30;
        tosend[21] = 31;
        tosend[22] = 32;
        tosend[23] = 33;
        tosend[24] = 34;
        tosend[25] = 35;
        tosend[26] = 255;
        // third chunk:
        tosend[27] = 38;
        tosend[28] = 39;
        tosend[29] = 40;
        tosend[30] = 41;
        tosend[31] = 42;
        tosend[32] = 43;
        tosend[33] = 44;
        tosend[34] = 45;
        tosend[35] = 255;

	//int cnt = write(uart0_filestream, tx_buffer, p_tx_buffer - tx_buffer); 
	int cnt = write(uart0_filestream, tosend, 27);
        // printf("Wyslano: %s\n", tosend); 
        if (cnt < 0)
	{
		printf("UART TX error\n");
		return 1;
	}

        unsigned char received[32], result[32];
        memset(received, 0, sizeof(received));
        memset(result, 0, sizeof(result));

        for (int i = 0; i < 8; i++) {
            int reclen = read(uart0_filestream, (void *) received,  8);
            if (reclen < 0) printf("Klops 1.");
            result[i] = received[0];
        }

        printf("Received:\n");
        for (int i = 0; i < 8; i++) {
            printf("%c %u\n", result[i], result[i]);
        }
	
        // close UART: ----------------------------------
	koniec:
	close(uart0_filestream);
	printf("%s\n", "Kuniec transmisji.");
	return 0;
}
