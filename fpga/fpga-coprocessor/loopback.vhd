--------------------------------------------------------------------------------
-- Entity that sends back the data it received, but saving them to memory
-- on the fly.
-- Based on: UART Simple loopback by Peter A Bennett (see reference in uart.vhd)
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity LOOPBACK is
    port 
    (  
        -- General
        CLOCK                   :   in      std_logic;
        RESET                   :   in      std_logic;    
        RX                      :   in      std_logic;
        TX                      :   out     std_logic;
		  LEDS                    :   out     std_logic_vector(7 downto 0)
    );
end LOOPBACK;

architecture RTL of LOOPBACK is
    ----------------------------------------------------------------------------
    -- UART constants
    ----------------------------------------------------------------------------
    
    constant BAUD_RATE              : positive := 115200;
    constant CLOCK_FREQUENCY        : positive := 50000000;
    
    ----------------------------------------------------------------------------
    -- Component declarations
    ----------------------------------------------------------------------------
    component UART is
        generic (
                BAUD_RATE           : positive;
                CLOCK_FREQUENCY     : positive
            );
        port (  -- General
                CLOCK               :   in      std_logic;
                RESET               :   in      std_logic;    
                DATA_STREAM_IN      :   in      std_logic_vector(7 downto 0);
                DATA_STREAM_IN_STB  :   in      std_logic;
                DATA_STREAM_IN_ACK  :   out     std_logic;
                DATA_STREAM_OUT     :   out     std_logic_vector(7 downto 0);
                DATA_STREAM_OUT_STB :   out     std_logic;
                DATA_STREAM_OUT_ACK :   in      std_logic;
                TX                  :   out     std_logic;
                RX                  :   in      std_logic
             );
    end component UART;
	 
	 component instr_mem is
        port (
            clk           : in  std_logic;
        	   read_addr     : in  integer range 0 to 63;
        	   write_addr    : in  integer range 0 to 63;
        	   we            : in  std_logic;
        	
        	   instr_mem_in  : in  std_logic_vector(7 downto 0);
        	   instr_mem_out : out std_logic_vector(7 downto 0)
        );
    end component;
    
    ----------------------------------------------------------------------------
    -- UART signals
    ----------------------------------------------------------------------------
    
	 type state_t is (idle, processing, processing2, reading, reading2, sending);
	 signal loopback_state               : state_t := idle;
	 
	 -- uart signals:
    signal uart_data_in             : std_logic_vector(7 downto 0);
    signal uart_data_out            : std_logic_vector(7 downto 0);
    signal uart_data_in_stb         : std_logic := '0';
    signal uart_data_in_ack         : std_logic;
    signal uart_data_out_stb        : std_logic;
    signal uart_data_out_ack        : std_logic := '0';
	 
	 -- instruction memory signals:
	 signal imem_read_addr           : integer range 0 to 63 := 0;
    signal imem_write_addr          : integer range 0 to 63 := 0;
    signal imem_we                  : std_logic := '0';
    signal imem_in                  : std_logic_vector(7 downto 0) := (others => '0');
    signal imem_out                 : std_logic_vector(7 downto 0);
	 
	 -- misc signals:
	 signal led_vec                  : std_logic_vector(7 downto 0) := (others => '0');
    signal buff                     : std_logic_vector(7 downto 0) := (others => '0');
  
begin

    ----------------------------------------------------------------------------
    -- UART instantiation
    ----------------------------------------------------------------------------

    UART_inst1 : UART
    generic map (
            BAUD_RATE           => BAUD_RATE,
            CLOCK_FREQUENCY     => CLOCK_FREQUENCY
    )
    port map    (  
            -- General
            CLOCK               => CLOCK,
            RESET               => RESET,
            DATA_STREAM_IN      => uart_data_in,
            DATA_STREAM_IN_STB  => uart_data_in_stb,
            DATA_STREAM_IN_ACK  => uart_data_in_ack,
            DATA_STREAM_OUT     => uart_data_out,
            DATA_STREAM_OUT_STB => uart_data_out_stb,
            DATA_STREAM_OUT_ACK => uart_data_out_ack,
            TX                  => TX,
            RX                  => RX
    );
	 
	 IMEM_inst1 : instr_mem
	 port map (
	         clk           => CLOCK,
				read_addr     => imem_read_addr,
        	   write_addr    => imem_write_addr,
        	   we            => imem_we,
        	
        	   instr_mem_in  => imem_in,
        	   instr_mem_out => imem_out
	 );
	 
	 
	 LEDS    <= led_vec;
	 -- output one cell of instr mem:
	 led_vec <= imem_out;
    
	 ----------------------------------------------------------------------------
    -- Simple loopback, retransmit any received data
    ----------------------------------------------------------------------------
    
    UART_LOOPBACK : process (CLOCK)
    begin
        if rising_edge(CLOCK) then
            if RESET = '1' then
				    loopback_state          <= idle;
                uart_data_in_stb        <= '0';
                uart_data_out_ack       <= '0';
                uart_data_in            <= (others => '0');
					 -- led_vec                 <= (others => '0');
					 buff                    <= (others => '0');
					 imem_we                 <= '0';
					 imem_write_addr         <= 0;
					 imem_read_addr          <= 0;
					 imem_in                 <= (others => '0');
            else
				
				case loopback_state is
				
				when idle =>
                uart_data_out_ack       <= '0';
                if uart_data_out_stb = '1' then
                    buff                <= uart_data_out;
						  imem_we             <= '1';
						  imem_in             <= uart_data_out;
						  
						  loopback_state      <= processing;
                end if;
					 
				when processing =>
				    uart_data_out_ack       <= '1';
				    -- output what we've received:
					 -- uart_data_in            <= buff;
					 -- save data to memory:
					 imem_we                 <= '0';
					 -- next state:
					 loopback_state          <= processing2;
                
					 
				when processing2 =>
				    imem_write_addr         <= imem_write_addr + 1;
					 -- we're not interested in what's being received right now:
					 uart_data_out_ack       <= '0';
					 
					 imem_read_addr          <= 0;
					 -- next state:
					 loopback_state          <= reading;
					 
				when reading =>
				    -- read from memory:
				    uart_data_in            <= imem_out;
					 loopback_state          <= reading2;
				
				when reading2 =>
					 uart_data_in_stb        <= '1';
					 -- start sending:
					 loopback_state          <= sending;
					 
				when sending =>
                -- Clear transmission request strobe upon acknowledge.
                if uart_data_in_ack = '1' then
                    uart_data_in_stb    <= '0';
						  
						  loopback_state      <= idle;
                end if;
					 
				end case;
					 
            end if;
        end if;
    end process;
            
end RTL;
