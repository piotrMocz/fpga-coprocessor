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
	 
	 component const_mem is
        port (
            clk          : in  std_logic;
         	write_addr   : in  integer range 0 to 63;
	         read_addr    : in  integer range 0 to 7;
	         we           : in  std_logic;
	
	         cmem_byte_in : in  std_logic_vector( 7 downto 0);
	         cmem_out     : out std_logic_vector(63 downto 0)
        );
    end component;
	 
	 component stack is
    port (
        clk       : in  std_logic;
	     enable    : in  std_logic;
	     rst       : in  std_logic;
	     empty     : out std_logic;
	     full      : out std_logic;
	     command   : in  std_logic;
	     push_data : in  std_logic_vector(63 downto 0);
	     pop_data  : out std_logic_vector(63 downto 0)
        );
    end component;
    
    ----------------------------------------------------------------------------
    -- UART signals
    ----------------------------------------------------------------------------
    
	 type state_t is (idle, processing, processing2, reading, sending, sending2, push, push2);
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
	 
	 -- constants memory signals:
	 signal cmem_write_addr          : integer range 0 to 63 := 0;
	 signal cmem_read_addr           : integer range 0 to 7  := 0;
	 signal cmem_we                  : std_logic := '0';
    signal cmem_in                  : std_logic_vector(7  downto 0) := (others => '0');
	 signal cmem_8b_out              : std_logic_vector(63 downto 0) := (others => '0');
	 
	 -- stack memory signals:
	 signal s_enable                 : std_logic;
    signal s_rst                    : std_logic;
    signal s_full                   : std_logic;
    signal s_empty                  : std_logic;
    signal s_command                : std_logic;
    signal s_popd                   : std_logic_vector(63 downto 0);
    signal s_pushd                  : std_logic_vector(63 downto 0);
	 
	 -- misc signals:
	 signal led_vec                  : std_logic_vector(7 downto 0) := (others => '0');
    signal buff                     : std_logic_vector(7 downto 0) := (others => '0');
	 signal consts                   : std_logic := '0';
  
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
	 
	 CMEM_inst1 : const_mem
	 port map (
	         clk           => CLOCK,
				write_addr    => cmem_write_addr,
				read_addr     => cmem_read_addr,
        	   we            => cmem_we,
				cmem_out      => cmem_8b_out,
			   cmem_byte_in  => cmem_in
	 );
	 
    stack_inst : stack
    port map (
       clk       => CLOCK,
 	    enable    => s_enable,
 	    rst       => s_rst,
 	    empty     => s_empty,
 	    full      => s_full,
 	    command   => s_command, -- 0 -> push, 1 -> pop
 	    push_data => s_pushd,
 	    pop_data  => s_popd
   );
	 
	 
	 LEDS    <= led_vec;
	 -- output one cell of instr mem:
	 --led_vec <= imem_out; -- "00" & std_logic_vector(to_unsigned(imem_write_addr, 6));
    
	 ----------------------------------------------------------------------------
    -- Simple loopback, retransmit any received data
    ----------------------------------------------------------------------------
    
    UART_LOOPBACK : process (CLOCK)
    begin
        if rising_edge(CLOCK) then
            if RESET = '1' then
				    loopback_state          <= idle;
					 consts                  <= '0';
                uart_data_in_stb        <= '0';
                uart_data_out_ack       <= '0';
                uart_data_in            <= (others => '0');
					 led_vec                 <= "01010101";
					 buff                    <= (others => '0');
					 imem_we                 <= '0';
					 imem_write_addr         <= 0;
					 imem_read_addr          <= 0;
					 imem_in                 <= (others => '0');
					 
					 cmem_we                 <= '0';
					 cmem_write_addr         <= 0;
					 cmem_read_addr          <= 0;
					 cmem_in                 <= (others => '0');
            else
				
	      case loopback_state is
				
	      when idle =>
             if uart_data_out_stb = '1' then
		           uart_data_out_ack   <= '1';
	   	        buff                <= uart_data_out;
		           if consts = '0' then
			            imem_we         <= '1';
			            imem_in         <= uart_data_out;
		           else
			            cmem_we         <= '1';
			            cmem_in         <= uart_data_out;
		           end if;
		           loopback_state      <= processing;
		       else
		           uart_data_out_ack   <= '0';
             end if;
					 
		  when processing =>
		      uart_data_out_ack      <= '0';
		      if consts = '0' then
			       imem_we            <= '0';
			       imem_write_addr    <= imem_write_addr + 1;
			       if buff = "11111111" then
			           consts         <= '1';
			       end if;
			       loopback_state     <= idle;
		      else
			       cmem_we            <= '0';
			       cmem_write_addr    <= cmem_write_addr + 1;
			       if buff = "11111111" then
			           loopback_state <= processing2;
			           imem_read_addr <= 0;
			       else
			           loopback_state <= idle;
			       end if;
		      end if;
                 
		 when processing2 =>
           --led_vec             <= led_vec(0) & led_vec(7 downto 1);
		     loopback_state      <= reading;
		     s_enable            <= '0';
				    	 
		 when reading =>
		     --led_vec <= imem_out;
		     if imem_out(7 downto 3) = "01000" then  -- "PUSH CASE"
					cmem_read_addr  <= to_integer(unsigned(imem_out(2 downto 0)));
			      loopback_state  <= push;
		     else
			      -- read from memory:
			      s_enable        <= '1';
			      s_command       <= '1';
			      loopback_state  <= sending;
		     end if;
				
		  when push =>
            --led_vec            <= led_vec(0) & led_vec(7 downto 1);
		      loopback_state     <= push2; -- wait for mem-out to be available
				
		  when push2 =>
		      s_enable           <= '1';
            s_command          <= '0';
		      s_pushd            <= cmem_8b_out;
				led_vec            <= cmem_8b_out(7 downto 0);
		      imem_read_addr     <= imem_read_addr + 1;
		      loopback_state     <= processing2;
					 
		 when sending =>
		      loopback_state     <= sending2;
		      uart_data_in       <= led_vec;-- s_popd(7 downto 0);
				-- led_vec            <= s_popd(7 downto 0);
            uart_data_in_stb   <= '1';    -- to musi byc wlaczone tuz przed wyslaniem.

                 -- a moze tu jeszcze jeden stan?
                 -- when mock_state =>
		 --     led_vec <= led_vec(0) & led_vec(7 downto 1);
                 --     loopback_state <= sending2;
                 --  (jesli robimy ten mock_state, to uart_data_in_stb <= '1';  musi byc tu
		
		when sending2 =>
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