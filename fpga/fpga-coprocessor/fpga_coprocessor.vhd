-- based on code from bealto.com

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity fpga_coprocessor is
  port (
   -- CLOCK:
	CLOCK_50   : in    std_logic;
	-- LEDS:
	LED        : out   std_logic_vector(7  downto 0);
	-- KEYS:
	KEY        : in    std_logic_vector(1  downto 0);
	-- SWITCHES:
	SW         : in    std_logic_vector(3  downto 0);
	-- SDRAM: 
	DRAM_ADDR  : out   std_logic_vector(12 downto 0);
	DRAM_BA    : out   std_logic_vector(1  downto 0);
	DRAM_CAS_N : out   std_logic;
	DRAM_CKE   : out   std_logic;
	DRAM_CLK   : out   std_logic;
	DRAM_CS_N  : out   std_logic;
	DRAM_DQ    : inout std_logic_vector(15 downto 0);
	DRAM_DQM   : out   std_logic_vector(1  downto 0);
	DRAM_RAS_N : out   std_logic;
	DRAM_WE_N  : out   std_logic;
	-- EPCS:
	EPCS_ASDO  : out   std_logic;
	EPCS_DATA0 : in    std_logic;
	EPCS_DCLK  : out   std_logic;
	EPCS_NCSO  : out   std_logic;
	-- EEPROM:
	I2C_SCLK   : out   std_logic;
	I2C_SDAT   : inout std_logic;
	-- ADC:
	ADC_CS_N   : out   std_logic;
	ADC_SADDR  : out   std_logic;
	ADC_SCLK   : out   std_logic;
	ADC_SDAT   : in    std_logic;
	-- 2x13 GPIO Header:
	GPIO_2     : inout std_logic_vector(12 downto 0);
	GPIO_2_IN  : in    std_logic_vector(2  downto 0);
	-- GPIO_0, GPIO_0 connect to GPIO Default:
	GPIO       : inout std_logic_vector(33 downto 0);
	GPIO_IN    : in    std_logic_vector(1  downto 0)
	);
	
end entity fpga_coprocessor;


architecture arch of fpga_coprocessor is

    
component basic_uart is
generic (
  DIVISOR: natural
);
port (
  clk: in std_logic;   -- system clock
  reset: in std_logic;
  
  -- Client interface
  rx_data: out std_logic_vector(7 downto 0);  -- received byte
  rx_enable: out std_logic;  -- validates received byte (1 system clock spike)
  tx_data: in std_logic_vector(7 downto 0);  -- byte to send
  tx_enable: in std_logic;  -- validates byte to send if tx_ready is '1'
  tx_ready: out std_logic;  -- if '1', we can send a new byte, otherwise we won't take it
  
  -- Physical interface
  rx: in std_logic;
  tx: out std_logic
);
end component;


component ALU is
port (
    clk        : in  std_logic;
	 inp1_data  : in  std_logic_vector(7 downto 0);
	 inp2_data  : in  std_logic_vector(7 downto 0);
	 -- enable     : in  std_logic;
	 outp       : out std_logic_vector(7 downto 0)
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
	vector    : in  std_logic;
	push_data : in  std_logic_vector(7 downto 0);
	pop_data  : out std_logic_vector(7 downto 0);
	push_vector : in std_logic_vector(63 downto 0);
	pop_vector : out std_logic_vector(63 downto 0)
   );
end component;

type fsm_state_t is (idle, received, push_val, push_finished, reading, readval, emitting, readlength, pushvector); --, received2, reading2, readval2, emitting2);
type state_t is
record
  fsm_state : fsm_state_t; -- FSM state
  tx_data   : std_logic_vector(7 downto 0);
  tx_enable : std_logic;
end record;

signal reset: std_logic;
signal reset_btn : std_logic;
signal uart_rx_data: std_logic_vector(7 downto 0);
signal uart_rx_enable: std_logic;
signal uart_tx_data: std_logic_vector(7 downto 0);
signal uart_tx_enable: std_logic;
signal uart_tx_ready: std_logic;

signal state,state_next: state_t;

signal alu_inp1   : std_logic_vector( 7 downto 0);
signal alu_inp2   : std_logic_vector( 7 downto 0);
signal alu_outp   : std_logic_vector( 7 downto 0);

signal stack_enable  : std_logic;
signal stack_rst     : std_logic;
signal stack_full    : std_logic;
signal stack_empty   : std_logic;
signal stack_command : std_logic;
signal stack_popd    : std_logic_vector(7 downto 0);
signal stack_pushd   : std_logic_vector(7 downto 0);
signal stack_vector  : std_logic;
signal stack_pushv   : std_logic_vector(63 downto 0);
signal stack_popv    : std_logic_vector(63 downto 0);

signal data_enable  : std_logic;
signal data_rst     : std_logic;
signal data_full    : std_logic;
signal data_empty   : std_logic;
signal data_command : std_logic;
signal data_popd    : std_logic_vector(7 downto 0);
signal data_pushd   : std_logic_vector(7 downto 0);
signal data_vector  : std_logic;
signal data_pushv   : std_logic_vector(63 downto 0);
signal data_popv    : std_logic_vector(63 downto 0);

begin

   reset_btn <= KEY(0);
 
   basic_uart_inst: basic_uart
   generic map (DIVISOR => 326)
   port map (
      clk       => CLOCK_50,
		reset     => reset,
      rx_data   => uart_rx_data,
		rx_enable => uart_rx_enable,
      tx_data   => uart_tx_data,
		tx_enable => uart_tx_enable,
		tx_ready  => uart_tx_ready,
      rx        => GPIO(11),
      tx        => GPIO( 9)
   );
   
   ALU_inst : ALU
   port map (
      clk       => CLOCK_50,
 	   inp1_data => alu_inp1,
 	   inp2_data => alu_inp2,
 	   outp      => alu_outp
   );
   
   stack_inst : stack
   port map (
      clk       => CLOCK_50,
 	   enable    => stack_enable,
 	   rst       => stack_rst,
 	   empty     => stack_empty,
 	   full      => stack_full,
 	   command   => stack_command, -- 0 -> push, 1 -> pop
		vector    => stack_vector,
 	   push_data => stack_pushd,
 	   pop_data  => stack_popd,
		push_vector => stack_pushv,
		pop_vector  => stack_popv
   );
	
	stack_data : stack
	port map (
	   clk       => CLOCK_50,
 	   enable    => data_enable,
 	   rst       => data_rst,
 	   empty     => data_empty,
 	   full      => data_full,
 	   command   => data_command, -- 0 -> push, 1 -> pop
		vector    => data_vector,
 	   push_data => data_pushd,
 	   pop_data  => data_popd,
		push_vector => data_pushv,
		pop_vector  => data_popv
	);
  
  
   reset_control: process (reset_btn) is
   begin
      if reset_btn = '1' then
         reset <= '0';
      else
         reset <= '1';
      end if;
   end process;
  
   fsm_clk: process (CLOCK_50,reset) is
   begin
      if reset = '1' then
         state.fsm_state <= idle;
         state.tx_enable <= '0';
	 	   state.tx_data   <= (others => '0');
      else
         if rising_edge(CLOCK_50) then
            state <= state_next;
         end if;
      end if;
   end process;
 

   fsm_next: process (CLOCK_50) is
   begin
      if rising_edge(CLOCK_50) then
   
      state_next <= state;
      case state.fsm_state is
     
      when idle =>
         if uart_rx_enable = '1' then
 		      state_next.tx_enable <= '0';
            -- finish transmission if we received a stopping byte
				if uart_rx_data = "11111111" then
				   stack_enable         <= '0';
					state_next.fsm_state <= push_finished;
				else
					stack_enable         <= '1';
					state_next.fsm_state <= push_val;
				end if;
				
				-- save the value to stack if it's enabled:
 		      stack_command <= '0'; -- push
 		      stack_pushd   <= uart_rx_data;
         end if;
			
		when push_val =>
		   stack_enable         <= '0';
		   state_next.fsm_state <= idle;
			
		when push_finished =>
		   stack_enable         <= '0';
			state_next.fsm_state <= received;
		 
      when received =>
         state_next.tx_enable <= '0';
 		   state_next.fsm_state <= reading;
 	
 		   stack_enable         <= '1';
 		   stack_command        <= '1'; -- pop
			stack_vector         <= '0';
			
 	   when reading =>
			case stack_popd is
				when "00010010" =>
					state_next.fsm_state <= readlength;
				when others =>
					 state_next.tx_enable <= '0';
 	             state_next.tx_data   <= stack_popd; --state.liczba1 + state.liczba2;
 	             state_next.fsm_state <= readval;	
 		          stack_enable         <= '0';
			end case;
		 
		when readlength =>
		     state_next.tx_data <= stack_popd;
			  stack_vector <= '1';
			  stack_command <= '1';
			  state_next.fsm_state <= pushvector;
			  data_enable <= '1';
			  data_command <= '0';
			  data_vector <= '1';
		
		when pushvector =>
			state_next.fsm_state <= readval;
			data_pushv <= stack_popv;
			state_next.fsm_state <= readval;
			stack_vector <= '0';
			state_next.tx_enable <= '1';
			data_command <= '1';
		 
		 when readval =>
         if uart_tx_ready = '1' then
            state_next.tx_enable <= '1';
            state_next.fsm_state <= emitting;
         end if;
       
      when emitting =>
		   if uart_tx_ready = '0' then
				state_next.tx_enable <= '0';
				state_next.fsm_state <= emitting;
			else
            if stack_empty = '1' then
				   state_next.fsm_state <= idle;
				else
				   state_next.tx_data <= data_popv(63 downto 56);
				end if;
		  end if;
       
     end case;
 	 
 	 end if;
   end process;
  
   fsm_output: process (CLOCK_50) is
   begin
      if rising_edge(CLOCK_50) then
         uart_tx_enable  <= state.tx_enable;
         uart_tx_data    <= state.tx_data;
         LED             <= uart_rx_data;
      end if;	 
   end process;

end arch;