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

type fsm_state_t is (idle, received, emitting);
type state_t is
record
  fsm_state: fsm_state_t; -- FSM state
  tx_data: std_logic_vector(7 downto 0);
  tx_enable: std_logic;
end record;

signal reset: std_logic;
signal reset_btn : std_logic;
signal uart_rx_data: std_logic_vector(7 downto 0);
signal uart_rx_enable: std_logic;
signal uart_tx_data: std_logic_vector(7 downto 0);
signal uart_tx_enable: std_logic;
signal uart_tx_ready: std_logic;

signal state,state_next: state_t;

begin

  reset_btn <= KEY(0);

  basic_uart_inst: basic_uart
  generic map (DIVISOR => 326)
  port map (
    clk => CLOCK_50, reset => reset,
    rx_data => uart_rx_data, rx_enable => uart_rx_enable,
    tx_data => uart_tx_data, tx_enable => uart_tx_enable, tx_ready => uart_tx_ready,
    rx => GPIO(11),
    tx => GPIO( 9)
  );

  reset_control: process (reset_btn) is
  begin
    if reset_btn = '1' then
      reset <= '0';
    else
      reset <= '1';
    end if;
  end process;
  
  -- pmod_1 <= uart_tx_enable;
  -- pmod_2 <= uart_tx_ready;
  
  fsm_clk: process (CLOCK_50,reset) is
  begin
    if reset = '1' then
      state.fsm_state <= idle;
      state.tx_data <= (others => '0');
      state.tx_enable <= '0';
    else
      if rising_edge(CLOCK_50) then
        state <= state_next;
      end if;
    end if;
  end process;

  fsm_next: process (state,uart_rx_enable,uart_rx_data,uart_tx_ready) is
  begin
    state_next <= state;
    case state.fsm_state is
    
    when idle =>
      if uart_rx_enable = '1' then
        state_next.tx_data <= uart_rx_data;
        state_next.tx_enable <= '0';
        state_next.fsm_state <= received;
      end if;
      
    when received =>
      if uart_tx_ready = '1' then
        state_next.tx_enable <= '1';
        state_next.fsm_state <= emitting;
      end if;
      
    when emitting =>
      if uart_tx_ready = '0' then
        state_next.tx_enable <= '0';
        state_next.fsm_state <= idle;
      end if;
      
    end case;
  end process;
  
  fsm_output: process (state) is
  begin
  
    uart_tx_enable <= state.tx_enable;
    uart_tx_data <= state.tx_data;
    led <= state.tx_data;
    
  end process;

end arch;