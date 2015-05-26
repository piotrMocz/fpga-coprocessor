library IEEE;
use IEEE.std_logic_1164.all;

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
begin

	LED(0) <= KEY(0); 
	LED(1) <= KEY(1);

end architecture arch;
