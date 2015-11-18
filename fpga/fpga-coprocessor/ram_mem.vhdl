-- ram memory
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram_mem is
port (
   clk           : in  std_logic;
	read_addr     : in  integer range 0 to 15;
	write_addr    : in  integer range 0 to 15;
	we            : in  std_logic;
	
	ram_in        : in  std_logic_vector(63 downto 0);
	ram_out       : out std_logic_vector(63 downto 0)
);
end ram_mem;

architecture arch of ram_mem is
   
	type ram_mem_t is array(0 to 15) of std_logic_vector(63 downto 0);
   signal ram_block  : ram_mem_t := (others => (others => '0'));
	--signal write_addr : integer range 0 to 63 := 0;
	
begin

	ram_ctrl : process(clk)
	begin
	   if rising_edge(clk) then
		   if we = '1' then
			   ram_block(write_addr) <= ram_in;
				
--				if write_addr < 63 then
--				   write_addr            <= write_addr + 1;
--				end if;
			end if;
			
			ram_out <= ram_block(read_addr);
		end if;
	end process;

end arch;