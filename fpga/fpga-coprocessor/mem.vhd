-- memory chunk that should be inferred as ram
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mem is
port (
   clk        : in  std_logic;
	write_addr : in  integer range 0 to 15; -- std_logic_vector(3 downto 0);
	read_addr  : in  integer range 0 to 15; -- std_logic_vector(3 downto 0);
	we         : in  std_logic;
	
	mem_in    : in  std_logic_vector(63 downto 0);
	mem_out   : out std_logic_vector(63 downto 0)
);
end mem;

architecture arch of mem is
   
	type mem_t is array(0 to 15) of std_logic_vector(63 downto 0);
   signal ram_block : mem_t;
	
begin

	ram_ctrl : process(clk)
	begin
	   if rising_edge(clk) then
		   if we = '1' then
			   ram_block(write_addr) <= mem_in;
			end if;
			
			mem_out <= ram_block(read_addr);
		end if;
	end process;

end arch;