-- this is a memory chunk for storing the program's constants

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity const_mem is
port (
   clk        : in  std_logic;
	write_addr : in  std_logic_vector(3 downto 0); -- integer range 0 to 15;
	read_addr  : in  std_logic_vector(3 downto 0); -- integer range 0 to 15;
	we         : in  std_logic;
	
	cmem_in    : in  std_logic_vector(63 downto 0);
	cmem_out   : out std_logic_vector(63 downto 0)
);
end const_mem;

architecture arch of const_mem is
   
	type mem is array(0 to 15) of std_logic_vector(63 downto 0);
   signal ram_block : mem;
	
begin

	ram_ctrl : process(clk)
	begin
	   if rising_edge(clk) then
		   if we = '1' then
			   ram_block(to_integer(unsigned(write_addr))) <= cmem_in;
			end if;
			
			cmem_out <= ram_block(to_integer(unsigned(read_addr)));
		end if;
	end process;

end arch;