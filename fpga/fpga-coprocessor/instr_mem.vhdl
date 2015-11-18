-- instruction memory
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity instr_mem is
port (
   clk           : in  std_logic;
	read_addr     : in  integer range 0 to 63;
	write_addr    : in  integer range 0 to 63;
	we            : in  std_logic;
	
	instr_mem_in  : in  std_logic_vector(7 downto 0);
	instr_mem_out : out std_logic_vector(7 downto 0)
);
end instr_mem;

architecture arch of instr_mem is
   
	type instr_mem_t is array(0 to 63) of std_logic_vector(7 downto 0);
   signal ram_block  : instr_mem_t := (others => (others => '0'));
	--signal write_addr : integer range 0 to 63 := 0;
	
begin

	ram_ctrl : process(clk)
	begin
	   if rising_edge(clk) then
		   if we = '1' then
			   ram_block(write_addr) <= instr_mem_in;
			end if;
			
			instr_mem_out <= ram_block(read_addr);
		end if;
	end process;

end arch;