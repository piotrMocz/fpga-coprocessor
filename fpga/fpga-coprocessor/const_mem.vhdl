-- memory chunk that should be inferred as ram
-- reads are in 8-byte chunks, whereas writes are byte-per-byte

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity const_mem is
port (
   clk          : in  std_logic;
	write_addr   : in  integer range 0 to 127;
	read_addr    : in  integer range 0 to 15;
	we           : in  std_logic;
	
	cmem_byte_in : in  std_logic_vector( 7 downto 0);
	cmem_out     : out std_logic_vector(63 downto 0)
);
end const_mem;

architecture arch of const_mem is
   
	type mem_t is array(0 to 127) of std_logic_vector(7 downto 0);
   signal ram_block : mem_t := (others => (others => '0'));
	
begin

	ram_ctrl : process(clk)
	begin
	   if rising_edge(clk) then
		   if we = '1' then
			   ram_block(write_addr) <= cmem_byte_in;
			end if;
			
			cmem_out( 7 downto  0) <= ram_block(read_addr * 8);
			cmem_out(15 downto  8) <= ram_block(read_addr * 8 + 1);
			cmem_out(23 downto 16) <= ram_block(read_addr * 8 + 2);
			cmem_out(31 downto 24) <= ram_block(read_addr * 8 + 3);
			cmem_out(39 downto 32) <= ram_block(read_addr * 8 + 4);
			cmem_out(47 downto 40) <= ram_block(read_addr * 8 + 5);
			cmem_out(55 downto 48) <= ram_block(read_addr * 8 + 6);
			cmem_out(63 downto 56) <= ram_block(read_addr * 8 + 7);
		end if;
	end process;

end arch;