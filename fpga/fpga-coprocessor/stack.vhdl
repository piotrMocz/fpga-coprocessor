-- this code is based on the sample empty-descending stack from vhdlguru

library ieee;
use ieee.std_logic_1164.all;

entity stack is
port (
   clk       : in  std_logic;
	enable    : in  std_logic;
	rst       : in  std_logic;
	empty     : out std_logic;
	full      : out std_logic;
	command   : in  std_logic;   -- 0 -> push, 1 -> pop
	push_data : in  std_logic_vector(7 downto 0);
	pop_data  : out std_logic_vector(7 downto 0)
   );
end stack;

architecture arch of stack is

   type   memory is array(16 downto 0) of std_logic_vector(7 downto 0);
   signal stack_mem : memory    := (others => (others => '0'));
	signal stack_ptr : integer   := 15;
	signal s_empty   : std_logic := '1';
	signal s_full    : std_logic := '0';
	
begin

   full  <= s_full;
	empty <= s_empty;
	
   stack_handler : process(rst, clk, enable, command) is
	begin
	   if rising_edge(clk) then
		   if enable = '1' and command = '0' and s_full = '0' then
			   stack_mem(stack_ptr) <= push_data;
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 15 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				   stack_ptr <= stack_ptr - 1;
				end if;
			end if;
			
			if enable = '1' and command = '1' and s_empty = '0' then
			   pop_data <= stack_mem(stack_ptr);
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 15 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				   stack_ptr <= stack_ptr + 1;
				end if;
			end if;
		end if;
	end process;
	
	
end arch;