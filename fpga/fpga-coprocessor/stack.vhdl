
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity stack is
port (
   clk       : in  std_logic;
	enable    : in  std_logic;
	rst       : in  std_logic;
	empty     : out std_logic;
	full      : out std_logic;
	command   : in  std_logic;   -- 0 -> push, 1 -> pop
	push_data : in  std_logic_vector(63 downto 0);
	pop_data  : out std_logic_vector(63 downto 0);
	
	outp      : out std_logic_vector( 7 downto 0)
   );
end stack;

architecture arch of stack is

   type   memory is array(16 downto 0) of std_logic_vector(63 downto 0);
   signal stack_mem : memory    := (others => (others => '0'));
	signal stack_ptr : integer   := 15;
	signal s_empty   : std_logic := '1';
	signal s_full    : std_logic := '0';
	signal s_outp    : std_logic_vector(7 downto 0);
	
begin
   full  <= s_full;
	empty <= s_empty;
	outp  <= s_outp;
	
   stack_handler : process(rst, clk) is
	begin
	   if rst = '1' then
	      s_empty   <= '1';
			s_full    <= '0';
			stack_ptr <= 15;
	   elsif rising_edge(clk) then
	      if enable = '1' and command = '0' and stack_ptr /= 0 then
			   stack_mem(stack_ptr) <= push_data;
				
				if stack_ptr = 1 then     -- full stack
				   s_full <= '1';
				else
				   s_full <= '0';
				end if;
				
				s_empty   <= '0';
				stack_ptr <= stack_ptr - 1;
			end if;
			
			if enable = '1' and command = '1' and stack_ptr /= 15 then
				pop_data <= stack_mem(stack_ptr+1);
				
				if stack_ptr = 15 then -- empty stack
				   s_empty <= '1';
				else
				   s_empty <= '0';
				end if;
				
				s_full    <= '0';
				stack_ptr <= stack_ptr + 1;
			end if;
			
			s_outp <= std_logic_vector(to_unsigned(stack_ptr, 8));
				 
		end if;
	end process;
	
	
end arch;