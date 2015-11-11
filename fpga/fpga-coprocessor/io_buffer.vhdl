-- this serves as a buffer for uart-based io
-- that is: we buffer incoming bytes until they make sense
-- (there're 2 or 8 bytes comprising either an instruction or a vector chunk)

library ieee;
use ieee.std_logic_1164.all;

entity io_buffer is
port (
   clk          : in  std_logic;
	enable       : in  std_logic;
	rst          : in  std_logic;
	empty        : out std_logic;
	full         : out std_logic;
	command      : in  std_logic;   -- 0 -> push, 1 -> pop
	push_data    : in  std_logic_vector(7  downto 0);
	pop_data     : out std_logic_vector(7  downto 0);
	
	vec_ready    : out std_logic;
	input_enable : in  std_logic;
	input        : in  std_logic_vector(63 downto 0);
	output       : out std_logic_vector(63 downto 0)
);
end io_buffer;

architecture arch of io_buffer is

   type   memory is array(8 downto 1) of std_logic_vector(7 downto 0);
   signal stack_mem : memory    := (others => (others => '0'));
   signal stack_ptr : integer   := 8;
   signal s_empty   : std_logic := '1';
   signal s_full    : std_logic := '0';
	
begin
   
	-- enables copying the whole stack in 1 cycle:
   output(63 downto 56) <= stack_mem(8);
	output(55 downto 48) <= stack_mem(7);
	output(47 downto 40) <= stack_mem(6);
	output(39 downto 32) <= stack_mem(5);
	output(31 downto 24) <= stack_mem(4);
	output(23 downto 16) <= stack_mem(3);
	output(15 downto  8) <= stack_mem(2);
	output( 7 downto  0) <= stack_mem(1);
	
	vec_ready <= s_full;
   full      <= s_full;
	empty     <= s_empty;
	
   iobuff_handler : process(rst, clk) is
	begin
	   if rising_edge(clk) then
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
			
			if enable = '1' and command = '1' and stack_ptr /= 8 then
				pop_data <= stack_mem(stack_ptr+1);
				
				if stack_ptr = 7 then -- empty stack
				   s_empty <= '1';
				else
				   s_empty <= '0';
				end if;
				
				s_full    <= '0';
				stack_ptr <= stack_ptr + 1;
			end if;
			
			if input_enable = '1' then
			   stack_mem(8) <= input(63 downto 56);
	         stack_mem(7) <= input(55 downto 48);
	         stack_mem(6) <= input(47 downto 40);
	         stack_mem(5) <= input(39 downto 32);
	         stack_mem(4) <= input(31 downto 24);
	         stack_mem(3) <= input(23 downto 16);
	         stack_mem(2) <= input(15 downto  8);
	         stack_mem(1) <= input( 7 downto  0);
				
				s_full    <= '1';
				s_empty   <= '0';
				stack_ptr <= 0;
			end if;
			
			if rst = '1' then
			   stack_ptr <= 8;
				s_full    <= '0';
				s_empty   <= '1';
			end if;
		end if;
	end process;
	
	
end arch;