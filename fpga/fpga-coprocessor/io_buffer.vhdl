-- this serves as a buffer for uart-based io
-- that is: we buffer incoming bytes until they make sense
-- (there're 2 or 8 bytes comprising either an instruction or a vector chunk)

library ieee;
use ieee.std_logic_1164.all;

entity io_buffer is
port (
   clk         : in  std_logic;
	enable      : in  std_logic;
	rst         : in  std_logic;
	empty       : out std_logic;
	full        : out std_logic;
	command     : in  std_logic;   -- 0 -> push, 1 -> pop
	push_data   : in  std_logic_vector(7  downto 0);
	pop_data    : out std_logic_vector(7  downto 0);
	
	instr_ready : out std_logic;
	vec_ready   : out std_logic;
	output      : out std_logic_vector(63 downto 0)
);
end io_buffer;

architecture arch of io_buffer is

   type   memory is array(7 downto 0) of std_logic_vector(7 downto 0);
   signal stack_mem : memory    := (others => (others => '0'));
   signal stack_ptr : integer   := 7;
   signal s_empty   : std_logic := '1';
   signal s_full    : std_logic := '0';
	
begin
   
	-- enables copying the whole stack in 1 cycle:
   output(63 downto 56) <= stack_mem(7);
	output(55 downto 48) <= stack_mem(6);
	output(47 downto 40) <= stack_mem(5);
	output(39 downto 32) <= stack_mem(4);
	output(31 downto 24) <= stack_mem(3);
	output(23 downto 16) <= stack_mem(2);
	output(15 downto  8) <= stack_mem(1);
	output( 7 downto  0) <= stack_mem(0);
	
	-- instr_ready <= (stack_ptr = 5);
	-- vec_ready   <= s_full;
	
	-- instr_ready <=> 2 bytes on stack, can pop instruction
	-- vec_ready   <=> 8 bytes on stack, can pop whole chunk
	flag_setter : process(clk) is
	begin
	   if rising_edge(clk) then
		   if stack_ptr = 5 then
			   instr_ready <= '1';
			else
			   instr_ready <= '0';
		   end if;
			
			if stack_ptr = 0 then
			   vec_ready <= '1';
			else
			   vec_ready <= '0';
			end if;
		end if;
	end process;

   full  <= s_full;
	empty <= s_empty;
	
   stack_handler : process(rst, clk, enable, command) is
	begin
	   if rising_edge(clk) then
		   if enable = '1' and command = '0' and s_full = '0' then
			   stack_mem(stack_ptr) <= push_data;
				
				if stack_ptr /= 0 then
				   stack_ptr <= stack_ptr - 1;
				end if;
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 7 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				end if;
			end if;
			
			if enable = '1' and command = '1' and s_empty = '0' then
			   pop_data <= stack_mem(stack_ptr+1);
				
				if stack_ptr < 7 then
				   stack_ptr <= stack_ptr + 1;
				end if;
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 7 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				end if;
			end if;
			
			if rst = '1' then
			   stack_ptr <= 7;
			end if;
		end if;
	end process;
	
	
end arch;