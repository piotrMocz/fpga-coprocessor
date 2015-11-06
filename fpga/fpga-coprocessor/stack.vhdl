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
	command   : in  std_logic;
	vector    : in  std_logic;
	push_data : in  std_logic_vector(7 downto 0);
	pop_data  : out std_logic_vector(7 downto 0);
	
	push_vector : in std_logic_vector(63 downto 0);
	pop_vector : out std_logic_vector(63 downto 0);
	
	outp      : out std_logic_vector(7 downto 0)
   );
end stack;

architecture arch of stack is

   type   memory is array(16 downto 0) of std_logic_vector(7 downto 0);
   signal stack_mem : memory    := (others => (others => '0'));
	signal stack_ptr : integer   := 15;
	signal s_empty   : std_logic := '1';
	signal s_full    : std_logic := '0';
	
begin
   outp <= stack_mem(stack_ptr);

   full  <= s_full;
	empty <= s_empty;
	
   stack_handler : process(rst, clk, enable, command, vector) is
	begin
	   if rising_edge(clk) then
		   if enable = '1' and command = '0' and s_full = '0' and vector = '0' then
			   stack_mem(stack_ptr) <= push_data;
				
				if stack_ptr /= 0 then
				   stack_ptr <= stack_ptr - 1;
				end if;
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 15 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				end if;
			end if;
			
			if enable = '1' and command = '1' and s_empty = '0' and vector = '0' then
			   pop_data <= stack_mem(stack_ptr+1);
				
				if stack_ptr < 15 then
				   stack_ptr <= stack_ptr + 1;
				end if;
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 15 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				end if;
			end if;
			
		 if enable = '1' and command = '0' and s_full = '0' and vector = '1' then
			   stack_mem(stack_ptr)   <= push_vector(7 downto 0);
				stack_mem(stack_ptr-1) <= push_vector(15 downto 8);
				stack_mem(stack_ptr-2) <= push_vector(23 downto 16);
				stack_mem(stack_ptr-3) <= push_vector(31 downto 24);
				stack_mem(stack_ptr-4) <= push_vector(39 downto 32);
				stack_mem(stack_ptr-5) <= push_vector(47 downto 40);
				stack_mem(stack_ptr-6) <= push_vector(55 downto 48);
				stack_mem(stack_ptr-7) <= push_vector(63 downto 56);
				
				if stack_ptr /= 0 then
				   stack_ptr <= stack_ptr - 8;
				end if;
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 15 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				end if;
			end if;
			
			if enable = '1' and command = '1' and s_empty = '0' and vector = '1' then
			   pop_vector(63 downto 56) <= stack_mem(stack_ptr+1);
				pop_vector(55 downto 48) <= stack_mem(stack_ptr+2);
				pop_vector(47 downto 40) <= stack_mem(stack_ptr+3);
				pop_vector(39 downto 32) <= stack_mem(stack_ptr+4);
				pop_vector(31 downto 24) <= stack_mem(stack_ptr+5);
				pop_vector(23 downto 16) <= stack_mem(stack_ptr+6);
				pop_vector(15 downto 8)  <= stack_mem(stack_ptr+7);
				pop_vector(7 downto 0)   <= stack_mem(stack_ptr+8);
				
				if stack_ptr < 15 then
				   stack_ptr <= stack_ptr + 8;
				end if;
				
				if stack_ptr = 0 then     -- full stack
				   s_full <= '1'; s_empty <= '0';
				elsif stack_ptr = 15 then -- empty stack
				   s_full <= '0'; s_empty <= '1';
				else
				   s_full <= '0'; s_empty <= '0';
				end if;
			end if;	
		
		end if;
			
	
	end process;
	
	
end arch;