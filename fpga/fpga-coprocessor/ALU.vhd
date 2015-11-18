library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;


entity ALU is
  port (
    -- CLOCK:
	 clk        : in  std_logic;
	 op1        : in  std_logic_vector(63 downto 0);
	 op2        : in  std_logic_vector(63 downto 0);
	 -- enable     : in  std_logic;
	 sum        : out std_logic_vector(63 downto 0);
	 diffr      : out std_logic_vector(63 downto 0)
  );
end entity ALU;


architecture arch of ALU is
begin

  sum( 7 downto  0)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1( 7 downto  0))) + to_integer(unsigned(op2( 7 downto  0))), 8));
  sum(15 downto  8)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(15 downto  8))) + to_integer(unsigned(op2(15 downto  8))), 8));
  sum(23 downto 16)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(23 downto 16))) + to_integer(unsigned(op2(23 downto 16))), 8));
  sum(31 downto 24)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(31 downto 24))) + to_integer(unsigned(op2(31 downto 24))), 8));
  sum(39 downto 32)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(39 downto 32))) + to_integer(unsigned(op2(39 downto 32))), 8));
  sum(47 downto 40)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(47 downto 40))) + to_integer(unsigned(op2(47 downto 40))), 8));
  sum(55 downto 48)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(55 downto 48))) + to_integer(unsigned(op2(55 downto 48))), 8));
  sum(63 downto 56)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(63 downto 56))) + to_integer(unsigned(op2(63 downto 56))), 8));

  diffr( 7 downto  0) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1( 7 downto  0))) - to_integer(unsigned(op2( 7 downto  0))), 8));
  diffr(15 downto  8) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(15 downto  8))) - to_integer(unsigned(op2(15 downto  8))), 8));
  diffr(23 downto 16) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(23 downto 16))) - to_integer(unsigned(op2(23 downto 16))), 8));
  diffr(31 downto 24) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(31 downto 24))) - to_integer(unsigned(op2(31 downto 24))), 8));
  diffr(39 downto 32) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(39 downto 32))) - to_integer(unsigned(op2(39 downto 32))), 8));
  diffr(47 downto 40) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(47 downto 40))) - to_integer(unsigned(op2(47 downto 40))), 8));
  diffr(55 downto 48) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(55 downto 48))) - to_integer(unsigned(op2(55 downto 48))), 8));
  diffr(63 downto 56) <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(63 downto 56))) - to_integer(unsigned(op2(63 downto 56))), 8));
  
end arch;