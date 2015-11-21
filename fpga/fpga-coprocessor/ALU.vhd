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
	 diffr      : out std_logic_vector(63 downto 0);
    prod       : out std_logic_vector(63 downto 0);
    division   : out std_logic_vector(63 downto 0);
	 modulo     : out std_logic_vector(63 downto 0);
    scalarProd : out std_logic_vector(63 downto 0)
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

  prod( 7 downto  0)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1( 7 downto  0))) * to_integer(unsigned(op2( 7 downto  0))), 8));
  prod(15 downto  8)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(15 downto  8))) * to_integer(unsigned(op2(15 downto  8))), 8));
  prod(23 downto 16)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(23 downto 16))) * to_integer(unsigned(op2(23 downto 16))), 8));
  prod(31 downto 24)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(31 downto 24))) * to_integer(unsigned(op2(31 downto 24))), 8));
  prod(39 downto 32)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(39 downto 32))) * to_integer(unsigned(op2(39 downto 32))), 8));
  prod(47 downto 40)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(47 downto 40))) * to_integer(unsigned(op2(47 downto 40))), 8));
  prod(55 downto 48)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(55 downto 48))) * to_integer(unsigned(op2(55 downto 48))), 8));
  prod(63 downto 56)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(63 downto 56))) * to_integer(unsigned(op2(63 downto 56))), 8));

  division( 7 downto  0)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1( 7 downto  0))) / to_integer(unsigned(op2( 7 downto  0))), 8));
  division(15 downto  8)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(15 downto  8))) / to_integer(unsigned(op2(15 downto  8))), 8));
  division(23 downto 16)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(23 downto 16))) / to_integer(unsigned(op2(23 downto 16))), 8));
  division(31 downto 24)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(31 downto 24))) / to_integer(unsigned(op2(31 downto 24))), 8));
  division(39 downto 32)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(39 downto 32))) / to_integer(unsigned(op2(39 downto 32))), 8));
  division(47 downto 40)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(47 downto 40))) / to_integer(unsigned(op2(47 downto 40))), 8));
  division(55 downto 48)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(55 downto 48))) / to_integer(unsigned(op2(55 downto 48))), 8));
  division(63 downto 56)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(63 downto 56))) / to_integer(unsigned(op2(63 downto 56))), 8));

  modulo( 7 downto  0)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1( 7 downto  0))) mod to_integer(unsigned(op2( 7 downto  0))), 8));
  modulo(15 downto  8)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(15 downto  8))) mod to_integer(unsigned(op2(15 downto  8))), 8));
  modulo(23 downto 16)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(23 downto 16))) mod to_integer(unsigned(op2(23 downto 16))), 8));
  modulo(31 downto 24)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(31 downto 24))) mod to_integer(unsigned(op2(31 downto 24))), 8));
  modulo(39 downto 32)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(39 downto 32))) mod to_integer(unsigned(op2(39 downto 32))), 8));
  modulo(47 downto 40)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(47 downto 40))) mod to_integer(unsigned(op2(47 downto 40))), 8));
  modulo(55 downto 48)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(55 downto 48))) mod to_integer(unsigned(op2(55 downto 48))), 8));
  modulo(63 downto 56)   <= std_logic_vector(to_unsigned(to_integer(unsigned(op1(63 downto 56))) mod to_integer(unsigned(op2(63 downto 56))), 8));

  scalarProd(63 downto 8)<= (others => '0');
  scalarProd(7  downto 0) <= std_logic_vector(to_unsigned(
       to_integer(unsigned(op1( 7 downto  0))) * to_integer(unsigned(op2( 7 downto  0)))
       + to_integer(unsigned(op1(15 downto  8))) * to_integer(unsigned(op2(15 downto  8)))
       + to_integer(unsigned(op1(23 downto 16))) * to_integer(unsigned(op2(23 downto 16)))
       + to_integer(unsigned(op1(31 downto 24))) * to_integer(unsigned(op2(31 downto 24)))
       + to_integer(unsigned(op1(39 downto 32))) * to_integer(unsigned(op2(39 downto 32)))
       + to_integer(unsigned(op1(47 downto 40))) * to_integer(unsigned(op2(47 downto 40)))
       + to_integer(unsigned(op1(55 downto 48))) * to_integer(unsigned(op2(55 downto 48)))
       + to_integer(unsigned(op1(63 downto 56))) * to_integer(unsigned(op2(63 downto 56)))
  ,8));
end arch;
