library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity ALU is
  port (
    -- CLOCK:
	 clk        : in  std_logic;
	 inp1_data  : in  std_logic_vector(7 downto 0);
	 inp2_data  : in  std_logic_vector(7 downto 0);
	 -- enable     : in  std_logic;
	 outp       : out std_logic_vector(7 downto 0)
  );
end entity ALU;


architecture arch of ALU is
begin

--  main_process : process (clk) is
--  begin
--    if rising_edge(clk) then
--	   if enable = '1' then
--		  outp <= inp1_data + inp2_data;
--		end if;
--	 end if;
--  end process;

  outp <= inp1_data + inp2_data;

end arch;