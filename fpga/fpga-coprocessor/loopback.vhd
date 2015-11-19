--------------------------------------------------------------------------------
-- Entity that sends back the data it received, but saving them to memory
-- on the fly.
-- Based on: UART Simple loopback by Peter A Bennett (see reference in uart.vhd)
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity LOOPBACK is
    port 
    (  
        -- General
        CLOCK                   :   in      std_logic;
        RESET                   :   in      std_logic;    
        RX                      :   in      std_logic;
        TX                      :   out     std_logic;
		  LEDS                    :   out     std_logic_vector(7 downto 0)
    );
end LOOPBACK;

architecture RTL of LOOPBACK is
    ----------------------------------------------------------------------------
    -- UART constants
    ----------------------------------------------------------------------------
    
    constant BAUD_RATE              : positive := 115200;
    constant CLOCK_FREQUENCY        : positive := 50000000;
    
    ----------------------------------------------------------------------------
    -- Component declarations
    ----------------------------------------------------------------------------
    component UART is
        generic (
                BAUD_RATE           : positive;
                CLOCK_FREQUENCY     : positive
            );
        port (  -- General
                CLOCK               :   in      std_logic;
                RESET               :   in      std_logic;    
                DATA_STREAM_IN      :   in      std_logic_vector(7 downto 0);
                DATA_STREAM_IN_STB  :   in      std_logic;
                DATA_STREAM_IN_ACK  :   out     std_logic;
                DATA_STREAM_OUT     :   out     std_logic_vector(7 downto 0);
                DATA_STREAM_OUT_STB :   out     std_logic;
                DATA_STREAM_OUT_ACK :   in      std_logic;
                TX                  :   out     std_logic;
                RX                  :   in      std_logic
             );
    end component UART;
	 
	 component instr_mem is
        port (
            clk           : in  std_logic;
        	   read_addr     : in  integer range 0 to 63;
        	   write_addr    : in  integer range 0 to 63;
        	   we            : in  std_logic;
        	
        	   instr_mem_in  : in  std_logic_vector(7 downto 0);
        	   instr_mem_out : out std_logic_vector(7 downto 0)
        );
    end component;
	 
	 component ram_mem is
        port (
            clk          : in  std_logic;
        	   read_addr    : in  integer range 0 to 15;
        	   write_addr   : in  integer range 0 to 15;
        	   we           : in  std_logic;
        	
        	   ram_in       : in  std_logic_vector(63 downto 0);
        	   ram_out      : out std_logic_vector(63 downto 0)
        );
    end component;
	 
	 component const_mem is
        port (
            clk          : in  std_logic;
         	write_addr   : in  integer range 0 to 63;
	         read_addr    : in  integer range 0 to 7;
	         we           : in  std_logic;
	
	         cmem_byte_in : in  std_logic_vector( 7 downto 0);
	         cmem_out     : out std_logic_vector(63 downto 0)
        );
    end component;
	 
	 component stack is
    port (
        clk       : in  std_logic;
	     enable    : in  std_logic;
	     rst       : in  std_logic;
	     empty     : out std_logic;
	     full      : out std_logic;
	     command   : in  std_logic;
	     push_data : in  std_logic_vector(63 downto 0);
	     pop_data  : out std_logic_vector(63 downto 0);
		  outp      : out std_logic_vector(7 downto 0)
        );
    end component;
	 
	 component ALU is
    port (
        -- CLOCK:
	     clk        : in  std_logic;
	     op1        : in  std_logic_vector(63 downto 0);
	     op2        : in  std_logic_vector(63 downto 0);
	     sum        : out std_logic_vector(63 downto 0);
	     diffr      : out std_logic_vector(63 downto 0)
    );
    end component;

    
    ----------------------------------------------------------------------------
    -- UART signals
    ----------------------------------------------------------------------------
    
	 type state_t is (idle, processing, processing2, reading, sending, sending2, push, push2, mock_state, pop_stack, vr1moving, vr1copying, vr2moving, vr2copying, vr_adding, vr_adding_inter, vr_adding2, vr_adding_fin, storing, storing2, loading, loading2, subbing, subbing_pre1, subbing_pre2, subbing_pre3, subbing_pre4, checking_zero1, checking_zero2);
	 signal loopback_state               : state_t := idle;
	 
	 -- uart signals:
    signal uart_data_in             : std_logic_vector(7 downto 0);
    signal uart_data_out            : std_logic_vector(7 downto 0);
    signal uart_data_in_stb         : std_logic := '0';
    signal uart_data_in_ack         : std_logic;
    signal uart_data_out_stb        : std_logic;
    signal uart_data_out_ack        : std_logic := '0';
	 
	 -- instruction memory signals:
	 signal imem_read_addr           : integer range 0 to 63 := 0;
    signal imem_write_addr          : integer range 0 to 63 := 0;
    signal imem_we                  : std_logic := '0';
    signal imem_in                  : std_logic_vector(7 downto 0) := (others => '0');
    signal imem_out                 : std_logic_vector(7 downto 0);
	 
	 -- ram memory signals:
	 signal ram_read_addr           : integer range 0 to 15 := 0;
    signal ram_write_addr          : integer range 0 to 15 := 0;
    signal ram_we                  : std_logic := '0';
    signal ram_vec_in              : std_logic_vector(63 downto 0) := (others => '0');
    signal ram_vec_out             : std_logic_vector(63 downto 0);
	 
	 -- constants memory signals:
	 signal cmem_write_addr          : integer range 0 to 63 := 0;
	 signal cmem_read_addr           : integer range 0 to  7 := 0;
	 signal cmem_we                  : std_logic := '0';
    signal cmem_in                  : std_logic_vector( 7 downto 0) := (others => '0');
	 signal cmem_8b_out              : std_logic_vector(63 downto 0) := (others => '0');
	 
	 -- stack memory signals:
	 signal s_enable                 : std_logic := '0';
    signal s_rst                    : std_logic := '0';
    signal s_full                   : std_logic;
    signal s_empty                  : std_logic;
    signal s_command                : std_logic := '0';
    signal s_popd                   : std_logic_vector(63 downto 0);
    signal s_pushd                  : std_logic_vector(63 downto 0) := (others => '0');
	 
	 -- vector register 1 signals:
	 signal vr1_enable                 : std_logic := '0';
    signal vr1_rst                    : std_logic := '0';
    signal vr1_full                   : std_logic;
    signal vr1_empty                  : std_logic;
    signal vr1_command                : std_logic := '0';
    signal vr1_popd                   : std_logic_vector(63 downto 0);
    signal vr1_pushd                  : std_logic_vector(63 downto 0) := (others => '0');
	 
	 -- vector register 2 signals:
	 signal vr2_enable                 : std_logic := '0';
    signal vr2_rst                    : std_logic := '0';
    signal vr2_full                   : std_logic;
    signal vr2_empty                  : std_logic;
    signal vr2_command                : std_logic := '0';
    signal vr2_popd                   : std_logic_vector(63 downto 0);
    signal vr2_pushd                  : std_logic_vector(63 downto 0) := (others => '0');
	 
	 -- misc signals:
	 signal led_vec                    : std_logic_vector( 7 downto 0) := (others => '0');
    signal buff                       : std_logic_vector( 7 downto 0) := (others => '0');
	 signal consts                     : std_logic := '0';
	 signal send_ctr                   : integer   := 8;
	 signal send_buff                  : std_logic_vector(63 downto 0) := (others => '0');
	 
	 signal stack_outp                 : std_logic_vector(7 downto 0);
	 signal vr1_outp                   : std_logic_vector(7 downto 0);
	 signal vr2_outp                   : std_logic_vector(7 downto 0);
	 
	 signal vr_sum                     : std_logic_vector(63 downto 0);
	 signal vr_diffr                   : std_logic_vector(63 downto 0);
	 signal alu_op1                    : std_logic_vector(63 downto 0) := (others => '0');
	 signal alu_op2                    : std_logic_vector(63 downto 0) := (others => '0');
  
begin

    ----------------------------------------------------------------------------
    -- UART instantiation
    ----------------------------------------------------------------------------

    UART_inst1 : UART
    generic map (
            BAUD_RATE           => BAUD_RATE,
            CLOCK_FREQUENCY     => CLOCK_FREQUENCY
    )
    port map    (  
            -- General
            CLOCK               => CLOCK,
            RESET               => RESET,
            DATA_STREAM_IN      => uart_data_in,
            DATA_STREAM_IN_STB  => uart_data_in_stb,
            DATA_STREAM_IN_ACK  => uart_data_in_ack,
            DATA_STREAM_OUT     => uart_data_out,
            DATA_STREAM_OUT_STB => uart_data_out_stb,
            DATA_STREAM_OUT_ACK => uart_data_out_ack,
            TX                  => TX,
            RX                  => RX
    );
	 
	 IMEM_inst1 : instr_mem
	 port map (
	         clk           => CLOCK,
				read_addr     => imem_read_addr,
        	   write_addr    => imem_write_addr,
        	   we            => imem_we,
        	
        	   instr_mem_in  => imem_in,
        	   instr_mem_out => imem_out
	 );
	 
	 CMEM_inst1 : const_mem
	 port map (
	         clk           => CLOCK,
				write_addr    => cmem_write_addr,
				read_addr     => cmem_read_addr,
        	   we            => cmem_we,
				cmem_out      => cmem_8b_out,
			   cmem_byte_in  => cmem_in
	 );
	 
	 RMEM : ram_mem
	 port map (
	         clk           => CLOCK,
				read_addr     => ram_read_addr,
        	   write_addr    => ram_write_addr,
        	   we            => ram_we,
        	
        	   ram_in        => ram_vec_in,
        	   ram_out       => ram_vec_out
	 );
	 
    stack_inst : stack
    port map (
       clk       => CLOCK,
 	    enable    => s_enable,
 	    rst       => s_rst,
 	    empty     => s_empty,
 	    full      => s_full,
 	    command   => s_command, -- 0 -> push, 1 -> pop
 	    push_data => s_pushd,
 	    pop_data  => s_popd,
		 outp      => stack_outp
   );
	
	 vreg1 : stack
    port map (
       clk       => CLOCK,
 	    enable    => vr1_enable,
 	    rst       => vr1_rst,
 	    empty     => vr1_empty,
 	    full      => vr1_full,
 	    command   => vr1_command, -- 0 -> push, 1 -> pop
 	    push_data => vr1_pushd,
 	    pop_data  => vr1_popd,
		 outp      => vr1_outp
   );
	
    vreg2 : stack
    port map (
       clk       => CLOCK,
 	    enable    => vr2_enable,
 	    rst       => vr2_rst,
 	    empty     => vr2_empty,
 	    full      => vr2_full,
 	    command   => vr2_command, -- 0 -> push, 1 -> pop
 	    push_data => vr2_pushd,
 	    pop_data  => vr2_popd,
		 outp      => vr2_outp
    );
	
    alu_inst : ALU
    port map (
        clk      => CLOCK,
	     op1      => alu_op1,
	     op2      => alu_op2,
	     sum      => vr_sum,
	     diffr    => vr_diffr
    );
	 
	 
	 LEDS    <= led_vec;
	 led_vec <= "0000000" & s_empty; -- vr2_outp;
	 
	 -- output one cell of instr mem:
	 --led_vec <= imem_out; -- "00" & std_logic_vector(to_unsigned(imem_write_addr, 6));
    
	 ----------------------------------------------------------------------------
    -- Simple loopback, retransmit any received data
    ----------------------------------------------------------------------------
    
    UART_LOOPBACK : process (CLOCK)
    begin
        if rising_edge(CLOCK) then
            if RESET = '1' then
				    loopback_state          <= idle;
					 consts                  <= '0';
                uart_data_in_stb        <= '0';
                uart_data_out_ack       <= '0';
                uart_data_in            <= (others => '0');
					 buff                    <= (others => '0');
					 send_ctr                <= 8;
					 send_buff               <= (others => '0');
					 -- led_vec                 <= (others => '0');
					 
					 s_enable                <= '0';
					 s_command               <= '0';
					 s_pushd                 <= (others => '0');
					 
					 imem_we                 <= '0';
					 imem_write_addr         <= 0;
					 imem_read_addr          <= 0;
					 imem_in                 <= (others => '0');
					 
					 ram_we                  <= '0';
					 ram_write_addr          <= 0;
					 ram_read_addr           <= 0;
					 ram_vec_in              <= (others => '0');
					 
					 cmem_we                 <= '0';
					 cmem_write_addr         <= 0;
					 cmem_read_addr          <= 0;
					 cmem_in                 <= (others => '0');
					 
					 alu_op1                 <= (others => '0');
					 alu_op2                 <= (others => '0');
            else
				
	      case loopback_state is
				
	      when idle =>
             if uart_data_out_stb = '1' then
		           uart_data_out_ack   <= '1';
	   	        buff                <= uart_data_out;
		           if consts = '0' then
			            imem_we         <= '1';
			            imem_in         <= uart_data_out;
		           else
			            cmem_we         <= '1';
			            cmem_in         <= uart_data_out;
		           end if;
		           loopback_state      <= processing;
		       else
		           uart_data_out_ack   <= '0';
             end if;
					 
		  when processing =>
		      uart_data_out_ack      <= '0';
		      if consts = '0' then
			       imem_we            <= '0';
			       imem_write_addr    <= imem_write_addr + 1;
			       if buff = "11111111" then
			           consts         <= '1';
			       end if;
			       loopback_state     <= idle;
		      else
			       cmem_we            <= '0';
			       cmem_write_addr    <= cmem_write_addr + 1;
			       if buff = "11111111" then
			           loopback_state <= processing2;
			           imem_read_addr <= 0;
			       else
			           loopback_state <= idle;
			       end if;
		      end if;
                 
		 when processing2 =>
		     loopback_state      <= reading;
		     s_enable            <= '0';
			  vr1_enable          <= '0';
			  vr2_enable          <= '0';
			  ram_we              <= '0';
				    	 
		 when reading =>
		     if imem_out(7 downto 3) = "01000" then  -- "PUSH [const]"
					cmem_read_addr  <= to_integer(unsigned(imem_out(2 downto 0)));
			      loopback_state  <= push;
		     elsif imem_out = "01011000" then        -- MOVS1
					s_enable        <= '1';
					s_command       <= '1';
					loopback_state  <= vr1moving;
			  elsif imem_out = "01011001" then        -- MOVS2
			      s_enable        <= '1';
					s_command       <= '1';
					loopback_state  <= vr2moving;
			  elsif imem_out = "01110100" then        -- ADDS
			      -- pop from both vr1 and vr2:
					vr1_enable      <= '1';
					vr1_command     <= '1';
					vr2_enable      <= '1';
					vr2_command     <= '1';
					loopback_state  <= vr_adding_inter;
					
			  elsif imem_out = "01110110" then          -- SUB
			      s_enable        <= '1';
					s_command       <= '1'; -- (pop)
					loopback_state  <= subbing_pre1;		
					
			  elsif imem_out(7 downto 3) = "01010" then -- STORE
			      s_enable        <= '1';
					s_command       <= '1';
					ram_write_addr  <= to_integer(unsigned(imem_out(2 downto 0)));
					loopback_state  <= storing;
			  elsif imem_out(7 downto 3) = "01001" then  -- LOAD
			      ram_read_addr   <= to_integer(unsigned(imem_out(2 downto 0)));
					loopback_state  <= loading;
			  
			  elsif imem_out = "11111111" then           -- STOP
			      -- read from memory:
			      s_enable        <= '1';
			      s_command       <= '1';
			      loopback_state  <= sending; 
			 
			  elsif imem_out(7 downto 6) = "10" then   -- JUMP
			      imem_read_addr  <= to_integer(unsigned(imem_out(5 downto 0)));
					loopback_state  <= processing2;
			  
			  elsif imem_out(7 downto 6) = "11" then   -- JUMPZ
			      s_enable        <= '1';
					s_command       <= '1';
					loopback_state  <= checking_zero1;
			  
			  elsif imem_out(7 downto 0) = "01100000" then -- LAB
               imem_read_addr  <= imem_read_addr + 1;
               loopback_state  <= processing2;	
					
			  else                                         -- STOP
			      -- read from memory:
			      s_enable        <= '1';
			      s_command       <= '1';
			      loopback_state  <= sending;
		     end if;
			  
		  ---- JUMPZ -----------------------------------------------------------------------
		  when checking_zero1 =>
		      s_enable           <= '0';
				loopback_state     <= checking_zero2;
				
		  when checking_zero2 =>
				if s_popd(63 downto 56) = "00000000" then
                imem_read_addr <= to_integer(unsigned(imem_out(5 downto 0))); -- do the jump
				else
                imem_read_addr <= imem_read_addr + 1;  -- no jump
				end if;
            loopback_state     <= processing2;
			  
		  ---- SUBTRACTION -----------------------------------------------------------------  
		  when subbing_pre1 =>
		      s_enable           <= '0';
				loopback_state     <= subbing_pre2;
		  
		  when subbing_pre2 =>
		      alu_op2            <= s_popd;
				s_enable           <= '1';
				s_command          <= '1'; -- (pop the next one)
				loopback_state     <= subbing_pre3;
		
		  when subbing_pre3 =>
		      s_enable           <= '0';
				loopback_state     <= subbing_pre4;
				
		  when subbing_pre4 =>
		      alu_op1            <= s_popd;
			   loopback_state     <= subbing;
				
		  when subbing =>
		      s_enable           <= '1';
				s_command          <= '0'; -- push the result
            s_pushd            <= vr_diffr;
				imem_read_addr     <= imem_read_addr + 1;
				loopback_state     <= processing2;	  
		  -----------------------------------------------------------------	  
			  
		  when vr_adding_inter =>
				vr1_enable         <= '0';
				vr2_enable         <= '0';
		      loopback_state     <= vr_adding;
				
		  when vr_adding =>
		      alu_op1            <= vr1_popd;
				alu_op2            <= vr2_popd;
				loopback_state     <= vr_adding2;
				
		  when vr_adding2 =>
		      -- push the result to the main stack:
				s_enable           <= '1';
				s_command          <= '0';
				s_pushd            <= vr_sum;
				
				imem_read_addr     <= imem_read_addr + 1;
				loopback_state     <= vr_adding_fin;
			
		  when vr_adding_fin =>
		      s_enable           <= '0';
				loopback_state     <= processing2;
			  
		  when vr2moving =>
		      s_enable           <= '0';
				loopback_state     <= vr2copying;
				
		  when vr2copying =>
		      -- led_vec            <= s_popd(7 downto 0);
				vr2_enable         <= '1';
				vr2_command        <= '0';
		      vr2_pushd          <= s_popd;
				imem_read_addr     <= imem_read_addr + 1;
		      loopback_state     <= processing2;		
				
		  when vr1moving =>
		      s_enable           <= '0';
				loopback_state     <= vr1copying;
				
		  when vr1copying =>	
		  		vr1_enable         <= '1';
				vr1_command        <= '0';
		      vr1_pushd          <= s_popd;
				imem_read_addr     <= imem_read_addr + 1;
		      loopback_state     <= processing2;
				
		  when storing =>
		      s_enable           <= '0';
            loopback_state     <= storing2;
		  
		  when storing2 =>
		      ram_vec_in         <= s_popd;
		      ram_we             <= '1';
				loopback_state     <= processing2;
				imem_read_addr     <= imem_read_addr + 1;
		  
        when loading =>
            loopback_state     <= loading2;

		  when loading2 =>
		      s_enable           <= '1';
				s_command          <= '0';
				s_pushd            <= ram_vec_out;
				imem_read_addr     <= imem_read_addr + 1;
				loopback_state     <= processing2;
		  when push =>
		      loopback_state     <= push2; -- wait for mem-out to be available
				
		  when push2 =>
		      s_enable           <= '1';
            s_command          <= '0';
		      s_pushd            <= cmem_8b_out;
		      imem_read_addr     <= imem_read_addr + 1;
		      loopback_state     <= processing2;
					 
		 when sending =>
		      s_enable           <= '0';
		      loopback_state     <= pop_stack;
				
		 when pop_stack =>
		      send_buff          <= s_popd;
				loopback_state     <= mock_state;

       when mock_state =>
		      --led_vec <= led_vec(0) & led_vec(7 downto 1);
            uart_data_in       <= send_buff(7 downto 0);
				-- rotate send_buff:
				send_buff          <= "00000000" & send_buff(63 downto 8);
				send_ctr           <= send_ctr - 1;
				
				loopback_state     <= sending2;
            uart_data_in_stb   <= '1'; 
		
		when sending2 =>
          -- Clear transmission request strobe upon acknowledge.
          if uart_data_in_ack = '1' then
			     uart_data_in_stb    <= '0';
				  if send_ctr = 0 then			  
			         if s_empty = '1' then
						    loopback_state  <= idle;
						else
						    s_enable        <= '1';
							 s_command       <= '1';
						    loopback_state  <= sending;
						end if;
						send_ctr        <= 8;
				  else
				      loopback_state  <= mock_state;
				  end if;
          end if;
					 
	   end case;
					 
      end if;
     end if;
    end process;
            
end RTL;