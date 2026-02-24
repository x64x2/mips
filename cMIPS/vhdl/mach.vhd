library ieee;
use ieee.std_logic_1164.all;

entity display_7seg is
port(
  data_i      : in  std_logic_vector(3 downto 0);
  decimal_i   : in  std_logic;
  disp_7seg_o : out std_logic_vector(7 downto 0));
end display_7seg;

architecture behavior of display_7seg is
  signal seg7 : std_logic_vector(6 downto 0);
begin
  
  with data_i select
    seg7 <= "0111111" when x"0",  -- 0
            "0000110" when x"1",  -- 1
            "1011011" when x"2",  -- 2
            "1001111" when x"3",  -- 3
            "1100110" when x"4",  -- 4
            "1101101" when x"5",  -- 5
            "1111101" when x"6",  -- 6
            "0000111" when x"7",  -- 7
	    "1111111" when x"8",  -- 8
            "1100111" when x"9",  -- 9
            "1110111" when x"A",  -- A
            "1111100" when x"B",  -- B
            "0111001" when x"C",  -- C
            "1011110" when x"D",  -- D
            "1111001" when x"E",  -- E
            "1110001" when x"F",  -- F
            "0000000" when others;
  disp_7seg_o <= decimal_i & seg7;
  
end behavior;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity base is
  generic (DEBOUNCE  : integer := 5); -- 1000;
  port(clk_i         : in std_logic;
       push_button_i : in std_logic_vector (11 downto 0);
       key_o         : out std_logic_vector (3 downto 0);
       ready_o       : out std_logic);
end teclado_base;

architecture behavior of base is
  signal key_int : std_logic_vector (3 downto 0);
begin

  with push_button_i select
    key_int <= "0001" when "000000000001",   -- 1
               "0010" when "000000000010",   -- 2
               "0011" when "000000000100",   -- 3
               "0100" when "000000001000",   -- 4
               "0101" when "000000010000",   -- 5
               "0110" when "000000100000",   -- 6
               "0111" when "000001000000",   -- 7
               "1000" when "000010000000",   -- 8
               "1001" when "000100000000",   -- 9
               "1010" when "001000000000",   -- *
               "1100" when "010000000000",   -- 0, cannot be "0000"
               "1011" when "100000000000",   -- #
               "1111" when others; 
  
  -- code for key 0 cannot be zero; value-holding register is reset to "0000"
  
U_DEBOUNCE: process (clk_i)
    variable count : integer range 1 to DEBOUNCE := 1;
  begin
    if rising_edge(clk_i) then
      ready_o <= '0';
      if count < DEBOUNCE then
        count := count + 1;
      elsif key_int /= "1111" then
        count := 1;
        ready_o <= '1';
        key_o <= key_int;
      end if;
      if count < DEBOUNCE and key_int = "1111" then
        count := 1;
      end if;
    end if;
  end process;

end behavior;

library ieee;
use ieee.std_logic_1164.all;

entity reset_sync is
  port(i_clk            : in std_logic;
       i_external_reset : in std_logic;
       o_reset_n        : out std_logic;
       o_reset          : out std_logic);
end reset_sync;

architecture rtl of reset_sync is
  
  component FFD is
    port(clk, rst, set, D : in std_logic; Q : out std_logic);
  end component FFD;
  signal q1,q1_n : std_logic;
begin  -- rtl 

  U1: FFD port map (i_clk, i_external_reset, '1', '1', q1);

  U2: FFD port map (i_clk, i_external_reset, '1', q1, o_reset);

  q1_n <= not(q1);
  
  U3: FFD port map (i_clk, '1', i_external_reset, q1_n, o_reset_n);

end rtl;
