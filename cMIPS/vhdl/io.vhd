--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: print_data
--             print an integer to stdout, 32bit hexadecimal
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity print_data is
  port (rst     : in  std_logic;
        clk     : in  std_logic;
        sel     : in  std_logic;
        rdy     : out std_logic;
        wr      : in  std_logic;
        addr    : in  reg32;
        data    : in  reg32);
end print_data;

architecture behavioral of print_data is

  file output : text open write_mode is "STD_OUTPUT";

begin

  rdy <= '1';

  U_WRITE_OUT: process(sel,clk)
    variable msg : line;
  begin
    if falling_edge(clk) and sel = '0' then
      write ( msg, string'(SLV32HEX(data)) );
      writeline( output, msg );
    end if;
  end process U_WRITE_OUT;

end behavioral;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: to_stdout
--             print a signle character to stdout
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity to_stdout is
  port (rst     : in  std_logic;
        clk     : in  std_logic;
        sel     : in  std_logic;
        rdy     : out std_logic;
        wr      : in  std_logic;
        addr    : in  std_logic_vector;
        data    : in  std_logic_vector);
end to_stdout;

architecture behavioral of to_stdout is
  
  file output : text open write_mode is "STD_OUTPUT";

begin

  rdy <= '1';

  U_WRITE_OUT: process(clk,sel)
    variable msg : line;
  begin
    if falling_edge(clk) and sel = '0' then
      if (data(7 downto 0) = x"00") or (data(7 downto 0) = x"0a") then
        writeline( output, msg );
      else
        write(msg, character'val(to_integer( unsigned(data(7 downto 0)))));
      end if;
    end if;
  end process U_WRITE_OUT;
  
end behavioral;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: write_data_to_file
--             write one 32bit integer to file "output.data"
--   if( addr(3 downto 0) ) = "0000" then write to file
--   if( addr(3 downto 0) ) = "0100" then close file
--   if( addr(3 downto 0) ) = "0111" then assert dump_ram
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity write_data_file is
  generic (OUTPUT_FILE_NAME : string := "output.data");
  port (rst      : in  std_logic;
        clk      : in  std_logic;
        sel      : in  std_logic;
        rdy      : out std_logic;
        wr       : in  std_logic;
        addr     : in  reg32;
        data     : in  reg32;
        byte_sel : in  reg4;
        dump_ram : out std_logic);
end write_data_file;

architecture behavioral of write_data_file is

  type uint_file_type is file of integer;
  file output_file: uint_file_type open write_mode is OUTPUT_FILE_NAME;

begin

  rdy <= '1';

  U_write_uint: process (clk,sel)
  begin

    dump_ram <= '0';

    if  falling_edge(clk) and sel = '0' then
      if addr(3 downto 0) = b"0000" then               -- data write
        if wr = '0' then
          write( output_file, to_integer(signed(data)) );
          -- assert FALSE
          --   report "IOwr[" & SLV32HEX(addr) &"]:"& SLV32HEX(data);
        end if;
      elsif addr(3 downto 0) = b"0100" then            -- close output file
        file_close(output_file);
      elsif addr(3 downto 0) = b"0111" then            -- dump RAM
        dump_ram <= '1';
      end if;
    end if;
    
  end process U_write_uint;

end behavioral;                         

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: read_data_from_file
--             read one 32bit integer from file "input.data"
--  if not EOF then write data to file
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity read_data_file is
  generic (INPUT_FILE_NAME : string := "input.data");
  port (rst      : in  std_logic;
        clk      : in  std_logic;
        sel      : in  std_logic;
        rdy      : out std_logic;
        wr       : in  std_logic;
        addr     : in  reg32;
        data     : out reg32;
        byte_sel : in  reg4);
end read_data_file;

architecture behavioral of read_data_file is

  type uint_file_type is file of integer;
  file input_file: uint_file_type open read_mode is INPUT_FILE_NAME;

  signal status : reg32 := (others => '0');

begin

  rdy <= '1';


  U_read_uint: process(clk,sel)
    variable datum : integer := 0;
    variable value : reg32;                 -- for debugging only
  begin

    data <= (others => 'X');

    if falling_edge(clk) and sel = '0' then
      if addr(3 downto 0) = b"0000" then               -- data read
        if wr = '1' then
          if not endfile(input_file) then
            read( input_file, datum );
            data <= std_logic_vector(to_signed(datum, 32));
            status <= x"00000000";        -- NOT_EndOfFile
            -- value := std_logic_vector(to_signed(datum, 32));   -- DEBUG
            -- assert FALSE
            --   report "IOrd[" & SLV32HEX(addr) &"]:"& SLV32HEX(value);
          else
            status <= x"00000001";        -- EndOfFile
          end if;
        else
          data <= (others => 'X');
        end if;
      else                                -- status read
        if wr = '1' then
          data <= status;
        else
          data <= (others => 'X');
        end if;
      end if;
    end if;
    
  end process U_read_uint;

end behavioral;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: generate interrupt after N clock cycles
--   Generates an interrupt after N cycles, N <= 2**30
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_wires.all;

entity do_interrupt is
  port (rst      : in    std_logic;
        clk      : in    std_logic;     -- clock pulses counted
        sel      : in    std_logic;
        rdy      : out   std_logic;
        wr       : in    std_logic;
        addr     : in    std_logic_vector;
        data_inp : in    std_logic_vector;
        data_out : out   std_logic_vector;
        irq      : out   std_logic);
  constant NUM_BITS : integer := 30;
  subtype c_width is std_logic_vector(NUM_BITS - 1 downto 0);
  constant START_COUNT : c_width := (others => '0');
end do_interrupt;

architecture behavioral of do_interrupt is

  component registerN is
    generic (NUM_BITS: integer);
    port(clk, rst, ld: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic_vector);
  end component registerN;

  component countNup is
    generic (NUM_BITS: integer);
    port(clk, rst, ld, en: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic_vector;
         co:           out std_logic);
  end component countNup;

  component FFD is
    port(clk, rst, set : in std_logic;
         D : in  std_logic;
         Q : out std_logic);
  end component FFD;

  signal Dlimit, Qlimit, Q: c_width;
  signal ld_cnt, ld_reg, en, cnt_en, int_en, equals : std_logic;
  signal i_ena, c_ena : std_logic;
begin

  ld_reg <= wr when sel = '0' else '1';
  ld_cnt <= not ld_reg;
  
  Dlimit <= data_inp(NUM_BITS-1 downto 0);

  U_LIMIT: registerN  generic map (NUM_BITS)
    port map (clk, rst, ld_reg, Dlimit, Qlimit);

  en <= cnt_en and (not equals);

  U_COUNTER: countNup generic map (NUM_BITS)
    port map (clk, rst, ld_cnt, en, START_COUNT, Q, open);

  i_ena <= data_inp(31) when (sel='0' and wr='0') else int_en;
  U_INTERR_EN: FFD port map (clk, rst, '1', i_ena, int_en);

  c_ena <= data_inp(30) when (sel='0' and wr='0') else cnt_en;
  U_COUNT_EN:  FFD port map (clk, rst, '1', c_ena, cnt_en);

  equals <= '1' when (Q = Qlimit(NUM_BITS-1 downto 0)) else '0';
  
  irq <= '1' when (equals = '1' and int_en = '1') else '0';

  data_out <= int_en & cnt_en & Q;

  rdy <= '1';  -- never generates wait states

end behavioral;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: simple UART
--   8 data bits
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_wires.all;

entity simple_uart is
  port (rst     : in    std_logic;
        clk     : in    std_logic;      -- processor clock
        sel     : in    std_logic;
        rdy     : out   std_logic;
        wr      : in    std_logic;
        addr    : in    std_logic;
        data_inp : in   std_logic_vector;
        data_out : out  std_logic_vector;
        txdat   : out   std_logic;      -- serial transmission (output)
        rxdat   : in    std_logic;      -- serial reception (input)
        rts     : out   std_logic;
        cts     : in    std_logic;
        irq     : out   std_logic;      -- interrupt request
        bit_rt  : out   std_logic_vector); -- communication speed; for TB only
end simple_uart;

architecture behavioral of simple_uart is

  component uart_int is
    port(clk, rst: in std_logic;
         s_ctrl, s_stat, s_tx, s_rx: in std_logic;  -- select 4 registers
         d_inp:  in  std_logic_vector;  -- 32 bit input
         d_out:  out std_logic_vector;  -- 32 bit output
         txdat:  out std_logic;         -- serial transmission (output)
         rxdat:  in  std_logic;         -- serial reception (input)
         rts:    out std_logic;
         cts:    in  std_logic;
         interr: out std_logic;         -- interrupt request
         bit_rt: out std_logic_vector); -- communication speed - for TB only
  end component uart_int;
  
  signal s_ctrl, s_stat, s_tx, s_rx: std_logic;
  signal d_inp, d_out : reg32;

begin

  rdy <= '1';

  U_UART: uart_int port map (clk, rst, s_ctrl,s_stat, s_tx,s_rx,
                             d_inp,d_out, txdat,rxdat, rts,cts, irq, bit_rt);
  
  -- a2  wr  register (aligned to word addresses)
  --  0  0  control
  --  0  1  status
  --  1  0  transmission
  --  1  1  reception
  s_ctrl <= '1' when sel = '0' and addr = '0' and wr = '0' else '0';
  s_stat <= '1' when sel = '0' and addr = '0' and wr = '1' else '0';
  s_tx   <= '1' when sel = '0' and addr = '1' and wr = '0' else '0';
  s_rx   <= '1' when sel = '0' and addr = '1' and wr = '1' else '0';
  
  data_out <= d_out ;
  
  d_inp <= data_inp;
  
end behavioral;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: to_7seg
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;
use work.p_wires.all;

entity to_7seg is
  port (rst      : in  std_logic;
        clk      : in  std_logic;
        sel      : in  std_logic;
        rdy      : out std_logic;
        wr       : in  std_logic;
        data     : in  std_logic_vector;
        display0 : out reg8;
        display1 : out reg8);
  constant NUM_BITS : integer := 10;    -- 2 decimal points, 2 hex digits
end to_7seg;

architecture behavioral of to_7seg is

  component registerN is
    generic (NUM_BITS: integer);
    port(clk, rst, ld: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic_vector);
  end component registerN;

  component display_7seg is
    port(data_i      : in  std_logic_vector(3 downto 0);
         decimal_i   : in  std_logic;
         disp_7seg_o : out std_logic_vector(7 downto 0));
  end component display_7seg;
  
  signal value : std_logic_vector(NUM_BITS-1 downto 0);
  
begin
  
  U_HOLD_data: registerN generic map (NUM_BITS)
    port map (clk, rst, sel, data(NUM_BITS-1 downto 0), value);

  U_DSP1: display_7seg port map (value(7 downto 4), value(9), display1);

  U_DSP0: display_7seg port map (value(3 downto 0), value(8), display0);

  rdy <= '1';
  
  U_sim: process(sel,rst)
  begin
    if rst = '1' then
      assert not(rising_edge(sel))
        report "dsp7seg: "&  SLV32HEX(data) severity NOTE;
    end if;
  end process;

end behavioral;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- peripheral: read_keys
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity read_keys is
  generic (DEB_CYCLES: natural);        -- debouncing interval
  port (rst      : in  std_logic;
        clk      : in  std_logic;
        sel      : in  std_logic;
        rdy      : out std_logic;
        data     : out reg32;
        key      : in  std_logic_vector (11 downto 0);
        sw       : in  std_logic_vector (3 downto 0));
  constant DEB_BITS : integer := 16;  -- debounce counter width
  constant CNT_MAX : integer := (2**DEB_BITS - 1);
  constant x_DEB_CYCLES : std_logic_vector(DEB_BITS-1 downto 0)
    := std_logic_vector(to_signed((CNT_MAX - DEB_CYCLES),DEB_BITS));
end read_keys;

architecture behavioral of read_keys is
  
  component FFD is
    port(clk, rst, set : in std_logic;
         D : in  std_logic; Q : out std_logic);
  end component FFD;

  component registerN is
    generic (NUM_BITS: integer);
    port(clk, rst, ld: in  std_logic;
         D:            in  std_logic_vector(NUM_BITS-1 downto 0);
         Q:            out std_logic_vector(NUM_BITS-1 downto 0));
  end component registerN;
  
  component countNup is
  generic (NUM_BITS: integer := 16);
  port(clk, rst, ld, en: in  std_logic;
       D:                in  std_logic_vector((NUM_BITS - 1) downto 0);
       Q:                out std_logic_vector((NUM_BITS - 1) downto 0);
       co:               out std_logic);
  end component countNup;

  type key_state is (st_idle, st_start, st_wait, st_load, st_release);
  signal key_current_st, key_next_st : key_state;
  -- signal key_dbg_st : integer;    -- debugging only
  
  signal cnt_ld, cnt_en, new_ld : std_logic;
  signal press, debounced, rdy_clr, ready : std_logic;
  signal keys_data, cpu_data : reg4;
  signal d : reg2;
  -- signal count : std_logic_vector(DEB_BITS-1 downto 0);  -- debugging only
begin
  
  data(31) <= ready;
  data(30 downto 8) <= (others => '0');
  
  data(7) <= sw(3);
  data(6) <= sw(2);
  data(5) <= sw(1);
  data(4) <= sw(0);
  data(3 downto 0) <= cpu_data(3 downto 0);
  
  rdy <= '1';

  U_DEBOUNCER: countNup generic map (DEB_BITS)
    port map (clk=>clk, rst=>rst, ld=>cnt_ld, en=>cnt_en,
              D=>x_DEB_CYCLES, Q=>open, co=>debounced); 
  
  U_NEW_DATA: registerN  generic map (4)
    port map (clk, rst, new_ld, keys_data, cpu_data);

  d <= new_ld & sel;                    -- new_ld, sel active in '0'
  with d select
    rdy_clr <= '1' when "00",
               '1' when "01",
               '0' when "10",
               ready when others;

  U_READY: FFD port map (clk, rst, '1', rdy_clr, ready);
  
  press <= BOOL2SL(keys_data /= b"0000");
  
  -- translate key position to key code
  -- code for key 0 cannot be zero; value-holding register is reset to "0000"
  with key select
    keys_data <= "0001" when "000000000001",   -- 1
                 "0010" when "000000000010",   -- 2
                 "0011" when "000000000100",   -- 3
                 "0100" when "000000001000",   -- 4
                 "0101" when "000000010000",   -- 5
                 "0110" when "000000100000",   -- 6
                 "0111" when "000001000000",   -- 7
                 "1000" when "000010000000",   -- 8
                 "1001" when "000100000000",   -- 9
                 "1010" when "001000000000",   -- *
                 "1111" when "010000000000",   -- 0, cannot be "0000"
                 "1011" when "100000000000",   -- #
                 "0000" when others; -- no key depressed

 U_Key_st_reg: process(rst,clk)
  begin
    if rst = '0' then
      key_current_st <= st_idle;
    elsif rising_edge(clk) then
      key_current_st <= key_next_st;
    end if;
  end process U_key_st_reg;

  -- key_dbg_st <= integer(key_state'pos(key_current_st)); -- for debugging

  U_key_st_transitions: process(key_current_st, press, debounced) --------
  begin
    case key_current_st is
      when st_idle =>                   -- 0
        if press = '1' then
          key_next_st <= st_start;
        else
          key_next_st <= st_idle;
        end if;
      when st_start =>                  -- 1
        key_next_st <= st_wait;
      when st_wait =>                   -- 2
        if debounced = '1' then
          key_next_st <= st_load;
        else
          key_next_st <= st_wait;
        end if;
      when st_load =>                   -- 3
        key_next_st <= st_release;
      when st_release =>                -- 4
        if press = '1' then
          key_next_st <= st_release;
        else
          key_next_st <= st_idle;
        end if;
    end case;
  end process U_key_st_transitions;   

  U_key_outputs: process(key_current_st) 
  begin
    case key_current_st is
      when st_idle  |st_release =>      -- 0,4
        new_ld  <= '1';
        cnt_ld  <= '0';
        cnt_en  <= '0';
      when st_start =>                  -- 1
        new_ld  <= '1';
        cnt_ld  <= '1';
        cnt_en  <= '0';
      when st_wait =>                   -- 2
        new_ld  <= '1';
        cnt_ld  <= '0';
        cnt_en  <= '1';
      when st_load =>                   -- 3
        new_ld  <= '0';
        cnt_ld  <= '1';
        cnt_en  <= '0';
    end case;
  end process U_key_outputs;   

  
end behavioral;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- LCD display controller
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_wires.all;

entity LCD_display is
  port (rst      : in    std_logic;
        clk      : in    std_logic;
        sel      : in    std_logic;
        rdy      : out   std_logic;
        wr       : in    std_logic;
        addr     : in    std_logic; -- 0=constrol, 1=data
        data_inp : in    std_logic_vector(31 downto 0);
        data_out : out   std_logic_vector(31 downto 0);
        LCD_DATA : inout std_logic_vector(7 downto 0);  -- bidirectional bus
        LCD_RS   : out   std_logic; -- LCD register select 0=ctrl, 1=data
        LCD_RW   : out   std_logic; -- LCD read=1, 0=write
        LCD_EN   : out   std_logic; -- LCD enable=1
        LCD_BLON : out   std_logic);
  constant NUM_BITS : integer := 8;
end LCD_display;

architecture behavioral of LCD_display is

  component wait_states is
    generic (NUM_WAIT_STATES :integer);
    port(rst   : in  std_logic;
       clk     : in  std_logic;
       sel     : in  std_logic;         -- active in '0'
       waiting : out std_logic);        -- active in '1'
  end component wait_states;
  
  component registerN is
    generic (NUM_BITS: integer);
    port(clk, rst, ld: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic_vector);
  end component registerN;

  component FFD is
    port(clk, rst, set, D : in std_logic; Q : out std_logic);
  end component FFD;

  type lcd_state is (st_init, st_idle, st_n, st_n1, st_n2, st_n3,
                     st_n4, st_n5, st_n6, st_n7, st_n8, st_n9, st_na, st_nb);
  attribute SYN_ENCODING of lcd_state : type is "safe";
  signal lcd_current_st, lcd_next_st : lcd_state;
  signal lcd_current : integer;         -- debugging only

  signal waiting, wait1, wait2, n_sel: std_logic;
  signal sel_rs, RS, sel_rw, RW,lcd_enable,lcd_read : std_logic;
  signal inp_data, out_data : reg8;
  
begin

  n_sel <= not(sel);
  
  U_WAIT_ON_READS: component wait_states generic map (1)
    port map (rst, clk, sel, wait1);

  U_WAIT2: FFD port map (clk, rst, '1', wait1, wait2);

  rdy <= not(wait1) and not(wait2) and waiting;

  sel_rs <= addr when sel = '0' else RS;
  U_INPUT_RS: FFD port map (clk, rst, '1', sel_rs, RS);

  U_INPUT: registerN generic map (NUM_BITS)
  port map (clk, rst, sel, data_inp(NUM_BITS-1 downto 0), inp_data);

  U_OUTPUT: registerN generic map (NUM_BITS)
  port map (clk, rst, lcd_read, out_data, data_out(NUM_BITS-1 downto 0));
  data_out(31 downto NUM_BITS) <= (others => '0');

  -- state register----------------------------------------------------
  U_st_reg: process(rst,clk)
  begin
    if rst = '0' then
      lcd_current_st <= st_init;
    elsif rising_edge(clk) then
      lcd_current_st <= lcd_next_st;
    end if;
  end process U_st_reg;

  lcd_current <= lcd_state'pos(lcd_current_st);  -- debugging only

  U_st_transitions: process(lcd_current_st, RW, sel)
  begin
    case lcd_current_st is
      when st_init =>                   -- 0
        lcd_next_st <= st_idle;

      when st_idle =>                   -- 1
        if sel = '0' then
          lcd_next_st <= st_n;
        else
          lcd_next_st <= st_idle;
        end if;

      when st_n =>                      -- 2
        lcd_next_st <= st_n1;
      when st_n1 =>                     -- 3, setup for Enable is 20ns
        lcd_next_st <= st_n2;

      when st_n2 =>                     -- 4, keep Enable=1 for 200ns
        lcd_next_st <= st_n3;
      when st_n3 =>                     -- 5, data setup is 100ns
        lcd_next_st <= st_n4;
      when st_n4 =>                     -- 6
        lcd_next_st <= st_n5;
      when st_n5 =>                     -- 7
        lcd_next_st <= st_n6;
      when st_n6 =>                     -- 8
        lcd_next_st <= st_n7;
      when st_n7 =>                     -- 9
        lcd_next_st <= st_n8;

      when st_n8 =>                     -- 10
        lcd_next_st <= st_n9;
      when st_n9 =>                     -- 11, data hold for Enable is >40ns
        lcd_next_st <= st_na;
      when st_na =>                     -- 12
        lcd_next_st <= st_nb;
      when st_nb =>                     -- 13
        lcd_next_st <= st_idle;
        
      when others =>                    -- ??
        lcd_next_st <= st_idle;         --  Enable cycle >500ns
    end case;
  end process U_st_transitions;

  U_st_outputs: process(lcd_current_st)
  begin
    case lcd_current_st is
      when st_init =>        
        lcd_enable <= '0';              -- disable
        lcd_read   <= '1';
        waiting    <= '1';

      when st_idle =>
        lcd_enable <= '0';              -- disable
        lcd_read   <= '1';
        waiting    <= '1';

      when st_n | st_n1 =>
        lcd_enable <= '0';              -- disable, waiting for setup
        lcd_read   <= '1';
        waiting    <= '0';

      when st_n2 | st_n3 | st_n4 | st_n5 | st_n6 | st_n7 =>
        lcd_enable <= '1';              -- enable, waiting
        lcd_read   <= '1';
        waiting    <= '0';
        
      when st_n8 =>
        lcd_enable <= '1';              -- enable, still waiting
        lcd_read   <= '0';
        waiting    <= '0';

      when st_n9 =>
        lcd_enable <= '1';              -- enable, still waiting
        lcd_read   <= '1';
        waiting    <= '0';

      when st_na =>
        lcd_enable <= '0';              -- disable, stop waiting
        lcd_read   <= '1';              --  hold inp data for 40ns
        waiting    <= '0';

      when st_nb =>
        lcd_enable <= '0';              -- disable, stop waiting
        lcd_read   <= '1';              --  hold inp data for 40ns
        waiting    <= '1';

      when others =>
        lcd_enable <= '0';              -- disable
        waiting    <= '1';
    end case;
  end process U_st_outputs;

end behavioral;