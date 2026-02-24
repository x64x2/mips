-- control register, least significant byte only
-- b0..b2: transmit/receive clock speed
--         000: 1/4 CPU clock rate -- for VHDL/C debugging only
--         001: 115.200 baud
--         010:  57.600 baud
--         011:  38.400 baud
--         100:  28.800 baud
--         101:  19.200 baud
--         110:  14.400 baud
--         111:   9.600 baud
-- b3=1:   signal interupt on RX buffer full, when a new octet is available
-- b4=1:   signal interupt on TX buffer empty, when TX space is available
-- b5,b6:  ignored, not used
-- b7=1:   turn on Request to Send (RTS)
-- 
-- status register, least significant byte only
-- b0: overurn error, last octet received overwrote previous in buffer
-- b1: framing error, last octet was not framed by START, STOP bits
-- b2: not used, returns zero
-- b3: interupt pending on RX buffer full
-- b4: interupt pending on TX buffer empty
-- b5: receive buffer is full
-- b6: transmit buffer is empty
-- b7: Clear to Send (CTS) is active
-- when CPU reads status register bits 0,1,3,4 are cleared
--
-- RX and TX circuits are dobule-buffered

library ieee; use ieee.std_logic_1164.all;
use work.p_WIRES.all;

entity uart_int is
  port(clk, rst: in std_logic;
       s_ctrl, s_stat, s_tx, s_rx: in std_logic;  -- select registers
       d_inp:  in  reg32;               -- input
       d_out:  out reg32;               -- output
       txdat:  out std_logic;           -- interface: serial transmission
       rxdat:  in  std_logic;           -- interface: serial reception
       rts   : out std_logic;           -- interface: request to send
       cts   : in  std_logic;           -- interface: clear to send
       inter: out std_logic;           -- interupt request
       bit_rt: out reg3);               -- communication speed, for debugging
end uart_int;

architecture estrutural of uart_int is

  component register8 is
    port(rel, rst, ld: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic_vector);
  end component register8;

  component par_ser10 is
    port(rel, rst, ld, desl: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic);
  end component par_ser10;

  component ser_par10 is
    port(rel, rst, desl: in  std_logic;
         D:            in  std_logic;
         Q:            out std_logic_vector);
  end component ser_par10;

  component FFD is
    port(clk, rst, set, D : in std_logic; Q : out std_logic);
  end component FFD;

  -- state machine for transmission-CPU interface
  type txcpu_state is (st_idle, st_check, st_done);
  signal txcpu_current_st, txcpu_next_st : txcpu_state;
  
  -- state machine for transmission circuit
  type tx_state is (st_idle, st_check, st_start,
                    st_b0, st_b1, st_b2, st_b3, st_b4, st_b5, st_b6, st_b7,
                    st_stop, st_done);
  signal tx_current_st, tx_next_st : tx_state;


  -- state machine for reception-CPU interface
  type rxcpu_state is (st_idle, st_copy, st_check, st_error);
  signal rxcpu_current_st, rxcpu_next_st : rxcpu_state;
  
  -- state machine for reception circuit
  type rx_state is (st_idle, st_check, st_start,
                    st_b0, st_b1, st_b2, st_b3, st_b4, st_b5, st_b6, st_b7,
                    st_stop, st_done);
  signal rx_current_st, rx_next_st : rx_state;

  -- for debugging only
  signal tx_dbg_st, txcpu_dbg_st, rx_dbg_st, rxcpu_dbg_st : integer; 

  signal ctrl, status, txreg, rxreg, received : reg8;
  signal tx_bit_rt, rx_bit_rt : std_logic;
  signal tx_ld, tx_shift, tx_next, tx_bfr_empt, tx_shr_full, en_tx_clk, txclk : std_logic;
  signal rx_ld, rx_shift, rx_next, rx_bfr_full, en_rx_clk, rx_done, rxclk : std_logic;
  signal txclk_rise : std_logic;
  signal rxclk_fall, rxclk_rise, rxdat_1to0, rxdat_new, rxdat_old : std_logic;
  signal a_overrun, a_framing, sel_delayed, reset_rxck : std_logic;
  signal sta_xmit_sto, sta_recv_sto : reg10;
  signal err_overrun, err_framing : std_logic;
  signal rx_int_set, inter_RX_full, tx_int_set, inter_TX_empty : std_logic;

  signal d_int_tx_empty, d_rx_int_set, d_err_framing, d_err_overrun : std_logic;

  signal tx_baud_div, rx_baud_div : integer := 0;
  
begin

  d_out <= x"000000" & received when s_rx = '1'   else
           x"000000" & status;

  rts <= ctrl(7);
  
  -- testing only: tells remote unit of transmission speed
  bit_rt <= ctrl(2 downto 0);
  
  U_ctrl:  register8 port map (clk,rst, s_ctrl, d_inp(7 downto 0), ctrl);

  status <= cts & tx_bfr_empt & rx_bfr_full &
            inter_TX_empty & inter_RX_full & 
            '0' & err_framing & err_overrun;

  inter <= inter_TX_empty or inter_RX_full;

  U_delay: FFD port map (clk, rst, '1', s_stat, sel_delayed);
  
  U_txreg: register8 port map (clk,rst, s_tx, d_inp(7 downto 0), txreg);

  
  sta_xmit_sto <= '1'& txreg & '0';      -- start (b0), octet, stop (b9)
  tx_next <= txclk_rise and tx_shift;
  U_transmit: par_ser10 port map (clk, rst, tx_ld, tx_next,
                                   sta_xmit_sto, txdat);

  tx_int_set <= ctrl(4) and tx_ld;
  d_int_tx_empty <= (inter_TX_empty or tx_int_set) and not(sel_delayed);
  U_tx_int: FFD port map (clk, rst, '1', d_int_tx_empty, inter_TX_empty);

  
  -- this state machine contols the CPU-transmission interface -------------
  U_TXCPU_st_reg: process(rst,clk)
  begin
    if rst = '0' then
      txcpu_current_st <= st_idle;
    elsif rising_edge(clk) then
      txcpu_current_st <= txcpu_next_st;
    end if;
  end process U_TXCPU_st_reg;

  txcpu_dbg_st <= integer(txcpu_state'pos(txcpu_current_st)); -- for debugging

  U_TXCPU_st_transitions: process(txcpu_current_st, s_tx, tx_shr_full) 
  begin
    case txcpu_current_st is
      when st_idle =>                   -- 0
        if s_tx = '1' then
          txcpu_next_st <= st_check;
        else
          txcpu_next_st <= st_idle;
        end if;
      when st_check =>                  -- 1
        if tx_shr_full = '1' then
          txcpu_next_st <= st_check;
        else
          txcpu_next_st <= st_done;
        end if;
      when st_done =>                   -- 2
        txcpu_next_st <= st_idle;
      when others =>
        assert false report "TX-CPU stateMachine broken"
          & integer'image(txcpu_state'pos(txcpu_current_st)) severity failure;
    end case;
  end process U_TXCPU_st_transitions;   

  
  U_TXCPU_outputs: process(txcpu_current_st)  
  begin
    case txcpu_current_st is
      when st_idle =>                   -- 0
        tx_ld       <= '0';
        tx_bfr_empt <= '1';
      when st_check =>                  -- 1
        tx_ld       <= '0';
        tx_bfr_empt <= '0';
      when st_done =>                   -- 2
        tx_ld       <= '1';
        tx_bfr_empt <= '0';
    end case;
  end process U_TXCPU_outputs;   

  U_TX_st_reg: process(rst,clk)
  begin
    if rst = '0' then
      tx_current_st <= st_idle;
    elsif rising_edge(clk) then
      tx_current_st <= tx_next_st;
    end if;
  end process U_TX_st_reg;

  tx_dbg_st <= integer(tx_state'pos(tx_current_st));  -- debugging
  
  U_TX_st_transitions: process(tx_current_st,tx_ld,txclk_rise)
  begin
    case tx_current_st is
      when st_idle =>
        if tx_ld = '1' then
          tx_next_st <= st_check;
        else
          tx_next_st <= st_idle;
        end if;
      when st_check =>
        tx_next_st <= st_start;
      when st_start =>
        if txclk_rise = '1' then      -- synchronize CPUclock with TXclock
          tx_next_st <= st_b0;
        else
          tx_next_st <= st_start;
        end if;
      when st_b0 =>
        if txclk_rise = '1' then
          tx_next_st <= st_b1;
        else
          tx_next_st <= st_b0;
        end if;
      when st_b1 =>
        if txclk_rise = '1' then
          tx_next_st <= st_b2;
        else
          tx_next_st <= st_b1;
        end if;
      when st_b2 =>
        if txclk_rise = '1' then
          tx_next_st <= st_b3;
        else
          tx_next_st <= st_b2;
        end if;
      when st_b3 =>
        if txclk_rise = '1' then
          tx_next_st <= st_b4;
        else
          tx_next_st <= st_b3;
        end if;
      when st_b4 =>
        if txclk_rise = '1' then
          tx_next_st <= st_b5;
        else
          tx_next_st <= st_b4;
        end if;
      when st_b5 =>
        if txclk_rise = '1' then
          tx_next_st <= st_b6;
        else
          tx_next_st <= st_b5;
        end if;
      when st_b6 =>
        if txclk_rise = '1' then
          tx_next_st <= st_b7;
        else
          tx_next_st <= st_b6;
        end if;
      when st_b7 =>
        if txclk_rise = '1' then
          tx_next_st <= st_stop;
        else
          tx_next_st <= st_b7;
        end if;
      when st_stop =>
        if txclk_rise = '1' then
          tx_next_st <= st_done;
        else
          tx_next_st <= st_stop;
        end if;
      when st_done =>
        tx_next_st  <= st_idle;
      when others =>
        assert false report "TX stateMachine broken"
          & integer'image(tx_state'pos(tx_current_st)) severity failure;
    end case;
  end process U_TX_st_transitions;   


  U_TX_outputs: process(tx_current_st)  
  begin
    case tx_current_st is
      when st_idle =>
        tx_shr_full <= '0';
        tx_shift    <= '0';
        en_tx_clk   <= '0';
      when st_check =>
        tx_shift    <= '0';
        tx_shr_full <= '1';
        en_tx_clk   <= '1';
      when st_start | st_b0 | st_b1 | st_b2 | st_b3 | st_b4 | st_b5 | st_b6 | st_b7 | st_stop =>
        tx_shift    <= '1';
        tx_shr_full <= '1';
        en_tx_clk   <= '1';
      when st_done =>
        tx_shr_full <= '1';
        tx_shift    <= '0';
        en_tx_clk   <= '0';
    end case;
  end process U_TX_outputs;  

  U_rxreg: register8 port map (clk,rst, rx_ld, rxreg, received);
  
  rx_next <= rx_shift and rxclk_rise;
  U_receive: ser_par10 port map (clk, rst, rx_next, rxdat, sta_recv_sto);
  rxreg <= sta_recv_sto(8 downto 1);

  U_edgeDetect0: FFD port map (clk, rst, '1', rxdat, rxdat_new);
  U_edgeDetect1: FFD port map (clk, rst, '1', rxdat_new, rxdat_old);
  rxdat_1to0 <= rxdat_old and not(rxdat_new);
  
  -- framing error: 10th bit not a STOP=1 or 1st bit not a START=0
  a_framing <= '1' when ( (rx_ld = '1') and
                          (sta_recv_sto(9)/='1' or sta_recv_sto(0)/='0') )
               else '0';

  d_err_framing <= (a_framing or err_framing) and not(sel_delayed);
  U_framing: FFD port map (clk, rst, '1', d_err_framing, err_framing);

  d_err_overrun <= (a_overrun or err_overrun) and not(sel_delayed);
  U_overrun: FFD port map (clk, rst, '1', d_err_overrun, err_overrun);
  
  rx_int_set   <= ctrl(3) and rx_done;
  d_rx_int_set <= (rx_int_set or inter_RX_full) and not(sel_delayed);
  U_rx_int: FFD port map (clk, rst, '1', d_rx_int_set, inter_RX_full);

  
  -- SM controls reception-CPU interface -------------------------------
  U_RXCPU_st_reg: process(rst,clk)
  begin
    if rst = '0' then
      rxcpu_current_st <= st_idle;
    elsif rising_edge(clk) then
      rxcpu_current_st <= rxcpu_next_st;
    end if;
  end process U_RXCPU_st_reg;

  rxcpu_dbg_st <= integer(rxcpu_state'pos(rxcpu_current_st));  -- debugging

  U_RXCPU_st_transitions: process(rxcpu_current_st, rx_done, s_rx)
  begin
    case rxcpu_current_st is
      when st_idle =>                   -- 0
        if rx_done = '1' then           -- rx buffer full
          rxcpu_next_st <= st_copy;
        else
          rxcpu_next_st <= st_idle;
        end if;
      when st_copy =>                   -- 1
        rxcpu_next_st <= st_check;
      when st_check =>                  -- 2
        if rx_done = '1' then
          rxcpu_next_st <= st_error;
        elsif s_rx = '1' then
          rxcpu_next_st <= st_idle;
        else
          rxcpu_next_st <= st_check;
        end if;
      when st_error =>                  -- 3
        rxcpu_next_st <= st_check;
      when others =>
        assert false report "RX-CPU stateMachine broken"
          & integer'image(rxcpu_state'pos(rxcpu_current_st)) severity failure;
    end case;
  end process U_RXCPU_st_transitions; 

  U_RXCPU_outputs: process(rxcpu_current_st)
  begin
    case rxcpu_current_st is
      when st_idle =>                   -- 0
        rx_ld       <= '0';
        rx_bfr_full <= '0';
        a_overrun   <= '0';
      when st_copy =>                   -- 1
        rx_ld       <= '1';
        rx_bfr_full <= '1';
        a_overrun   <= '0';        
      when st_check =>                  -- 2
        rx_ld       <= '0';
        rx_bfr_full <= '1';
        a_overrun   <= '0';
      when st_error =>                  -- 3
        rx_ld       <= '0';
        rx_bfr_full <= '1';
        a_overrun   <= '1';    -- assert overrun error, char overwritten
    end case;
  end process U_RXCPU_outputs;  
  
  -- SM controls data reception circuit ------
  U_RX_st_reg: process(rst,clk)
  begin
    if rst = '0' then
      rx_current_st <= st_idle;
    elsif rising_edge(clk) then
      rx_current_st <= rx_next_st;
    end if;
  end process U_RX_st_reg;

  rx_dbg_st <= integer(rx_state'pos(rx_current_st));  -- debugging only
  
  U_RX_st_transitions: process(rx_current_st, rxclk_fall, rxdat_1to0)
  begin
    case rx_current_st is
      when st_idle =>
        if rxdat_1to0 = '1' then    -- start bit = falling edge on rxdat
          rx_next_st <= st_check;
        else
          rx_next_st <= st_idle;
        end if;
      when st_check =>
        rx_next_st <= st_start;
      when st_start =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b0;
        else
          rx_next_st <= st_start;
        end if;
      when st_b0 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b1;
        else
          rx_next_st <= st_b0;
        end if;
      when st_b1 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b2;
        else
          rx_next_st <= st_b1;
        end if;
      when st_b2 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b3;
        else
          rx_next_st <= st_b2;
        end if;
      when st_b3 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b4;
        else
          rx_next_st <= st_b3;
        end if;
      when st_b4 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b5;
        else
          rx_next_st <= st_b4;
        end if;
      when st_b5 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b6;
        else
          rx_next_st <= st_b5;
        end if;
      when st_b6 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_b7;
        else
          rx_next_st <= st_b6;
        end if;
      when st_b7 =>
        if rxclk_fall = '1' then
          rx_next_st <= st_stop;
        else
          rx_next_st <= st_b7;
        end if;
      when st_stop =>
        if rxclk_fall = '1' then
          rx_next_st <= st_done;
        else
          rx_next_st <= st_stop;
        end if;
      when st_done =>
        rx_next_st <= st_idle;
      when others =>
        assert false report "RX stateMachine broken"
          & integer'image(rx_state'pos(rx_current_st)) severity failure;
    end case;
  end process U_RX_st_transitions; 

  U_RX_outputs: process(rx_current_st)
  begin
    case rx_current_st is
      when st_idle =>
        rx_done    <= '0';
        rx_shift   <= '0';
        reset_rxck <= '0';
        en_rx_clk  <= '0';
      when st_check =>
        rx_done    <= '0';
        rx_shift   <= '0';
        reset_rxck <= '1';
        en_rx_clk  <= '1';
      when st_start | st_b0 | st_b1 | st_b2 | st_b3 | st_b4 | st_b5 | st_b6 | st_b7 | st_stop =>
        rx_done    <= '0';
        rx_shift   <= '1';
        reset_rxck <= '0';
        en_rx_clk  <= '1';
      when st_done =>
        rx_done    <= '1';
        rx_shift   <= '0';
        reset_rxck <= '0';
        en_rx_clk  <= '0';
    end case;
  end process U_RX_outputs; 

  -- U_bit_rt_tx: counter8 port map (clk,rst,tx_ld,en_tx_clk,x"00",tx_bit_rt);
  with ctrl(2 downto 0) select
    tx_baud_div <=      4/2 when b"000",
                      434/2 when b"001",
                      868/2 when b"010",
                     1302/2 when b"011",
                     1736/2 when b"100",
                     2604/2 when b"101",
                     3472/2 when b"110",
                     5208/2 when others;

  U_bit_rt_tx: process(clk, rst, tx_ld, en_tx_clk)
    variable baud_cnt : integer range 0 to 50000000;
  begin
     if rst = '0' then
      baud_cnt  := 0;
      txclk <= '0';
      txclk_rise <= '0';
    elsif tx_ld = '1' and rising_edge(clk) then
      baud_cnt  := 1;
      txclk <= '0';
      txclk_rise <= '0';
    elsif en_tx_clk = '1' and rising_edge(clk) then
      if baud_cnt = tx_baud_div then
        if txclk = '0' then
          txclk_rise <= '1';
        else
          txclk_rise <= '0';
        end if;
        txclk <= not(txclk);
        baud_cnt := 1;
      else
        baud_cnt := baud_cnt + 1;
        txclk_rise <= '0';
      end if;
    end if;
  end process U_bit_rt_tx;


  -- U_bit_rt_rx:counter8 port map(clk,rst,reset_rxck,en_rx_clk,00,rx_bit_rt);
  with ctrl(2 downto 0) select
    rx_baud_div <=     4/2 when b"000",
                     434/2 when b"001",
                     868/2 when b"010",
                    1302/2 when b"011",
                    1736/2 when b"100",
                    2604/2 when b"101",
                    3472/2 when b"110",
                    5208/2 when others;


  U_bit_rt_rx: process(clk, rst, reset_rxck, en_rx_clk)
    variable baud_cnt : integer range 0 to 50000000;
  begin
     if rst = '0' then
      baud_cnt  := 0;
      rxclk <= '0';
      rxclk_fall <= '0';
      rxclk_rise <= '0';
    elsif reset_rxck = '1' and rising_edge(clk) then
      baud_cnt  := 1;
      rxclk <= '1';
      rxclk_fall <= '0';
      rxclk_rise <= '0';      
    elsif en_rx_clk = '1' and rising_edge(clk) then
      if baud_cnt = rx_baud_div then
        if rxclk = '1' then
          rxclk_fall <= '1';
        else
          rxclk_fall <= '0';
          rxclk_rise <= '1';
        end if;
        baud_cnt := 1;
        rxclk <= not(rxclk);
      else
        baud_cnt := baud_cnt + 1;
        rxclk_fall <= '0';
        rxclk_rise <= '0';
      end if;
    end if;
  end process U_bit_rt_rx;

  
end estrutural;

library ieee; use ieee.std_logic_1164.all;
use work.p_WIRES.all;

entity register8 is
  port(rel, rst, ld: in  std_logic;
        D:           in  reg8;
        Q:           out reg8);
end register8;

architecture functional of register8 is
  signal value: reg8;
begin

  process(rel, rst, ld)
  begin
    if rst = '0' then
      value <= x"00";
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value;
end functional;

library ieee; use ieee.std_logic_1164.all;
use work.p_WIRES.all;

entity par_ser10 is
  port(rel, rst, ld, desl: in  std_logic;
       D:                  in  reg10;
       Q:                  out std_logic);
end par_ser10;

architecture functional of par_ser10 is
begin

  process(rel, rst, ld, desl, D)
    variable value: reg10;
  begin
    if rst = '0' then
      value := b"1111111111";
      Q <= '1';
    elsif ld = '1' and rising_edge(rel) then
      value := D;
    elsif desl = '1' and rising_edge(rel) then
      Q <= value(0);
      value(8 downto 0) := value(9 downto 1);
      value(9) := '1';                  -- when idle, send stop-bits
    end if;
  end process;

end functional;

library ieee; use ieee.std_logic_1164.all;
use work.p_WIRES.all;

entity ser_par10 is
  port(rel, rst, desl: in  std_logic;
       D:              in  std_logic;
       Q:              out reg10);
end ser_par10;

architecture functional of ser_par10 is
begin

  process(rel, rst, desl)
    variable value: reg10;
  begin
    if rst = '0' then
      value := b"0000000000";
    elsif desl = '1' and rising_edge(rel) then
      value(8 downto 0) := value(9 downto 1);
      value(9) := D;
    end if;
    Q <= value;
  end process;
  
end functional;