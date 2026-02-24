library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;
use work.p_mem.all;

entity RAM is
  generic (LOAD_FILE_NAME : string := "data.bin";
           DUMP_FILE_NAME : string := "dump.data");
  port (rst      : in    std_logic;
        clk      : in    std_logic;
        sel      : in    std_logic;         -- active in '0'
        rdy      : out   std_logic;         -- active in '0'
        wr       : in    std_logic;         -- active in '0'
        strobe   : in    std_logic;         -- active in '1'
        addr     : in    reg32;
        data_inp : in    reg32;
        data_out : out   reg32;
        byte_sel : in    reg4;
        dump_ram : in    std_logic);        -- dump RAM contents
  constant N_WORDS : natural := 8192;
  constant ADDRS_BITS : natural := log2_ceil(N_WORDS);
  subtype ram_address is integer range 0 to N_WORDS-1;
  subtype ram_addr_bits is std_logic_vector(ADDRS_BITS-1 downto 0);
end entity fpga_RAM;

architecture rtl of fpga_RAM is

  component mf_ram1port
    generic (N_WORDS : integer; ADDRS_BITS : integer);
    port (address    : in  std_logic_vector (ADDRS_BITS-1 downto 0);
          clken      : in  std_logic;
          clock      : in  std_logic;
          data       : in  std_logic_vector (7 downto 0);
          wren       : in  std_logic;
          q          : out std_logic_vector (7 downto 0));
  end component mf_ram1port;

  component wait_states is
    generic (NUM_WAIT_STATES :integer);
    port(rst     : in  std_logic;
         clk     : in  std_logic;
         sel     : in  std_logic;         -- active in '0'
         waiting : out std_logic);        -- active in '1'
  end component wait_states;
  
  signal we0,we1,we2,we3 : std_logic := '0';
  signal di,do : reg32;

  signal r_addr : ram_address := 0;
  signal r_address : ram_addr_bits;

  signal waiting, enable : std_logic;
  
begin  -- rtl

  U_BUS_WAIT: wait_states generic map (RAM_WAIT_STATES)
    port map (rst, clk, sel, waiting);

  rdy <= not(waiting);

  enable <= not(sel);
 
  -- CPU acesses are word-addressed; RAM is byte-addressed, 4-bytes wide
  r_addr <= to_integer( unsigned(addr( (ADDRS_BITS-1+2) downto 2 ) ) );

  r_address <= addr( ADDRS_BITS-1+2 downto 2 );
  
  U_ram0: mf_ram1port generic map (N_WORDS, ADDRS_BITS) port map (
    r_address, enable, strobe, di(7  downto  0), we0, do(7  downto 0));

  U_ram1: mf_ram1port  generic map (N_WORDS, ADDRS_BITS) port map (
    r_address, enable, strobe, di(15 downto  8), we1, do(15 downto 8));

  U_ram2: mf_ram1port  generic map (N_WORDS, ADDRS_BITS) port map (
    r_address, enable, strobe, di(23 downto 16), we2, do(23 downto 16));

  U_ram3: mf_ram1port  generic map (N_WORDS, ADDRS_BITS) port map (
    r_address, enable, strobe, di(31 downto 24), we3, do(31 downto 24));


  accessRAM: process(sel, strobe, wr, r_addr, byte_sel, data_inp, do)
  begin

    if sel = '0' then

      if wr = '0' then                  -- WRITE to MEM
        
        assert (r_addr >= 0) and (r_addr < (DATA_MEM_SZ/4))
          report "ramWR index out of bounds: " & natural'image(r_addr)
          severity failure;

        case byte_sel is                -- partial word stores
          when b"1111"  =>      -- SW
            we3 <= '1';
            we2 <= '1';
            we1 <= '1';
            we0 <= '1';
            di  <= data_inp;
          when b"1100" =>       -- SH, upper
            we3 <= '1';
            we2 <= '1';
            we1 <= '0';
            we0 <= '0';
            di(31 downto 16) <= data_inp(15 downto 0);
            di(15 downto 0)  <= (others => 'X');
          when b"0011" =>       -- SH. lower
            we3 <= '0';
            we2 <= '0';
            we1 <= '1';
            we0 <= '1';
            di(15 downto 0)  <= data_inp(15 downto 0);
            di(31 downto 16) <= (others => 'X');
          when b"0001" =>       -- SB
            we3 <= '0';
            we2 <= '0';
            we1 <= '0';
            we0 <= '1';
            di(7 downto 0)  <= data_inp(7 downto 0);
            di(31 downto 8) <= (others => 'X');
          when b"0010" =>
            we3 <= '0';
            we2 <= '0';
            we1 <= '1';
            we0 <= '0';
            di(31 downto 16) <= (others => 'X');
            di(15 downto 8)  <= data_inp(7 downto 0);
            di(7 downto 0)   <= (others => 'X');
          when b"0100" =>
            we3 <= '0';
            we2 <= '1';
            we1 <= '0';
            we0 <= '0';
            di(31 downto 24) <= (others => 'X');
            di(23 downto 16) <= data_inp(7 downto 0);
            di(15 downto 0)  <= (others => 'X');
          when b"1000" =>
            we3 <= '1';
            we2 <= '0';
            we1 <= '0';
            we0 <= '0';
            di(31 downto 24) <= data_inp(7 downto 0);
            di(23 downto 0)  <= (others => 'X');
          when others =>
            we3 <= '0';
            we2 <= '0';
            we1 <= '0';
            we0 <= '0';
            di  <= (others => 'X');

        end case;
        -- assert false report "ramWR["& natural'image(r_addr) &"] "
        --   & SLV32HEX(data_out) &" bySel=" & SLV2STR(byte_sel); -- DEBUG

        data_out <= (others => 'X');
        
      else                              -- READ from MEM, wr /= 0

        we3 <= '0';
        we2 <= '0';
        we1 <= '0';
        we0 <= '0';
        di  <= (others => 'X');

        assert (r_addr >= 0) and (r_addr < (DATA_MEM_SZ/4))
          report "ramRD index out of bounds: " & natural'image(r_addr)
          severity failure;

        -- byte/half selection done at CPU
        data_out(31 downto 24) <= do(31 downto 24);
        data_out(23 downto 16) <= do(23 downto 16);
        data_out(15 downto  8) <= do(15 downto  8);
        data_out(7  downto  0) <= do(7  downto  0);

        -- assert false report "ramRD["& natural'image(r_addr) &"] "
        --   & SLV32HEX(do) &" bySel="& SLV2STR(byte_sel);  -- DEBUG

      end if;                           -- wr

    else      -- sel /= 0

      we3 <= '0';
      we2 <= '0';
      we1 <= '0';
      we0 <= '0';
      di       <= (others => 'X');
      data_out <= (others => 'X');

    end if;
        
  end process accessRAM;

end architecture rtl;
