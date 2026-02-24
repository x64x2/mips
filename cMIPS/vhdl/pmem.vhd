library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

package p_MEM is

  -- To simplify (and accelerate) the RAM address decoding,
  --  the BASE of the RAM addresses MUST be allocated at an
  --  address that is above/larger the RAM capacity.  Otherwise, the
  --  base must be subtracted from the address on every reference,
  --  which means having an adder in the critical path.  Bad idea.

  -- The address ranges for ROM, RAM and I/O must be distinct in the
  -- uppermost 12 bits of the address (bits 31..20).
  constant HI_SEL_BITS : integer := 31;
  constant LO_SEL_BITS : integer := 20;
  
  -- x_IO_ADDR_RANGE can have only ONE bit set, thus being a power of 2
  
  -- begin DO NOT change these names as several scripts depend on them --
  constant x_BASE_ADDR : reg32   := x"00000000";
  constant x_INST_MEM_SZ    : reg32   := x"00002000";  
  constant x_DATA_BASE_ADDR : reg32   := x"00400000";  
  constant x_DATA_MEM_SZ    : reg32   := x"00002000";
  constant x_IO_BASE_ADDR   : reg32   := x"0F000000";
  constant x_IO_MEM_SZ      : reg32   := x"00002000";
  constant x_IO_ADDR_RANGE  : reg32   := x"00000020";
  constant x_EXCEPTION_0000 : reg32   := x"00000080";
  constant x_EXCEPTION_0180 : reg32   := x"000000c0";
  constant x_EXCEPTION_0200 : reg32   := x"00000140";
  constant x_ENTRY_POINT    : reg32   := x"00000300";
  -- end DO NOT change these names --

  constant INST_BASE_ADDR  : integer := to_integer(signed(x_INST_BASE_ADDR));
  constant INST_MEM_SZ     : integer := to_integer(signed(x_INST_MEM_SZ));
  constant DATA_BASE_ADDR  : integer := to_integer(signed(x_DATA_BASE_ADDR));
  constant DATA_MEM_SZ     : integer := to_integer(signed(x_DATA_MEM_SZ));
  constant IO_BASE_ADDR    : integer := to_integer(signed(x_IO_BASE_ADDR));
  constant IO_MEM_SZ       : integer := to_integer(signed(x_IO_MEM_SZ));
  constant IO_ADDR_RANGE   : integer := to_integer(signed(x_IO_ADDR_RANGE));

  constant IO_ADDR_MASK    : integer := (0 - IO_ADDR_RANGE);
  constant x_IO_ADDR_MASK  : reg32   := std_logic_vector(to_signed(0 - IO_ADDR_RANGE, 32));

  -- maximum number of IO devices, must be power of two.
  constant IO_MAX_NUM_DEVS : integer := 16;
  
  -- I/O addresses are IO_ADDR_RANGE apart 
  constant IO_PRINT_ADDR   : integer := IO_BASE_ADDR;
  constant IO_STDOUT_ADDR  : integer := IO_BASE_ADDR + 1*IO_ADDR_RANGE;
  constant IO_STDIN_ADDR   : integer := IO_BASE_ADDR + 2*IO_ADDR_RANGE;
  constant IO_READ_ADDR    : integer := IO_BASE_ADDR + 3*IO_ADDR_RANGE;
  constant IO_WRITE_ADDR   : integer := IO_BASE_ADDR + 4*IO_ADDR_RANGE;
  constant IO_COUNT_ADDR   : integer := IO_BASE_ADDR + 5*IO_ADDR_RANGE;
  constant IO_FPU_ADDR     : integer := IO_BASE_ADDR + 6*IO_ADDR_RANGE;
  constant IO_UART_ADDR    : integer := IO_BASE_ADDR + 7*IO_ADDR_RANGE;
  constant IO_STATS_ADDR   : integer := IO_BASE_ADDR + 8*IO_ADDR_RANGE;
  constant IO_DSP7SEG_ADDR : integer := IO_BASE_ADDR + 9*IO_ADDR_RANGE;
  constant IO_KEYBD_ADDR   : integer := IO_BASE_ADDR + 10*IO_ADDR_RANGE;
  constant IO_LCD_ADDR     : integer := IO_BASE_ADDR + 11*IO_ADDR_RANGE;
  constant IO_HIGHEST_ADDR : integer :=
    IO_BASE_ADDR + (IO_MAX_NUM_DEVS - 1)*IO_ADDR_RANGE;

  -- DATA CACHE parameters ----------------------------------------------
  
  -- The combination of capacity, associativity and block/line size
  --  MUST be such that DC_INDEX_BITS >= 6 (64 sets/way)
  constant DC_TOTAL_CAPACITY  : natural := 2*1024;
  constant DC_NUM_WAYS        : natural := 1;  -- direct mapped
  constant DC_VIA_CAPACITY    : natural := DC_TOTAL_CAPACITY / DC_NUM_WAYS;
  constant DC_BTS_PER_WORD    : natural := 32;
  constant DC_BYTES_PER_WORD  : natural := 4;
  constant DC_WORDS_PER_BLOCK : natural := 4;
  constant DC_NUM_WORDS       : natural := DC_VIA_CAPACITY / DC_BYTES_PER_WORD;
  constant DC_NUM_BLOCKS      : natural := DC_NUM_WORDS / DC_WORDS_PER_BLOCK;
  constant DC_INDEX_BITS      : natural := log2_ceil( DC_NUM_BLOCKS );
  constant DC_WORD_SEL_BITS   : natural := log2_ceil( DC_WORDS_PER_BLOCK );
  constant DC_BYTE_SEL_BITS   : natural := log2_ceil( DC_BYTES_PER_WORD );

  -- constants for CONFIG1 cop0 register
  constant DC_SETS_PER_WAY: reg3 :=
    std_logic_vector(to_signed(DC_INDEX_BITS - 6, 3));
  constant DC_LINE_SIZE: reg3 :=
    std_logic_vector(to_signed(DC_WORD_SEL_BITS + 1, 3));
  constant DC_ASSOCIATIVITY: reg3 :=
    std_logic_vector(to_signed(DC_NUM_WAYS - 1, 3));

  -- INSTRUCTION CACHE parameters ---------------------------------------

  -- The combination of capacity, associativity and block/line size
  --  MUST be such that IC_INDEX_BITS >= 6 (64 sets/via)
  constant IC_TOTAL_CAPACITY  : natural := 1024; -- 2*1024;
  constant IC_NUM_WAYS        : natural := 1;  -- direct mapped
  constant IC_VIA_CAPACITY    : natural := IC_TOTAL_CAPACITY / IC_NUM_WAYS;
  constant IC_BTS_PER_WORD    : natural := 32;
  constant IC_BYTES_PER_WORD  : natural := 4;
  constant IC_WORDS_PER_BLOCK : natural := 4;
  constant IC_NUM_WORDS       : natural := IC_VIA_CAPACITY / IC_BYTES_PER_WORD;
  constant IC_NUM_BLOCKS      : natural := IC_NUM_WORDS / IC_WORDS_PER_BLOCK;
  constant IC_INDEX_BITS      : natural := log2_ceil( IC_NUM_BLOCKS );
  constant IC_WORD_SEL_BITS   : natural := log2_ceil( IC_WORDS_PER_BLOCK );
  constant IC_BYTE_SEL_BITS   : natural := log2_ceil( IC_BYTES_PER_WORD );

  -- constants for CONFIG1 cop0 register
  constant IC_SETS_PER_WAY: reg3 :=
    std_logic_vector(to_signed(IC_INDEX_BITS - 6, 3));
  constant IC_LINE_SIZE: reg3 :=
    std_logic_vector(to_signed(IC_WORD_SEL_BITS + 1, 3));
  constant IC_ASSOCIATIVITY: reg3 :=
    std_logic_vector(to_signed(IC_NUM_WAYS - 1, 3));

  -- constants to access statistics counter
  constant dcache_Stats_ref   : reg3 := "000";
  constant dcache_Stats_rdhit : reg3 := "001";
  constant dcache_Stats_wrhit : reg3 := "010";
  constant dcache_Stats_flush : reg3 := "011";
  constant icache_Stats_ref   : reg3 := "100";
  constant icache_Stats_hit   : reg3 := "101";
  
end p_MEM;

-- package body p_MEM is
-- end p_MEM;