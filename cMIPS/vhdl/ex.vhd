library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_WIRES.all;

package p_excp is
  
  type excp_type is (exNOP,
                          exMTC0, exMFC0,  -- 2
                          exERET,       -- 3
                          exEI,exDI,    -- 5
                          exBREAK, exTRAP, exSYSCALL,  -- 8
                          exRESV_INSTR, exWAIT,  -- 10
                          IFaddressError, MMaddressErrorLD, MMaddressErrorST,
                          exTLBld, exTLBst, exTLBmod,  --16
                          exOvfl,       -- 17
                          exLL,exSC,    -- 18,19  these are treated by COP0
                          invalid_excp);

  attribute enum_encoding of excp_type : type is
    "000000 000001 000010 000011 000100 000101 000110 000111 001000 001001 001010 001011 001100 001101 001110 001111 010000 010001 010010 010011 010100";

--  010101 010110 010111 011000 011001 011010 011011 011100 011101 011110 011111 100000 100001 100010";


  
  -- Table 8-1 Coprocessor 0 Registers, pg 55
  constant cop0reg_BadVAddr : reg5 := b"01000";  -- 8
  constant cop0reg_COUNT    : reg5 := b"01001";  -- 9
  constant cop0reg_COMPARE  : reg5 := b"01011";  -- 11
  constant cop0reg_STATUS   : reg5 := b"01100";  -- 12
  constant cop0reg_CAUSE    : reg5 := b"01101";  -- 13
  constant cop0reg_EPC      : reg5 := b"01110";  -- 14
  constant cop0reg_CONFIG   : reg5 := b"10000";  -- 16
  constant cop0reg_LLAddr   : reg5 := b"10001";  -- 17
  constant cop0reg_ErrorPC  : reg5 := b"11110";  -- 30

  
  -- Table 8-25 Cause Register ExcCode Field, pg 95
  constant cop0code_Int  : reg5 := b"00000";  --  0, interrupt=00 (CAUSE lsB)
  constant cop0code_Mod  : reg5 := b"00001";  --  1, AddrError if/ld=x04
  constant cop0code_TLBL : reg5 := b"00010";  --  2, AddrError if/ld=x08
  constant cop0code_TLBS : reg5 := b"00011";  --  3, AddrError if/ld=x0c
  constant cop0code_AdEL : reg5 := b"00100";  --  4, AddrError if/ld=x10
  constant cop0code_AdES : reg5 := b"00101";  --  5, AddrError store=x14
  constant cop0code_IBE  : reg5 := b"00110";  --  6, BusErrorExcp if=x18
  constant cop0code_DBE  : reg5 := b"00111";  --  7, BusErrorExcp ld/st=x1c
  constant cop0code_Sys  : reg5 := b"01000";  --  8, syscall=x20
  constant cop0code_Bp   : reg5 := b"01001";  --  9, breakpoint=x24
  constant cop0code_RI   : reg5 := b"01010";  -- 10, reserved instruction=x28
  constant cop0code_Ov   : reg5 := b"01100";  -- 12, arithmetic overflow=x30
  constant cop0code_Tr   : reg5 := b"01101";  -- 13, trap=x34
  constant cop0code_NULL : reg5 := b"11111";  -- 1f, (no excp)=x3c


  -- kernel mode, all else disabled
  constant RESET_STATUS: std_logic_vector(31 downto 0) := x"10000002";

  -- COUNTER disabled, kernel mode, excpCode = noexcp
  constant RESET_CAUSE:  std_logic_vector(31 downto 0) := x"0880007c";


  -- Table 8-19 Status Register Field Descriptions, pg 79
  constant STATUS_CU3:  integer := 31;	-- COP-1 absent=0 (always)
  constant STATUS_CU2:  integer := 30;	-- COP-1 absent=0 (always)
  constant STATUS_CU1:  integer := 29;	-- COP-1 absent=0 (always)
  constant STATUS_CU0:  integer := 28;	-- COP-0 present=1 (always)
  constant STATUS_BEV:  integer := 22;	-- locationVect at bootstrap=1
  constant STATUS_TS:   integer := 21;	-- TLBmatchesSeveral=1
  constant STATUS_SR:   integer := 20;	-- softReset=1
  constant STATUS_NMI:  integer := 19;	-- reset/softReset=0, NMI=1
  constant STATUS_IM7:  integer := 15;	-- hw interrupt-7 req eabled=1
  constant STATUS_IM6:  integer := 14;	-- hw interrupt-6 req eabled=1
  constant STATUS_IM5:  integer := 13;	-- hw interrupt-5 req eabled=1
  constant STATUS_IM4:  integer := 12;	-- hw interrupt-4 req eabled=1
  constant STATUS_IM3:  integer := 11;	-- hw interrupt-3 req eabled=1
  constant STATUS_IM2:  integer := 10;	-- hw interrupt-2 req eabled=1
  constant STATUS_IM1:  integer := 9;	-- sw interrupt-1 req eabled=1
  constant STATUS_IM0:  integer := 8;	-- sw interrupt-0 req eabled=1
  constant STATUS_SUP:  integer := 4;	-- in supervisor mode=1 (not used)
  constant STATUS_UM:   integer := 3;	-- in user mode=1
  constant STATUS_ERL:  integer := 2;	-- at error level=1
  constant STATUS_EXL:  integer := 1;	-- at excp level=1
  constant STATUS_IE:   integer := 0;	-- interrupt enabled=1


  -- Table 8-24 Cause Register Field Descriptions, pg 92
  constant CAUSE_BD:   integer := 31;	-- exceptn in branch-delay-slot=1
  constant CAUSE_TI:   integer := 30;	-- timer interrupt pending=1
  constant CAUSE_CE1:  integer := 29;	-- COP # in COP-UnusableExcp
  constant CAUSE_CE0:  integer := 28;	-- COP # in COP-UnusableExcp
  constant CAUSE_DC:   integer := 27;	-- COUNT reg is disabled=1
  constant CAUSE_PCI:  integer := 26;	-- perfCounter interr pndng=1
  constant CAUSE_IV:   integer := 23;	-- use special interrVector=1
  constant CAUSE_WP:   integer := 22;	-- watch deferred=1 (not used)
  constant CAUSE_IP7:  integer := 15;	-- hw interrupt-7 pending=1
  constant CAUSE_IP6:  integer := 14;	-- hw interrupt-6 pending=1
  constant CAUSE_IP5:  integer := 13;	-- hw interrupt-5 pending=1
  constant CAUSE_IP4:  integer := 12;	-- hw interrupt-4 pending=1
  constant CAUSE_IP3:  integer := 11;	-- hw interrupt-3 pending=1
  constant CAUSE_IP2:  integer := 10;	-- hw interrupt-2 pending=1
  constant CAUSE_IP1:  integer := 9;	-- sw interrupt-1 pending=1
  constant CAUSE_IP0:  integer := 8;	-- sw interrupt-0 pending=1
  constant CAUSE_ExcCodehi: integer := 6;      -- excp code
  constant CAUSE_ExcCodelo: integer := 2;      -- excp code


end p_EXCP;

-- package body p_EXCP is
-- end p_EXCP;