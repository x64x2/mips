-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- CPU core
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;
use work.p_memory.all;
use work.p_exception.all;

entity core is
  port (
    rst    : in    std_logic;
    clk    : in    std_logic;
    phi2   : in    std_logic;
    i_aVal : out   std_logic;
    i_wait : in    std_logic;
    i_addr : out   std_logic_vector;
    instr  : in    std_logic_vector;
    d_aVal : out   std_logic;
    d_wait : in    std_logic;
    d_addr : out   std_logic_vector;
    data_inp : in  std_logic_vector;
    data_out : out std_logic_vector;
    wr     : out   std_logic;
    b_sel  : out   std_logic_vector;
    nmi    : in    std_logic;
    irq    : in    std_logic_vector);
end core;

architecture rtl of core is

-- fields of the control table
--    aVal:  std_logic;        -- addressValid, enable data-mem=0
--    wmem:  std_logic;        -- READ=1/WRITE=0 in/to memory
--    i:     instr_type;       -- instruction
--    wreg:  std_logic;        -- register write=0
--    selB:  std_logic;        -- B ALU input, reg=0 ext=1
--    fun:   std_logic;        -- check function_field=1
--    oper:  t_alu_fun;        -- ALU operation
--    muxC:  reg3;             -- select result mem=0 ula=1 jr=2 pc+8=3
--    c_sel: reg2;             -- select destination reg RD=0 RT=1 31=2
--    extS:  std_logic;        -- sign-extend=1, zero-ext=0
--    PCsel: reg2;             -- PCmux 0=PC+4 1=beq 2=j 3=jr
--    br_t:  t_comparison;     -- branch: 0=no 1=beq 2=bne
--    exp:  reg2              -- stage with exception 0=no,1=rf,2=ex,3=mm
  
  constant ctrl_table : t_control_mem := (
  --aVal wmem ins wreg selB fun oper muxC  csel extS PCsel br_t exp
    ('1','1',iALU, '1','0','1',opNOP,"001","00", '0', "00",cNOP,"00"),--ALU=0
    ('1','1',RIMM, '1','0','0',opNOP,"001","00", '1', "00",cOTH,"00"),--BR=1
    ('1','1',J,    '1','0','0',opNOP,"001","00", '0', "10",cNOP,"00"),--j=2
    ('1','1',JAL,  '0','0','0',opNOP,"011","10", '0', "10",cNOP,"00"),--jal=3
    ('1','1',BEQ,  '1','0','0',opNOP,"001","00", '1', "01",cEQU,"00"),--beq=4
    ('1','1',BNE,  '1','0','0',opNOP,"001","00", '1', "01",cNEQ,"00"),--bne=5
    ('1','1',BLEZ, '1','0','0',opNOP,"001","00", '1', "01",cLEZ,"00"),--blez=6
    ('1','1',BGTZ, '1','0','0',opNOP,"001","00", '1', "01",cGTZ,"00"),--bgtz=7
    ('1','1',ADDI, '0','1','0',opADD,"001","01", '1', "00",cNOP,"10"),--addi=8
    ('1','1',ADDIU,'0','1','0',opADD,"001","01", '1', "00",cNOP,"00"),--addiu=9
    ('1','1',SLTI, '0','1','0',opSLT,"001","01", '1', "00",cNOP,"10"),--slti=10
    ('1','1',SLTIU,'0','1','0',opSLTU,"001","01",'1', "00",cNOP,"00"),--sltiu11
    ('1','1',ANDI, '0','1','0',opAND,"001","01", '0', "00",cNOP,"00"),--andi=12
    ('1','1',ORI,  '0','1','0',opOR, "001","01", '0', "00",cNOP,"00"),--ori=13
    ('1','1',XORI, '0','1','0',opXOR,"001","01", '0', "00",cNOP,"00"),--xori=14
    ('1','1',LUI,  '0','1','0',opLUI,"001","01", '0', "00",cNOP,"00"),--lui=15
    ('1','1',COP0, '1','0','1',opNOP,"110","01", '0', "00",cNOP,"00"),--COP0=16
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--17
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--18
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--19
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--beql=20
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--bnel=21
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--blzel=22
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--bgtzl=23
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--24
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--25
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--26
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--27
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--28
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--29
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--30
    ('1','1',SPEC3,'0','0','0',opSPC,"001","00", '0', "00",cNOP,"00"),--special3
    ('0','1',LB,   '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--lb=32
    ('0','1',LH,   '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--lh=33
    ('0','1',LWL,  '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--lwl=34
    ('0','1',LW,   '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--lw=35
    ('0','1',LBU,  '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--lbu=36
    ('0','1',LHU,  '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--lhu=37
    ('0','1',LWR,  '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--lwr=38
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--39
    ('0','0',SB,   '1','1','0',opADD,"001","00", '1', "00",cNOP,"00"),--sb=40
    ('0','0',SH,   '1','1','0',opADD,"001","00", '1', "00",cNOP,"00"),--sh=41
    ('1','1',NIL,  '1','1','0',opNOP,"001","00", '0', "00",cNOP,"00"),--swl=42
    ('0','0',SW,   '1','1','0',opADD,"001","00", '1', "00",cNOP,"00"),--sw=43
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--44
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--45
    ('1','1',NIL,  '1','1','0',opNOP,"001","00", '0', "00",cNOP,"00"),--swr=46
    ('1','1',NIL,  '1','1','0',opNOP,"001","00", '0', "00",cNOP,"00"),--cache=47
    ('0','1',LL,   '0','1','0',opADD,"000","01", '1', "00",cNOP,"00"),--ll=48
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--lwc1=49
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--lwc2=50
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--pref=51
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--52
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--ldc1=53
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--ldc2=54
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--55
    ('0','0',SC,   '0','1','0',opADD,"111","01", '1', "00",cNOP,"00"),--sc=56
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--swc1=57
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--swc2=58
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--59
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--60
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--sdc1=61
    ('1','1',NIL,  '1','0','0',opNOP,"001","00", '0', "00",cNOP,"00"),--sdc2=62
    ('1','1',NOP,  '1','0','0',opNOP,"000","00", '0', "00",cNOP,"00") --63
    );

-- fields of the function table (opcode=0)
--    i:     instr_type;       -- instruction
--    wreg:  std_logic;        -- register write=0
--    selB:  std_logic;        -- B ALU input, reg=0 ext=1
--    oper:  t_alu_fun;        -- ALU operation
--    muxC:  reg3;             -- select result mem=0 ula=1 jr=2 pc+8=3
--    trap:  std_logic;        -- trap on compare
--    move:  std_logic;        -- conditional move
--    sync:  std_logic;        -- synch the memory hierarchy
--    PCsel: reg2;             -- PCmux 0=PC+4 1=beq 2=j 3=jr
--    exp:  reg2              -- stage with exception 0=no,1=rf,2=ex,3=mm
  
  constant func_table : t_function_mem := (
  -- i    wreg selB oper   muxC trap mov syn PCsel exp
    (iSLL, '0','0',opSLL,  "001",'0','0','0',"00","00"),  --sll=0
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --1, FlPoint
    (iSRL, '0','0',opSRL,  "001",'0','0','0',"00","00"),  --srl=2
    (iSRA, '0','0',opSRA,  "001",'0','0','0',"00","00"),  --sra=3
    (SLLV, '0','0',opSLLV, "001",'0','0','0',"00","00"),  --sllv=4
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --5
    (SRLV, '0','0',opSRLV, "001",'0','0','0',"00","00"),  --srlv=6
    (SRAV, '0','0',opSRAV, "001",'0','0','0',"00","00"),  --srav=7
    (JR,   '1','0',opNOP,  "001",'0','0','0',"11","00"),  --jr=8
    (JALR, '0','0',opNOP,  "011",'0','0','0',"11","00"),  --jalr=9
    (MOVZ, '0','0',opMOVZ, "001",'0','1','0',"00","00"),  --movz=10
    (MOVN, '0','0',opMOVN, "001",'0','1','0',"00","00"),  --movn=11
    (SYSCALL,'1','0',trNOP,"001",'1','0','0',"00","00"),  --syscall=12
    (BREAK,'1','0',trNOP,  "001",'1','0','0',"00","00"),  --break=13
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --14
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --15
    (MFHI, '0','0',opMFHI, "100",'0','0','0',"00","00"),  --mfhi=16
    (MTHI, '1','0',opMTHI, "001",'0','0','0',"00","00"),  --mthi=17
    (MFLO, '0','0',opMFLO, "101",'0','0','0',"00","00"),  --mflo=18
    (MTLO, '1','0',opMTLO, "001",'0','0','0',"00","00"),  --mtlo=19
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --20
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --21
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --22
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --23
    (MULT, '1','0',opMULT, "001",'0','0','0',"00","00"),  --mult=24
    (MULTU,'1','0',opMULTU,"001",'0','0','0',"00","00"),  --multu=25
    (DIV,  '1','0',opDIV,  "001",'0','0','0',"00","00"),  --div=26
    (DIVU, '1','0',opDIVU, "001",'0','0','0',"00","00"),  --divu=27
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --28
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --29
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --30
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --31
    (ADD,  '0','0',opADD,  "001",'0','0','0',"00","10"),  --add=32
    (ADDU, '0','0',opADDU, "001",'0','0','0',"00","00"),  --addu=33
    (SUB,  '0','0',opSUB,  "001",'0','0','0',"00","10"),  --sub=34
    (SUBU, '0','0',opSUBU, "001",'0','0','0',"00","00"),  --subu=35
    (iAND, '0','0',opAND,  "001",'0','0','0',"00","00"),  --and=36
    (iOR,  '0','0',opOR,   "001",'0','0','0',"00","00"),  --or=37
    (iXOR, '0','0',opXOR,  "001",'0','0','0',"00","00"),  --xor=38
    (iNOR, '0','0',opNOR,  "001",'0','0','0',"00","00"),  --nor=39
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --40
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --41
    (SLT,  '0','0',opSLT,  "001",'0','0','0',"00","10"),  --slt=42
    (SLTU, '0','0',opSLTU, "001",'0','0','0',"00","00"),  --sltu=43
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --44
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --45
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --46
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --47
    (TGE,  '1','0',trGEQ,  "001",'1','0','0',"00","01"),  --tge=48
    (TGEU, '1','0',trGEU,  "001",'1','0','0',"00","01"),  --tgeu=49
    (TLT,  '1','0',trLTH,  "001",'1','0','0',"00","01"),  --tlt=50
    (TLTU, '1','0',trLTU,  "001",'1','0','0',"00","01"),  --tltu=51
    (TEQ,  '1','0',trEQU,  "001",'1','0','0',"00","01"),  --teq=52
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --53
    (TNE,  '1','0',trNEQ,  "001",'1','0','0',"00","01"),  --tne=54
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --55
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --56
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --57
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --58
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --59
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --60
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --61
    (NIL,  '1','0',opNOP,  "001",'0','0','0',"00","00"),  --62
    (NOP,  '1','0',opNOP,  "001",'0','0','0',"00","00")   --63
    );

  -- fields of the register-immediate control table (opcode=1)
  --   i:     instr_type;       -- instruction
  --   wreg:  std_logic;        -- register write=0
  --   selB:  std_logic;        -- B ALU input, reg=0 ext=1
  --   br_t:  t_comparison;     -- comparison type: ltz,gez
  --   muxC:  reg3;             -- select result mem=0 ula=1 jr=2 *al(pc+8)=3
  --   c_sel: reg2              -- select destination reg rd=0 rt=1 31=2
  --   trap:  std_logic;        -- trap on compare
  --   PCsel: reg2;             -- PCmux 0=PC+4 1=beq 2=j 3=jr
  --   exp:  reg2              -- stage with exception 0=no,1=rf,2=ex,3=mm
  
  constant rimm_table : t_rimm_mem := (
  -- i    wreg selB br_t muxC  csel trap PCsel exp
    (BLTZ, '1','0',cLTZ, "001","00",'0',"01","00"),  --0bltz
    (BGEZ, '1','0',cGEZ, "001","00",'0',"01","00"),  --1bgez
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --2
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --3
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --4
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --5
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --6
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --7
    (TGEI, '1','1',tGEQ, "001","00",'1',"00","10"),  --8tgei
    (TGEIU,'1','1',tGEU, "001","00",'1',"00","10"),  --9tgeiu
    (TLTI, '1','1',tLTH, "001","00",'1',"00","10"),  --10tlti
    (TLTIU,'1','1',tLTU, "001","00",'1',"00","10"),  --11tltiu
    (TEQI, '1','1',tEQU, "001","00",'1',"00","10"),  --12teqi
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --13
    (TNEI, '1','1',tNEQ, "001","00",'1',"00","10"),  --14tnei
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --15
    (BLTZAL,'0','0',cLTZ,"011","10",'0',"01","00"),  --16bltzal
    (BGEZAL,'0','0',cGEZ,"011","10",'0',"01","00"),  --17bgezal
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --18
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --19
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --20
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --21
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --22
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --23
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --24
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --25
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --26
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --27
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --28
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --29
    (NIL,  '1','0',cNOP, "001","00",'0',"00","00"),  --30
    (NOP,  '1','0',cNOP, "001","00",'0',"00","00")   --31
    );

  -- Table 8-30 Config Register Field Descriptions, pg 101
  constant CONFIG0 : reg32 := (
    '1'&        -- M, Config1 implemented = 1
    b"000"&     -- K23, with MMU, kseg2,kseg3 coherency algorithm
    b"000"&     -- KU, with MMU, kuseg coherency algorithm
    b"000000000"& -- Impl, implementation dependent = 0
    '0'&        -- BE, little endian = 0
    b"00"&      -- AT, MIPS32 = 0
    b"001"&     -- AR, Release 2 = 1
    b"000"&     -- MT, MMU type = 0, none
    b"000"&     -- nil, always zero = 0
    '1'&        -- VI, Instruction Cache is virtual = 1
    b"000"      -- K0, Kseg0 coherency algorithm
    );

  -- Table 8-31 Config1 Register Field Descriptions, pg 103
  constant CONFIG1 : reg32 := (
    '0'&               -- M, Config2 implemented = 0
    b"000000"&         -- MMUsz, MMU entries minus 1
    IC_SETS_PER_WAY  & -- ICS, IC sets per way
    IC_LINE_SIZE     & -- ICL, IC line size
    IC_ASSOCIATIVITY & -- ICA, IC associativity
    DC_SETS_PER_WAY  & -- DCS, DC sets per way
    DC_LINE_SIZE     & -- DCL, DC line size = 3 16 bytes/line
    DC_ASSOCIATIVITY & -- DCA, DC associativity = 0 direct mapped
    '0'&        -- C2, No coprocessor 2 implemented = 0
    '0'&        -- MD, No MDMX ASE implemented = 0
    '0'&        -- PC, No performance counters implemented = 0
    '0'&        -- WR, No watch registers implemented = 0
    '0'&        -- CA, No code compression implemented = 0
    '0'&        -- EP, No EJTAG implemented = 0
    '0'         -- FP, No FPU implemented = 0
    );

   
  -- control pipeline registers ------------ 
  component reg_exp_IF_RF is
    port(clk, rst, ld: in  std_logic;
         IF_exp_type: in  exception_type;
         RF_exp_type: out exception_type;
         IF_PC:        in  std_logic_vector;
         RF_PC:        out std_logic_vector);
  end component reg_exp_IF_RF;

  component reg_exp_RF_EX is
    port(clk, rst, ld: in  std_logic;
         RF_can_trap:     in  std_logic_vector;
         EX_can_trap:     out std_logic_vector;
         RF_exception:    in  exception_type;
         EX_exception:    out exception_type;
         RF_trap_instr:   in  instr_type;
         EX_trap_instr:   out instr_type;
         RF_cop0_reg:     in  std_logic_vector;
         EX_cop0_reg:     out std_logic_vector;
         RF_cop0_sel:     in  std_logic_vector;
         EX_cop0_sel:     out std_logic_vector;
         RF_is_delayslot: in  std_logic;
         EX_is_delayslot: out std_logic;
         RF_PC:           in  std_logic_vector;
         EX_PC:           out std_logic_vector;
         RF_nmi:          in  std_logic;
         EX_nmi:          out std_logic;
         RF_interrupt:    in  std_logic;
         EX_interrupt:    out std_logic;
         RF_int_req:      in  std_logic_vector;
         EX_int_req:      out std_logic_vector;
         RF_tr_is_equal:  in  std_logic;
         EX_tr_is_equal:  out std_logic;
         RF_tr_less_than: in  std_logic;
         EX_tr_less_than: out std_logic);
  end component reg_exp_RF_EX;

  component reg_exp_EX_MM is
    port(clk, rst, ld:  in  std_logic;
         EX_can_trap:   in  std_logic_vector;
         MM_can_trap:   out std_logic_vector;
         EX_exp_type:  in  exception_type;
         MM_exp_type:  out exception_type;
         EX_PC:         in  std_logic_vector;
         MM_PC:         out std_logic_vector;
         EX_cop0_LLbit: in  std_logic;
         MM_cop0_LLbit: out std_logic;
         EX_cop0_a_c:   in  std_logic_vector;
         MM_cop0_a_c:   out std_logic_vector;
         EX_cop0_val:   in  std_logic_vector;
         MM_cop0_val:   out std_logic_vector;
         EX_trapped:    in  std_logic;
         MM_ex_trapped: out std_logic);
  end component reg_exp_EX_MM;

  component reg_exp_MM_WB is
    port(clk, rst, ld:  in  std_logic;
         MM_can_trap:   in  std_logic_vector;
         WB_can_trap:   out std_logic_vector;
         MM_exp_type:  in  exception_type;
         WB_exp_type:  out exception_type;
         MM_PC:         in  std_logic_vector;
         WB_PC:         out std_logic_vector;
         MM_cop0_LLbit: in  std_logic;
         WB_cop0_LLbit: out std_logic;
         MM_abort:      in  std_logic;
         WB_abort:      out std_logic;
         MM_cop0_a_c:   in  std_logic_vector;
         WB_cop0_a_c:   out std_logic_vector;
         MM_cop0_val:   in  std_logic_vector;
         WB_cop0_val:   out std_logic_vector);
  end component reg_exp_MM_WB;

  signal i_addr_error : std_logic;
 
  signal interrupt,EX_interrupt, exception_stall : std_logic;
  signal exception_taken, interrupt_taken, trap_taken : std_logic;
  signal nullify, nullify_EX, abort, MM_abort,WB_abort : std_logic;
  signal IF_exp_type,RF_exp_type,EX_exp_type,WB_exp_type: exception_type := exNOP;
  signal MM_exp_type, MM_exp_type_i, MM_addr_error, MM_exp_TLB : exception_type;
  signal trap_instr,EX_trap_instr: instr_type;
  signal RF_PC,EX_PC,MM_PC,WB_PC, LLaddr: reg32;
  signal EX_LLbit,MM_LLbit,WB_LLbit: std_logic;
  signal LL_update,LL_SC_abort,LL_SC_differ,EX_trapped,MM_ex_trapped: std_logic;
  signal int_req, EX_int_req: reg8;
  signal RF_nmi,EX_nmi : std_logic;
  signal can_trap,EX_can_trap,MM_can_trap,WB_can_trap: reg2;
  signal is_trap, tr_signed, tr_stall: std_logic;
  signal tr_is_equal,EX_tr_is_equal, tr_less_than,EX_tr_less_than: std_logic;
  signal tr_fwd_A, tr_fwd_B, tr_result : reg32;
  signal exp_IF_RF_ld,exp_RF_EX_ld,exp_EX_MM_ld,exp_MM_WB_ld: std_logic;
  signal update, not_stalled: std_logic;
  signal update_reg : reg5;
  signal status_update,epc_update,compare_update: std_logic;
  signal cause_update, disable_count, compare_set, compare_clr: std_logic;
  signal STATUSinp,STATUS, CAUSEinp,CAUSE, EPCinp,EPC : reg32;
  signal COUNT,COMPARE : reg32;
  signal count_eq_compare,count_update,count_enable : std_logic;
  signal exception,EX_exception,is_exception: exception_type := exNOP;
  signal ExcCode : reg5 := cop0code_NULL;
  signal exception_num, exception_dec : integer;       -- for debugging only
  signal next_instr_in_delay_slot,EX_is_delayslot : std_logic;
  signal cop0_sel, EX_cop0_sel, epc_source : reg3;
  signal cop0_reg,EX_cop0_reg : reg5;
  signal cop0_inp, RF_cop0_val,EX_cop0_val,MM_cop0_val,WB_cop0_val : reg32;
  signal EX_cop0_a_c,MM_cop0_a_c,WB_cop0_a_c : reg5;
  signal BadVAddr, BadVAddr_inp : reg32;
  signal BadVAddr_update, BadVAddr_source : std_logic;

  
  -- other components ------------ 
  
  component FFD is
    port(clk, rst, set, D : in std_logic; Q : out std_logic);
  end component FFD;

  component adder32 is
    port(A, B : in  std_logic_vector;
         C    : out std_logic_vector);
  end component adder32;

  component mf_alt_add_4 IS
    port(datab : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
         result : OUT STD_LOGIC_VECTOR (31 DOWNTO 0) );
  end component mf_alt_add_4;

  component mf_alt_adder IS
    port(dataa  : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
         datab  : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
         result : OUT STD_LOGIC_VECTOR (31 DOWNTO 0));
  end component mf_alt_adder;

  component subtr32 IS
  port(A,B : in  std_logic_vector (31 downto 0);
       C   : out std_logic_vector (31 downto 0);
       sgnd    : in  std_logic;
       ovfl,lt : out std_logic);
  end component subtr32;
  
  component reg_bank is
    port(wrclk, rdclk, wren: in  std_logic;
         a_rs, a_rt, a_rd:   in  std_logic_vector;
         C:                  in  std_logic_vector;
         A, B:               out std_logic_vector);
  end component reg_bank;
  
  component register32 is
    generic (INITIAL_VALUE: std_logic_vector);
    port(clk, rst, ld: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic_vector);
  end component register32;

  component counter32 is
    generic (INITIAL_VALUE: std_logic_vector);
    port(clk, rst, ld, en: in  std_logic;
         D:            in  std_logic_vector;
         Q:            out std_logic_vector);
  end component counter32;
  
  component alu is
    port(clk, rst: in  std_logic;
         A, B:     in  std_logic_vector;
         C:        out std_logic_vector;
         LO:       out std_logic_vector;
         HI:       out std_logic_vector;
         move_ok:  out std_logic;
         fun:      in  t_alu_fun;
         postn:    in  std_logic_vector;
         shamt:    in  std_logic_vector;
         ovfl:     out std_logic);
  end component alu;

  signal PC,PC_aligned : reg32;
  signal PCinp,PCinp_noexp, PCincd : reg32;
  signal instr_fetched : reg32;
  signal PCload, IF_RF_ld : std_logic;
  signal PCsel : reg2;
  signal exp_PCsel : reg3;

  signal rom_stall, iaVal, if_stalled, stalled : std_logic;
  signal ram_stall, daVal, mm_stalled : std_logic;
  signal br_target, br_addend, br_tgt_pl4, br_tgt_displ, j_target : reg32;
  signal RF_PCincd, RF_instruction : reg32;
  signal eq_fwd_A,eq_fwd_B : reg32;

  -- register fetch/read and instruction decode --  
  component reg_IF_RF is
    port(clk, rst, ld: in  std_logic;
         PCincd_d:     in  std_logic_vector;
         PCincd_q:     out std_logic_vector;
         instr:        in  std_logic_vector;
         RF_instr:     out std_logic_vector);
  end component reg_IF_RF;

  signal opcode, func: reg6;
  signal ctrl_word:  t_control_type;
  signal funct_word: t_function_type;
  signal rimm_word:  t_rimm_type;
  signal syscall_n : reg20;
  signal displ16: reg16;
  signal br_operand: reg32;
  signal br_opr: reg2;
  signal br_equal,br_negative,br_eq_zero: boolean;
  signal flush_RF_EX: boolean := FALSE;
  signal is_branch: std_logic;
  -- attribute BUFFERED of is_branch : signal is "HIGH_DRIVE";
  signal c_sel : reg2;
  
  -- execution and beyond --  
  signal RF_EX_ld, EX_MM_ld, MM_WB_ld: std_logic;
  signal a_rs,EX_a_rs, a_rt,EX_a_rt,MM_a_rt, a_rd: reg5;
  signal a_c,EX_a_c,MM_a_c,WB_a_c: reg5;
  signal move,EX_move,MM_move, is_load,EX_is_load : std_logic;
  signal muxC,EX_muxC,MM_muxC,WB_muxC: reg3;
  signal wreg,EX_wreg_pre,EX_wreg,MM_wreg_cond,MM_wreg,WB_wreg: std_logic;
  signal aVal,EX_aVal,EX_aVal_cond,MM_aVal: std_logic;
  signal wrmem,EX_wrmem,EX_wrmem_cond,MM_wrmem, m_sign_ext: std_logic;
  signal mem_t, EX_mem_t,MM_mem_t: reg4;
  signal WB_mem_t : reg2;
  -- attribute CLOCK_SIGNAL of MM_wrmem : signal is "no";
  -- attribute BUFFERED     of MM_wrmem : signal is "HIGH_DRIVE";

  signal alu_inp_A,alu_fwd_B,alu_inp_B : reg32;
  signal alu_move_ok, MM_alu_move_ok, ovfl,MM_ovfl : std_logic;
  
  signal selB,EX_selB:  std_logic;
  signal oper,EX_oper: t_alu_fun;
  signal EX_postn, shamt,EX_shamt: reg5;
  signal regs_A,EX_A,MM_A,WB_A, regs_B,EX_B,MM_B:reg32;
  signal displ32,EX_displ32: reg32;
  signal result,MM_result,WB_result,WB_C: reg32;
  signal pc_p8,EX_pc_p8,MM_pc_p8,WB_pc_p8 : reg32;
  signal HI,MM_HI,WB_HI, LO,MM_LO,WB_LO : reg32;

  -- data memory --
  signal rd_data_raw, rd_data, WB_rd_data, WB_mem_data: reg32;
  signal MM_B_data, WB_B_data: reg32;
  signal jr_stall, br_stall, fwd_lwlr, sw_stall : std_logic;
  -- attribute CLOCK_SIGNAL of rom_stall,ram_stall : signal is "no";
  -- attribute BUFFERED     of rom_stall,ram_stall : signal is "HIGH_DRIVE";
  signal fwd_mem, WB_addr2: reg2;


  component reg_RF_EX is
    port(clk, rst, ld: in  std_logic;
         selB:       in  std_logic;
         EX_selB:    out std_logic;
         oper:       in  t_alu_fun;
         EX_oper:    out t_alu_fun;
         a_rs:       in  std_logic_vector;
         EX_a_rs:    out std_logic_vector;
         a_rt:       in  std_logic_vector;
         EX_a_rt:    out std_logic_vector;
         a_c:        in  std_logic_vector;
         EX_a_c:     out std_logic_vector;
         wreg:       in  std_logic;
         EX_wreg:    out std_logic;
         muxC:       in  std_logic_vector;
         EX_muxC:    out std_logic_vector;
         move:       in  std_logic;
         EX_move:    out std_logic;
         postn:      in  std_logic_vector;
         EX_postn:   out std_logic_vector;
         shamt:      in  std_logic_vector;
         EX_shamt:   out std_logic_vector;
         aVal:       in  std_logic;
         EX_aVal:    out std_logic;
         wrmem:      in  std_logic;
         EX_wrmem:   out std_logic;
         mem_t:      in  std_logic_vector;
         EX_mem_t:   out std_logic_vector;
         is_load:    in  std_logic;
         EX_is_load: out std_logic;
         A:          in  std_logic_vector;
         EX_A:       out std_logic_vector;
         B:          in  std_logic_vector;
         EX_B:       out std_logic_vector;
         displ32:    in  std_logic_vector;
         EX_displ32: out std_logic_vector;
         pc_p8:      in  std_logic_vector;
         EX_pc_p8:   out std_logic_vector);
  end component reg_RF_EX;
      
  component reg_EX_MM is
    port(clk, rst, ld: in  std_logic;
         EX_a_rt:    in  std_logic_vector;
         MM_a_rt:    out std_logic_vector;
         EX_a_c:     in  std_logic_vector;
         MM_a_c:     out std_logic_vector;
         EX_wreg:    in  std_logic;
         MM_wreg:    out std_logic;
         EX_muxC:    in  std_logic_vector;
         MM_muxC:    out std_logic_vector;
         EX_aVal:    in  std_logic;
         MM_aVal:    out std_logic;
         EX_wrmem:   in  std_logic;
         MM_wrmem:   out std_logic;
         EX_mem_t:   in  std_logic_vector;
         MM_mem_t:   out std_logic_vector;
         EX_A:       in  std_logic_vector;
         MM_A:       out std_logic_vector;
         EX_B:       in  std_logic_vector;
         MM_B:       out std_logic_vector;
         EX_result:  in  std_logic_vector;
         MM_result:  out std_logic_vector;
         HI:         in  std_logic_vector;
         MM_HI:      out std_logic_vector;
         LO:         in  std_logic_vector;
         MM_LO:      out std_logic_vector;
         EX_alu_move_ok: in  std_logic;
         MM_alu_move_ok: out std_logic;
         EX_move:    in  std_logic;
         MM_move:    out std_logic;
         EX_pc_p8:   in  std_logic_vector;
         MM_pc_p8:   out std_logic_vector);
  end component reg_EX_MM;
  
  component reg_MM_WB is
    port(clk, rst, ld: in  std_logic;
         MM_a_c:     in  std_logic_vector;
         WB_a_c:     out std_logic_vector;
         MM_wreg:    in  std_logic;
         WB_wreg:    out std_logic;
         MM_muxC:    in  std_logic_vector;
         WB_muxC:    out std_logic_vector;
         MM_A:       in  std_logic_vector;
         WB_A:       out std_logic_vector;
         MM_result:  in  std_logic_vector;
         WB_result:  out std_logic_vector;
         MM_HI:      in  std_logic_vector;
         WB_HI:      out std_logic_vector;
         MM_LO:      in  std_logic_vector;
         WB_LO:      out std_logic_vector;
         rd_data:    in  std_logic_vector;
         WB_rd_data: out std_logic_vector;
         MM_B_data:  in  std_logic_vector;
         WB_B_data:  out std_logic_vector;
         MM_addr2:   in  std_logic_vector;
         WB_addr2:   out std_logic_vector;
         MM_oper:    in  std_logic_vector;
         WB_oper:    out std_logic_vector;
         MM_pc_p8:   in  std_logic_vector;
         WB_pc_p8:   out std_logic_vector);
  end component reg_MM_WB;

-- pipeline ===========================================================
begin

  -- INSTR_FETCH_STATE_MACHINE: instruction-bus control
  U_ifetch_stalled: FFD port map (clk => phi2, rst => rst, set => '1',
                                  D => stalled, Q => if_stalled);

  -- iaVal <= '1' when ((phi0 = '1' and if_stalled = '0')) else '0';
  
  i_aVal <= '0'; -- interface signal/port, always fetch new instruction
  iaVal  <= '0'; -- internal signal
  
  rom_stall <= not(iaVal) and not(i_wait);

  stalled <= ram_stall or rom_stall;
  not_stalled <= not(stalled);

-- end INSTR_FETCH_STATE_MACHINE --------------------------
  
 
  -- PROGRAM COUNTER AND INSTRUCTION FETCH ------------------

  PCload <= '1' when ( (rom_stall = '1') or (ram_stall = '1') or
                       (jr_stall = '1')  or (br_stall = '1')  or
                       (sw_stall = '1')  or (tr_stall = '1')  or
                       (exception_stall = '1') )
            else '0';
  IF_RF_ld <= '1' when ( (rom_stall = '1') or (ram_stall = '1') or
                         (jr_stall = '1')  or (br_stall = '1')  or
                         (sw_stall = '1')  or (tr_stall = '1')  or
                         (exception_stall = '1') )
              else '0';
  RF_EX_ld <= rom_stall or ram_stall; -- or exception_stall;
  EX_MM_ld <= rom_stall or ram_stall;
  MM_WB_ld <= rom_stall or ram_stall;

  
  exp_IF_RF_ld <= '1' when ( (rom_stall = '1') or (ram_stall = '1') or
                              (jr_stall = '1')  or (br_stall = '1')  or
                              (sw_stall = '1')  or (tr_stall = '1')  or
                              (exception_stall = '1') )
                   else '0';
  exp_RF_EX_ld <= rom_stall or ram_stall; -- or exception_stall;
  exp_EX_MM_ld <= rom_stall or ram_stall;
  exp_MM_WB_ld <= rom_stall or ram_stall;


  with PCsel select
  PCinp_noExp <= PCincd    when b"00",     -- next instruction
                  br_target when b"01",     -- taken branch
                  j_target  when b"10",     -- jump
                  eq_fwd_A  when b"11",     -- jump register regs_A
                  (others => 'X') when others;

  with exp_PCsel select
    PCinp <= PCinp_noExp     when b"000",  -- no exception
             EPC              when b"001",  -- ERET
             x_EXCEPTION_0180 when b"010",  -- single exception handler
             x_EXCEPTION_0200 when b"011",  -- separate interrupt handler
             x_EXCEPTION_0000 when b"100",  -- NMI or soft-reset handler
             (others => 'X')  when others;

  IF_exp_type <= IFaddressError when PC(1 downto 0) /= b"00" else
                  exNOP;
    
  PIPE_PC: register32 generic map (x_BASE_ADDR)
    port map (clk, rst, PCload, PCinp, PC);

  PC_aligned <= PC(31 downto 2) & b"00";
  
  -- U_INCPC: adder32 port map (x"00000004", PC_aligned, PCincd);
  -- PCincd <= std_logic_vector( 4 + signed(PC_aligned) );
  U_INCPC: mf_alt_add_4 PORT MAP( datab => PC_aligned, result => PCincd );
  
  i_addr <= PC_aligned;    -- fetch instruction from aligned address

  abort <= MM_abort or WB_abort;
  
  instr_fetched <= instr when (nullify = '0' and abort = '0'
                               and PC(1 downto 0) = b"00") else
                   NULL_INSTRUCTION; -- x"fc000000";

  
  PIPE_IF_RF: reg_IF_RF
    port map (clk,rst, IF_RF_ld, PCincd, RF_PCincd,
              instr_fetched, RF_instruction);


  -- INSTRUCTION DECODE AND REGISTER FETCH -----------------

  opcode <= RF_instruction(31 downto 26) when (nullify = '0' and abort = '0') else
            NULL_INSTRUCTION (31 downto 26);
  
  a_rs      <= RF_instruction(25 downto 21);
  a_rt      <= RF_instruction(20 downto 16);
  a_rd      <= RF_instruction(15 downto 11);
  shamt     <= RF_instruction(10 downto  6);
  func      <= RF_instruction( 5 downto  0);
  displ16   <= RF_instruction(15 downto  0);
  syscall_n <= RF_instruction(25 downto  6);

  
  ctrl_word   <= ctrl_table( to_integer(unsigned(opcode)) );
  
  funct_word  <=
    func_table( to_integer(unsigned(func)) ) when opcode = b"000000" else
    func_table( 63 );                   -- empty and void table entry
                 
  rimm_word   <= 
    rimm_table( to_integer(unsigned(a_rt)) ) when opcode = b"000001" else
    rimm_table( 31 );                   -- empty and void table entry

  is_branch <= '1' when ((ctrl_word.br_t /= cNOP)
                         or((rimm_word.br_t /= cNOP)and(rimm_word.trap='0')))
                 else '0';

  is_trap <= '1' when ((funct_word.trap = '1')or(rimm_word.trap = '1'))
                 else '0';
  
  next_instr_in_delay_slot <= '1' when ((ctrl_word.PCsel  /= "00") or
                                        (funct_word.PCsel /= "00") or
                                        (rimm_word.PCsel  /= "00"))
                              else '0';
  
  
  RF_STOP_SIMULATION: process (rst, phi2, opcode, func,
                               ctrl_word, funct_word, rimm_word,
                               RF_PC, exception, syscall_n)
  begin
    
    if rst = '1' and phi2 = '1' then

      -- normal end of simulation, instruction "wait 0"
      assert not(exception = exWAIT and syscall_n = x"80000")
        report LF & "mips BREAKPOINT at PC="& SLV32HEX(RF_PC) &
        " opc="& SLV2STR(opcode) & " fun=" & SLV2STR(func) &
        " brk=" & SLV2STR(syscall_n) & 
        LF & "SIMULATION ENDED (correctly?) AT exit();"
        severity failure;

      -- simulation aborted by instruction "wait N"
      assert not(exception = exWAIT and syscall_n /= x"80000")
        report LF & "INVALID REFERENCE at PC="& SLV32HEX(EPC) &
        " opc="& SLV2STR(opcode) & " fun=" & SLV2STR(func) &
        " cause(6..2)=" & SLV2STR(RF_instruction(10 downto 6)) & 
        LF & "SIMULATION ABORTED AT EXCEPTION HANDLER;"
        severity failure;

      -- abort on invalid/unimplemented opcodes
      if opcode = b"000000" and funct_word.i = NIL then
        assert (1=0)
          report LF & "INVALID OPCODE at PC="& SLV32HEX(RF_PC) &
          " opc="& SLV2STR(opcode) & " instr=" & SLV32HEX(RF_instruction) &
          LF & "SIMULATION ABORTED"
          severity failure;
      elsif opcode = b"000001" and rimm_word.i = NIL then
        assert (1=0)
          report LF & "INVALID OPCODE at PC="& SLV32HEX(RF_PC) &
          " opc="& SLV2STR(opcode) & " instr=" & SLV32HEX(RF_instruction) &
          LF & "SIMULATION ABORTED"
          severity failure;
      elsif ctrl_word.i = NIL then
        assert (1=0)
          report LF & "INVALID OPCODE at PC="& SLV32HEX(RF_PC) &
          " opc="& SLV2STR(opcode) & " instr=" & SLV32HEX(RF_instruction) &
          LF & "SIMULATION ABORTED"
          severity failure;
      end if;
        
    end if;
  end process RF_STOP_SIMULATION;

  
  move <= funct_word.move when opcode = b"000000" else '0';
  
  U_regs: reg_bank
    port map (clk, phi2, WB_wreg, a_rs,a_rt, WB_a_c,WB_C, regs_A,regs_B);

  
  -- U_PC_plus_8: adder32 port map (x"00000004", RF_PCincd, pc_p8); -- (PC+4)+4
  -- pc_p8 <= std_logic_vector( 4 + signed(RF_PCincd) );   -- (PC+4)+4
  U_PC_plus_8: mf_alt_add_4 PORT MAP( datab => RF_PCincd, result => pc_p8 );

  
  displ32 <= x"FFFF" & displ16 when
                         (displ16(15) = '1' and ctrl_word.extS = '1') else
             x"0000" & displ16;
  
  j_target <= RF_PCincd(31 downto 28) & RF_instruction(25 downto 0) & b"00";

  RF_JR_STALL: process (funct_word,a_rs,EX_a_c,MM_a_c,EX_wreg,MM_wreg)
  begin
    if ( (funct_word.PCsel = b"11")and          -- load-delay slot
            (EX_a_c /= a_rs)and(EX_wreg = '0')and
            (MM_a_c =  a_rs)and(MM_wreg = '0')and(MM_a_c /= b"00000") ) then
      jr_stall <= '1';
    elsif ( (funct_word.PCsel = b"11")and       -- ALU hazard
         (EX_a_c =  a_rs)and(EX_wreg = '0')and(EX_a_c /= b"00000") ) then
      jr_stall <= '1';
    else
      jr_stall <= '0';
    end if; 
  end process RF_JR_STALL;

  
  RF_SW_STALL: process (ctrl_word,a_rs,EX_a_c,EX_wreg,EX_is_load)
    variable is_store : boolean := false;
  begin
    case ctrl_word.i is
      when LB | LH | LWL | LW | LBU | LHU | LWR =>
        is_load <= '1';
        is_store := FALSE;
      when SB | SH | SW  =>
        is_store := TRUE;
        is_load <= '0';
      when others => is_load <= '0'; is_store := FALSE;
    end case;
    if ( is_store and (EX_is_load = '1') and
         (EX_a_c =  a_rs)and(EX_wreg = '0')and(EX_a_c /= b"00000") ) then
      sw_stall <= '1';
    else
      sw_stall <= '0';
    end if; 
  end process RF_SW_STALL;
  

  RF_FORWARDING_BRANCH: process (a_rs,a_rt,EX_wreg,EX_a_c,MM_wreg,MM_a_c,
                                MM_aVal,MM_result,regs_A,regs_B,is_branch)
  begin
    br_stall <= '0';

    if ( (is_branch = '1') and          -- forward_A:
         (EX_wreg = '0') and (EX_a_c = a_rs) and (EX_a_c /= b"00000") ) then
      br_stall <= '1';
      eq_fwd_A <= regs_A;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rs) and (MM_a_c /= b"00000")
           and (MM_aVal = '0')) then    -- LW load-delay slot
      if (is_branch = '1') then
        br_stall <= '1';
      end if;
      eq_fwd_A <= regs_A;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rs) and (MM_a_c /= b"00000")
           and (MM_aVal = '1')) then    -- non-LW
      eq_fwd_A <= MM_result;
    else
      eq_fwd_A <= regs_A;
    end if;

    if ( (is_branch = '1') and          -- forward_B:
         (EX_wreg = '0') and (EX_a_c = a_rt) and (EX_a_c /= b"00000") ) then
      br_stall <= '1';
      eq_fwd_B <= regs_B;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rt) and (MM_a_c /= b"00000")
           and (MM_aVal = '0')) then    -- LW load-delay slot
      if (is_branch = '1') then
        br_stall <= '1';
      end if;
      eq_fwd_B <= regs_B;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rt) and (MM_a_c /= b"00000")
           and (MM_aVal = '1')) then    -- non-LW
      eq_fwd_B <= MM_result;
    else
      eq_fwd_B <= regs_B;
    end if;
  end process RF_FORWARDING_BRANCH;

  br_equal    <= (eq_fwd_A = eq_fwd_B);
  br_negative <= (eq_fwd_A(31) = '1');
  br_eq_zero  <= (eq_fwd_A = x"00000000");
  

  RF_BR_tgt_select: process (br_equal,br_negative,br_eq_zero,
                        ctrl_word,rimm_word) 
    variable branch_type, regimm_br_type : t_comparison;
    variable i_br_opr : reg2;
  begin
    branch_type    := ctrl_word.br_t;
    regimm_br_type := rimm_word.br_t;

    i_br_opr := b"01";          -- assume not taken, PC+4 + 4 (delay slot)
    case branch_type is
      when cNOP =>              -- no branch, PC+4
        i_br_opr := b"00";      -- x"00000000";
      when cEQU =>              -- beq
        if br_equal then i_br_opr := b"10";  -- br_target;
        end if;
      when cNEQ =>              -- bne
        if (not br_equal) then i_br_opr := b"10";  -- br_target;
        end if;
      when cLEZ =>
        if (br_negative or br_eq_zero) then i_br_opr := b"10";  -- br_target;
        end if;
      when cGTZ =>
        if not(br_negative or br_eq_zero) then i_br_opr := b"10";  -- br_tgt;
        end if;
      when cOTH =>              -- bltz,blez,bgtz,bgez
        case regimm_br_type is
          when cLTZ =>
            if (br_negative) then i_br_opr := b"10";  -- br_target;
            end if;
          when cGEZ =>
            if (not br_negative) then i_br_opr := b"10";  -- br_target;
            end if;
          when others => 
            i_br_opr := b"00";    -- x"00000000";
        end case;
      when others => 
        i_br_opr := b"00";        -- x"00000000";
    end case;
    br_opr <= i_br_opr;
    -- assert false report
    --   "branch_add32 A="& SLV32HEX(RF_PCincd) &" B="& SLV32HEX(br_operand) &
    --   " A+B="& SLV32HEX(br_target); -- DEBUG
  end process RF_BR_tgt_select;

  -- U_BR_ADDER: adder32 port map (RF_PCincd, br_operand, br_target);
  -- br_target <= std_logic_vector( signed(RF_PCincd) + signed(br_operand) );

  -- branch target computation is in the citical path; add early, select late
  br_addend <= displ32(29 downto 0) & b"00";
  U_BR_tgt_pl_4:     mf_alt_add_4 port map (RF_PCincd, br_tgt_pl4 );
  U_BR_tgt_pl_displ: mf_alt_adder port map (RF_PCincd, br_addend, br_tgt_displ);
    
  with br_opr select
    br_target <= br_tgt_pl4    when b"01",
                 br_tgt_displ  when b"10",
                 RF_PCincd     when others;
  
  
  RF_DECODE_FUNCT: process (opcode,IF_RF_ld,ctrl_word,funct_word,rimm_word,
                            func,shamt, a_rs,a_rd, STATUS, MM_abort,
                            IF_exp_type,RF_exp_type,MM_exp_type)
    variable i_wreg : std_logic;
    variable i_csel : reg2;
    variable i_oper : t_alu_fun := opNOP;
    variable i_exception : exception_type;
    variable i_trap : instr_type;
    variable i_cop0_reg : reg5;
    variable i_cop0_sel : reg3;
  begin

    i_wreg := '1';
    i_exception := exNOP;
    i_oper := opNOP;
    i_csel := "00";
    i_trap := NOP;
    i_cop0_reg := b"00000";
    i_cop0_sel := b"000";

    case opcode is
      when b"000000" =>                 -- ALU
        i_wreg := funct_word.wreg;
        selB   <= funct_word.selB;
        i_oper := funct_word.oper;
        muxC   <= funct_word.muxC;
        i_csel := ctrl_word.c_sel;
        PCsel  <= funct_word.PCsel;
        i_trap := funct_word.i;
        if (funct_word.trap = '1') then  -- traps
          case funct_word.i is
            when SYSCALL => i_exception := exSYSCALL;
            when BREAK   => i_exception := exBREAK;
            when others  => i_exception := exTRAP;
          end case;
        end if;

      when b"000001" =>                 -- register immediate
        i_wreg := rimm_word.wreg;
        selB   <= rimm_word.selB;
        muxC   <= rimm_word.muxC;
        i_csel := rimm_word.c_sel;
        PCsel  <= rimm_word.PCsel;
        i_trap := rimm_word.i;
        i_oper := opNOP;                -- no ALU operation        

        if (rimm_word.trap = '1') then  -- traps
          i_exception := exTRAP;
        end if;

      when b"010000" =>                 -- COP-0
        i_cop0_reg := a_rd;
        i_cop0_sel := func(2 downto 0);
        case a_rs is
          when b"00100" =>              -- MTC0
            i_exception := exMTC0;
          when b"00000" =>              -- MFC0
            i_exception := exMFC0;
            i_wreg     := '0';
          when b"10000" =>              -- ERET
            case func is
              when b"011000" => i_exception := exERET;
              when b"100000" => i_exception := exWAIT;
              when others =>    i_exception := exRESV_INSTR;
            end case;
          when b"01011" =>              -- EI and DI
            case func is
              when b"100000" =>    -- EI;
              i_exception := exEI;
              i_wreg := '0';
              when b"000000" =>    -- DI;
              i_exception := exDI;
              i_wreg := '0';
              when others => i_exception := exRESV_INSTR;
            end case;
          when others => i_exception := exRESV_INSTR;
        end case;
        selB   <= '0';
        i_oper := opNOP;
        muxC   <= ctrl_word.muxC;
        i_csel := ctrl_word.c_sel;
        PCsel  <= ctrl_word.PCsel;

      when b"011111" =>                 -- special3
        case func is
          when b"100000" =>             -- BSHFL 
            i_csel := ctrl_word.c_sel;
            case shamt is
              when b"00010" =>          -- word swap bytes within halfwords
                i_oper := opSWAP;
              when b"10000" =>          -- sign-extend byte
                i_oper := opSEB;
              when b"11000" =>          -- sign-extend halfword
                i_oper := opSEH;
              when  others =>
                i_oper := opNOP;
            end case;
          when b"000000" =>             -- extract bit field
            i_csel := b"01";             -- dest = rt
            i_oper := opEXT;
          when b"000100" =>             -- insert bit field
            i_csel := b"01";            -- dest = rt
            i_oper := opINS;
          when others => i_exception := exRESV_INSTR;
        end case;
        i_wreg := ctrl_word.wreg;
        selB   <= ctrl_word.selB;
        muxC   <= ctrl_word.muxC;
        PCsel  <= ctrl_word.PCsel;

      when others =>
        case opcode is
          when b"110000" => i_exception := exLL;  -- not REALLY exceptions
          when b"111000" => i_exception := exSC;
          when b"111111" =>
            if MM_abort = '1' then
              i_exception := MM_exp_type;
            else
              i_exception := RF_exp_type;      -- delayed by pipe
            end if;
          when others    => null; -- i_exception := exRESV_INSTR;
        end case;
        i_wreg := ctrl_word.wreg;
        selB   <= ctrl_word.selB;
        i_oper := ctrl_word.oper;
        muxC   <= ctrl_word.muxC;
        i_csel := ctrl_word.c_sel;
        PCsel  <= ctrl_word.PCsel;
    end case;
    oper  <= i_oper;
    c_sel <= i_csel;
    trap_instr <= i_trap;
    cop0_reg   <= i_cop0_reg;
    cop0_sel   <= i_cop0_sel;

    if IF_RF_ld = '1' then              -- bubble (OR flush_RF_EX)
      wreg      <= '1';
      aVal      <= '1';
      wrmem     <= '1';
      exception <= exNOP;
    else
      wreg      <= i_wreg;
      aVal      <= ctrl_word.aVal;
      wrmem     <= ctrl_word.wmem;
      exception <= i_exception;
    end if;
  end process RF_DECODE_FUNCT;

  exception_dec <= exception_type'pos(exception);  -- debugging only
 
  can_trap <= ctrl_word.exp or funct_word.exp or rimm_word.exp;
  
  RF_DECODE_MEM_REF: process (ctrl_word)
    variable i_type : reg4;
    -- bit3: LWL,LWR=1, bit2: signed=1, bits10:xx,byte,half,word
  begin
    case ctrl_word.i is
      when LB        => i_type := b"0101";  -- signed, byte (sign extend)
      when LH        => i_type := b"0110";  -- signed, half-word
      when LW | LL   => i_type := b"0011";  -- word
      when LBU       => i_type := b"0001";  -- unsigned, byte (zero extend)
      when LHU       => i_type := b"0010";  -- unsigned, half-word
      when SB        => i_type := b"0001";
      when SH        => i_type := b"0010";
      when SW | SC   => i_type := b"0011";
      when LWL       => i_type := b"1011";  -- unaligned LOADS
      when LWR       => i_type := b"1111";  -- unaligned LOADS
      when others    => i_type := b"0000";
    end case;
    mem_t <= i_type;
  end process RF_DECODE_MEM_REF;

  with c_sel select                     -- select destination register
    a_c <= a_rd when b"00",  -- type-R
           a_rt when b"01",  -- type-I
           b"11111" when b"10", -- jal
           b"00000" when others;

  PIPE_RF_EX: reg_RF_EX
    port map (clk,rst, RF_EX_ld, selB,EX_selB, oper,EX_oper,
              a_rs,EX_a_rs, a_rt,EX_a_rt, a_c,EX_a_c,
              wreg,EX_wreg_pre, muxC,EX_muxC, move,EX_move,
              a_rd,EX_postn, shamt,EX_shamt, aVal,EX_aVal,
              wrmem,EX_wrmem, mem_t,EX_mem_t, is_load,EX_is_load, 
              regs_A,EX_A, regs_B,EX_B, displ32,EX_displ32,
              pc_p8,EX_pc_p8);


  -- EXECUTION ---------------------------------------------
  EX_FORWARDING_ALU: process (EX_a_rs,EX_a_rt,EX_a_c,
                              MM_a_c,MM_wreg,WB_a_c,WB_wreg,
                              EX_A,EX_B,MM_result,WB_C)
    variable i_A,i_B : reg32;
  begin
    FORWARD_A:
    if ((MM_wreg = '0')and(MM_a_c /= b"00000")and(MM_a_c = EX_a_rs)) then
      i_A := MM_result;
    elsif ((WB_wreg = '0')and(WB_a_c /= b"00000")and(WB_a_c = EX_a_rs)) then
      i_A := WB_C;
    else
      i_A := EX_A;
    end if;
    alu_inp_A <= i_A;
    -- assert false report -- DEBUG
    --   "FWD_A: alu_A="&SLV32HEX(alu_inp_A)&" alu_B="&SLV32HEX(alu_fwd_B);

    FORWARD_B:
    if ((MM_wreg = '0')and(MM_a_c /= b"00000")and(MM_a_c = EX_a_rt)) then
      i_B := MM_result;
    elsif ((WB_wreg = '0')and(WB_a_c /= b"00000")and(WB_a_c = EX_a_rt)) then
      i_B := WB_C;
    else
      i_B := EX_B;
    end if;
    alu_fwd_B <= i_B;
    -- assert false report -- DEBUG
    --   "FWD_B: alu_A="&SLV32HEX(alu_inp_A)&" alu_B="&SLV32HEX(alu_fwd_B);
  end process EX_FORWARDING_ALU;
  
  alu_inp_B <= alu_fwd_B when (EX_selB = '0') else EX_displ32;

  U_ALU: alu port map(clk,rst,
                      alu_inp_A, alu_inp_B, result, LO, HI,
                      alu_move_ok, EX_oper,EX_postn,EX_shamt, ovfl);

  EX_wreg <= EX_wreg_pre                -- movz,movn, move/DO_NOT move
             or nullify_EX              -- abort wr if prev excep in EX
             or abort;                  -- abort write if exception in MEM

  EX_wrmem_cond <= EX_wrmem
                   or nullify_EX        -- abort write if exception in EX
                   or LL_SC_abort       -- abort write if SC fails
                   or abort;            -- abort write if exception in MEM

  EX_aVal_cond <= EX_aVal
                  or nullify_EX         -- abort ref if previous excep in EX
                  or abort;             -- abort ref if exception in MEM
  
  -- ----------------------------------------------------------------------
  PIPE_EX_MM: reg_EX_MM
    port map (clk,rst, EX_MM_ld,
              EX_a_rt,MM_a_rt, EX_a_c,MM_a_c, EX_wreg,MM_wreg,
              EX_muxC,MM_muxC, EX_aVal_cond,MM_aVal, EX_wrmem_cond,MM_wrmem,
              EX_mem_t,MM_mem_t,
              EX_A,MM_A, alu_fwd_B,MM_B,
              result,MM_result, HI,MM_HI, LO,MM_LO,
              alu_move_ok,MM_alu_move_ok, EX_move,MM_move,
              EX_pc_p8,MM_pc_p8);


  -- MEMORY ---------------------------------------------------------------

  -- DATA_BUS_STATE_MACHINE: data-bus control
  U_dmem_stalled: FFD port map (clk => phi2, rst => rst, set => '1',
                                D => stalled, Q => mm_stalled);

  d_aVal <= MM_aVal;  -- interface signal/port
  daVal  <= MM_aVal;  -- internal signal
  
  ram_stall <= not(daVal) and not(d_wait);
  -- end DATA_BUS_STATE_MACHINE -------------------------------------
 
  wr <= MM_wrmem or abort;
  
  rd_data_raw <= data_inp when (MM_wrmem = '1' and MM_aVal = '0') else
                 (others => 'X');
  
  MM_MEM_INTERFACE: process(MM_mem_t,MM_aVal,MM_wrmem, MM_result, rd_data_raw)
    variable bytes_read : reg32;
    variable i_byte_sel : reg4;
    variable i_byte : reg8;
    variable i_half : reg16;
    constant c_24_ones  : reg24 := b"111111111111111111111111";
    constant c_24_zeros : reg24 := b"000000000000000000000000";
    constant c_16_ones  : reg16 := b"1111111111111111";
    constant c_16_zeros : reg16 := b"0000000000000000";
  begin

    MM_addr_error <= exNOP;

    case MM_mem_t(1 downto 0) is  -- 10:xx,by,hf,wd
      when b"11" =>
        i_byte_sel := b"1111";              -- LW, SW, LWL, LWR
        bytes_read := rd_data_raw;
        d_addr     <= MM_result(31 downto 2) & b"00";   -- align reference
        
        if ( MM_mem_t(3) = '0' and          -- normal LOAD, not LWL,LWR
             MM_aVal = '0' and MM_result(1 downto 0) /= b"00" ) then
          if MM_wrmem = '1' then
            MM_addr_error <= MMaddressErrorLD;
          else
            MM_addr_error <= MMaddressErrorST;
          end if;
        -- else
        --   MM_addr_error <= exNOP;
        end if;

      when b"10" =>
        d_addr     <= MM_result(31 downto 1) & '0' ;    -- align reference
        if MM_result(1) = '0' then                      -- LH*, SH
          i_byte_sel := b"0011";
          i_half     := rd_data_raw(15 downto 0);
        else
          i_byte_sel := b"1100";
          i_half     := rd_data_raw(31 downto 16);
        end if;
        if MM_mem_t(2) = '1' and i_half(15) = '1' then  -- mem_t(2):signed=1
          bytes_read := c_16_ones  & i_half;
        else
          bytes_read := c_16_zeros & i_half;
        end if;

        if MM_aVal = '0' and MM_result(0) /= '0' then
          if MM_wrmem = '1' then
            MM_addr_error <= MMaddressErrorLD;
          else
            MM_addr_error <= MMaddressErrorST;
          end if;
        -- else
        --   MM_addr_error <= exNOP;
        end if;
        
      when b"01" =>                                     -- LB*, SB
        d_addr     <= MM_result;
        case MM_result(1 downto 0) is
          when b"00"  => i_byte_sel := b"0001";
                         i_byte     := rd_data_raw(7  downto  0);
          when b"01"  => i_byte_sel := b"0010";
                         i_byte     := rd_data_raw(15 downto  8);
          when b"10"  => i_byte_sel := b"0100";
                         i_byte     := rd_data_raw(23 downto 16);
          when others => i_byte_sel := b"1000";
                         i_byte     := rd_data_raw(31 downto 24);
        end case;
        if MM_mem_t(2) = '1' and i_byte(7) = '1' then -- mem_t(2):signed=1
          bytes_read := c_24_ones  & i_byte;
        else
          bytes_read := c_24_zeros & i_byte;
        end if;
        -- MM_addr_error <= exNOP;
        
      when others =>
        d_addr     <= "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";  -- MM_result;
        i_byte_sel := b"0000";
        bytes_read := (others => 'X');
        -- MM_addr_error <= exNOP;

    end case;

    b_sel    <= i_byte_sel;
    rd_data  <= bytes_read;

    -- assert MM_addr_error = exNOP  -- DEBUG
    --   report "SIMULATION ERROR -- data addressing error: " &
    --   integer'image(exception_type'pos(MM_addr_error)) &
    --   " at address: " & SLV32HEX(MM_result)
    --   severity error;

  end process MM_MEM_INTERFACE;

  -- forwarding for LW -> SW 
  MM_FORWARDING_MEM: process (MM_aVal,MM_wrmem,MM_a_rt,WB_a_c,WB_wreg,WB_C,MM_B)
    variable f_m: reg2;
    variable i_data : reg32;
  begin
    FORWARD_M: if ( (MM_wrmem = '0') and (MM_aVal = '0') )
    then
      if ( (MM_a_rt = WB_a_c) and (WB_wreg = '0') and (WB_a_c /= b"00000")) then
        f_m    := "01";                 -- forward from WB
        i_data := WB_C;
      else
        f_m    := "00";                 -- not forwarding
        i_data := MM_B;
      end if;
    else
      f_m    := "11";                   -- not a write, (others=>'Z')
      i_data := (others => 'X');
    end if;
    fwd_mem  <= f_m;                    -- for debugging
    data_out <= i_data;
  end process MM_FORWARDING_MEM;

  -- forwarding for LWL, LWR
  MM_FWD_LWLR: process (MM_aVal,MM_wreg_cond,MM_a_rt,WB_a_c,WB_wreg,WB_C,MM_B)
    variable f_m: std_logic;
    variable i_data : reg32;
  begin
    FORWARD_M: if ( (MM_wreg_cond = '0') and (MM_aVal = '0') and
                    (MM_a_rt = WB_a_c) and (WB_wreg = '0') and
                    (WB_a_c /= b"00000") ) then
      f_m    := '1';                  -- forward from WB
      i_data := WB_C;
    else
      f_m    := '0';                  -- not forwarding
      i_data := MM_B;
    end if;
    fwd_lwlr <= f_m;                  -- for debugging
    MM_B_data <= i_data;
  end process MM_FWD_LWLR;

  
  MM_wreg_cond <= '1' when ((ram_stall = '1')
                            or (abort = '1')    -- abort write if exptn in MEM
                            or (MM_move = '1' and MM_alu_move_ok = '0'))
                  else MM_wreg;

  PIPE_MM_WB: reg_MM_WB
    port map (clk,rst, MM_WB_ld, 
              MM_a_c,WB_a_c, MM_wreg_cond,WB_wreg, MM_muxC,WB_muxC,
              MM_A,WB_A, MM_result,WB_result, MM_HI,WB_HI,MM_LO,WB_LO,
              rd_data,WB_rd_data, MM_B_data,WB_B_data,
              MM_result(1 downto 0),WB_addr2, MM_mem_t(3 downto 2),WB_mem_t,
              MM_pc_p8,WB_pc_p8);

  -- merge unaligned loads  LWL,LWR
  mergeLOAD: process (WB_rd_data, WB_B_data, WB_addr2, WB_mem_t)
    variable mem, reg, res : reg32;
  begin
    mem := WB_rd_data;
    reg := WB_B_data;

    case WB_mem_t is
         
      when "10" =>   -- LWL
        case WB_addr2 is
          when "00" =>
            res := mem( 7 downto  0) & reg(23 downto 0);
          when "01" =>
            res := mem(15 downto  0) & reg(15 downto 0);
          when "10" =>
            res := mem(23 downto  0) & reg( 7 downto 0);
          when others =>
            res := mem;
        end case;

      when "11" =>   -- LWR
        case WB_addr2 is
          when "01" =>
            res := reg(31 downto 24) & mem(31 downto  8);
          when "10" =>
            res := reg(31 downto 16) & mem(31 downto 16);
          when "11" =>
            res := reg(31 downto  8) & mem(31 downto 24);
          when others =>
            res := mem;
        end case;

      when others =>  -- normal LOAD
        res := mem;
    end case;
    WB_mem_data <= res;
  end process mergeLOAD;

      
  with WB_muxC select WB_C <=
    WB_mem_data  when b"000",           -- from memory
    WB_result    when b"001",           -- from ALU
    WB_A         when b"010",           -- A, for jr
    WB_pc_p8     when b"011",           -- PC+8 for jal
    WB_HI        when b"100",           -- MFHI
    WB_LO        when b"101",           -- MFLO
    WB_cop0_val  when b"110",           -- from COP0 registers
    (x"0000000" & b"000" & WB_LLbit) when b"111",  -- from LLbit
    x"00000000"  when others;           -- invalid selection

  -- IF instruction fetch ---------------------------------------------
  
  PIPE_exp_IF_RF: reg_exp_IF_RF
    port map (clk, rst, exp_IF_RF_ld,
              IF_exp_type,RF_exp_type, PC,RF_PC);

  -- RF decode & register fetch ---------------------------------------------

  RF_nmi     <= nmi;
  int_req(7) <= (irq(5) or count_eq_compare);
  int_req(6) <= irq(4);
  int_req(5) <= irq(3);
  int_req(4) <= irq(2);
  int_req(3) <= irq(1);
  int_req(2) <= irq(0);
  int_req(1) <= CAUSE(CAUSE_IP1);
  int_req(0) <= CAUSE(CAUSE_IP0);

  interrupt <= int_req(7) or int_req(6) or int_req(5) or int_req(4) or
               int_req(3) or int_req(2) or int_req(1) or int_req(0);

  tr_signed <= '0' when ((funct_word.trap = '1' and
                          ((funct_word.oper = trGEU)or(funct_word.oper = trLTU)))
                         or
                         (rimm_word.trap = '1' and
                          ((rimm_word.br_t = tGEU)or(rimm_word.br_t = tLTU))))
               else '1';
  
  tr_is_equal <= '1' when (tr_fwd_A = tr_fwd_B) else '0';

  U_COMP_TRAP: subtr32
    port map (tr_fwd_A, tr_fwd_B, tr_result, tr_signed, open, tr_less_than);
  

  RF_FORWARDING_TRAPS: process (a_rs,a_rt,rimm_word,displ32,
                                EX_wreg,EX_a_c,MM_wreg,MM_a_c,
                                MM_aVal,MM_result,regs_A,regs_B,is_trap)
  begin
    tr_stall <= '0';

    if ( (is_trap = '1') and          -- forward_A:
         (EX_wreg = '0') and (EX_a_c = a_rs) and (EX_a_c /= b"00000") ) then
      tr_stall <= '1';
      tr_fwd_A <= regs_A;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rs) and (MM_a_c /= b"00000")
           and (MM_aVal = '0')) then    -- LW load-delay slot
      if (is_trap = '1') then
        tr_stall <= '1';
      end if;
      tr_fwd_A <= regs_A;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rs) and (MM_a_c /= b"00000")
           and (MM_aVal = '1')) then    -- non-LW
      tr_fwd_A <= MM_result;
    else
      tr_fwd_A <= regs_A;
    end if;

    if ( (is_trap = '1') and (rimm_word.selB = '1') ) then -- from immediate
         tr_fwd_B <= displ32;
    elsif ( (is_trap = '1') and          -- forward_B:
         (EX_wreg = '0') and (EX_a_c = a_rt) and (EX_a_c /= b"00000") ) then
      tr_stall <= '1';
      tr_fwd_B <= regs_B;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rt) and (MM_a_c /= b"00000")
           and (MM_aVal = '0')) then    -- LW load-delay slot
      if (is_trap = '1') then
        tr_stall <= '1';
      end if;
      tr_fwd_B <= regs_B;
    elsif ((MM_wreg = '0') and (MM_a_c = a_rt) and (MM_a_c /= b"00000")
           and (MM_aVal = '1')) then    -- non-LW
      tr_fwd_B <= MM_result;
    else
      tr_fwd_B <= regs_B;
    end if;
  end process RF_FORWARDING_TRAPS;
    
  -- ----------------------------------------------------------------------    
  PIPE_exp_RF_EX: reg_exp_RF_EX
    port map (clk, rst, exp_RF_EX_ld, can_trap,EX_can_trap,
              exception,EX_exception, trap_instr,EX_trap_instr, 
              cop0_reg,EX_cop0_reg, cop0_sel,EX_cop0_sel,
              next_instr_in_delay_slot,EX_is_delayslot,
              RF_PC,EX_PC, RF_nmi,EX_nmi,
              interrupt,EX_interrupt, int_req,EX_int_req,
              tr_is_equal,EX_tr_is_equal, tr_less_than,EX_tr_less_than);
  
  -- EX execute exception ---------------------------------------------

  -- check for overflow in EX, send it to MM for later processing
  ex_trapped <= '1' when (EX_can_trap = b"10" and ovfl = '1') else '0';
  
  is_exception <= exOvfl when MM_ex_trapped = '1' else EX_exception;
  
  COP0_DECODE_EXCEPTION_AND_UPDATE_STATUS:
  process (rst, EX_a_rt, EX_PC, is_exception, EX_trap_instr,
           EX_cop0_reg, EX_cop0_sel, EX_nmi, EX_interrupt,EX_int_req,
           EX_is_delayslot, cop0_inp, EX_tr_is_equal, EX_tr_less_than,
           COUNT, COMPARE, STATUS, CAUSE, EPC, BadVAddr,
           rom_stall,ram_stall)
    
    variable newSTATUS, i_COP0_rd : reg32;
    variable i_update,i_epc_update,i_stall,i_nullify,i_take_trap : std_logic;
    variable i_a_c,i_update_r : reg5;
    variable i_epc_source : reg3;
    variable i_exp_PCsel : reg3;

  begin

    exception_num <= exception_type'pos(is_exception); -- for debugging only

    newSTATUS    := STATUS;      
    i_epc_update := '1';
    i_epc_source := b"000";
    i_exp_PCsel := b"000";     -- PC <= normal processing PC
    i_update     := '0';
    i_update_r   := b"00000";
    i_a_c        := b"00000";
    i_COP0_rd    := x"00000000";
    i_stall      := '0';
    i_nullify    := '0';
    i_take_trap  := '0';

    nullify_EX      <= '0';
    exception_taken <= '0';             -- for debugging only
    interrupt_taken <= '0';
    trap_taken      <= '0';
    ExcCode         <= cop0code_NULL;
    BadVAddr_source <= '0';
    BadVAddr_update <= '1';

    case is_exception is

      when exMTC0 =>            -- move to COP-0
        i_update_r := EX_cop0_reg;
        case EX_cop0_reg is
          when cop0reg_STATUS =>
            newSTATUS := cop0_inp;
            i_update   := '1';
            i_stall    := '1';
          when cop0reg_COUNT | cop0reg_COMPARE | cop0reg_CAUSE =>
            i_update   := '1';
            i_stall    := '1';
          when cop0reg_EPC =>
            i_epc_update := '0';
            i_epc_source := b"100";     -- EX_B
            i_stall      := '1';
          when others =>
            i_stall  := '0';
            i_update := '0';
        end case;
        
      when exEI =>              -- enable interrupts
        newSTATUS(STATUS_IE) := '1';
        i_update   := '1';
        i_update_r := cop0reg_STATUS;
        i_COP0_rd  := STATUS;
        i_a_c      := EX_a_rt;
        i_stall    := '1';
        
      when exDI =>              -- disable interrupts
        newSTATUS(STATUS_IE) := '0';
        i_update   := '1';
        i_update_r := cop0reg_STATUS;
        i_COP0_rd  := STATUS;
        i_a_c      := EX_a_rt;
        i_stall    := '1';

      when exMFC0 =>            -- move from COP-0
        case EX_cop0_reg is
          when cop0reg_COUNT    => i_COP0_rd := COUNT;
          when cop0reg_COMPARE  => i_COP0_rd := COMPARE;
          when cop0reg_STATUS   => i_COP0_rd := STATUS;
          when cop0reg_CAUSE    => i_COP0_rd := CAUSE;
          when cop0reg_EPC      => i_COP0_rd := EPC;
          when cop0reg_BadVAddr => i_COP0_rd := BadVAddr;
          when cop0reg_CONFIG   =>
            if EX_cop0_sel = b"000" then
              i_COP0_rd := CONFIG0;
            else
              i_COP0_rd := CONFIG1;
            end if;
          when others           => i_COP0_rd := (others => 'X');
        end case;
        i_a_c   := EX_a_rt;
        i_stall := '1';

      when exERET =>            -- exception return
        i_update     := '1';
        i_update_r   := cop0reg_STATUS;
        i_stall      := '0';
        i_exp_PCsel := b"001";          -- PC <= EPC
        i_nullify    := '1';             -- nullify instructions in IF,RF

      when exTRAP | exSYSCALL | exBREAK =>   -- trap instruction
        ExcCode <= cop0code_Tr;
        i_stall    := '0';
        case EX_trap_instr is
          when TEQ | TEQI =>
            i_take_trap := EX_tr_is_equal;
          when TNE | TNEI =>
            i_take_trap := not(EX_tr_is_equal);
          when TLT | TLTI | TLTU | TLTIU =>
            i_take_trap := EX_tr_less_than;
          when TGE | TGEI | TGEU | TGEIU =>
            i_take_trap := not(EX_tr_less_than);
          when SYSCALL =>
            i_take_trap := '1';
            ExcCode     <= cop0code_Sys;
          when BREAK =>
            i_take_trap := '1';
            ExcCode     <= cop0code_Bp;
          when others =>
            i_take_trap := '0';
        end case;
        if  i_take_trap = '1' then
          trap_taken <= '1';
          newSTATUS(STATUS_EXL) := '1';   -- at exception level
          newSTATUS(STATUS_UM)  := '0';   -- enter kernel mode          
          newSTATUS(STATUS_IE)  := '0';   -- disable interrupts
          i_update   := '1';
          i_update_r := cop0reg_STATUS;
          i_stall    := '0';
          i_epc_update := '0';
          i_nullify    := '1';          -- nullify instructions in IF,RF
          if EX_is_delayslot = '1' then -- instr is in delay slot
            i_epc_source := b"010";     -- EX_PC, re-execute branch/jump
          else
            i_epc_source := b"001";     -- RF_PC
          end if;
          i_exp_PCsel := b"010";       -- PC <= exception_180
        else
          trap_taken <= '0';
        end if;

      when exLL =>                      -- load linked (not a real exception)
        i_update   := '1';
        i_update_r := cop0reg_LLaddr;

      -- when exSC => null; if treated here, SC might delay an interrupt
        

      when exRESV_INSTR =>      -- reserved instruction ABORT SIMULATION
          assert true                   -- invalid opcode
            report LF & "invalid opcode (resv instr) at PC="& SLV32HEX(EX_PC)
            severity failure;


      when exOvfl =>                    -- OVERFLOW happened one cycle earlier
        newSTATUS(STATUS_EXL) := '1';   -- at exception level
        exception_taken <= '1';
        i_update        := '1';
        i_update_r      := cop0reg_STATUS;
        i_epc_update    := '0';
        i_exp_PCsel    := b"010";      -- PC <= exception_0180
        ExcCode         <= cop0code_Ov;
        i_nullify       := '1';         -- nullify instructions in IF,RF
        nullify_EX      <= '1';         -- and instruction in EX
        i_epc_source    := b"010";      -- bad address is in exp_EX_PC
        
        
      when IFaddressError | MMaddressErrorLD | MMaddressErrorST =>
        -- fetch/load/store from UNALIGNED ADDRESS
        newSTATUS(STATUS_EXL) := '1';   -- at exception level
        exception_taken <= '1';
        i_update        := '1';
        i_update_r      := cop0reg_STATUS;
        i_epc_update    := '0';
        i_exp_PCsel    := b"010";      -- PC <= exception_0180
        BadVAddr_update <= '0';
        if is_exception = MMaddressErrorST then
          ExcCode <= cop0code_AdES;
        else
          ExcCode <= cop0code_AdEL;
        end if;
        if is_exception = IFaddressError then
          i_nullify       := '1';       -- nullify instructions in IF,RF
          i_epc_source    := b"010";    -- bad address is in exp_EX_PC
          badVAddr_source <= '0';       -- instruction fetch
        else
          i_epc_source    := b"011";    -- bad address is in exp_MM_PC
          badVAddr_source <= '1';       -- load/store
        end if;
        
          
      when others =>                    -- interrupt pending?

        if ( (EX_nmi = '1') and (STATUS(STATUS_ERL) = '0') ) then
          -- non maskable interrupt
          -- assert false report "NM interrupt PC="&SLV32HEX(PC) severity note;
          exception_taken <= '1';
          newSTATUS(STATUS_BEV) := '1'; -- locationVector at bootstrap
          newSTATUS(STATUS_TS)  := '0'; -- not TLBmatchesSeveral
          newSTATUS(STATUS_SR)  := '0'; -- not softReset
          newSTATUS(STATUS_NMI) := '1'; -- non maskable interrupt
          newSTATUS(STATUS_ERL) := '1'; -- at error level
          i_update   := '1';
          i_update_r := cop0reg_STATUS;
          i_stall    := '0';
          i_epc_update := '0';
          i_nullify    := '1';          -- nullify instructions in IF,RF
          if EX_is_delayslot = '1' then -- instr is in delay slot
            i_epc_source := b"010";     -- EX_PC, re-execute branch/jump
          else
            i_epc_source := b"001";     -- RF_PC
          end if;
          i_exp_PCsel := b"100";       -- PC <= exception_0000
        
        elsif ( (STATUS(STATUS_EXL) = '0') and (STATUS(STATUS_ERL) = '0') and
                (STATUS(STATUS_IE) = '1')  and (EX_interrupt = '1')  and
                (rom_stall = '0' and ram_stall = '0')) then
          -- normal interrupt
          -- assert false report "interrupt PC="&SLV32HEX(PC) severity note;
          interrupt_taken <= '1';       -- debugging only     
          newSTATUS(STATUS_UM)  := '0'; -- enter kernel mode          
          newSTATUS(STATUS_EXL) := '1'; -- at exception level
          newSTATUS(STATUS_IE)  := '0'; -- disable interrupts
          ExcCode      <= cop0code_Int;
          i_update     := '1';
          i_update_r   := cop0reg_STATUS;
          i_stall      := '0';
          i_epc_update := '0';
          i_nullify    := '1';          -- nullify instructions in IF,RF
          if EX_is_delayslot = '1' then -- instr is in delay slot
            i_epc_source := b"010";     -- EX_PC, re-execute branch/jump
          else
            i_epc_source := b"001";     -- RF_PC
          end if;
          if CAUSE(CAUSE_IV) = '1' then
            i_exp_PCsel := b"011";     -- PC <= exception_0200
          else
            i_exp_PCsel := b"010";     -- PC <= exception_0180
          end if;

        end if; -- NMI or else interrupt 

    end case;

    newSTATUS(STATUS_CU3) := '0';  -- COP-3 absent (always)
    newSTATUS(STATUS_CU2) := '0';  -- COP-2 absent (always)
    newSTATUS(STATUS_CU1) := '0';  -- COP-1 absent (always)
    newSTATUS(STATUS_CU0) := '1';  -- COP-0 present=1 (always)


    STATUSinp    <= newSTATUS;
    EX_cop0_val  <= i_COP0_rd;
    EX_cop0_a_c  <= i_a_c;              -- only for forwarding COP0 values
    update       <= i_update;
    update_reg   <= i_update_r;
    epc_update   <= i_epc_update;
    epc_source   <= i_epc_source;
    exp_PCsel   <= i_exp_PCsel;
    exception_stall <= i_stall;
    nullify      <= i_nullify;
    
  end process COP0_DECODE_EXCEPTION_AND_UPDATE_STATUS;


  COP0_FORWARDING:
  process (EX_a_rt,EX_a_c, MM_a_c,MM_wreg,MM_result, WB_a_c,WB_wreg, 
           WB_C,EX_B, MM_cop0_val, MM_cop0_a_c,EX_cop0_a_c)
    variable i_B : reg32;
  begin
    if ((MM_wreg = '0')and(MM_a_c /= b"00000")and(MM_a_c = EX_a_rt)) then
      i_B := MM_result;
      -- assert false report "FWD_cop0 MM: inp="&SLV32HEX(cop0_inp); -- DEBUG
    elsif ((WB_wreg = '0')and(WB_a_c /= b"00000")and(WB_a_c = EX_a_rt)) then
      i_B := WB_C;
      -- assert false report "FWD_cop0 WB: inp="&SLV32HEX(cop0_inp); -- DEBUG
    elsif ((MM_wreg = '0')and
           (MM_cop0_a_c /= b"00000")and(MM_cop0_a_c = EX_cop0_a_c)) then
      i_B := MM_cop0_val;
      -- assert false report "FWD_cop0 CP: inp="&SLV32HEX(cop0_inp); -- DEBUG
    else
      i_B := EX_B;
      -- assert false report "FWD_cop0 EX: inp="&SLV32HEX(cop0_inp); -- DEBUG
    end if;
    cop0_inp <= i_B;
  end process COP0_FORWARDING;


  status_update <= '0' when (update = '1' and update_reg = cop0reg_STATUS and
                             not_stalled = '1')
                   else '1';
  
  COP0_STATUS: register32 generic map (RESET_STATUS)
    port map (clk, rst, status_update, STATUSinp, STATUS);

  -- CAUSE ------------------------------
  COP0_COMPUTE_CAUSE:
  process(rst, update,update_reg, EX_int_req, ExcCode, cop0_inp,
          EX_is_delayslot, count_eq_compare,count_enable, CAUSE)
    variable newCAUSE : reg32;
  begin
      newCAUSE(CAUSE_BD)     := EX_is_delayslot; -- instr is in delay slot
      newCAUSE(CAUSE_TI)     := count_eq_compare;
      newCAUSE(CAUSE_CE1)    := '0';
      newCAUSE(CAUSE_CE0)    := '0';
      newCAUSE(CAUSE_DC)     := CAUSE(CAUSE_DC);
      newCAUSE(CAUSE_PCI)    := '0';
      newCAUSE(25 downto 24) := b"00";
      newCAUSE(CAUSE_IV)     := CAUSE(CAUSE_IV);
      newCAUSE(CAUSE_WP)     := '0';
      newCAUSE(21 downto 16) := b"000000";      
      newCAUSE(CAUSE_IP7)    := EX_int_req(7);
      newCAUSE(CAUSE_IP6)    := EX_int_req(6);
      newCAUSE(CAUSE_IP5)    := EX_int_req(5);
      newCAUSE(CAUSE_IP4)    := EX_int_req(4);
      newCAUSE(CAUSE_IP3)    := EX_int_req(3);
      newCAUSE(CAUSE_IP2)    := EX_int_req(2);
      newCAUSE(CAUSE_IP1)    := CAUSE(CAUSE_IP1);
      newCAUSE(CAUSE_IP0)    := CAUSE(CAUSE_IP0);
      newCAUSE(7)            := '0';
      newCAUSE(6 downto 2)   := ExcCode;
      newCAUSE(1 downto 0)   := b"00";

      if (update = '1' and update_reg = cop0reg_CAUSE) then
        CAUSEinp <= newCAUSE(CAUSE_BD downto CAUSE_CE0) &
                    cop0_inp(CAUSE_DC) & cop0_inp(CAUSE_PCI) & b"00" &
                    cop0_inp(CAUSE_IV) & 
                    newCAUSE(CAUSE_WP  downto CAUSE_IP2) &
                    cop0_inp(CAUSE_IP1 downto CAUSE_IP0) & '0' &
                    newCAUSE(6 downto 2) & b"00";
      else
        CAUSEinp <= newCAUSE;
      end if;
  end process COP0_COMPUTE_CAUSE;

  COP0_CAUSE_HOLD: process(rst,clk,
                           ExcCode,is_exception,EX_cop0_reg,not_stalled)
  begin
    if rst = '0' then                   -- hold CAUSE until it is read
      cause_update <= '0';
    elsif ( rising_edge(clk) and (ExcCode /= cop0code_NULL) ) then
      cause_update <= '1';              -- syscall/trap/interrupt/exception
    elsif ( rising_edge(clk) and (is_exception = exMFC0) and
            EX_cop0_reg = cop0reg_CAUSE and (not_stalled = '1') ) then
      cause_update <= '0';              -- CAUSE is being read
    end if;
  end process COP0_CAUSE_HOLD;

  COP0_CAUSE: register32 generic map (RESET_CAUSE)
    port map (clk, rst, cause_update, CAUSEinp, CAUSE);


  -- EPC ------------------------------
  with epc_source select EPCinp <=
    PC              when b"000",        -- instr fetch exception
    RF_PC           when b"001",        -- invalid instr exception
    EX_PC           when b"010",        -- interrupt, eret, overflow
    WB_PC           when b"011",        -- data memory exception
    alu_fwd_B       when others; -- b"100",        -- mtc0
    -- (others => 'X') when others;        -- invalid selection
    
  COP0_EPC: register32 generic map (x"00000000")
    port map (clk, rst, epc_update, EPCinp, EPC);


  -- COUNT & COMPARE ------------------------------ not_stalled = '1'
  compare_update <= '0' when (update = '1' and update_reg = cop0reg_COMPARE)
                    else '1';
  
  COP0_COMPARE: register32 generic map(x"00000000")
    port map (clk, rst, compare_update, cop0_inp, COMPARE);

  count_update <= '0' when (update = '1' and update_reg = cop0reg_COUNT)
                    else '1';
  
  COP0_COUNT: counter32 generic map (x"00000001")
    -- port map (clk, rst, count_update, count_enable, cop0_inp, COUNT);
    port map (clk, rst, count_update, PCload, cop0_inp, COUNT); -- DEBUG

  compare_set <= (count_eq_compare or BOOL2SL(COUNT = COMPARE))
                 when compare_update = '1'
                 else '0';
            
  COP0_COUNT_INTERRUPT: FFD
    port map (clk, rst, '1', compare_set, count_eq_compare);
  
  disable_count <= CAUSE(CAUSE_DC)
                   when (cause_update='0' and CAUSE(CAUSE_DC) /= count_enable)
                   else count_enable;     -- load new CAUSE(CAUSE_DC)
  COP0_DISABLE_COUNT: FFD port map (clk,'1',rst,disable_count, count_enable);

  -- BadVAddr ------------------------------------
  BadVAddr_inp <= EX_PC when badVAddr_source = '0'  -- instruction fetch
                  else WB_result;                   -- load/store
  
  COP0_BadVAddr: register32 generic map(x"00000000")
    port map (clk, rst, BadVAddr_update, BadVAddr_inp, BadVAddr);

  -- LLaddr & LLbit ------------------------------
  LL_update <= '0' when (update = '1' and update_reg = cop0reg_LLAddr)
               else '1';
  
  COP0_LLaddr: register32 generic map(x"00000000")
    port map (clk, rst, LL_update, result, LLaddr);

  LL_SC_differ <= '0' when (result = LLaddr) else '1';

  LL_SC_abort <= (LL_SC_differ or EX_LLbit) when (is_exception = exSC) else
                 '0';
  
  COP0_LLbit: process(rst,phi2)
    begin
      if rst = '0' then
        EX_LLbit    <= '0';             -- break SC -> LL
      elsif rising_edge(phi2) then
        case is_exception is
          when exERET =>
            EX_LLbit <= '0';            -- break SC -> LL
          when exLL =>
            EX_LLbit <= not LL_update;  -- update only if instr is a LL
          when others =>
            null; -- LL_SC_abort <= '0';
        end case;
      end if;
    end process COP0_LLbit;
    
    EX_exp_type <= exNOP;
    
  -- ----------------------------------------------------------------------
  PIPE_exp_EX_MM: reg_exp_EX_MM
    port map (clk, rst, exp_EX_MM_ld, EX_can_trap,MM_can_trap,   
              EX_exp_type,MM_exp_type_i, EX_PC,MM_PC,
              EX_LLbit,MM_LLbit,
              EX_cop0_a_c,MM_cop0_a_c, EX_cop0_val,MM_cop0_val,
              EX_trapped, MM_ex_trapped);

    COP0_MM_EXCEPTIONS: process(MM_addr_error, MM_exp_type_i)
    begin

      if ( MM_exp_type_i = exNOP and       -- nothing bad from EX
           MM_addr_error /= exNOP ) then    -- something wrong in MM
        MM_abort <= '1';
        MM_exp_type <= MM_addr_error;
      else
        MM_abort <= '0';
        MM_exp_type <= exNOP;
      end if;

    end process COP0_MM_EXCEPTIONS;
                    
  -- ----------------------------------------------------------------------    
  PIPE_exp_MM_WB: reg_exp_MM_WB
    port map (clk, rst, exp_MM_WB_ld, MM_can_trap,WB_can_trap,   
              MM_exp_type, WB_exp_type, MM_PC,WB_PC,
              MM_LLbit,WB_LLbit, MM_abort,WB_abort, 
              MM_cop0_a_c,WB_cop0_a_c, MM_cop0_val,WB_cop0_val);

  -- WB is shared with datapath -------------------------------------------  
  -- nothing to do here

end rtl;
