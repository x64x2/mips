	# mips-as-mips32 -o start.o start.s
	.include "mips.s"
	.text
	.set noreorder
	.align 2
	.extern main
	.global _start
	.global _exit
	.global exit
	.org x_INST_BASE_ADDR,0
	.ent _start
_start: nop

	# set STATUS, cop0, kernel mode, interrupts disabled
        li   $k0, cop0_STATUS_reset
        mtc0 $k0, cop0_STATUS

	# set CAUSE, "no exceptions", interrVec separated from exceptVec
	li   $k0, cop0_CAUSE_reset   # 0x0080007c
	mtc0 $k0, cop0_CAUSE
	
	# initialize SP: ramTop-8
	li   $sp,(x_DATA_BASE_ADDR+x_DATA_MEM_SZ-8)

	# set STATUS, cop0, user mode, hw interrupt IRQ2,IRQ3 enabled
        li   $k0, 0x10000c09
        mtc0 $k0, cop0_STATUS
	
	nop
	jal main  # on returning from main(), MUST go into exit()
	nop       #  to stop the simulation.
exit:	
_exit:	nop	  # flush pipeline
	nop
	nop
	nop
	nop
	wait 0    # then stop VHDL simulation
	nop
	nop
	.end _start

	.global _excp_0180
	.global excp_0180
	.global _excp_0200
	.global excp_0200
	.global _excp_0000
	.global excp_0000

	.org x_EXCEPTION_0000,0 # exception vector_0000
	.ent _excp_0000
excp_0000:
_excp_0000:
        mfc0 $k0, cop0_STATUS
	j nmi_reset_handler
	nop
	#excp_0000ret:
	#	li   $k0, 0x1000ff09   # enable interrupts, user mode
	#       mtc0 $k0, cop0_STATUS
	# ret

	# handler for NMI or soft-reset -- simply abort simulation
nmi_reset_handler:
	mfc0 $k1,cop0_CAUSE    # read CAUSE
	nop
	wait 0x38             
	nop
	# j exp_0000ret       #  OR do something else!
	.end _exp_0000


	# handler for various exceptional conditions and HW errors
	.org x_EXCEPTION_0180,0  # exception vector_180
	.ent _exp_0180
exp_0180:
_exp_0180:
        mfc0 $k0, cop0_CAUSE
	andi $k0, $k0, 0x3f    # keep only ExceptionCode
	slt  $k1, $k0, 0x20    # not an address/bus error -- Table 8-25
	beq  $k1, $zero, excp_0180ret
	nop
	and  $k0, $k0, 0x1f    # keep type of address error
	lui  $k1, %hi(excp_tbl)
        ori  $k1, $k1, %lo(excp_tbl)
	add  $k1, $k1, $k0
	jr   $k1
	nop
exp_tbl: j excp_0180ret       # interrupt, do nothing and return
	wait 0x04  # addr error      -- abort simulation
	wait 0x08  # addr error      -- abort simulation
	wait 0x0c  # addr error      -- abort simulation
	wait 0x10  # addr error LD   -- abort simulation
	wait 0x14  # addr error ST   -- abort simulation
	wait 0x18  # bus error IF    -- abort simulation
	wait 0x1c  # bus error LD/ST -- abort simulation
	wait 0xff  # any other -- should never arrive here, abort simulation
	nop

excp_0180ret:
	li   $k0, 0x1000ff09   	# enable interrupts, switch to user mode
        mtc0 $k0, cop0_STATUS
	eret
	.end _excp_0180
	
	# name all handlers here
	.extern countCompare
	.extern extCounter
	.extern UARTinterr
	.org x_EXCEPTION_0200,0   # exception vector_200, interrupt handlers
	.ent _excp_0200
	.set M_CauseIM,0x0000ff00 # keep bits 15..8 -> IM = IP
	.set M_StatusIEn,0x0000ff09 # user mode, enable all interrupts
exp_0200:
_exp_0200:
	mfc0 $k0, cop0_CAUSE
	andi $k0, $k0, M_CauseIM  # Keep only IP bits from Cause
	mfc0 $k1, cop0_STATUS
	and  $k0, $k0, $k1        # and mask with IM bits 
	beq  $k0, $zero, Dismiss  
	nop
	
	# Find out which irq is active and dispatch to handler
hand_7:	andi $k1, $k0, 0x8000	  # handle IP7=HW5
	# beq  $k1, $zero, hand_6
	beq  $k1, $zero, hand_3  
	nop
	j countCompare
	nop
hand_6:	andi $k1, $k0, 0x4000	  # handle IP6=HW4
	beq  $k1, $zero, hand_5
	nop
	j exp_0200ret            # add proper handler here
	nop
hand_5:	andi $k1, $k0, 0x2000	  # handle IP5=HW3
	beq  $k1, $zero, hand_4
	nop
	j exp_0200ret            # add proper handler here
	nop
hand_4:	andi $k1, $k0, 0x1000	  # handle IP4=HW2
	beq  $k1, $zero, hand_3
	nop
	j exp_0200ret            # add proper handler here
	nop
hand_3:	andi $k1, $k0, 0x0800	  # handle IP3=HW1
	beq  $k1, $zero, hand_2
	nop
	j UARTinterr
	nop
hand_2:	andi $k1, $k0, 0x0400	  # handle IP2=HW0
	beq  $k1, $zero, hand_1
	nop
	j extCounter
	nop
hand_1:	andi $k1, $k0, 0x0200	  # handle IP1=SW1
	beq  $k1, $zero, hand_0
	nop
	j excp_0200ret            # add proper handler here
	nop
hand_0:	andi $k1, $k0, 0x0100	  # handle IP0=SW0
	beq  $k1, $zero, Dismiss
	nop
	j exp_0200ret		  # add proper handler here
	nop
	
Dismiss:                # No pending request, must have been noise
	nop             #  do nothing and return

exp_0200ret:
	li   $k0, 0x08800000
	mtc0 $k0, cop0_CAUSE

	mfc0 $k0, cop0_STATUS	# Read STATUS register
	lui  $k1, 0xffff           #  and do not modify its contents
	ori  $k0, $k0, M_StatusIEn #  except for re-enabling interrupts
	ori  $k1, $k1, 0xfff9      #  and going into user mode
	and  $k0, $k1, $k0
	mtc0 $k0, cop0_STATUS	
	eret			# Return from interrupt
	nop
	.end _excp_0200
	# normal code starts here -- do not edit next line
	.org x_ENTRY,0

	