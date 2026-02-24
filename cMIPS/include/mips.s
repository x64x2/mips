
	# see vhdl/mem.vhd for addresses
        .set x_BASE_ADDR,0x00000000
        .set x_MEM_SZ,0x00002000

        .set x_BASE_ADDR,0x00400000
        .set x_MEM_SZ,0x00002000
	
        .set x_IO_BASE_ADDR,0x0F000000
        .set x_IO_MEM_SZ,0x00002000
	.set x_IO_ADDR_RANGE,0x00000020

	.set HW_counter_addr,(x_IO_BASE_ADDR +  5 * x_IO_ADDR_RANGE)
	.set HW_uart_addr,   (x_IO_BASE_ADDR +  7 * x_IO_ADDR_RANGE)
	.set HW_seg_addr,(x_IO_BASE_ADDR +  9 * x_IO_ADDR_RANGE)
	.set HW_key_addr,  (x_IO_BASE_ADDR + 10 * x_IO_ADDR_RANGE)
	.set HW_lcd_addr,    (x_IO_BASE_ADDR + 11 * x_IO_ADDR_RANGE)

	# see vhdl/exp.vhd for addresses
	.set x_EXCEPTION_0000,0x00000080
	.set x_EXCEPTION_0180,0x000000c0
	.set x_EXCEPTION_0200,0x00000140
	.set x_ENTRY,0x00000300

	.set cop0_COUNT,$9
	.set cop0_COMPARE,$11
	.set cop0_STATUS,$12
	.set cop0_CAUSE,$13
	.set cop0_EPC,$14
	.set cop0_CONFIG,$16
	.set cop0_CONFIG_f0,0
	.set cop0_CONFIG_f1,1
	.set cop0_Addr,$17
	.set cop0_Error,$30

	
	# reset: COP0 present, at exception level, all else disabled
	.set cop0_STATUS_reset,0x10000002

	# reset: COUNTER stopped, use special interrVector, no interrupts
	.set cop0_CAUSE_reset, 0x0880007c
