	.file	1 "mips.c"
	.section .mdebug.abi32
	.previous
	.gnu_attribute 4, 1
	.text
	.align	2
	.globl	print
	.set	nomips16
	.ent	print
	.type	print, @function
print:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	li	$2,251658240			# 0xf000000
	j	$31
	sw	$4,0($2)

	.set	macro
	.set	reorder
	.end	print
	.size	print, .-print
	.align	2
	.globl	to_stdout
	.set	nomips16
	.ent	to_stdout
	.type	to_stdout, @function
to_stdout:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	andi	$4,$4,0x00ff
	li	$2,251658240			# 0xf000000
	j	$31
	sw	$4,32($2)

	.set	macro
	.set	reorder
	.end	to_stdout
	.size	to_stdout, .-to_stdout
	.align	2
	.globl	from_stdin
	.set	nomips16
	.ent	from_stdin
	.type	from_stdin, @function
from_stdin:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	li	$2,251658240			# 0xf000000
	lw	$3,64($2)
	lw	$2,68($2)
	nop
	bne	$2,$0,$L5
	nop

	sb	$3,0($4)
$L5:
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	from_stdin
	.size	from_stdin, .-from_stdin
	.align	2
	.globl	readInt
	.set	nomips16
	.ent	readInt
	.type	readInt, @function
readInt:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	li	$2,251658240			# 0xf000000
	lw	$3,96($2)
	lw	$2,100($2)
	nop
	bne	$2,$0,$L8
	nop

	sw	$3,0($4)
$L8:
	j	$31
	nop

	.set	macro
	.set	reorder
	.end	readInt
	.size	readInt, .-readInt
	.align	2
	.globl	writeInt
	.set	nomips16
	.ent	writeInt
	.type	writeInt, @function
writeInt:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	li	$2,251658240			# 0xf000000
	j	$31
	sw	$4,128($2)

	.set	macro
	.set	reorder
	.end	writeInt
	.size	writeInt, .-writeInt
	.align	2
	.globl	writeClose
	.set	nomips16
	.ent	writeClose
	.type	writeClose, @function
writeClose:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	li	$3,1			# 0x1
	li	$2,251658240			# 0xf000000
	j	$31
	sw	$3,132($2)

	.set	macro
	.set	reorder
	.end	writeClose
	.size	writeClose, .-writeClose
	.align	2
	.globl	startCounting
	.set	nomips16
	.ent	startCounting
	.type	startCounting, @function
startCounting:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	andi	$4,$4,0xffff
	li	$2,-2147483648			# 0xffffffff80000000
	or	$4,$4,$2
	li	$2,251658240			# 0xf000000
	j	$31
	sw	$4,160($2)

	.set	macro
	.set	reorder
	.end	startCounting
	.size	startCounting, .-startCounting
	.align	2
	.globl	stopCounting
	.set	nomips16
	.ent	stopCounting
	.type	stopCounting, @function
stopCounting:
	.frame	$sp,0,$31		# vars= 0, regs= 0/0, args= 0, gp= 0
	.mask	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	li	$2,251658240			# 0xf000000
	j	$31
	sw	$0,160($2)

	.set	macro
	.set	reorder
	.end	stopCounting
	.size	stopCounting, .-stopCounting
	.ident	"GCC: (GNU) 4.8.2"
