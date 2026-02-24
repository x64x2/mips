#!/bin/bash

if [ ! -v tree ] ; then
  export tree="$(echo $PWD | sed -e 's:\(/.*/mips\)/.*:\1:')"
fi

PATH=/usr/bin/mips/

if [ -x /opt/cross/bin/mips-gcc ] ; then
    export PATH=$PATH:$HOME_PATH
elif [ -x /home/soft/linux/mips/cross/bin/mips-gcc ] ; then
    export PATH=$PATH:$WORK_PATH
else
    echo -e "\n\n\tPANIC: cross-compiler not installed\n\n" ; exit 1;
fi


usage() {
cat << EOF
usage:  $0 [options] SOURCE.s
        creates {prog,data}.bin to be input by textbench

OPTIONS:
   -h    Show this message
   -O n  Optimization level, defaults to n=0 {0,1,2,3}
   -v    Verbose, creates memory map: SOURCE.map
   -mif  Generate output file ROM.mif for Altera's FPGAs
EOF
}

errorED()
{
cat <<EOF


	$pkg_vhd NEWER than header files;
	problem running edMemory.sh in $0


EOF
exit 1
}


if [ $# = 0 ] ; then usage ; exit 1 ; fi

miffile=false
verbose=false
unset mem_map
level=0


bin="${tree}"/bin
include="${tree}"/include
srcVHDL="${tree}"/vhdl


c_ld="${include}"/mips.ld
c_s="${include}"/mips.s

while true ; do

    case "$1" in
        -h) usage ; exit 1
            ;;
        -O) level=$2
            shift
            ;;
	-O1) level=1
	    ;;
	-O2) level=2
	    ;;
	-O3) level=3
	    ;;
        -v) verbose=true
            ;;
        -mif) miffile=true
            ;;
        -x) set -x
            ;;
        *)  inp=${1%.s}
            if [ ${inp}.s != $1 ] ; then
                usage ; echo "  invalid option: $1"; exit 1 ; fi
            break
            ;;
    esac
    shift
done

if [ -z $inp ] ; then usage ; exit 1 ; fi

pkg_vhd="${srcVHDL}"/packageMemory.vhd

if [ $pkg_vhd -nt $c_ld -o  $pkg_vhd -nt $c_s ] ; then
    "${bin}"/edMemory.sh -v || errorED || exit 1
fi

if [ $verbose = true ] ; then  mem_map="-Map ${inp}.map" ; fi

asm=${inp}.s
obj=${inp}.o
elf=${inp}.elf
bin=prog.bin
dat=data.bin

(mips-as -O${level} -EL -mips32r2  -I "${include}" -o $obj $asm || exit 1) &&\
  mips-ld -EL -e _start ${mem_map} -I "${include}" --script $c_ld -o $elf $obj &&\
  mips-objcopy -S -j .text -O binary $elf $bin &&\
  mips-objcopy -S -j .data -j .rodata -O binary $elf $dat &&\
  chmod a-x $bin $dat &&\
  if [ $verbose = true ] ; then
    mips-objdump -z -D -EL -M reg-names=numeric --show-raw-insn \
        --section .text --section .data --section .rodata --section .bss  $elf
  fi &&\
  if [ $miffile = true ] ; then
    elf2mif.sh "$elf" || exit 1

  fi
