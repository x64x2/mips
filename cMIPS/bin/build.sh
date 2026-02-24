#!/bin/bash
if [ ! -v tree ] ; then
  export tree="$(echo $PWD | sed -e 's:^\(/.*/MIPS\)/.*:\1:')"
fi

bin="${tree}"/bin
include="${tree}"/include
srcVHDL="${tree}"/vhdl

c_ld="${include}"/mips.ld
c_s="${include}"/mips.s
c_h="${include}"/mips.h

pkg_vhd="$srcVHDL/mem.vhd"

if [ $pkg_vhd -nt $c_ld -o\
     $pkg_vhd -nt $c_s  -o\
     $pkg_vhd -nt $c_h  ] ; then
   "${bin}"/build.sh -v || exit 1
fi

cd "${src}"

simulator=mips

pkg="packageWires.vhd packageMemory.vhd packageExcp.vhd"

src="altera.vhd macnica.vhd aux.vhd memory.vhd cache.vhd instrcache.vhd ram.vhd rom.vhd units.vhd io.vhd uart.vhd pipestages.vhd exception.vhd core.vhd tb_mips.vhd"

if [ ! -f .last_import ] ; then
   ghdl -i ${pkg}
   ghdl -i ${src}
   touch .last_import
fi