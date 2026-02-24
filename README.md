




this VHDL model mimics the pipeline design described in **patterson & hennessy's book(computer organisation and design)**

> this was made for an _Altera EP4CE30F23_. 


### features

    processor model runs C code, compiled with GCC
    testbench includes processor, RAM, ROM and file I/O
    core has all forwarding paths and is fully interlocked for data and control hazards
    the instructions break, syscall, trap, mfc0, mtc0, eret, ei, di, ll, sc are implemented
    partial-word loads and stores (word, half-word, byte) implemented at the processor's memory interface

