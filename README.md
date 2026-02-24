




this VHDL model mimics the pipeline design described in **Patterson & Hennessy's book(Computer Organisation and Design)**

 - the TLB and assorted control registers will be included soon. the model was made for an _Altera EP4CE30F23_. 


### features
    Processor model runs C code, compiled with GCC
    Testbench includes processor, RAM, ROM and file I/O
    Core has all forwarding paths and is fully interlocked for data and control hazards
    Coprocessor0 is partially implemented, six hardware interrupts + NMI implemented in "Interrupt Compatibility Mode"
    The instructions break, syscall, trap, mfc0, mtc0, eret, ei, di, ll, sc are implemented
    Partial-word loads and stores (word, half-word, byte) implemented at the processor's memory interface

