MEMORY
{
    RAM : ORIGIN = 0x00000000, LENGTH = 0x00a00000
}

SECTIONS
{
    .text : {
        . = ALIGN(4);
        *(.text)
    }  > RAM

    .data : {
        . = ALIGN(4);
        *(.data);
        . = ALIGN(4);
    } > RAM
}
