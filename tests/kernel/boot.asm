.set MB_MAGIC,       0x1BADB002
.set MB_PAGE_ALIGN,  1 << 0
.set MB_MEMORY_INFO, 1 << 1
.set MB_FLAGS,       MB_PAGE_ALIGN | MB_MEMORY_INFO
.set MB_CHECKSUM,    -(MB_MAGIC + MB_FLAGS)

/* multiboot header */
.section .multiboot
.align 4
  .long MB_MAGIC
  .long MB_FLAGS
  .long MB_CHECKSUM

.section .stack, "aw", @nobits
stack_bottom:
  .skip 32768 # 32 KiB
stack_top:

.extern kmain
.section .text
.global _start
_start:
  cli
  cld

  /* set up stack */
  mov $stack_top, %esp

  and $-16, %esp

  /* call kmain with multiboot info */
  push %ebx
  push %eax
  call kmain

  cli

.loop:
  hlt
  jmp .loop