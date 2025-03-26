../../target/debug/amulet kernel.am --nostdlib --target=i386-elf

clang --target=i386-elf -g -fno-PIC -W -Wall -ffreestanding -mno-red-zone \
  -nostdinc -c kernel.ll -o kernel.o

# Compile assembly files (if any)
x86_64-elf-as --32 boot.asm -o boot.o

x86_64-elf-ld -m elf_i386 -nostdlib -T linker.ld --gc-sections -o kernel.elf boot.o kernel.o

# Make sure it's a Multiboot compliant kernel
grub-file --is-x86-multiboot kernel.elf
if [ $? -ne 0 ]; then
  echo "ERROR: kernel.elf is not a Multiboot compliant kernel"
  exit 1
fi

# Create a bootable ISO
mkdir -p isodir/boot/grub
cp kernel.elf isodir/boot/
echo 'menuentry "MyOS" { multiboot /boot/kernel.elf }' >isodir/boot/grub/grub.cfg
grub-mkrescue -o myos.iso isodir
rm -rf isodir
