# The tools that the build runs, for the scripts that run them and for
# check-env to check.  Source it from the top of the tree.  Each can be set
# in the environment, for another distribution's names and paths.
IDLE=${IDLE:-idle}
GRUB_MKRESCUE=${GRUB_MKRESCUE:-grub2-mkrescue}
GRUB_MKIMAGE=${GRUB_MKIMAGE:-grub2-mkimage}
GRUB_DIR=${GRUB_DIR:-/usr/lib/grub/i386-pc}
