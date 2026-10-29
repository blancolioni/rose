/* rose-unwind.c: what GCC's unwinder (unwind-dw2.o and unwind-dw2-fde-dip.o
   from libgcc_eh.a) needs from the C library (#170).

   A userland program is a single static executable with no tasking, so
   there is one object to search for unwind tables, and nothing to lock.  */

extern void exit (int) __attribute__ ((noreturn));

/* The start of the program's .eh_frame_hdr section, from its linker
   script.  */

extern char __eh_frame_hdr_start[];

/* The layout of glibc's struct dl_find_object on i386, which has
   dlfo_eh_dbase (DLFO_STRUCT_HAS_EH_DBASE).  */

struct dl_find_object
{
  unsigned long long int dlfo_flags;
  void *dlfo_map_start;
  void *dlfo_map_end;
  void *dlfo_link_map;
  void *dlfo_eh_frame;
  void *dlfo_eh_dbase;
  unsigned int __dlfo_eh_dbase_pad;
  unsigned long long int __dlfo_reserved[7];
};

/* Every address is in the program, whose unwind tables start at
   __eh_frame_hdr_start.  */

int
_dl_find_object (void *address, struct dl_find_object *result)
{
  (void) address;
  result->dlfo_flags = 0;
  result->dlfo_map_start = (void *) 0;
  result->dlfo_map_end = (void *) -1;
  result->dlfo_link_map = (void *) 0;
  result->dlfo_eh_frame = __eh_frame_hdr_start;
  result->dlfo_eh_dbase = (void *) 0;
  return 0;
}

/* The unwinder calls abort when the unwind tables are inconsistent.  */

void
abort (void)
{
  exit (1);
}

/* Without tasking, a once-only call is made on the first call, and the
   mutexes and condition variables do nothing.  */

int
pthread_once (int *once_control, void (*init_routine) (void))
{
  if (*once_control == 0)
    {
      *once_control = 1;
      init_routine ();
    }
  return 0;
}

int
pthread_mutex_lock (void *mutex)
{
  (void) mutex;
  return 0;
}

int
pthread_mutex_unlock (void *mutex)
{
  (void) mutex;
  return 0;
}

int
pthread_cond_wait (void *cond, void *mutex)
{
  (void) cond;
  (void) mutex;
  return 0;
}

int
pthread_cond_broadcast (void *cond)
{
  (void) cond;
  return 0;
}
