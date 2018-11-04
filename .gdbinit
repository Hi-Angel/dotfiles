set history save on
set history filename ~/.gdb-history
set history size 1000
set height 0
set pagination off
set print pretty on
#set auto-load python-scripts on

# LO autoloads helpers for GDB.
set auto-load safe-path /

# print-pointer-cast: prints second arg as a pointer of first arg
define pc
  print *($arg0*)$arg1
end

## helpers mostly useful for reverse-engineering
# they assume Linux AMD64 ABI, though watch out for compiler optimizations

# argn functions below accept an optional argument to cast an arg to.
# In the lack of such just prints its hex
define arg1
  # print ($argc == 0)? $rdi : *($arg0*)$rdi
  if ($argc == 0)
    print /x $rdi
  else
    pc $arg0 $rdi
  end
end

define arg2
  # print ($argc == 0)? $rsi : *($arg0*)$rsi
  if ($argc == 0)
    print /x $rsi
  else
    pc $arg0 $rsi
  end
end

define arg3
  # print ($argc == 0)? $rdx : *($arg0*)$rdx
  if ($argc == 0)
    print /x $rdx
  else
    pc $arg0 $rdx
  end
end

# just a python function example that casts an arg to int*
# class AsIntPtr(gdb.Function):
#     def __init__(self):
#         super ().__init__("_as_int_ptr")

#     def invoke(self, arg):
#         int_type = gdb.lookup_type('int')
#         int_ptr_type = int_type.pointer()
#         return arg.cast(int_ptr_type)

# AsIntPtr()

source ~/gdb.py

# stupid asan neither work under gdb, nor can disable itself instead of screwing the session
set environment ASAN_OPTIONS=detect_leaks=0
