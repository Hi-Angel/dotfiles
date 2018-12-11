# class my_print:
#     def __init__(self, val):
#         self.val = val
#     def to_string(self):
#         return self.val

# def lookup_type_my_int (val):
#     if str(val.type) == 'int':
#         return my_print(val)
#     return None

# gdb.pretty_printers.append(lookup_type_my_int)

class GrepCmd (gdb.Command):
    """Execute command, but only show lines matching the pattern
    Usage: grep_cmd <cmd> <pattern> """

    def __init__ (_):
        super ().__init__ ("grep_cmd", gdb.COMMAND_STATUS)

    def invoke (_, args_raw, __):
        args = gdb.string_to_argv(args_raw)
        if len(args) != 2:
            print("Wrong parameters number. Usage: grep_cmd <cmd> <pattern>")
        else:
            for line in gdb.execute(args[0], to_string=True).splitlines():
                if args[1] in line:
                    print(line)

GrepCmd() # required to get it registered
