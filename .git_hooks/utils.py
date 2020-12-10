import subprocess
from typing import List

def find_i(iterable_object, comparator):
    """find an element with comparator provided, returns index"""
    for i, obj in enumerate(iterable_object):
        if comparator(obj):
            return i
    return None

def find(iterable_object, comparator):
    """find an element with comparator provided, returns obj"""
    for obj in iterable_object:
        if comparator(obj):
            return obj
    return None

def run_cmd_no_fail(cmd: str):
    ret = subprocess.run(cmd.split(), stdout=subprocess.PIPE)
    return (ret.stdout.decode('utf-8').split('\n'), ret.returncode)

def run_cmd(cmd: str) -> List[str]:
    ret = subprocess.run(cmd.split(), stdout=subprocess.PIPE)
    stdout = ret.stdout.decode('utf-8')
    if ret.returncode != 0:
        raise Exception(f'cmd {cmd} returned error. stdout was {stdout}')
    return stdout.split('\n') if len(stdout) > 0 else []

def run_cmd_shell(cmd: str) -> List[str]:
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               shell=True, encoding='utf-8', errors='ignore')
    (output, _) = process.communicate()
    if (process.returncode != 0):
        raise Exception(f'Command failed, exiting. Failing cmd is: {cmd}, output is:\n{output}')
    return output.split('\n') if len(output) > 0 else []

def overwrite_file(fd, new_content):
    """Overwrites a file using a descriptor. The content may be a str or a list of strings"""
    fd.seek(0)
    if type(new_content == list):
        fd.write('\n'.join(new_content))
    else:
        fd.write(new_content)
    fd.truncate()

def readlines(fd):
    """Replacement for readlines(), which may insert stray newline characters"""
    return fd.read().splitlines()
