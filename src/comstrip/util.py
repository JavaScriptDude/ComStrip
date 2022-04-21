
import sys, os, pathlib, time, re, platform
import texttable as tt
from datetime import datetime

class C_():
    RE_ALPHA_NUM_USCORE=re.compile("^[0-9a-zA-Z_]*$")
    OS_NAME = os.name
    OS_PLATFORM = platform.system()
    TIMESTAMP = "%y-%m-%d %H:%M:%S"

def os_gate():
    if isWin():
        raise Exception(f"This code base is written and tested for Linux but should work for any posix compliant system. Future release will include support for your windows")    

def isWin():
    return C_.OS_NAME == 'nt'

def isFreeBSD():
    return C_.OS_PLATFORM == 'FreeBSD'

def isLinux():
    return C_.OS_PLATFORM == 'Linux'

def isCygwin():
    return (C_.OS_PLATFORM.find("CYGWIN_NT") == 0)

def ismuex(*a):
    return not bool(sum(map(lambda v: bool(v if isinstance(v, bool) else not v is None), a)) > 1)

def getClassName(o):
    if o == None: return None
    return type(o).__name__

def getMachineDTMS(dt:datetime=None):
    dt = datetime.now() if dt is None else dt
    return dt.strftime("%y%m%d-%H%M%S.%f")[:-3]

def exit(s, code=1):
    if not s is None:
        print(s)
    print('~')
    sys.stdout.flush()
    sys.stderr.flush()
    sys.exit(code)

def splitPath(s):
    f = os.path.basename(s)
    p = s[:-(len(f))-1]
    p = toPosixPath(getAbsPath(p))
    return f, p


def toPosixPath(s:str, strip_slash:bool=False, ensure_slash:bool=False):
    s = s.strip().replace('\\', '/')
    if strip_slash:
        return s.rstrip('/')
    if ensure_slash and not s[-1:] == '/':
        return '%s/' % s
    return s


def toWinPath(s:str, strip_slash:bool=False, ensure_slash:bool=False):
    s = s.replace('/','\\')
    if strip_slash and s[-1:] == '\\':
        return s.rstrip('\\')
    if ensure_slash and not s[-1:] == '\\':
        return '%s\\' % s
    return s


def getAbsPath(s:str, posix:bool=True, strip_slash:bool=False, ensure_slash:bool=False):
    s = os.path.abspath( pathlib.Path(s).expanduser() )
    if posix:
        s = toPosixPath(s, strip_slash, ensure_slash)
    else:
        assert not (strip_slash or ensure_slash),\
            f"strip_slash and ensure_slash are only applicable when posix = True"

    return s


def pc(*args):
    if len(args) == 0: return
    if len(args) == 1: print(args[0]); return
    a = []
    for i, v in enumerate(args): a.append( ( v if i == 0 or isinstance(v, (int, float, complex, str)) else str(v) ) )
    print( a[0].format(*a[1:]) )



def pre(s, iChars=2):
    sPad = ' ' * iChars
    iF = s.find('\n')
    if iF == -1:
        return sPad + s
    sb = []
    iFL = 0
    while iF > -1:
        sb.append(sPad + s[iFL:iF])
        iFL = iF + 1
        iF = s.find('\n', iF + 1)
    sb.append('' if iF == len(s) else sPad + s[iFL:])
    return '\n'.join(sb)


class StopWatch:
    def __init__(self):
        self.start()
    def start(self):
        self._startTime = time.time()
    def getStartTime(self):
        return self._startTime
    def elapsed(self, prec=3):
        prec = 3 if prec is None or not isinstance(prec, int) else prec
        diff= time.time() - self._startTime
        return round(diff, prec)


def quickTT(header:list, max_width:int=120) -> tt:
    table = tt.Texttable(max_width=max_width)
    table.set_cols_align(list('r'*len(header)))
    table.set_cols_dtype(list('t'*len(header)))
    table.set_deco(table.VLINES)
    table.header(header)
    return table


def getCNStr(o):
    return f"({getClassName(o)}) - {str(o)}"

def funcName(depth=1):
    return sys._getframe(depth).f_code.co_name

def called(s=None):
    print(f"\n{funcName(2)}() called{' - ' + s if isinstance(s, str) else ''}")

def started(s=None):
    print(f"\n{'-' * 100}\n{funcName(2)}() started{' - ' + s if isinstance(s, str) else ''}\n")
