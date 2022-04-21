#!/usr/bin/env python
#########################################
# .: ComStrip :.
# Code Comment Stripper and Whitespace remover
# .: usage :.
# comstrip --meld --noblank --notrail --file <file1> [<file1> [<file3>]]
# .: Other :.
# Author: Timothy C. Quinn
# Home: https://github.com/JavaScriptDude/ComStrip
# Licence: https://opensource.org/licenses/MIT
# .: Todo :.
# [.] Handle notrail option
# [.] Handle noblank option
# [.] Handle ignore_tab option
# [.] Use TheFuzz package to see if diff's were Mods, removes or replaces 
#          See: https://github.com/seatgeek/thefuzz
#               https://stackoverflow.com/a/41730575/286807
# [.] Get this working in Cygwin
#########################################
__package__ = __name__
import os
import sys
import stat
import traceback
import argparse
import shutil
import tempfile
import shutil
import pexpect
import tempfile
import subprocess
import difflib
from tiotrap import TextIOTrap
from datetime import datetime
from thefuzz import fuzz
import texttable as tt
import comstrip.util as util

pc = util.pc
exit = util.exit

def main():
    argp = argparse.ArgumentParser(prog='comstrip')
    argp.add_argument('-b', '--noblank'   , help='Ignore noblank Lines'
                                        , default=False, action='store_true')
    argp.add_argument('-t', '--notrail', help='Ignore notrail spaces'
                                        , default=False, action='store_true')
    argp.add_argument('-v', '--verbose' , help='Verbose output'
                                        , default=False, action='store_true')
    argp.add_argument('-s', '--stats'   , help='Show diff statistics'
                                        , default=False, action='store_true')
    argp.add_argument('-m', '--meld'    , help='Use meld to display diffs'
                                        , default=False, action='store_true')
    argp.add_argument('-d', '--basedir' , help='Base path to find files'
                                        , type=str, default=None)
    argp.add_argument('-E', '--emacs'   , help='Path to emacs executable'
                                        , type=str, default=None)
    argp.add_argument('-f', '--file'    , help='File(s) to scan'
                                        , type=str, nargs='+')
    argp.add_argument('-T', '--difftool', help="Executable and opts for diff tool in single string. use %%F1%% %%F2%% for file names with comments stripped and %%OF1%% %%OF2%% for originals. Use %%Q%% for double quote."
                                        , type=str, default=None)
    

    opts = argp.parse_args()

    assert len(opts.file) > 0, f"At least one file is required; see (--file)"
    assert len(opts.file) < 4, f"Maximum number of files is 3; see (--file)"

    if opts.basedir: 
        opts.basedir = util.getAbsPath(opts.basedir, posix=True, strip_slash=True)

    files = []
    for i, file in enumerate(opts.file):
        file = util.toPosixPath(file.strip())
        assert len(file) > 0, f"Empty file passed"
        if opts.basedir:
            (_, _pt) = util.splitPath(file)
            if not _pt == '' and file[0] != '/':
                raise AssertionError("File {0} passed is invalid. When providing basedir parameter, file path must start with a {1}".format(i+1, '\\' if util.isWin() else '/'))
            
        file_full_path = file if opts.basedir is None else f"{opts.basedir}{file}"
        assert not os.path.isdir(file_full_path), f"File given in --file is a directory: '{file_full_path}'"
        assert os.path.isfile(file_full_path), f"File given in --file cannot be found: '{file_full_path}'"
        files.append(file_full_path)

    if opts.difftool or opts.stats:
        assert len(files) > 1, f"Invalid number of files for diff. Expecting two or three. Got: {len(files)}"

    if opts.meld and opts.difftool:
        raise AssertionError("--meld and --difftol options are mutually exclusive")


    try:
        comstip = None
        try:
            comstip = ComStrip(noblank=opts.noblank, notrail=opts.notrail, verbose=opts.verbose)
        except ArgsError as ex:
            argp.print_help()
            exit(f"\n{ex.args[0]}", code=1)

        d_cs_ret:dict = comstip.process(files=files)

        if opts.difftool:
            _diff_cmd = opts.difftool.replace(r"%Q%", '"')

        _issues = []
        _files_out = []
        css:CSStat=None
        for i, f_in in enumerate(files):
            if not f_in in d_cs_ret:
                raise Exception(f"File key {f_in} not found in return from comstrip.process() d_cs_ret: {d_cs_ret}")
            css = d_cs_ret[f_in]
            _f_out = css.file_out
            _files_out.append(_f_out)
            f_in_use = f_in if opts.basedir is None else f_in.replace(opts.basedir, '')
            if not css.ok:
                _issues.append(f"Issue calling comstrip for file '{f_in}': {css.message}")
            else:
                if opts.difftool:
                    _diff_cmd = _diff_cmd.replace(f'%OF{i+1}%', f_in_use)
                    _diff_cmd = _diff_cmd.replace(f'%F{i+1}%', _f_out)

        if len(_issues)>0:
            s = util.pre(util.join(_issues, '\n'))
            exit(f"Issues with tool:\n{s}\n", code=1)

        _f_in_short_1=_f_out_1=_f_in_short_2=_f_out_2=_f_in_short_3=_f_out_3=None
        if opts.stats or opts.meld:
            _f_in_short_1 = files[0] if opts.basedir is None else files[0].replace(opts.basedir, '')
            _f_out_1 = _files_out[0]
            if len(files) > 1:
                _f_in_short_2 = files[1] if opts.basedir is None else files[1].replace(opts.basedir, '')
                _f_out_2 = _files_out[1]
            if len(files) > 2:
                _f_in_short_3 = files[2] if opts.basedir is None else files[2].replace(opts.basedir, '')
                _f_out_3 = _files_out[2]
        
        if opts.stats: # Show diff summary
            for i in range(1, len(files)+1):
                _out_L      = _f_out_1      if i in (1,3)  else _f_out_2
                _in_L_short = _f_in_short_1 if i in (1,3)  else _f_in_short_2
                _out_R      = _f_out_2      if i == 1 else _f_out_3
                _in_R_short = _f_in_short_2 if i == 1  else _f_in_short_3
                
                ds = get_diff(_out_L, _out_R)
                print(f"\nStats: {_in_L_short} <-> {_in_R_short}{'' if opts.basedir is None else ' (' + opts.basedir + ')'}:")
                print(util.pre(ds.__str__(as_table=True)))

                if len(files) == 2: break
            
        if opts.difftool:
            print(f"Launching diff tool: {_diff_cmd} ...")
            os.system(_diff_cmd)

        elif opts.meld:
            dq = '"'
            cmd = ["meld"]
            if len(files) == 1:
                cmd.append(f"--label={dq}(orig <-> stripped){dq}")
                cmd.append(files[0])
            elif len(files) == 2:
                cmd.append(f"--label={dq}({_f_in_short_1} <-> {_f_in_short_2}){dq}")
            else:
                cmd.append(f"--label={dq}({_f_in_short_1} <-> {_f_in_short_2}) <-> {_f_in_short_3}){dq}")

            for f_out in _files_out:
                cmd.append(f_out)

            print(f"Launching: {' '.join(cmd)} ...")
            try:
                p = subprocess.run(cmd, check=True, stdout=subprocess.PIPE)
            except Exception as ex:
                exit(f"Failed while calling meld: {util.getClassName(ex)} - {ex.args[0]}", code=2)

        else:
            table = util.quickTT(['Orig Src', 'Clean Src', 'Ok', 'Note'])
            for k, css in d_cs_ret.items():
                table.add_row([k if opts.basedir is None else k.replace(opts.basedir, ''), css.file_out, css.ok, {'-' if css.ok else css.message}])

            pc(f"\n{table.draw()}\n")


    except Exception as ex:
        exc_type, exc_value, exc_traceback = sys.exc_info()
        aTB = traceback.format_tb(exc_traceback)
        exit("Program Exception:\nStack:\n{}\n Error: {} - {}".format('\n'.join(aTB), exc_type.__name__, exc_value), code=1)





__chk_file=None # to silence type checkers
def _ComStrip__chk_file(self, file:str, alias:str, ind:int=None):
    def _errstr(s):
        sb=[]
        if not ind is None: sb.append(f" at index {ind}")
        return f"File specified for {alias} {''.join(sb)} {s}"

    if not isinstance(file, str) or file.strip() == '': 
        raise ArgsError(_errstr(f" is not a string. Got: {util.util.getClassName(file)}"), 1)

    full_path = file if self.base_dir is None else f"{self.base_dir}{file}"

    if not os.path.isfile(full_path):
        raise ArgsError(_errstr(f"not found in file system - {full_path}"), 1)

    return file


class ComStrip():
    def __init__(self, base_dir:str=None, noblank:bool=False, ignore_tab:bool=False, notrail:bool=False, verbose:bool=False, emacs:str=None):
        global _verbose, C_

        util.os_gate()

        if not base_dir is None:
            base_dir = util.getAbsPath(base_dir, posix=True, strip_slash=True)
            if isinstance(base_dir, str):
                base_dir = util.getAbsPath(base_dir, posix=True, strip_slash=True)
                assert os.path.isdir(base_dir),\
                    f"Invalid base_dir. Not found!"

            else:
                base_dir = None

        self.base_dir = base_dir
        self.verbose = verbose
        self.noblank = noblank
        self.notrail = notrail

        if not _verbose: _verbose = verbose
        
        if emacs:
            if not os.path.isfile(emacs):
                raise ArgsError("\nEmacs executable path passed is invalid", 1)
            self.emacs_path = emacs
        else:
            self.emacs_path = shutil.which('emacs')
            if not self.emacs_path:
                raise ArgsError("\nEmacs not found on system", 1)


    # Mutually exlcusive:
    # - file: str (single file)
    #       Returns CSStat
    # - files: list(of str)
    #       Returns dict of CSStat with key of file_in
    # - diff: Diff
    #       Returns Diff
    # - diffset: DiffSet
    #       Returns DiffSet
    def process(self, **kwargs):
        global _verbose, C_
    
        file = kwargs.get('file', None)
        files = kwargs.get('files', None)
        diff = kwargs.get('diff', None)
        diffset = kwargs.get('diffset', None)

        # check params
        assert util.ismuex(file, files, diff, diffset),\
            f"Arguments files, diff, diffset are mutually exclusive"

        if diff:
            mode = 'diff'
            assert isinstance(diff, Diff),\
                f"diff must be a Diff. Got: {util.getClassName(diff)}"
            file_left = diff.file_left
            file_right = diff.file_right
            __chk_file(self, diff.get_full_path(file_left), 'diff.file_left')
            __chk_file(self, diff.get_full_path(file_right), 'diff.file_right')
            files = [file_left, file_right]

        elif diffset:
            mode = 'diffset'
            assert isinstance(diffset, DiffSet),\
                f"diffset must be a DiffSet. Got: {util.getClassName(diffset)}"
            files=[]
            for i, diff_c in enumerate(diffset):
                files += [__chk_file(self, diff_c.file_left, 'diffset diff - file_left', i), __chk_file(self, diff_c.file_right, 'diffset diff - file_right', i)]

        elif files:
            mode = 'files'
            assert isinstance(files, list),\
                f"files must be a list. Got: {util.getClassName(files)}"
            for i, file in enumerate(files):
                __chk_file(self, file, 'files', i)

        else:
            mode = 'file'
            assert isinstance(file, str),\
                f"file must be a str. Got: {util.getClassName(files)}"
            __chk_file(self, file, 'file')
            files = [file]
                


        if mode == 'diffset':
            _base_dir=diffset.base_dir
        elif mode == 'diff':
            _base_dir=diff.base_dir
        else:
            _base_dir=self.base_dir
    
        _getdiffs = mode in ('diff', 'diffset')


        pexpect_out = []
        def _emacs_write(s):# pexpect adds extra newline for each write
            if len(s) > 0:
                s = s.replace('\r', '\n')
                s = s[:-1] if s[-1:] == '\n' else s
                pexpect_out.append(s)
                
        ttrap = TextIOTrap(write_handler=_emacs_write)

        def _spawn(cmd):
            child = pexpect.spawn(cmd)
            child.setwinsize(10,2048)
            child.logfile = ttrap
            child.expect(pexpect.EOF)


        cwd = os.getcwd()
        def _cmd(f):
            file_loc = util.toWinPath(f) if util.isWin() else util.toPosixPath(f)
            return f"""{self.emacs_path} --file="{file_loc}" --quick --batch --eval '(load "{C_.COMSTRIP_EL}")' """

        # strip lines / whitespace (in tmp file)
        _altfiles=None
        _linecounts=None
        
        _filterwhtsp = (self.noblank or self.notrail)
        if _getdiffs or _filterwhtsp:
            if _filterwhtsp: _altfiles={}
            if _getdiffs: _linecounts={}
            _tmp=[]
            for i, file in enumerate(files):

                file_full = self._get_full_path(_base_dir, file)

                _getcount = _getdiffs and (i%2 == 0)
                if _filterwhtsp:
                    (_f, _) = util.splitPath(file_full)
                    tmp_file = f"/tmp/zz_cs_{datetime.now().strftime('%y%m%d-%H%M%S.%f')}_{_f}"
                    _altfiles[tmp_file] = file_full
                    _tmp.append(tmp_file)

                if _getcount: _iLC = 0
                if _filterwhtsp:
                    with open(file_full, 'r') as fI:
                        with open(tmp_file, 'w') as fO:
                            for j, line in enumerate(fI):
                                line = line.rstrip('\r\n')
                                line_rs = line.rstrip(' ')
                                if line_rs == '' and self.noblank: continue
                                if _getcount: _iLC += 1
                                fO.write(f"{line if not self.notrail else line_rs}\n")

                elif _getcount: # is diff and left file
                    _iLC = fast_lc(file_full)
                
                if _getcount: _linecounts[file_full] = _iLC

                files = _tmp


        if len(files) == 1: # use pexpect to run emacs directly
            file_use = files[0] if _altfiles else self._get_full_path(_base_dir, files[0])
            cmd = _cmd(file_use)
            if _verbose:
                pc(f"[V] running cmd: {cmd} ...")
            _spawn(cmd)

        else: # write to temporary shell file and then execute it as this is faster
            # if _verbose:
            #     s = '\n'.join(files)
            #     pc(f"[V] files dump:\n{s}")

            sh_file = tempfile.NamedTemporaryFile(suffix='.sh', prefix=f'zz_comstrip_{util.getMachineDTMS()}_', dir="/tmp").name

            with open(sh_file, 'w') as f:
                f.write("#!/bin/bash")
                _t=[]
                for file in files:
                    if not _altfiles:
                        file = self._get_full_path(_base_dir, file.strip())
                    # skip dupes
                    if file in _t: 
                        if _verbose:
                            pc(f"[V] skipping dupe: {file}")
                        continue
                    _t.append(file)

                    cmd = _cmd(file)
                    f.write("\n")
                    f.write(cmd)


            st = os.stat(sh_file)
            os.chmod(sh_file, st.st_mode | stat.S_IEXEC)

            # if _verbose:
            #     pc(f"[VERBOSE] sh file: {sh_file}")
            #     with open(sh_file, 'r') as f:
            #         pc( '\n'.join(f.readlines()) )
            #     pc("~end~")

            _spawn(sh_file)

            # Delete sh file
            assert sh_file.find("/tmp/zz_") == 0, f"Invalid sh file: {sh_file}"
            os.remove(sh_file)


        elisp_out = ''.join(pexpect_out)

        if elisp_out.find("tramp-file-name-structure") > -1:
            raise Exception("Issue detected with Emacs. Its likely due to version. Version know to work ok: 28.1. See: https://www.reddit.com/r/emacs/comments/u7i04k/emacs_cannot_open_file_within_zfs_snapshot_folders/")

        elisp_lines = elisp_out.split('\n')
        def _eo(): return f"Full elisp output:\n{elisp_out}\n~end~\n"
        _dout = {}
        for i, line in enumerate(elisp_lines):
            # if _verbose:
            #     pc(f"[V] - Elisp Line [{i:02}]: {line}")
            if line.find('.:CSMSG_S:.') == 0:
                iF = line.find('.:CSMSG_E:.')
                assert iF > 0,\
                    f"Expecting to find .:CSMSG_E:. at end of line!. line: {line}. {_eo()}"
                sTok = line[11:iF]
                aTok = sTok.split("\t")
                assert len(aTok) == 3,\
                    f"Unexpected CSMSG string in. Need 3 tokens split by tab. Got: {sTok}. {_eo()}"

                (status, file_sent, msg) = aTok

                assert status in ('ok', 'failed'),\
                    f"invalid status token in CSMSG block #{i+1}. Got: '{status}'! {_eo()}"
            
                assert not file_sent == '',\
                    f"file_sent token in CSMSG block #{i+1} cannot be noblank! {_eo()}"

                assert not msg == '',\
                    f"msg token in CSMSG block #{i+1} cannot be noblank! {_eo()}"

                assert not file_sent in _dout,\
                    f"Duplicate file_sent detected ({file_sent}) in CSMSG block #{i+1}! {_eo()}"

                if _altfiles:
                    file_in = _altfiles[file_sent]
                else:
                    file_in = file_sent

                _dout[file_in] = CSStat(status.strip(), file_in.strip(), msg.strip())

        # Delete temp files
        if _altfiles:
            for tmp_file in _altfiles:
                assert tmp_file.find("/tmp/zz_") == 0, f"Invalid temp file: {tmp_file}"
                os.remove(tmp_file)


        

        if _getdiffs:
            diffs_c = [diff] if diff else diffset.diffs if diffset else None
            if diffs_c:
                for diff_c in diffs_c:
                    _lr=[None, None]
                    _errs=[]
                    
                    bBothOk=True
                    for i in (0,1):
                        _fname = diff_c.file_right if i else diff_c.file_left
                        # get base_dir


                        _fname_full = self._get_full_path(_base_dir, _fname)

                        if not _fname_full in _dout:
                            if _verbose:
                                pc(f"[V] _dout dump for debug:")
                                for i, (k, v) in enumerate(_dout.items()): pc(f"  [{i}] - {v}\n")
                            raise Exception(f"{'file_right' if i else 'file_left'} from diff not found in _dout! _fname_full: {_fname_full}")
                        css = _dout[_fname_full]
                        diff_c.csstatus = css
                        if not css.ok:
                            bBothOk = False
                            _errs.append(f"{'file_right' if i else 'file_left'} from diff ({_fname_full}) had elisp issue: {css.message}")

                        else:
                            _lr[i] = css.file_out

                    if not bBothOk:
                        diff_c.ok = False
                        diff_c.message = ' '.join(_errs)

                    else:
                        diff_c.stats = get_diff(_lr[0], _lr[1], lc=_linecounts[self._get_full_path(_base_dir, diff_c.file_left)])
                        diff_c.ok = True

                    
        if mode in ('file', 'files'):
            _dret = {}
            iBaseFrom = 0 if self.base_dir is None else len(self.base_dir)+1
            css=None
            for file_in_full in _dout:
                css = _dout[file_in_full]
                file_in_use = file_in_full[iBaseFrom:]
                _dret[file_in_use] = css

            if mode == 'file':
                return css
            else:
                return _dret
        
        elif mode == 'diffset': 
            return diffset

        elif mode == 'diff': 
            return diff

    def get_diffs(self):
        pass

    @staticmethod
    def _get_full_path(_base_dir, file):
        return file if _base_dir is None else f"{_base_dir}{file}"





class DiffStats():
    add_l :int # lines added
    add_c :int # chars added
    mod_l :int # lines modified
    mod_c :int # chars modified
    rem_l :int # lines removed
    rem_c :int # chars removed
    tot_l :int # total lines (left file)
    gnudiff :str # gnu diff output
    def __init__(self, data):
        (addL, addC, modL, modC, remL, remC, lc, gnudiff) = data
        self.add_l = addL
        self.add_c = addC
        self.mod_l = modL
        self.mod_c = modC
        self.rem_l = remL
        self.rem_c = remC
        self.tot_l = lc
        self.gnudiff = gnudiff

    def __str__(self, as_table:bool=False):
        t=self
        if as_table:
            table = tt.Texttable(max_width=200)
            table.set_cols_align( ['r', 'r', 'r', 'r', 'r'] )
            table.set_cols_dtype( ['i', 'i', 'i', 'i', 'i'] )
            table.set_deco(table.VLINES)
            table.header(['add_l', 'mod_l', 'rem_l', 'tot_l', 'tot_chars'])
            table.add_row([self.add_l, self.mod_l, self.rem_l, self.tot_l, (self.add_c + self.mod_c + self.rem_c)])
            return table.draw()
        else:
            return f"ln add: {t.add_l}, ln mod: {t.mod_l}, ln rem: {t.rem_l}, ln tot: {t.tot_l}, chars tot: {(t.add_c + t.mod_c + t.rem_c)}"
    def __repr__(self):
        return f"<DiffStats> {self.__str__()}"


class DiffSet():
    diffs:list=None # list(of Diff)
    def __init__(self, base_dir:str=None):
        self.diffs=[]
        if not base_dir is None:
            base_dir = util.getAbsPath(base_dir, posix=True, strip_slash=True)
            assert os.path.isdir(base_dir), f"base_dir provided does not exist: {base_dir}"
        self.base_dir = base_dir

    def add(self, file_left:str, file_right:str):
        self.diffs.append(Diff(file_left, file_right, base_dir=self.base_dir))

    def __iter__(self):
        return iter(self.diffs)

    def __str__(self):
        sb=[]
        for i, diff in enumerate(self):
            (_f,_) = util.splitPath(diff.file_left)
            if i > 0: sb.append('\n')
            sb.append(f'Diff for {_f}: {str(diff)}')
        return ''.join(sb)



class Diff():
    ok:bool=False
    file_left:str
    file_right:str
    # initialized in ComStrip.process
    css_left = None # CSStat for left file
    css_right = None # CSStat for right file
    stats: DiffStats = None
    def __init__(self, file_left, file_right, base_dir:str=None):
        if not base_dir is None:
            base_dir = util.getAbsPath(base_dir, posix=True, strip_slash=True)
            assert os.path.isdir(base_dir), f"base_dir directory not found: {base_dir}"

        self.base_dir = base_dir
        _file_left_use = self.get_full_path(file_left)
        _file_right_use = self.get_full_path(file_right)

        assert os.path.isfile(_file_left_use),f"file_left not found: {_file_left_use}"
        assert os.path.isfile(_file_right_use),f"file_left not found: {_file_right_use}"
        
        self.file_left = file_left
        self.file_right = file_right

    def get_full_path(self, file):
        return file if self.base_dir is None else f"{self.base_dir}{file}"

    def __str__(self):
        return f"status: {'ok' if self.ok else 'fail'} stats: {str(self.stats)}"



# Get DiffStats using gnu diff   
def get_diff(path_left, path_right, lc:int=None) -> 'DiffStats':
    global C_
    if not isinstance(path_left, str):
        raise AssertionError(f"path_left must be string. got: {util.getCNStr(path_left)}")
    if not isinstance(path_right, str):
        raise AssertionError(f"path_right must be string. got: {util.getCNStr(path_right)}")

    util.os_gate()

    lc = fast_lc(path_left) if lc is None \
                                 else lc

    cmd = ['diff', '-ubwB', path_left, path_right]
    if _verbose:
        print(f"[V] Running cmd: {' '.join(cmd)}")

    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()

    if p.returncode > 1:
        print("!!!!!! Warning. Return code !=0 `{}`: {}. Code: {}".format(cmd, stderr, p.returncode))
        return (0,0, stderr)

    elif len(stderr) > 0:
        # return code is > 1 (no error), but there is a message in stderr
        if _verbose:
            print(f"`{cmd}` return code is 0 but had stderr msg: {stderr}")

    if p.returncode == 0:
        return (0, 0, None)

    assert stdout != None, "WARNING - stdout is None!"


    gnudiff = stdout.decode('utf-8')

    if len(stdout) == 0:
        return (0, 0, 0, 0, lc, "")
    

    addL_t = 0
    modL_t = 0
    remL_t = 0
    nochg = -1

    iF = gnudiff.find("@@")
    assert iF > -1, f"Invalid gnudiff. @@ not found.\nudiff output:\n{gnudiff}\n~end~\n"

    # returns (addL, addC, modL, modC, remL, remC)
    def _pgroup(addlines, remlines):
        def _cc(lines):
            i=0
            for line in lines: i+=len(line)
            return i
        iA = len(addlines)
        iR = len(remlines)
        
        if iA > 0 and iR == 0:
            return (iA, _cc(addlines), 0, 0, 0, 0)
        elif iR > 0 and iA == 0:
            return (0, 0, 0, 0, iR, _cc(remlines))

        _aL=_mL=_rL=0
        _aC=_mC=_rC=0

        AL_use = []
        ML_use = []
        RL_use = []
        _aC = _mC = _rC = 0
        
        aModIndxs=[]
        for rline in remlines:
            bIsMod=False
            for j, aline in enumerate(addlines):
                iFuzz = fuzz.ratio(rline, aline)
                if iFuzz < 60 and iFuzz > 40:
                    pc()
                if iFuzz >= 60: # is mod
                    bIsMod=True
                    ML_use.append((rline, aline))
                    diffs = [s for s in difflib.ndiff(rline, aline) if s[0]!=' ']
                    iD = int(len(diffs) * 0.75)
                    _mC += (1 if iD == 0 else iD)
                    aModIndxs.append(j)
                    break
            
            if not bIsMod:
                RL_use.append(rline)
                _rC += len(rline)

        for i, aline in enumerate(addlines):
            if not i in aModIndxs:
                AL_use.append(aline)
                _aC +=  len(aline)


        _aL = len(AL_use)
        _mL = len(ML_use)
        _rL = len(RL_use)

        return (_aL, _aC, _mL, _mC, _rL, _rC)

    addL_t=modL_t=remL_t=0
    addC_t=modC_t=remC_t=0
    addL_c=modL_c=remL_c=0
    addC_c=modC_c=remC_c=0
    addl_c=reml_c=None
    inmaingr=insubgr=False
    c=None
    stdebug=False

    def _outgr(ismain):
        nonlocal inmaingr, insubgr, addL_t, modL_t, remL_t \
                         , addC_t, modC_t, remC_t \
                         , addL_c, modL_c, remL_c \
                         , addC_c, modC_c, remC_c \
                         , reml_c, addl_c
        if stdebug:
            pc()

        if ismain: # out of maingroup
            # dont set inmaingr to False
            addL_t += addL_c
            modL_t += modL_c
            remL_t += remL_c
            addC_t += addC_c
            modC_t += modC_c
            remC_t += remC_c
            addL_c = modL_c = remL_c = 0
            addC_c = modC_c = remC_c = 0
            
        else: # out of subgroup
            insubgr = False
            (_aL, _aC, _mL, _mC, _rL, _rC) = _pgroup(addl_c, reml_c)
            reml_c=[]
            addl_c=[]
            addL_c += _aL
            modL_c += _mL
            remL_c += _rL
            addC_c += _aC
            modC_c += _mC
            remC_c += _rC


    for line in gnudiff.splitlines():
        if line[0:3] in ('---', '+++'): continue

        if line [0:2] == '@@': # new main group
            if insubgr: _outgr(ismain=False)
            if inmaingr: _outgr(ismain=True)
            inmaingr=True
            continue

        c=line[0]
        M = (c == '-')
        P = (c == '+')

        if P and line.find("log.p(") > -1:
            stdebug=True



        if M or P:
            if stdebug:
                pc()
            if not insubgr:
                insubgr = True
                reml_c=[]
                addl_c=[]
            if M: 
                reml_c.append(line[1:].strip())
            if P: 
                addl_c.append(line[1:].strip())

        else:
            if insubgr: 
                _outgr(ismain=False)

    if insubgr: _outgr(ismain=False)
    if inmaingr: _outgr(ismain=True)


    return DiffStats( (addL_t, addC_t, modL_t, modC_t, remL_t, remC_t, lc, gnudiff) )
           


class CSStat():
    ok  :bool
    file_in  :str
    file_out :str
    message  :str
    def __init__(self, status, file_in, file_out):
        self.file_in = file_in
        self.ok = (status == 'ok')
        if self.ok:
            self.file_out = file_out
            assert os.path.isfile(self.file_out),\
                f"File out returned from emacs::comstrip.el ({self.file_out}) not found! file in = '{file_in}'"
            self.message = "-"
        else:
            self.file_out = "-"
            self.message = file_out

    def __str__(self):
        return f"status: {'ok' if self.ok else 'failed'}, file_in: '{self.file_in}', file_out: '{self.file_out}', msg: '{self.message}'"

    def __repr__(self):
        return f"<CSStat> {self.__str__()}"
  

class ArgsError(Exception):
    pass


def fast_lc(fname):
    def _make_gen(reader):
        while True:
            b = reader(2 ** 16)
            if not b: break
            yield b

    with open(fname, "rb") as f:
        count = sum(buf.count(b"\n") for buf in _make_gen(f.raw.read))
    return count


_verbose = ("--verbose" in sys.argv)
class C_(util.C_):
    PKG_ROOT = __file__.replace('/src/comstrip/comstrip.py', '')
    COMSTRIP_EL = None

(_f, _p) = util.splitPath(__file__)
C_.COMSTRIP_EL = f"{_p}/comstrip.el"

if __name__ == '__main__':
    main()
