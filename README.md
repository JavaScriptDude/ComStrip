## ComStrip

Universal Code Comment Stripper and Diff Analysis tools leveraging Emacs Language support 

### Installation

```
python3 -m pip install commstrip
```

### Requirements
* emacs 28.1+
* language files for languages that need processing

Note: This code is presently written for Linux and should work out of the box on MacOS and Cygwin but has not been tested there yet. Windows support is planned.

### Create Alias
Add to `~/.bashrc`:
```
function __comstrip {
	python3 /dpool/vcmain/dev/lisp/comstrip/src/comstrip/comstrip.py "$@"
}
alias comstrip='__comstrip'
```

### CLI Usage:

#### CLI - 3-way meld compare
Compare /r1/src1.py to /r2/src2.py and /r2/src3.py, strip comments, blank lines and trailing spaces and launch in meld
```
comstrip --basedir /dpool/vcmain/dev/lisp/comstrip/test/data --noblank --notrail --meld --file /r1/src1.py /r2/src2.py /r3/src3.py
```

#### CLI - Stats only
Compare /r1/src1.py to /r3/src1.py, strip comments, blank lines and trailing spaces and show stats
```
comstrip --basedir /dpool/vcmain/dev/lisp/comstrip/test/data --noblank --notrail --stats --file /r1/src1.py /r3/src2.py
```

Output:
```
Stats: /r1/src1.py <-> /r3/src1.py (/dpool/vcmain/dev/lisp/comstrip/test/data):
  add_l | mod_l | rem_l | tot_l | tot_chars
    161 |    25 |    28 |   109 |      7310

  Orig Src    |                       Clean Src                        |  Ok  | Note
/r1/vtscan.py | /tmp/cs_zz_cs_220420-211346.451785_vtscan_211347211.py | True |    -
/r3/vtscan.py | /tmp/cs_zz_cs_220420-211346.452998_vtscan_211347429.py | True |    -
```

#### CLI - Use Meld to troubleshoot stripping of comments etc.
Call with --meld option and pass only one file Meld will be launched showing the before and after of comment stripping
This can be used as you tweak your emacs packages to ensure that the Language processsing is working as expected
```
comstrip --meld --noblank --notrail --file /dpool/vcmain/dev/lisp/comstrip/test/data/r3/src1.py
```

### Python API Usage:
* ComStrip - Main API Object
* DiffStats - Object containging diff information including stats
* Diff - Representation of two files and contains a `stats:DiffStats` after processing
* DiffSet - Collection of Diffs for batch processing
* CSStat - Contains info about Comment Strip Processing: ok, file_in, file_out, message

Because of overhead of invoking sub-processes, it is best to use API that can process mulitple files in one batch, it is strongly recommended to use `<ComStrip>.process(diffset:DiffSet)` or `<ComStrip>.process(files:list)` to leverage the optimizations in those APIs.

See `test/hand_tests.py` for implementation of the four API call types: `diffset:DiffSet`, `diff:Diff`, `files:list` and `file:str`.


### Debug using VSCode after git checkout
If you want to play around without having to install, just do a git checkout of the repo and add to your `.vscode/settings.json`:
```
{
    "python.envFile": "${workspaceFolder}/dev.env"
}
```

### TODO:
[.] Make proper installer for system to get comstrip command working without alias
[.] Port to Windows
[.] Verify in Cygwin and MacOs
[.] Build out proper testing
[.] Implement leading tab handling