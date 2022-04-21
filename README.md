## ComStrip

Universal Code Comment Stripper and Diff Analysis tools leveraging Emacs Language support 

### Installation

```
python3 -m pip install commstrip
```

### Requirements
* emacs 28.1+
* language files for languages that need processing

Note: This code is presently written for Linux and shoudl work out of the box on MacOS and Cygwin. Windows support is planned.

### Create Alias - Add to ~/.bashrc
```
function __comstrip {
	python3 /dpool/vcmain/dev/lisp/comstrip/src/comstrip/comstrip.py "$@"
}
alias comstrip='__comstrip'
```

### CLI Usage:

#### 3-way meld compare
Compare /r1/src1.py to /r2/src2.py and /r2/src3.py, strip comments, blank lines and trailing spaces and launch in meld
```
comstrip --basedir /dpool/vcmain/dev/lisp/comstrip/test/data --noblank --notrail --meld --file /r1/src1.py /r2/src2.py /r3/src3.py
```

## Stats only
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

### Python API Usage:
See `test/comstrip_test.py` for API examples


### TODO:
[.] Make proper installer for system to get comstrip command working without alias
[.] Port to Windows
[.] Verify in Cygwin and MacOs
[.] Build out proper testing
[.] Implement leading tab handling
