import os
import texttable as tt
from comstrip import ComStrip, DiffSet, Diff, CSStat, csutil
# TODO:
# [.] Make proper unittest2 based tests



def main():
    test_DiffSet()
    test_Diff()
    test_files(use_base_dir=True)
    test_files(use_base_dir=False)
    test_file(use_base_dir=True)
    test_file(use_base_dir=False)


# Getting Comment Stripping and Diffs using a DiffSet
# - DiffSet is a collection of Diffs to be processed in a batch
# - Will stip comments, whitespace, save to a temp file and gather diff stat
def test_DiffSet():
    csutil.started()

    _pL = "/r1"
    _pR = "/r2"

    _base_dir = C_.TEST_DATA_DIR

    sw = csutil.StopWatch()
    ds = DiffSet(base_dir=_base_dir)
    for file in ('src1.py', 'src2.py'):
        ds.add(f'{_pL}/{file}' ,f'{_pR}/{file}')
    
    comstip = ComStrip(
             base_dir=_base_dir
            ,noblank=True
            ,notrail=True
    )

    comstip.process(diffset=ds)
    
    table = tt.Texttable(max_width=200)
    table.set_cols_align( ['l', 'l', 'l', 'c', 'r', 'r', 'r', 'r', 'r'] )
    table.set_cols_dtype( ['t', 't', 't', 't', 'i', 'i', 'i', 'i', 'i'] )
    table.set_deco(table.VLINES)
    table.header(['Path L', 'Path R', 'File', 'Ok', 'add_l', 'mod_l', 'rem_l', 'tot_l', 'tot_c'])
    
    for diff in ds:
        (_fL,_pL) = csutil.splitPath(diff.file_left)
        (_fR,_pR) = csutil.splitPath(diff.file_right)
        st = diff.stats
        table.add_row([_pL, _pR, _fL, diff.ok, st.add_l, st.mod_l, st.rem_l, st.tot_l, (st.add_c + st.mod_c + st.rem_c)])

    pc(f"Data Returned:\n{table.draw()}\n")

    # pc(ds)

    pc(f"\n{csutil.funcName()}() done. Elapsed: {sw.elapsed(1)}s")


# Getting Comment Stripping for single Diff of two files
# - Will stip comments, whitespace, save to a temp file and gather diff stat
def test_Diff():
    csutil.started()

    sw = csutil.StopWatch()
    diff = Diff(f'/r1/src2.py', f'/r2/src2.py', base_dir=C_.TEST_DATA_DIR)
    
    comstip = ComStrip(noblank=True, notrail=True)

    comstip.process(diff=diff)

    pc(f"\nStats: {diff.file_left} <-> {diff.file_right}{' (' + C_.TEST_DATA_DIR + ')'}:")
    pc(csutil.pre(diff.stats.__str__(as_table=True)))

    pc(f"\n{csutil.funcName()}() done. Elapsed: {sw.elapsed(1)}s")


# Getting for a list of files to run in a large batch
# - Will stip comments, whitespace, save to a temp file
def test_files(use_base_dir:bool):
    csutil.started(f"use_base_dir: {use_base_dir}")
    _pL = "/r1"
    _pR = "/r2"

    _base_dir = C_.TEST_DATA_DIR

    sw = csutil.StopWatch()
    files = []
    for file in ('src1.py', 'src2.py'):
        files.append(f"{'' if use_base_dir else _base_dir + '/'}{_pL}/{file}")
        files.append(f"{'' if use_base_dir else _base_dir + '/'}{_pR}/{file}")
    
    comstip = ComStrip(
             base_dir=(_base_dir if use_base_dir else None)
            ,noblank=True
            ,notrail=True
    )

    d_info = comstip.process(files=files)

    table = tt.Texttable(max_width=200)
    table.set_cols_align( ['l', 'l', 'l', 'l'] )
    table.set_cols_dtype( ['t', 't', 't', 't'] )
    table.set_deco(table.VLINES)
    table.header(['file', 'ok', 'no_com_file', 'ok_rsn'])
    for file_in, css in d_info.items():
        table.add_row([file_in, css.ok, css.file_out, {'-' if css.ok else css.message}])

    pc(f"Data Returned:\n{table.draw()}\n")

    pc(f"\n{csutil.funcName()}() done. Elapsed: {sw.elapsed(1)}s")
    
# Getting for a single file
# - Will stip comments, whitespace, save to a temp file
def test_file(use_base_dir:bool):
    global _verbose
    csutil.started(f"use_base_dir: {use_base_dir}")
    sw = csutil.StopWatch()

    _base_dir = C_.TEST_DATA_DIR

    comstip = ComStrip(base_dir=(_base_dir if use_base_dir else None), noblank=True, notrail=True)

    file = f"{'' if use_base_dir else _base_dir}/r1/src1.py"
    css = comstip.process(file=file)
    

    pc(f"\nOutput: {css}")

    pc(f"\n{csutil.funcName()}() done. Elapsed: {sw.elapsed(1)}s")

pc = csutil.pc

class C_(csutil.C_):
    TEST_DATA_DIR = None
    CWD = os.getcwd()


C_.TEST_DATA_DIR = (f"{C_.CWD}/tests" if not C_.CWD in __file__ else __file__.replace('/hand_tests.py', '')) + "/data"


if __name__ == '__main__':
    main()
