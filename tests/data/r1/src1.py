from __future__ import print_function
#########################################
# .: vtscan :.
# Verifies a file using VirusTotal API
# .: Sample :.
# export VT_API_KEY=<virus_total_api_key>
# .: usage :.
# vtscan <path_to_file>
# -or-
# python3 vtscan.py <path_to_file>
# .: deployment :.
# # put vtscan.py in a folder on your machine
# % alias vtscan="python3 <path_to_vtscan_folder>/vtscan.py"
# .: Other :.
# Author: Timothy C. Quinn
# Home: https://github.com/JavaScriptDude/vtscan
# Licence: https://opensource.org/licenses/MIT
# .: Todo :.
# [.] Add verbosity argument --verbose | -v
# [.] Streamline the output code
# [.] Generate Google search URLS for the user
#########################################

import os, sys, json, hashlib, traceback, pathlib
from virus_total_apis import PublicApi as VirusTotalPublicApi


def main(argv):

    # Check for Api key
    if "VT_API_KEY" not in os.environ:
        exit("Missing Virus total API Key. Please set VT_API_KEY environment variable!", 1)

    API_KEY=os.environ["VT_API_KEY"]
    if API_KEY.strip() == "":
        exit("Missing Virus total API Key. Please set VT_API_KEY environment variable!", 1)

    # Get fullpath (first arg)
    fullpath = argv[0]
    fname, fpath = splitPath(fullpath)

    # Verify that file exists
    if not os.path.isfile(fullpath):
        print("Please specify path to an existing file")

    # Get sha1 checksum of file
    digest_md5 = getChecksum(fullpath, 'md5')
    digest_sha1 = getChecksum(fullpath, 'sha1')
    digest_sha256 = getChecksum(fullpath, 'sha256')

    vt = VirusTotalPublicApi(API_KEY)

    response = vt.get_file_report(digest_sha256)

    if not response['response_code'] == 200:
        exit("Bad general response_code from virus total: {}".format(json.dumps(response, sort_keys=False, indent=4)), 1)

    res = response['results']
    if not res['response_code'] == 1:
        if res['resource'] == '87b566abab9888ff362058f90818f8dae6cf3e2a67de645e446a2999983a91a2':
            exit("""
.: VTScan Summary :.
- File: {0}
- Path: {1}
- md5: {2}
- sha1: {3}
- sha256: {4}
- Warning: {5}
-          {6}""".format(
                    fname, fpath
                    ,digest_md5, digest_sha1, digest_sha256
                    ,"File not found in VirusTotal database. Therefore its safety is unknown."
                    ,"Check if the download source has checksums to verify or Try googling one or all the checksums."
            ), 1)
        else:
            exit("Bad result response_code from virus total: {}".format(json.dumps(res, sort_keys=False, indent=4)), 1)


    # print("Raw virus total results: {}".format(json.dumps(res, sort_keys=False, indent=4)), 1)
    

    # Lets be paranoid and verify the checksums found
    warnings: list = []
    if not res['md5'] == digest_md5:
        warnings.append("MD5 Checksums do not match:\noriginal: {}\nreturned: {}".format(digest_md5, res['md5']))
    if not res['sha1'] == digest_sha1:
        warnings.append("SHA1 Checksums do not match:\noriginal: {}\nreturned: {}".format(digest_sha1, res['sha1']))
    if not res['sha256'] == digest_sha256:
        warnings.append("SHA256 Checksums do not match:\noriginal: {}\nreturned: {}".format(digest_sha256, res['sha256']))
        

    issues = res['positives']

    print("""
.: VTScan Summary :.
-   File: {0}
-   Path: {1}
-    md5: {2}
-   sha1: {3}
- sha256: {4}
- issues: {5} out of {6} {7}
- {8}
~      """.format(
             fname, fpath
            ,digest_md5, digest_sha1, digest_sha256
            ,issues, res['total'], ('(see above)' if issues > 0 else '(100% pass)' )
            ,"{} {}".format( ('permalink:' if issues == 0 else 'Navigate to for more info:'), res['permalink'])
    ))

    if len(warnings) > 0:
        print("Possible API Issues Detected in Python:\n{}\n~".format('\n'.join(warnings)))




def getChecksum(fullpath, csumtype):
    if csumtype == 'md5':
        h  = hashlib.md5()
    elif csumtype == 'sha1':
        h  = hashlib.sha1()
    elif csumtype == 'sha256':
        h  = hashlib.sha256()
    else:
        raise Exception("Unexpected csumtype: {}".format(csumtype))
    b  = bytearray(128*1024)
    mv = memoryview(b)
    with open(fullpath, 'rb', buffering=0) as f:
        for n in iter(lambda : f.readinto(mv), 0):
            h.update(mv[:n])
    return h.hexdigest()


def exit(s, exitCode=1):
    if not s is None:
        print(s)
    print('~')
    sys.stdout.flush()
    sys.stderr.flush()
    sys.exit(exitCode)


def splitPath(s):
    f = os.path.basename(s)
    p = s[:-(len(f))-1]
    p = toPosixPath(getAbsPath(p))
    return f, p

def toPosixPath(s:str, strip_slash:bool=False, ensure_slash:bool=False):
    s = s.strip().replace('\\', '/')
    if strip_slash and s[-1:] == '/':
        return s[:-1]
    if ensure_slash and not s[-1:] == '/':
        return '%s/' % s
    return s

def getAbsPath(s:str):
    return os.path.abspath( pathlib.Path(s).expanduser() )


if __name__ == '__main__':
    iExit = 0
    try:
        main(sys.argv[1:])
    except Exception:
        exc_type, exc_value, exc_traceback = sys.exc_info()
        aTB = traceback.format_tb(exc_traceback)
        exit("Fatal exception: {}\n - msg: {}\n stack: {}".format(exc_type, exc_value, '\n'.join(aTB)), exitCode=1)
    sys.exit(iExit)
