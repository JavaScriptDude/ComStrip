from __future__ import print_function
#########################################
# .: vtscan :.
# Verifies a file using VirusTotal API
# .: install dependencies :.
# python3 -m pip install -r requirements.txt
# .: Sample :.
# export VT_API_KEY=<virus_total_api_key>
# .: usage :.
# vtscan <path_to_file>
# -or-
# python3 vtscan.py <path_to_file>
# .: deployment :.
# # put vtscan.py in a folder on your computer by hand or using git
# % alias vtscan="python3 <path_to_vtscan_folder>/vtscan.py"
# .: Other :.
# Author: Timothy C. Quinn
# Home: https://github.com/JavaScriptDude/vtscan
# Licence: https://opensource.org/licenses/MIT
# .: Todo :.
# [.] Get Hyperlink working
# [.] Add CLI Arg for GUI
#########################################

import os, sys, json, hashlib, traceback, pathlib, argparse, subprocess, shutil, qrcode, tempfile, webbrowser, pyperclip, time
from virus_total_apis import PublicApi as VirusTotalPublicApi
from PySide2.QtCore import QObject
from PySide2.QtQml import QQmlApplicationEngine
from PySide2.QtWidgets import QApplication




_is_windows = hasattr(sys, 'getwindowsversion')


def main():
    argp = argparse.ArgumentParser(prog="vtscan")
    argp.add_argument("--verbose", "-v", action='store_true')
    argp.add_argument("--nogui", "-n", action='store_true')
    argp.add_argument("--links", "-L", action='store_true')
    argp.add_argument("--browser", "-b", type=str, help="Browser to launch for Virus Total Info or other searches")
    argp.add_argument("file", type=str, help="File to scan")

    args = argp.parse_args()

    api_call_failed : bool = False
    got_results : bool = False
    result_issues : int = -1
    warnings : list = []

    _script_name, _script_path = splitPath(os.path.realpath(__file__))

    _qr_png_path = None
    

    
    

    # Check for Api key
    if "VT_API_KEY" not in os.environ:
        argp.print_help()
        exit("\nMissing Virus total API Key. Please set VT_API_KEY environment variable!", 1)

    API_KEY=os.environ["VT_API_KEY"]
    if API_KEY.strip() == "":
        argp.print_help()
        exit("\nMissing Virus total API Key. Please set VT_API_KEY environment variable!", 1)

    # Verify that file exists
    if not os.path.isfile(args.file):
        argp.print_help()
        exit("\nPlease specify path to an existing file", 1)

    # Get args.file (first arg)
    fname, fpath = splitPath(args.file)

    # Get sha1 checksum of file
    digest_md5 = getChecksum(args.file, 'md5')
    digest_sha1 = getChecksum(args.file, 'sha1')
    digest_sha256 = getChecksum(args.file, 'sha256')

    # print("digest_sha256 = " + digest_sha256)

    vt = VirusTotalPublicApi(API_KEY)

    response = vt.get_file_report(digest_sha256)


    if not 'response_code' in response:
        api_call_failed = True
        warnings.append("Call to Virus Total API Failed")                    
        if 'error' in response:
            err_msg = response['error']
            if err_msg.find("Max retries exceeded with url") > -1:
                warnings.append("Please check your network connection")                    

    elif not response['response_code'] == 200:
        api_call_failed = True
        warnings.append("Bad general response_code from Virus Total")                    


    if not api_call_failed: # Dig into the results...
        res = response['results']

        if not res['response_code'] == 1:
            if res['verbose_msg'] == 'The requested resource is not among the finished, queued or pending scans':
                warnings.append("File not found in VirusTotal database. Therefore its safety is unknown.")
                warnings.append("Alternate verifications may be required")
            else:
                api_call_failed = True
                warnings.append("Bad result response_code from virus total: {}")

        # print("Raw virus total results: {}".format(json.dumps(res, sort_keys=False, indent=4)), 1)
        
        if len(warnings) == 0:
            # Lets be paranoid and verify the checksums found
            if not res['md5'] == digest_md5:
                warnings.append("MD5 Checksums do not match:\n -    Original: {}\n - Virus Total: {}".format(digest_md5, res['md5']))
            if not res['sha1'] == digest_sha1:
                warnings.append("SHA1 Checksums do not match:\n -    Original: {}\n - Virus Total: {}".format(digest_sha1, res['sha1']))
            if not res['sha256'] == digest_sha256:
                warnings.append("SHA256 Checksums do not match:\n -    Original: {}\n - Virus Total: {}".format(digest_sha256, res['sha256']))
            
            got_results = True
            result_issues = res['positives']


    if api_call_failed or args.verbose:
        print(".: Raw Virus Total Response :.\n" + json.dumps(response, sort_keys=False, indent=4) + "\n")


    print("""
.: Details :.
- md5: {0}
- sha1: {1}
- sha256: {2}""".format(digest_md5, digest_sha1, digest_sha256) )

    if got_results:
        print("- Permalink: " + res['permalink'])

        if not args.nogui:
            # Encoding data using make() function
            img = qrcode.make(res['permalink'])
            
            # Get Temp dir (windows only)
            _tmp_dir = None
            if _is_windows:
                if 'TEMP' not in os.environ:
                    raise Exception("TEMP Variable does not exist in users environment! cannot continue.")
                
                _tmp_dir = API_KEY=os.environ["TEMP"]
                if not os.path.isdir(_tmp_dir): 
                    raise Exception(f"TEMP dir does not exist: {_tmp_dir}")

            # Create Temp File name
            _tf = tempfile.NamedTemporaryFile(suffix='.png', prefix='_qrcode_', dir=_tmp_dir)
            _qr_png_path = _tf.name
            _tf.close()

            # Saving as an image file
            img.save(_qr_png_path)


    print("\n.: File :.\n- File: {0}\n- Path: {1}".format(fname, fpath) )


    if got_results:
        print("\n.: Virus Total Summary :.")
        if result_issues == 0:
            print("- Detections: 0 out of {} (100% pass)".format(res['total']))
        else:
            print("- Detections: {} out of {} (Go to VirusTotal for more details)".format(result_issues, res['total']))


    if len(warnings) > 0:
        print("\n.: Warnings :.")
        for warning in warnings:
            print("- {}".format(warning))

    

    if not api_call_failed and args.browser:
        exe = shutil.which(args.browser)
        if exe is None:
            print("\n  Note: Browser not launched executable not found: " + args.browser)
        else:
            if not got_results:
                print("""
  Signature not found in Virus Total so will search in google
  and bing for hash signatures. If no results are found,
  it is strongly recommended to take care with this file.""")
            else:
                print("\n  Note: Launching Virus Total in " + args.browser)
                urls = [res['permalink']]

            for url in urls:
                cmd = [exe, url]
                subprocess.Popen(cmd, stderr=subprocess.DEVNULL, stdout=subprocess.DEVNULL)

    if not api_call_failed and not got_results:
                print("""
  If this is an installer, executable, or other file that does not contain
  personal information (for example not a zip archive of personal files),
  you may want to consider uploading to VirusTotal to do a deep scan at:
 - https://www.virustotal.com/gui/home/upload""")


    if not args.nogui:
        vtdata = VTData()
        vtdata.fname = fname
        vtdata.fpath = fpath
        vtdata.digest_md5 = digest_md5
        vtdata.digest_sha1 = digest_sha1
        vtdata.digest_sha256 = digest_sha256
        vtdata.got_results = got_results
        vtdata.result_issues = result_issues
        vtdata.qr_png_path = _qr_png_path
        vtdata.permalink = res['permalink'] if res and 'permalink' in res else '-'
        vtdata.total = res['total'] if res and 'total' in res else '-'
        vtdata.script_path = _script_path
        
        app = MyQtApp(vtdata)
        app.start()
        

class VTData():
    def __init__(self):
        pass


class MyQtApp():

    def __init__(self, vtdata:VTData):
        self._vtdata = vtdata
        self._app = QApplication([])
        self._engine = QQmlApplicationEngine()

        

        _constants = {
            'Labels': {
                'Indent': 140
            }
        }
        self._engine.rootContext().setContextProperty("C", _constants)
        qml = None
        with open(f'{vtdata.script_path}/vtscan.qml') as f:
            lines = [ line.strip('\n') for line in list(f) ]
            qml = str.encode('\n'.join(lines))

        
        self._engine.loadData(qml)
        root_obj = self._engine.rootObjects()
        if not len(root_obj) == 1:
            raise Exception("Issue Parsing QML. Exiting Program")
        self._win = root_obj[0]

     


        self.setText("txtFile", vtdata.fname)
        self.setText("txtPath", vtdata.fpath)
        self.setText("txtMd5", vtdata.digest_md5)
        self.setText("txtSha1", vtdata.digest_sha1)
        self.setText("txtSha256", vtdata.digest_sha256)
        if vtdata.got_results:
            self.setText("txtLink", f"""<a href="{vtdata.permalink}">VirusTotal.com</a>""")
            self.setText("txtRes", vtdata.digest_sha256)
            if vtdata.result_issues == 0:
                self.setText("txtRes", "Detections: 0 out of {} (100% pass)".format(vtdata.total))
            else:
                self.setText("txtRes", "Detections: {} out of {} (Go to VirusTotal for more details)".format(vtdata.result_issues, vtdata.total))

            o = self._win.findChild(QObject, "qrcode")
            _qr_png_path_c = vtdata.qr_png_path.replace('\\', '/').replace(':', '::') if _is_windows else vtdata.qr_png_path
            
            o.setProperty('source', f'file://{_qr_png_path_c}')

            # Link signal in qml to python event
            self._win.maTxtLink_click.connect(self.maTxtLink_click)


            # self._maTxtLink = self._win.findChild(QObject, 'maTxtLink')
            # self._maTxtLink.clicked.connect(self.maTxtLink_click)
            # self._maTxtLink.pressed.connect(self.maTxtLink_pressed)

        else:
            self.setText("txtLink", 'n/a')
            self.setText("txtRes", "Not Registered in VirusTotal")


    def start(self):
        self._app.exec_()

    def setText(self, name, value):
            o = self._win.findChild(QObject, name)
            o.setProperty("text", value)

    def maTxtLink_click(self, right_click):
        if right_click:
            self.setText('txtStatusBar', "Permalink copied to clipboard")
            pyperclip.copy(self._vtdata.permalink)
        else:
            webbrowser.open(self._vtdata.permalink)


    def maTxtLink_pressed(self):
        print("HERE")


def Foobar():
    print("HERE")




def getChecksum(path, csumtype):
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
    with open(path, 'rb', buffering=0) as f:
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
        main()
    except Exception:
        exc_type, exc_value, exc_traceback = sys.exc_info()
        aTB = traceback.format_tb(exc_traceback)
        exit("Program Exception:\nStack:\n{}\n Error: {} - {}".format('\n'.join(aTB), exc_type.__name__, exc_value), exitCode=1)
    sys.exit(iExit)
