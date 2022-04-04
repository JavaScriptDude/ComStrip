# EmacsComStrip
CLI Based Comment Stripper Using Emacs


Emacs has excelent language scemantics processing and also has a batch mode to allow execution of lisp code in a headless fashion.

This tool allows the user to use emacs to strip source code comments from any source langage.

## usage:
``` emacs --file=<path_to_source_code> --quick --batch --eval '(load "./comstrip.lisp")' ```

This tool will read in <source_code>, use emacs to strip comments out and the write to /tmp/<file_name>_comstrip

## Requirements:
* emacs
* language files for languages that need processing

Note: This code is presently written for *nix but can easily be updated to be x-platform. This is my first `lisp` program so still in the early stages.

