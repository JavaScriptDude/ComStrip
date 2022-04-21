ELISP="(load \"/dpool/vcmain/dev/lisp/comstrip/comstrip.lisp\")"
emacs --file=/dpool/vcmain/dev/py/ffr/ffr.py --quick --batch --eval "$ELISP"
emacs --file=/dpool/vcmain/dev/py/ffrc/ffrc.py --quick --batch --eval "$ELISP"
emacs --file=/dpool/vcmain/dev/py/qcorelite/qcorelite.py --quick --batch --eval "$ELISP"