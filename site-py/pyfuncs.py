from Pymacs import lisp, print_lisp, Symbol
import os
import glob

interactions = {}

def break_on_whitespace():
    start, end = lisp.point(), lisp.mark(True)
    words = lisp.buffer_substring(start, end).split()
    lisp.delete_region(start, end)
    lisp.insert('\n'.join(words))


def byte_compile_el_files():
    dirs = [".emacs.d/", ".emacs.d/site-lisp/",
            ".emacs.d/setup/", ".emacs.d/defuns/"]

    home = os.environ['HOME']
    for d in dirs:
        files = glob.glob(os.path.join(home, d) + "*.el")
        for f in files:
            lisp.message("Byte compiling '%s'" % f)
            lisp._eval('(byte-compile-file "%s")' % f)

def delete_elc_files():
    dirs = [".emacs.d/", ".emacs.d/site-lisp/",
            ".emacs.d/setup/", ".emacs.d/defuns/"]

    home = os.environ['HOME']
    for d in dirs:
        files = glob.glob(os.path.join(home, d) + "*.elc")
        for f in files:
            lisp._eval('(delete-file "%s")' % f)

interactions[break_on_whitespace] = ''
interactions[byte_compile_el_files] = ''
interactions[delete_elc_files] = ''
