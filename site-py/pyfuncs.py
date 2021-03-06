from Pymacs import lisp, print_lisp, Symbol
import os
import glob

interactions = {}

def break_on_whitespace():
    start, end = lisp.point(), lisp.mark(True)
    words = lisp.buffer_substring(start, end).split()
    lisp.delete_region(start, end)
    lisp.insert('\n'.join(words))


def convert_attr_to_dict_loop():
    """
    Convert highlighted region from:
    item.attr.elem
    to item['attr']['elem']
    """
    start, end = lisp.point(), lisp.mark(True)
    subs = lisp.buffer_substring(start, end)
    elems = subs.split('.')

    result = elems[0] + ".".join(["['%s']" % elem for elem in elems[1:]])
    lisp.delete_region(start, end)
    lisp.insert(result)


def dict_prettify_region():
    """
    Convert highlighted region dict content to prettified dict
    """
    import json, ast
    start, end = lisp.point(), lisp.mark(True)
    region = lisp.buffer_substring(start, end)
    data_dict = ast.literal_eval(region)
    result = json.dumps(data_dict, indent=2)
    lisp.delete_region(start, end)
    lisp.insert(result)


def byte_compile_el_files():
    dirs = [".emacs.d/", ".emacs.d/site-lisp/",
            ".emacs.d/setup/", ".emacs.d/defuns/",
            "/home/bro/programmer/emacs/predictive/predictive/"]

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


def read_glossaries(file=""):
    import re
    ifile = open("/home/bro/master/master_opp/oppgave/glossaries.tex", 'r')
    text = ifile.read()
    ifile.close()

    acr_pattern = re.compile(r"""
    \\newacronym{(?P<id>[^}]*)}{(?P<short>[^}]*)}{(?P<long>[^}]*)}
    """, re.VERBOSE | re.MULTILINE)

    acronyms = []
    for match in acr_pattern.finditer(text):
        v = "(%s) (%s) (%s)\n" % (match.group("id"),
                                  match.group("short"),
                                  match.group("long"))
        lisp.message("Acronym: '%s'" % v)
        acronyms.append(match.group("id"))

    lisp._eval('(LaTeX-add-glossaries \"%s\")' %
               "\" \"".join(acronyms))


interactions[break_on_whitespace] = ''
interactions[byte_compile_el_files] = ''
interactions[delete_elc_files] = ''
interactions[read_glossaries] = ''
interactions[convert_attr_to_dict_loop] = ''
interactions[dict_prettify_region] = ''
