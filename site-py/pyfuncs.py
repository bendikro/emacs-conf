from Pymacs import lisp

#def break_on_whitespace():
#    start = lisp.point()
#    end = lisp.mark(True)
#    if start > end:
#        start, end = end, start
#    text = lisp.buffer_substring(start, end)
#    words = text.split()
#    replacement = '\n'.join(words)
#    lisp.delete_region(start, end)
#    lisp.insert(replacement)
#
#interactions = {break_on_whitespace: ''}

interactions = {}

def break_on_whitespace():
    start, end = lisp.point(), lisp.mark(True)
    words = lisp.buffer_substring(start, end).split()
    lisp.delete_region(start, end)
    lisp.insert('\n'.join(words))

interactions[break_on_whitespace] = ''
