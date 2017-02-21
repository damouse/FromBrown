import glob
from subprocess import call

# Doesn't execute directly, so just copy and execute the commands as printed. Sloppy, but not worth dealing with now.
for f in glob.glob('./*'):
    if 'mll' in f or 'mly' in f:
        continue

    if 'ml' in f or 'mli' in f:
        name = f.replace('./', '')
        tname = name + '.tmp'

        print 'camlp4 ' + name + ' -parser o -printer o > ' + tname
        print 'cat ' + tname + ' > ' + name
        print 'rm ' + tname
