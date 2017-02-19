import glob
from subprocess import call

for f in glob.glob('./*'):
    if 'mll' in f or 'mly' in f:
        continue

    if 'ml' in f or 'mli' in f:
        name = f.replace('./', '')
        tname = name + '.tmp'

        # print 'camlp4 ' + name + ' -parser o -printer o > ' + tname

        print 'camlp4 ' + name + ' -parser o -printer o > ' + tname
        print 'cat ' + tname + ' > ' + name
        print 'rm ' + tname
