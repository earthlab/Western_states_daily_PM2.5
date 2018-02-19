import urllib
import re

# STILL NEED TO FIX

urlpath =urllib.url('http://www.divms.uiowa.edu/~jni/courses/ProgrammignInCobol/presentation/')
string = urlpath.read().decode('utf-8')

pattern = re.compile('BAECV_'[2008-2014]) #the pattern actually creates duplicates in the list

filelist = pattern.findall(string)
print(filelist)

for filename in filelist:
    filename=filename[:-1]
    remotefile = html.read('http://www.divms.uiowa.edu/~jni/courses/ProgrammignInCobol/presentation/' + filename)
    localfile = open(filename,'wb')
    localfile.write(remotefile.read())
    localfile.close()
    remotefile.close()
