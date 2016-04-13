#!/usr/bin/python
import sys
from subprocess import Popen,PIPE
from os import listdir, getcwd
from os.path import isfile, join,basename,dirname
from optparse import OptionParser
from os.path import exists
from tempfile import NamedTemporaryFile
from filecmp import cmp

class Test():
    def __init__(self,jlFile,category):
        self.file=jlFile
        self.category=category
        basename=jlFile[:-3]
        self.inputfile = basename+'.input' if isfile(basename+'.input') else ""
        self.outputfile= basename+'.output' if isfile(basename+'.output') else ""
        self.log=[]
        self.okCompilation=False

    def __str__(self):
        return self.file + " " + self.category + " " + str(self.okCompilation)

    def checkCompilation(self):
        if self.category == "bad":
            if self.returncode != 0:
                self.log+=["OK: exit code not 0"]
            else:
                self.log+=["ERROR (bad test accepted): exit code 0"]
                return False
            if 'ERROR' in self.err.decode('utf-8'):
                self.log+=["OK: \"ERROR\" is in stderr"]
            else:
                self.log+=["ERROR (bad test accepted): \"ERROR\" is not in stderr"]
                return False
        if self.category == "good" or "extension" in self.category:
            if self.returncode == 0:
                self.log+=["OK: exit code  is 0"]
            else:
                self.log+=["ERROR (good test rejected): exit code is not 0"]
            if 'OK' in self.err.decode('utf-8'):
                self.log+=["OK: \"OK\" is in stderr"]
            else:
                self.log+=["ERROR: \"OK\" is not in stderr"]
                return False
        return True

    @property
    def logOutput(self):
        return '\n'.join(self.log)

    def runGood(self):
        if not exists("a.out"):
            self.log+=["ERROR: the file a.out does not exist.",]
            return False
        outfile=NamedTemporaryFile()
        if self.inputfile: p=Popen(['./a.out'],stdout=outfile,stdin=open(self.inputfile))
        else:              p=Popen(['./a.out'],stdout=outfile)
        ret_code = p.wait()
        outfile.flush()
        #TODO check that the execution didnt explote
        if self.outputfile:
            if cmp(self.outputfile,outfile.name): self.okRun = True
            else: self.log+=["ERROR: the output of a.out does not match: %s" % self.outputfile]
        else: self.okRun = True

    def compile(self,jlc):
        d = dirname(jlc)
        if not d:
            d = '.'
        print join(d,basename(jlc))
        p = Popen([join(d,basename(jlc)), self.file], stdout=PIPE, stderr=PIPE, cwd=getcwd())
        self.output, self.err = p.communicate()
        self.returncode = p.returncode
        self.okCompilation=self.checkCompilation()
        self.okRun = False

def loadTests(testsDir,submission):
   badTestsDir=join(testsDir,"bad")
   tests = [ Test(join(badTestsDir,f),"bad") for f in listdir(badTestsDir) if isfile(join(badTestsDir,f)) and join(badTestsDir,f)[-3:]=='.jl']

   goodTestsDir=join(testsDir,"good")
   tests += [ Test(join(goodTestsDir,f),"good") for f in listdir(goodTestsDir) if isfile(join(goodTestsDir,f)) and join(goodTestsDir,f)[-3:]=='.jl']

   if submission == 'C':
      extTestsDir=join(testsDir,"extensions")
      for ext in listdir(extTestsDir):
          extDir=(join(extTestsDir,ext))
          tests += [ Test(join(extDir,f),"extension - "+ext) for f in listdir(extDir) if isfile(join(extDir,f)) and join(extDir,f)[-3:]=='.jl']
 
   return tests

class Summary():
   def analyzeGood(self,test):
       total=[0,0]
       if test.category not in self.shouldWork: self.shouldWork[test.category]=[0,0,0,[]]
       good=self.shouldWork[test.category]
       good[2]+=1
       if test.okCompilation:
           good[0]+=1
           total[0]+=1
           if self.submission=="B" or self.submission=="C":
               total[1]+=1
               if test.okRun:
                   good[1]+=1
                   total[0]+=1
               else:
                   good[3]+=[test]
       else:
           good[3]+=[test]
       self.shouldWork[test.category]=good
       return total

   def __init__(self,tests,submission):
       self.submission=submission
       #compiled/run/total/[failedtests]
       self.shouldWork={}
       bad=[0,0,0,[]]
       #passed/total
       total=[0,0]

       for test in tests:
           total[1]+=1
           if test.category=="good" or "extension" in test.category:
               subtotal=self.analyzeGood(test)
               total=list(map(lambda x,y : x+y,total,subtotal))
           if test.category=="bad":
               bad[2]+=1
               if test.okCompilation:
                   bad[0]+=1
                   total[0]+=1
               else:
                   bad[3]+=[test]

       self.good=self.shouldWork.get('good', [0,0,0,[]])
       self.bad=bad
       self.total=total

   def __str__(self):
       c1=10
       c2=10
       ret=['']
       fails=self.good[3]+self.bad[3]
       good=''

       if self.submission=='A':
           ret+=['Compiled/Total']
           good="%i/%i" % (self.good[0],self.good[2])
       elif self.submission=='B' or self.submission=='C':
           ret+=['Run/Compiled/Total']
           good="%i/%i/%i" % (self.good[1],self.good[0],self.good[2])

       bad= "%i/%i" % (self.bad[0],self.bad[2])
       mistakes = len(self.good[3])
       result = '1 mistake found' if mistakes==1 else 'OK' if not mistakes else "%s mistakes found" % mistakes
       ret+=["good".rjust(c1)+good.rjust(c2)+" <- %s" % (result)]
       mistakes = len(self.bad[3])
       result = '1 mistake found' if mistakes==1 else 'OK' if not mistakes else "%s mistakes found" % mistakes
       ret+=["bad".rjust(c1)+bad.rjust(c2)+" <- %s" % (result)]

       if self.submission=='C':
           ret+=["",'extensions:']
           for (i,j) in self.shouldWork.iteritems():
            if 'extension' not in i: continue
            good="%i/%i/%i" % (j[1],j[0],j[2])
            mistakes = len(j[3])
            result = '1 mistake found' if mistakes==1 else 'OK' if not mistakes else "%s mistakes found" % mistakes
            ret+=[i.split(' - ')[1].rjust(c1)+good.rjust(c2)+" <- %s" % (result)]

       total="%i/%i" % (self.total[0],self.total[1])
       ret+=["--"+"-"*c1,"total".rjust(c1)+total.rjust(c2)]
       ret+= ["",'Failed tests:']+[ "\n - %s\n%s" %(f.file,f.logOutput) for f in fails] if fails else ["","All test passed!"]

       return '\n'.join(ret)

def showProgress():
   sys.stderr.write('.')
   sys.stderr.flush()

def main(jlc,testsDir,submission):
   showProgress();
   tests=loadTests(testsDir,submission)

   for test in tests:
     showProgress();
     test.compile(jlc)
     if (submission == "B" or submission == "C") and test.category is not "bad":
        showProgress();
        test.runGood()
   
   sys.stdout.write("%s\n" % Summary(tests,submission))
   
if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-c", "--compiler", dest="compiler", help="Your compiler (default ./jlc)", metavar="FILE",default="./jlc")
    parser.add_option("-s", "--submission", dest="submission", help="testing for which submission (A, B or C?)", metavar="A|B|C")
    parser.add_option("-t", "--testsuite", dest="testsuite", help="testsuite directory (default ./testsuite)", default="./testsuite", metavar="DIR")
    (options, args) = parser.parse_args()

    main(options.compiler,options.testsuite,options.submission)
