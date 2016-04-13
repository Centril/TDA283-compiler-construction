#!/usr/bin/python
import sys
from subprocess import Popen,PIPE,STDOUT
from os.path import exists,join
import tempfile
from optparse import OptionParser

DESTDIR=tempfile.mkdtemp()
#DESTDIR="/tmp/b"

def color(c,t):
    palette={ "green":'\031[91m',
              "red":'\033[91m',
	      "purple":'\035[91m',
              "yellow":'\033[91m',
	      "gray": '\037[91m'}
    return palette[c]+t+'\033[0m'

class Stage():
    def __init__(self,name,do,checkExistance=[],checkNExistance=[],actions=[],shell=False,stderrGrep=[],stderrNGrep=[],stdoutGrep=[],stdoutNGrep=[],returncode=None):
        self.state="pending"
        self.name=name
        self.do=do
        self.checkExistance=checkExistance
        self.checkNExistance=checkNExistance
	self.checklog=""
	self.actions=actions
	self.shell=shell
	self.stderrGrep=stderrGrep
	self.stderrNGrep=stderrNGrep
	self.stdoutGrep=stdoutGrep
	self.stdoutNGrep=stdoutNGrep
        self.expectReturncode=returncode

    @property
    def check(self):
	result=True
	for f in self.checkExistance:
	  df=join(DESTDIR,f)
	  if exists(df):
	     #self.checklog+=color("green","OK ") + "%s exists\n" % (df,)
	     self.checklog+="OK " + "%s exists\n" % (df,)
          else:
	     self.checklog+=color("red","FAIL ")
	     #self.checklog+="FAIL " + "%s does not exist\n" % (df,)
	     self.checklog+="%s does not exist\n" % (df,)
	     result=False
	for e in self.stderrNGrep:
	  if e in self.err:
	     self.checklog+=color("red","ERROR")
	     result=False
	for e in self.stdoutNGrep:
	  if e in self.output:
	     self.checklog+=color("red","ERROR")
	     result=False
	if self.expectReturncode is not None:
	  if self.expectReturncode != self.returncode:
	     self.checklog+=color("red","FAIL ")
	     self.checklog+="return code %s is not is not expected (%s)\n" % (self.returncode, self.expectReturncode)
	     result=False
	return result

    def run(self):
        sys.stdout.write("%s -> " % self.name)
        sys.stdout.flush()
        self.cmd=" ".join(self.do)

        tmpStdOut=tempfile.TemporaryFile()
        p=Popen(self.do, stdout=PIPE, stderr=PIPE,shell=self.shell,close_fds=True)
        self.returncode = p.wait()
        self.output, self.err = p.communicate()
	check=self.check
	if not check and "halt" in self.actions:
           sys.stdout.write("%s\n%s" % (self.log,"FATAL ERROR"))
           sys.exit()
        else:
	   r='OKEY' if check else 'FAILED'
           sys.stdout.write("%s\n" % r)
	if "stdout" in self.actions:
           sys.stdout.write("%s\n" % self.output)

        sys.stdout.flush()


    @property
    def log(self):
        return '''
----------
run:
%s

stdout:
%s

stderr:
%s

checks:
%s
----------
        ''' % (self.cmd,self.output,self.err,self.checklog)

def main(submission,toGrade):
   stages=[
     Stage(
	name="untar submission",
	do=["tar", "xzfv",submission,"-C",DESTDIR],
	checkExistance=["src/","doc/","lib/","src/Makefile"],
	# checkNExistance=["jlc"],
	actions=[ "halt", ],
        ),
     Stage(
        name="make the compiler",
        do=["cd %s ; make " % join(DESTDIR,"src")],
        checkExistance=["jlc"],
        returncode=0,
        stderrNGrep=["Exception in thread"],
        actions=["halt"],
        shell=True
        ),
     Stage(
        name="copy testsuite and Tester",
        do=["cp -r testsuite Tests.py %s " % DESTDIR],
        checkExistance=["Tests.py","testsuite"],
        actions=["halt"],
        shell=True
        )
     ]
   stageA=Stage(
        name="Submission A: parse tests",
        do=["cd %s ; ./Tests.py -s A" % DESTDIR],
        stdoutNGrep=["Failed tests"],
	# TODO check that possitibly than the test passed
        actions=["stdout"],
        shell=True
        )
   stageB=Stage(
        name="Submission B: run good tests",
        do=["cd %s ; ./Tests.py -s B" % DESTDIR],
        stdoutNGrep=["Failed tests"],
	# TODO check that possitibly than the test passed
        actions=["stdout"],
        shell=True
        )

   stageC=Stage(
        name="Submission C: run extensions",
        do=["cd %s ; ./Tests.py -s C" % DESTDIR],
        stdoutNGrep=["Failed tests"],
	# TODO check that possitibly than the test passed
        actions=["stdout"],
        shell=True
        )

   if   toGrade == 'A': stages+=[stageA]
   elif toGrade == 'B': stages+=[stageB]
   elif toGrade == 'C': stages+=[stageC]
   else: stages+=[stageA,stageB]

   for stage in stages:
      stage.run()

if __name__ == "__main__":
    usage = "usage: %prog [options] tarball"
    parser = OptionParser(usage=usage)
    parser.add_option("-s", "--submission", dest="submission", help="testing for which submission (A, B or C?)", metavar="A|B|C")
    (options, args) = parser.parse_args()

    main(args[0],options.submission)

