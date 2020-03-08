Install and get started with ACT-R/EMMA on Mac OS


== I. INSTALL LISP ==
Clozure Common Lisp (formerly OpenMCL)
1) download .dmg from http://trac.clozure.com/ccl/
2) mount dmg and copy contents to /Applications/ccl
3) copy "scripts/ccl" and/or "scripts/ccl64" to "/usr/local/bin/" (with sudo)
4) edit both and replace value in "CCL_DEFAULT_DIRECTORY=" with "/Applications/ccl/"
5) make the script(s) executable: "sudo chmod +x ccl(64)"


== II. GETTING ACT-R ==
svn://jordan.psy.cmu.edu/usr/local/svnroot/actr6

Instead of retrieving and installing ACT-R, you can put the provided actr6 directory into your Applications folder (you can then skip II., III., and IV.).


== III. MODULES ==
From modules copy
- emma.lisp
- parsing-module.lisp
to actr6/other-files/


== IV. RUNNING ACT-R ==
1) in terminal navigate to the ACT-R directory
2) start LISP:  
     > ccl
3) load ACT-R into LISP:
     > (load "load-act-r-6.lisp")
     If you want to recompile all modules (necessary, when running ACT-R for 
       the first time or when you change something, e.g. in EMMA), type the 
       following before loading:
     > (push :actr-recompile *features*)
3) run ACT-R environment by double clicking "environment/Start Environment OSX"
4) connect Lisp to ACT-R environment:
     > (start-environment)


== V. USEFUL FUNCTIONS ==
(defun rs (sn &optional (params nil))
	(read-sentence sn params))

(defun ra (times &optional (params nil))
	(read-all-times times params))

(defun rl (limit &optional (params nil))
	(read-list limit params))

(search-param-space (&optional experiment (iterations 10) (param-space '*pspace*))


== VI. IMPORTANT MODEL PARAMETERS (interface.lisp) ==
*surprisal-on* 		nil
*surprisal-hl-on*	t
*retrieval-on* 		t

*visual-encoding-factor*    .003
*visual-encoding-exponent*  .4
*prep-time*                 .120

*surprisal-factor*          .001
*retrieval-latency*         0.2
*surprisal-hl-factor*       .0


== VII. OUTPUT ==
fixations.txt:
#EXPERIMENT	#ITERATION	#SENTNR		#WORDPOS	#WORD	#FIXTIME
