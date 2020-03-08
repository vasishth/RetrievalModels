=== INSTALLATION ===

Clozure Common Lisp (formerly OpenMCL)
1) download .dmg from http://trac.clozure.com/ccl/
2) mount dmg and copy contents to /Applications/ccl
3) copy "scripts/ccl" and/or "scripts/ccl64" to "/usr/local/bin/" (with sudo)
4) edit both and replace value in "CCL_DEFAULT_DIRECTORY=" with "/Applications/ccl/"
5) make the script(s) executable: "sudo chmod +x ccl(64)"

ACT-R
svn: svn://jordan.psy.cmu.edu/usr/local/svnroot/actr6

EMMA
- copy emma.lisp from "extras" to "other-files"

Running ACT-R
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


=== USAGE ===

MODEL:
Load model 'readpsc.lisp' (simple) or 'readpsc-timeout.lisp' (with regression 
ability).
The sentences and the measures of frequency, surprisal, and retrieval are 
internally loaded from 'psc-sentences.lisp'.

INTERFACE:
Both models load 'interface.lisp'. There you can set the parameters.
Example:
  *visual-encoding-factor*     .002
  *visual-encoding-exponent*   .3
  *my-prep-time*               .120

  *retrieval-factor*           .2
  *surprisal-factor*           .005

You can also turn on and off surprisal and retrieval:
  *retrieval-on* 		nil
  *surprisal-on* 		T

Surprisal can be used in both models (simple and time-out model).
Retrieval only works in the time-out model.


EXAMPLE COMMANDS:
Read a sentence number 14:
> (read-sentence 14)

Read the first 30 sentences:
> (read-list 30)

Read the whole corpus 10 times:
> (ra 10)

OUTPUT:
When running a range of sentences or the whole corpus, the recorded fixation 
times are collected in output/fixations.txt with the following column specifications:
#RUN	#SENTENCE	#WORDPOS	#WORD	#FIXTIME

