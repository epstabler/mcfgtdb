MCFG top down parsing
=====================

If your platform does not provide native compilation, use "make byte" instead of "make".

If your platform supports the OCaml native code compilation, for an example, type:

   make
   ./mcfgtdb
   a b b c d d

We have a few example grammars in this directory. To use one of them, for example mcfg1.ml, execute:

   newg mcfg1

this should copy mcfg1.ml to g0.ml and compile it, and start again as described above. The minimum probability bound on derivations is specified in the last line of top.ml

After compiling, to keep the executable and docs but remove intermediate files:
   
   make clean

To clean up everything and re-compile:

   make realclean
   make

For more details, see README.pdf and the wiki https://github.com/epstabler/mcfgtdb/wiki
