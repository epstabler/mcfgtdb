MCFG top down parsing
=====================

For description, see README.pdf and the wiki https://github.com/epstabler/mcfgtdb/wiki

If your platform supports OCaml native code compilation, for an example, type:

   make
   ./mcfgtdb
   a b b c d d

If your platform does not provide native compilation, use "make byte" instead of "make".
To change grammar, for example to mcfg1.ml, execute:

   newg mcfg1

this copies mcfg1.ml to g0.ml and recompiles. 
The minimum probability bound on derivations is specified in the last line of top.ml


