MCFG top down beam parsing
==========================

This parser is described in

- Stabler (2011) Top-down recognizers for MCFGs and MGs.
Proceedings of the Workshop on Cognitive Modeling and Computational Linguistics CMCL,
49th Annual Meeting of the {A}ssociation for Computational Linguistics.
http://www.linguistics.ucla.edu/people/stabler/Stabler11-CMCL.pdf

For description, see README.pdf and the wiki https://github.com/epstabler/mcfgtdb/wiki

If your platform supports OCaml native code compilation, simply type:

      make
      ./mcfgtdb
      a b b c d d

If your platform does not provide native compilation, use "make byte" instead of "make".
To change grammar, for example to mcfg1.ml, execute:

      newg mcfg1

this copies mcfg1.ml to g0.ml and recompiles. 
The minimum probability bound on derivations is specified in the last line of top.ml


