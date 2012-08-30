open Makeq (* for the priority queues *)
type cat = int
type imap = (int * int) list
type nrhs = (cat * imap) list
type irhs = IT of string | IE | IN of nrhs
type iproduction = cat * irhs
type iprule = iproduction * float
type ing = iprule list
type expansion = irhs * float
type expansions = expansion list
type indices = int list list
type iCat = cat * indices
type input = string list
val printIlist : out_channel -> int list -> unit
val printImap : out_channel -> (int * int) list -> unit
val printInrhs : out_channel -> ('a -> string) -> ('a * (int * int) list) list -> unit
val printIRHS : out_channel -> (cat -> string) -> irhs -> unit
val printIPRule : out_channel -> (cat -> string) -> (cat * irhs) * float -> unit
val printING : (cat -> string) -> ing -> unit
val least : 'a list -> 'a
module ICatPoset : sig type t = iCat val le : iCat -> iCat -> bool end
module IQ :
  sig
    type t = Makeq(ICatPoset).t
    val empty : t
    val is_empty : t -> bool
    val add : ICatPoset.t -> t -> t
    exception Empty
    val extract_min : t -> ICatPoset.t * t
    val merge : t -> t -> t
  end
type der = input * IQ.t * float
module DerivationPoset : sig type t = der val le : der -> der -> bool end
module DQ :
  sig
    type t = Makeq(DerivationPoset).t
    val empty : t
    val is_empty : t -> bool
    val add : DerivationPoset.t -> t -> t
    exception Empty
    val extract_min : t -> DerivationPoset.t * t
    val merge : t -> t -> t
  end
val ilist : out_channel -> int list -> unit
val printIQ : out_channel -> (cat -> string) -> IQ.t -> unit
val printDQ : out_channel -> (cat -> string) -> DQ.t -> unit
val numberOfCats : ((int * 'a) * 'b) list -> int
val ingExpansions : ing -> expansions array
val extendIndices : indices -> imap -> indices
val insertPredictions : indices -> IQ.t -> nrhs -> IQ.t
exception InitialIndexDiffers
val trimIndices : int -> indices -> indices
val trimmedElements : int -> IQ.t -> iCat list
val trimIQ : IQ.t -> IQ.t
val prunedInsert : DQ.t -> float -> der -> DQ.t
val extendDerivation : DQ.t -> float -> indices -> input -> IQ.t -> float -> expansion -> DQ.t
val derive : (cat -> string) -> expansions array -> float -> DQ.t -> bool
val recognize : ing -> (cat -> string) -> float -> input -> bool
