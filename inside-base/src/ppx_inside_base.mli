(** This value is for preprocessors to know whether they are running inside base, in
    which case they may want to avoid using Base.X and instead use X directly. *)

val get : unit -> bool
