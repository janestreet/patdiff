open Core.Std

val warn_if_no_trailing_newline_in_both_default : bool

type t = private
  { output                              : Patdiff_core.Output.t
  ; rules                               : Patdiff_core.Format.Rules.t
  ; ext_cmp                             : string option
  ; produce_unified_lines               : bool
  ; unrefined                           : bool
  ; keep_ws                             : bool
  ; split_long_lines                    : bool
  ; context                             : int
  ; shallow                             : bool
  ; quiet                               : bool
  ; double_check                        : bool
  ; mask_uniques                        : bool
  ; old_alt                             : string option
  ; new_alt                             : string option
  ; location_style                      : Patdiff_core.Format.Location_style.t
  ; warn_if_no_trailing_newline_in_both : bool
  }
[@@deriving fields]

include Invariant.S with type t := t

val override
  :  ?output                              : Patdiff_core.Output.t
  -> ?rules                               : Patdiff_core.Format.Rules.t
  -> ?ext_cmp                             : string option
  -> ?produce_unified_lines               : bool
  -> ?unrefined                           : bool
  -> ?keep_ws                             : bool
  -> ?split_long_lines                    : bool
  -> ?context                             : int
  -> ?shallow                             : bool
  -> ?quiet                               : bool
  -> ?double_check                        : bool
  -> ?mask_uniques                        : bool
  -> ?old_alt                             : string option
  -> ?new_alt                             : string option
  -> ?location_style                      : Patdiff_core.Format.Location_style.t
  -> ?warn_if_no_trailing_newline_in_both : bool
  -> t
  -> t

val load : ?quiet_errors:bool -> string -> t option

val dark_bg : t Lazy.t

val light_bg : t Lazy.t

module Config : sig
  type t [@@deriving sexp]
end

module Old_config : sig
  type t [@@deriving sexp]
  val to_new_config : t -> Config.t
end

val parse : Config.t -> t
