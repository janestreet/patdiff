open Core.Std

type t = {
  output: Patdiff_core.Output.t;
  rules: Patdiff_core.Format.Rules.t;
  ext_cmp: string option;
  produce_unified_lines : bool;
  unrefined: bool;
  keep_ws: bool;
  split_long_lines: bool;
  context: int;
  shallow: bool;
  quiet: bool;
  double_check: bool;
  mask_uniques: bool;
  old_alt: string option;
  new_alt: string option;
}

val load : ?quiet_errors:bool -> string -> t option

module Config : sig
  type t with sexp
end

module Old_config : sig
  type t with sexp
  val to_new_config : t -> Config.t
end

val parse : Config.t -> t
