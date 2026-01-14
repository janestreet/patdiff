(** Library for parsing text that contains ANSI escape codes.

    Implements types for parsing the whole spec, but with a focus on representing Select
    Graphic Rendition (SGR) codes through the [Style] module, Control Sequence Introducer
    (CSI) codes through the [Control] module, and terminal hyperlinks (OSC-8) through the
    [Hyperlink] module.

    More details on SGR:
    https://en.wikipedia.org/wiki/ANSI_escape_code#Select_Graphic_Rendition_parameters

    More details on CSI:
    https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands

    The primary type [Ansi_text.t] is defined in [text_with_ansi.ml], and represents text
    as a list of [`Text] and [Ansi.t] elements. The functions defined in [parser.ml] and
    [input_output.ml] handle parsing to/from strings. These serve three primary purposes:
    1) stripping out ANSI codes or rendering them in a more human-readable form for use in
       expect-tests.
    2) performing text-wrapping and similar operations that depend on string-length in a
       way that ignores ANSI codes and respects Unicode character widths.
    3) simplifying ANSI codes in text that contains lots of them.

    The [Text_with_style_ranges] module is intended for applying styles to specific ranges
    within a text or for parsing text with such styles. This is used by the version of
    patdiff4 in the Fe web-ui to extract the ranges of the styles that patdiff applied. *)

module Color : sig
  include module type of Color
end

module Attr : sig
  include module type of Attr
end

module Style : sig
  include module type of Style
end

module Control : sig
  include module type of Control
end

module Text : sig
  include module type of Text
end

module Style_ranges : sig
  include module type of Style_ranges
end

module Text_with_style_ranges : sig
  include module type of Text_with_style_ranges
end

include module type of Text_with_ansi
include module type of Parser
include module type of Input_output
module Unknown_esc = Unknown_esc
