module Color : sig
  include module type of Color
end

module Attr : sig
  include module type of Attr
end

module Style : sig
  include module type of Style
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

include module type of Text_with_styles
include module type of Input_output
