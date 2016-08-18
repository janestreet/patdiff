#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"patdiff"
  [ oasis_lib "patdiff_lib"
  ; file "META" ~section:"lib"
  ; oasis_exe "patdiff" ~dest:"patdiff"
  ; file "patdiff.man" ~dest:"man1/patdiff.1" ~section:"man"
  ; file "bin/patdiff-git-wrapper" ~section:"bin"
  ]
