let package_name = "patdiff"

let sections =
  [ ("bin",
    [ ("built_exec_patdiff", Some "patdiff")
    ],
    [])
  ; ("man",
    [],
    [ ("patdiff.man", Some "man1/patdiff.1")
    ])
  ]
