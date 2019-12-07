let apply f x =
    1 + f(x + 1)

(* 
[
  structure_item (test6.ml[1,0+0]..test6.ml[2,16+16])
    Tstr_value Nonrec
    [
      <def>
        pattern (test6.ml[1,0+4]..test6.ml[1,0+9])
          Tpat_var "apply/80"
        expression (test6.ml[1,0+10]..test6.ml[2,16+16]) ghost
          Texp_function
          Nolabel
          [
            <case>
              pattern (test6.ml[1,0+10]..test6.ml[1,0+11])
                Tpat_var "f/81"
              expression (test6.ml[1,0+12]..test6.ml[2,16+16]) ghost
                Texp_function
                Nolabel
                [
                  <case>
                    pattern (test6.ml[1,0+12]..test6.ml[1,0+13])
                      Tpat_var "x/82"
                    expression (test6.ml[2,16+4]..test6.ml[2,16+16])
                      Texp_apply
                      expression (test6.ml[2,16+6]..test6.ml[2,16+7])
                        Texp_ident "Stdlib!.+"
                      [
                        <arg>
                          Nolabel
                          expression (test6.ml[2,16+4]..test6.ml[2,16+5])
                            Texp_constant Const_int 1
                        <arg>
                          Nolabel
                          expression (test6.ml[2,16+8]..test6.ml[2,16+16])
                            Texp_apply
                            expression (test6.ml[2,16+8]..test6.ml[2,16+9])
                              Texp_ident "f/81"
                            [
                              <arg>
                                Nolabel
                                expression (test6.ml[2,16+9]..test6.ml[2,16+16])
                                  Texp_apply
                                  expression (test6.ml[2,16+12]..test6.ml[2,16+13])
                                    Texp_ident "Stdlib!.+"
                                  [
                                    <arg>
                                      Nolabel
                                      expression (test6.ml[2,16+10]..test6.ml[2,16+11])
                                        Texp_ident "x/82"
                                    <arg>
                                      Nolabel
                                      expression (test6.ml[2,16+14]..test6.ml[2,16+15])
                                        Texp_constant Const_int 1
                                  ]
                            ]
                      ]
                ]
          ]
    ]
]
*)