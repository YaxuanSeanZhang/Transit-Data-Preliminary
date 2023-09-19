get_onewaytab <-
  function(a_variable) {
    rbindlist(list(
      "2022" = svy_2022p[, .(
        variable = eval(a_variable),
        trips = sum(Final_unlinked_weight_fctr),
        raw_n = length(Final_unlinked_weight_fctr)
      ), keyby = .(value = get(a_variable))],
      "2016" = svy_2016[, .(
        variable = eval(a_variable),
        trips = sum(Unlinked_Weight.AE),
        raw_n = length(Unlinked_Weight.AE)
      ), keyby = .(value = get(a_variable))]
    ),
    idcol = "survey"
    )
  }


get_twowaytab <-
  function(a_variable, b_variable) {
    rbindlist(list(
      "2022" = svy_2022p[, .(
        variable = eval(a_variable),
        variable2 = eval(b_variable),
        trips = sum(Final_unlinked_weight_fctr),
        raw_n = length(Final_unlinked_weight_fctr)
      ), keyby = .(value = get(a_variable),
                   value2 = get(b_variable))],
      "2016" = svy_2016[, .(
        variable = eval(a_variable),
        variable2 = eval(b_variable),
        trips = sum(Unlinked_Weight.AE),
        raw_n = length(Unlinked_Weight.AE)
      ), keyby = .(value = get(a_variable),
                   value2 = get(b_variable))]
    ),
    idcol = "survey"
    )
  }
