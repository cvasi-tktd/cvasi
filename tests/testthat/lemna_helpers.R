# converts a scenario object defined by the `lemna` package to a scenario
# of the `cvasi` package
lemna2scenario <- function(sc) {
  if(!is(sc, "lemna_scenario"))
    stop("argument is not a lemna scenario")

  forc <- sc$envir
  forc$conc <- NULL

  Lemna_SETAC() %>%
    set_init(sc$init) %>%
    set_param(sc$param) %>%
    set_exposure(sc$envir$conc) %>%
    set_times(sc$times) %>%
    set_forcings(forc)
}
