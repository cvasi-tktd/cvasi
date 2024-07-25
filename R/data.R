#' A GUTS-RED-SD scenario of the fathead minnow
#'
#' The scenario consists of a parameterized GUTS-RED-SD model and a constant
#' exposure series. Model parameters were derived from an acute fish toxicity
#' study of the fathead minnow and chlorpyrifos by Geiger *et al.* (1988).
#' The dataset is also referred to as *GUTS Ring-test dataset C* by
#' EFSA (2018). Fitted parameters were estimated using the [morse] package.
#'
#' The background mortality rate (`hb`) was set to zero.
#'
#' @seealso [GUTS-RED-models]
#' @references
#' Geiger D.L., Call D.J., and Brooke L.T., 1988: *Acute toxicities of organic
#' chemicals to fathead minnows (Pimephales promelas): Volume IV*, pp. 195-197.
#' University of Wisconsin-Superior, Center for Lake Superior Environmental Studies.
#' ISBN 9780961496838.
#'
#' EFSA PPR Panel (EFSA Panel on Plant Protection Products and their Residues),
#' Ockleford C, Adriaanse P, Berny P, et al., 2018: *Scientific Opinion on the
#' state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for
#' regulatory risk assessment of pesticides for aquatic organisms*. EFSA Journal 2018;
#' 16(8):5377, 188 pp. \doi{10.2903/j.efsa.2018.5377}
#'
#' @source https://mosaic.univ-lyon1.fr/guts
"minnow_sd"

#' A GUTS-RED-IT scenario of the fathead minnow
#'
#' The scenario consists of a parameterized GUTS-RED-IT model and a constant
#' exposure series. Model parameters were derived from an acute fish toxicity
#' study of the fathead minnow and chlorpyrifos by Geiger *et al.* (1988).
#' The dataset is also referred to as *GUTS Ring-test dataset C* by
#' EFSA (2018). Fitted parameters were estimated using the [morse] package.
#'
#' The background mortality rate (`hb`) was set to zero.
#'
#' @seealso [GUTS-RED-models]
#' @references
#' Geiger D.L., Call D.J., and Brooke L.T., 1988: *Acute toxicities of organic
#' chemicals to fathead minnows (Pimephales promelas): Volume IV*, pp. 195-197.
#' University of Wisconsin-Superior, Center for Lake Superior Environmental Studies.
#' ISBN 9780961496838.
#'
#' EFSA PPR Panel (EFSA Panel on Plant Protection Products and their Residues),
#' Ockleford C, Adriaanse P, Berny P, et al., 2018: *Scientific Opinion on the
#' state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for
#' regulatory risk assessment of pesticides for aquatic organisms*. EFSA Journal 2018;
#' 16(8):5377, 188 pp. \doi{10.2903/j.efsa.2018.5377}
#'
#' @source https://mosaic.univ-lyon1.fr/guts
"minnow_it"

#' Lemna data published by Schmitt (2013)
#'
#' Data set for the parametrisation of a mechanistic combined
#' toxicokinetic-toxicodynamic (TK/TD) and growth model for the aquatic
#' macrophytes Lemna spp. as published by Schmitt *et al.* (2013).
#' The growth model was parameterised by Schmitt et al. based on these
#' data while toxicokinetic and toxicodynamic parameters were determined by
#' calibrating the model using substance specific effect data of metsulfuron-methyl.
#'
#' @seealso [Lemna-models]
#' @references
#' Schmitt W., Bruns E., Dollinger M., and Sowig P., 2013: *Mechanistic TK/TD-model
#' simulating the effect of growth inhibitors on Lemna populations*. Ecol Model 255,
#' pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
"metsulfuron"

#' A Lemna_SETAC scenario with variable environment
#'
#' A mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and growth
#' model for the aquatic macrophytes Lemna spp. as published by
#' Klein *et al.* (2021).
#'
#' The scenario will simulate a period of 365 days, a start
#' population of 80 g/m² dry weight, variable environmental conditions, and a
#' complex, time-varying exposure pattern.
#'
#' The scenario setup was published by Hommen *et al*. (2015). Exposure pattern
#' and substance specific parameters are of exemplary character
#' and represent the herbicide *metsulfuron-methyl*. The parameters were
#' derived by Schmitt et al. (2013) based on literature
#' data.
#'
#' @seealso [Lemna-models]
#' @references
#' Hommen U., Schmitt W., Heine S., Brock Theo CM., Duquesne S., Manson P., Meregalli G.,
#'   Ochoa-Acuña H., van Vliet P., Arts G., 2015: How TK-TD and Population Models for
#'   Aquatic Macrophytes Could Support the Risk Assessment for Plant Protection
#'   Products. Integr Environ Assess Manag 12(1), pp. 82-95.
#'   \doi{10.1002/ieam.1715}
#'
#' Klein J., Cedergreen N., Heine S., Reichenberger S., Rendal C.,
#'   Schmitt W., Hommen U., 2021: Refined description of the *Lemna* TKTD growth model
#'   based on *Schmitt et al.* (2013) - equation system and default parameters.
#'   Report of the working group *Lemna* of the SETAC Europe Interest Group Effect
#'   Modeling. Version 1, uploaded on 22. Sept. 2021.
#'   <https://www.setac.org/group/effect-modeling.html>
#'
#' Schmitt W., Bruns E., Dollinger M., Sowig P., 2013: Mechanistic TK/TD-model
#'   simulating the effect of growth inhibitors on *Lemna* populations. Ecol Model
#'   255, pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
#'
#' @examples
#' # Simulate the example scenario
#' focusd1 %>% simulate()
#'
"focusd1"

#' A DEB abj scenario of Americamysis bahia
#'
#' Species parameters were collected from the AddMyPet database entry on
#' Americamysis bahia (Opossum shrimp). The exposure series consists of a
#' constant exposure resulting in medium effects on length and reproduction.
#'
#' @seealso [DEB_abj()]
#' @source <https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_web/Americamysis_bahia/Americamysis_bahia_res.html>
#'
"americamysis"

#' A DEBtox Daphnia magna scenario
#'
#' Species and substance parameters were collected from test runs of the
#' original DEBtox Daphnia model.
#'
#' @seealso [DEB_Daphnia()]
"dmagna"

#' A Lemna data set with multiple treatment levels
#'
#' Data are from Schmitt 2013 publication.
#'
#' @seealso [Lemna-models]
#' @references
#' Schmitt W., Bruns E., Dollinger M., and Sowig P., 2013: *Mechanistic TK/TD-model
#' simulating the effect of growth inhibitors on Lemna populations*. Ecol Model 255,
#' pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
"Schmitt2013"

#' An algae scenario
#'
#' Data are from Weber 2012 publication.
#'
#' @seealso [Algae_TKTD]
#' @references
#' Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#' and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#' population modeling to assess the effects of time-variable exposure of
#' isoproturon on the green algae Desmodesmus subspictatus and
#' Pseudokirchneriella subcapitata. Environmental Toxicology and
#' Chemistry, 31, 899-908. \doi{10.1002/etc.1765}
"Rsubcapitata"
