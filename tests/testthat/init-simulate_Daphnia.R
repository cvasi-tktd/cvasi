#
# load results from the original Daphnia magna model
#
tfs_result <- data.frame()
for(part in c("damage","length","repro","surv")) {
  state <- toupper(substring(part, 1, 1))
  fn <- test_path(paste0("../data/DEBtox_Daphnia/test_",part,".txt"))
  df <- read.table(fn, stringsAsFactors = FALSE)
  names(df) <- c("time","set1_control","set1_control2",paste0("set1_VAR1_",1:3),
                 "set2_control","set2_control2",paste0("set2_VAR2_",1:3),
                 "set3_control","set3_control2",paste0("set3_CST_",1:5))
  df$var <- state

  df %>%
    tidyr::pivot_longer(!c(time,var), names_to="scenario") %>%
    dplyr::mutate(set=substring(scenario,1,4), run=substring(scenario,6)) %>%
    dplyr::select(time,var,set,run,value) %>%
    dplyr::filter(run != "control2") %>%
    dplyr::bind_rows(tfs_result) -> tfs_result

}
rm(part,fn,state,df)

tfs_result %>%
  tidyr::pivot_wider(names_from=var, values_from=value) %>%
  dplyr::arrange(set,run,time) %>%
  dplyr::select(time,set,run,D,L,R,S) -> tfs_result

#
# load original exposure series
#
tfs_exposure <- list()

exp <- read.table(test_path("../data/DEBtox_Daphnia/Data_exposure_profile_Dmagna_TFS_Cst.txt"), stringsAsFactors=FALSE, header=FALSE)
names(exp) <- c("time","control", paste0("exp.",seq(length(exp)-2)))
# fix constant exposure series
if(nrow(exp)==2) {
  exp[2,-1] <- exp[1,-1]
}
tfs_exposure$cst <- exp


# variable exposure series No. 1
exp <- read.table(test_path("../data/DEBtox_Daphnia/Data_exposure_profile_Dmagna_TFS_Var1.txt"), stringsAsFactors=FALSE, header=FALSE)
names(exp) <- c("time","control", paste0("exp.",seq(length(exp)-2)))
tfs_exposure$var1 <- exp

# variable exposure series No. 2
exp <- read.table(test_path("../data/DEBtox_Daphnia/Data_exposure_profile_Dmagna_TFS_Var2.txt"), stringsAsFactors=FALSE, header=FALSE)
names(exp) <- c("time","control", paste0("exp.",seq(length(exp)-2)))
tfs_exposure$var2 <- exp
rm(exp)

#
# model parameters
#
tfs_param <- list(
  L0 = 0.9676,
  Lp = 2.42,
  Lm = 4.614,
  rB = 0.1106,
  Rm = 16.69,
  f = 1,
  hb = 0.001608,
  Lf = 0,
  Lj = 0,
  Tlag = 0,
  kd = 2.060,
  zb = 5.201,
  bb = 0.04422,
  zs = 11.75,
  bs = 0.1759,
  FBV    = 0.02,
  KRV    = 1,
  kap    = 0.8,
  yP     = 0.8*0.8,
  Lm_ref = 4,
  len    = 2,
  Tbp    = 3,
  MoA = 1,
  FB = 0
)

#
# initial state
#
tfs_init <- c(D=0, L=tfs_param$L0, R=0, S=1)
