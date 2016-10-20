install_packages <- function(pkgs) {
  stopifnot(is.character(pkgs))
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg) 
    }
  }
}

install_packages("devtools") 
install_packages("magrittr") 
install_packages("dplyr") 

install_github("bcgov/envreportutils")
install_github("bcgov/bcmaps")
install_github("bcgov/rems")
install_github("bcgov/wqbc")
