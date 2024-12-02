# install package
install.packages("devtools")
install.packages("knitr")

# reader
# windows: Rtools

devtools::has_devel(debug = TRUE)
pkgbuild::check_build_tools(debug = TRUE)

# create package

setwd(r"D:\Study\master lecture\longi data\final_project\Longi-Final")
library(devtools)
create_package("linear_model")