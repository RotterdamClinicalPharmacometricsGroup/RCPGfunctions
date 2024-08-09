library(devtools)

create_package("//storage.erasmusmc.nl/m/MyDocs/670009/My Documents/Documents/Github/RCPGfunctions")

use_git()

use_r("focb_locf")


load_all()

roxygen2::roxygenise()

use_mit_license()
