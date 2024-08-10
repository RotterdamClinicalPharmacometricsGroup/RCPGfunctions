library(devtools)

create_package("//storage.erasmusmc.nl/m/MyDocs/670009/My Documents/Documents/Github/RCPGfunctions")

system("git config user.name 'T. Preijers'")
system("git config user.name ") #check

# Create functions --------------------------------------------------------
# use_r("focb_locf")
# use_r("cls_env")
# use_r("seq_by_id")
# use_r("seq_by_occ")
# use_r("cov_ABW")
# use_r("cov_BMI")
# use_r("cov_BSA")
# use_r("cov_FFM")
# use_r("cov_IBW")
# use_r("cov_LBW")



# Load functions into environment -----------------------------------------
load_all()



# Create roxygen documentation for all functions --------------------------
roxygen2::roxygenise()


# Create license ----------------------------------------------------------
use_mit_license()
