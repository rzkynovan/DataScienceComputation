install.packages("usethis")
install.packages("gitcreds")

library(usethis)
library(gitcreds)

use_git_config(user.name = "rzkynovan",
               user.email = "rizkynovan21@gmail.com")

usethis::create_github_token()
gitcreds::gitcreds_set()
