## Codes are used to avoid that users upload weird sequences never approved
## and then complain.
## See help(verification_hash)
verification_hash <- c(
  "c2bba3c97909e573a3f7b25dad61380d",
  "9d6657ac360ac62b123f45737fe07b43",
  "90a0b766b152243abab71c7e9a676828"
)
usethis::use_data(verification_hash, overwrite = TRUE)
