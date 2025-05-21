#' Function to standardise UK seabird names
#' @param x a string vector.
#' @param sep a string to be used as a separator between the words of a birds name, e.g. "_" (default) or " ".
#' @param hyphen a string to be used where a hyphen exists in a name (e.g. great black-backed gull). Defaults to the same as sep.
#' @param case_option choose between four options (1, 2, 3 or 4) for whether the outputs are lowercase or uppercase:
#'      1: all lowercase
#'      2: first letter uppercase
#'      3: first letter of each word uppercase
#'      4: all uppercase

fix_seabird_names <- function(x, sep = "_", hyphen = "-", case_option = 2){
  # use the separator as the hyphen unless otherwise stated
  if(is.null(hyphen)){hyp <- sep}else{hyp <- hyphen}

  # vector of replacements to make. \\s? is used within the search strings meaning "optional space"
  rep_patterns <- c(
    "_"     = "",  #remove underscores
    "-"       = hyp, #replace hyphens with chosen hyphen
    "\\b(?<!northern)gannet|\\bnorthern\\s?gannet"                         = "northern gannet",
    "\\b(?<!blacklegged)kittiwake|\\bblack.*legged\\s?kittiwake"           = paste0("black", hyp, "legged kittiwake"),
    "\\blesser\\s?black.*backed\\s?gull"                                   = paste0("lesser black", hyp, "backed gull"),
    "\\bgreat\\s?black.*backed\\s?gull"                                    = paste0("great black", hyp, "backed gull"),
    "\\bblack.*headed\\s?gull"                                             = paste0("black", hyp, "headed gull"),
    "mew\\s?gull|common\\s?gull"                                           = "common gull",
    "manx\\s?shearwater"                                                   = "manx shearwater",
    "\\b(?<!northern)fulmar|\\bnorthern\\s?fulmar"                         = "northern fulmar",
    "\\b^storm.*petrel|\\beuropean\\s?storm.*petrel"                       = paste0("european storm", hyp, "petrel"),
    "storm.?petrel"                                                        = paste0(" storm", hyp, "petrel"),
    "\\bleach.?s\\s?storm.*petrel"                                         = paste0("leach's storm", hyp, "petrel"),
    "\\b(?<!great)cormorant|\\bgreat\\s?cormorant"                         = "great cormorant",
    "\\b(?<!european)shag|\\beuropean\\s?shag"                             = "european shag",
    "\\b(?<!atlantic)puffin|\\batlantic\\s?puffin"                         = "atlantic puffin",
    "black\\s?guillemot|tystie"                                            = "black guillemot",
    "\\b(?<!common)guillemot|\\bcommon\\s?guillemot|\\b(?<!common)murre|common\\s?murre" = "common guillemot",
    "\\bred.*throated\\s?diver|\\bred.*throated\\s?loon"                   = paste0("red", hyp, "throated diver"),
    "\\bblack.*throated\\s?diver|\\bblack.*throated\\s?loon"               = paste0("black", hyp, "throated diver"),
    "great\\s?northern\\s?diver|common\\s?loon"                            = "great northern diver",
    "common\\s?scoter"                                                     = "common scoter",
    "common"                                                               = "common ",
    "auk"                                                                  = " auk",
    "tern"                                                                 = " tern",
    "gull"                                                                 = " gull",
    "^ gull"                                                               = "gull",
    "skua"                                                                 = " skua",
    "diver|loon"                                                           = " diver",
    "\\s{1,}" = " " #replace any amount of whitespace with a single space
  )

  # make all letters lowercase and apply replacements
  names <- tolower(x) %>%
    str_replace_all(., rep_patterns)

  # Apply case transformation based on case_option argument
  if(case_option == 1) {
    names <- tolower(names)        # All lowercase (default)
  } else if(case_option == 2) {
    names <- str_to_sentence(names)# First letter of the first word capitalised
  } else if(case_option == 3) {
    names <- str_to_title(names)   # First letter of every word capitalised
  } else if(case_option == 4) {
    names <- toupper(names)        # All letters capitalised
  } else {
    stop("Invalid case_option. Please choose 1, 2, 3, or 4.")
  }

  # replace spaces with chosen separator
  names <- str_replace_all(names, " ", sep)
  names
}

# Example to test it works
testbirds <- c("gannet", "Northern_gannet", "Lesser Black-backed gull", "Great_blackbacked_gull", "blackleggedkittiwake", "guillemot", "common_guillemot", "leach's storm-petrel", "common_murre", "fulmar", "sandwich tern", "black-necked grebe", "common      gull", "storm petrel", "storm-petrel", "european stormpetrel", "european storm-petrel", "leachsstormpetrel", "white-facedstormpetrel's", "Adouin's gull", "Allgulls", "gull")

fix_seabird_names(testbirds, sep ="_", hyphen = "-", case_option = 2)
