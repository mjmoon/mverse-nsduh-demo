if(!require(nsduhus)) {
    remotes::install_github("mjmoon/nsduhus")
    library(nsduhus)
}
library("dplyr")

# download NSDUH data from 2005 to 2013 -----
download_nsduhus_zip(as.character(2005:2013))
uncompress_nsduhus_zip(save_to_wd = TRUE)
nsduh_data <- load_nsduhus(read_from_wd = TRUE)
nsduh_data <- lapply(nsduh_data, function(x) {
    colnames(x) <- tolower(colnames(x))
    x
})
# select columns ------
# race: newrace2
#  - 1: non-hispanic white
#  - 2: non-hispanic black
#  - 3: non-hispanic native american
#  - 4: non-hispanic native hawaiian/pacific islanders
#  - 5: non-hispanic asian
#  - 6: non-hispanic mixed race
#  - 7: hispanic
#
# age: catag3           catage              catag2              catag6
#  - 1: 12-17           - 1: 12-17          - 1: 12-17          - 1: 12-17
#  - 2: 18-25           - 2: 18-25          - 2: 18-25          - 2: 18-25
#  - 3: 26-34           - 3: 26-34          - 3: 26 or older    - 3: 26-34
#  - 4: 35-49           - 4: 35 or older                        - 4: 35-49
#  - 5: 50 or older                                             - 5: 50-64
#                                                               - 6: 65 or older
#
#                       catag7                              age2
#                       - 1: 12-13                          - 1-10: 12-21
#                       - 2: 14-15                          - 11: 22-23
#                       - 3: 16-17                          - 12: 24-25
#                       - 4: 18-20                          - 13: 26-29
#                       - 5: 21-25                          - 14: 30-34
#                       - 6: 26-34                          - 15: 35-49
#                       - 7: 35 or older                    - 16: 50-64
#                                                           - 17: 65 or older
#
# sex: irsex
#  - 1: male
#  - 2: female
#
# income: income
#  - 1: less than 20,000
#  - 2: 20,000 - 49,999
#  - 3: 50,000 - 74,999
#  - 4: 75,000 or more
# 
# population density: coutyp2       
#  - 1: large metropolitan
#  - 2: small metropolitan
#  - 3: non-metropolitan
#
#       pden00
#       - 1: segment in a CBSA with 1M or more persons
#       - 2: segment in a CBSA with fewer than 1M persons    
#       - 3: segment not in a CBSA
#
# health insurance: (combination)
#   irinsur4
#  - 1: has health insurance
#  - 2: does not have health insurance
#  irprvhlt
#  - 1: has private health insurance
#  - 2: does not have private health insurance 
#
# past year major depression episode: (combination)
#  amdeyr: for adults
#  - 1: yes
#  - 2: no
#  ymdeyr: for youths
#  - 1: yes
#  - 2: no
#  mdeyr: for both (when ymdeyr is not available)
#  - 1: yes
#  - 2: no
#
# nicotine dependence in the past month: dnicnsp
#  - 0: no
#  - 1: yes
#
# alcohol use disorder in the past year: abodalc
#  - 0: no
#  - 1: yes
#
# other drug use disorder in the past year: (combination)
#  abodcoc: cocaine
#  - 0: no
#  - 1: yes
#  abodhal: hallucinogen
#  - 0: no
#  - 1: yes
#  abodinh: inhalant
#  - 0: no
#  - 1: yes
#  abodmrj: marijuana
#  - 0: no
#  - 1: yes
#  abodsed: sedative
#  - 0: no
#  - 1: yes
#  abodstm: stimulant
#  - 0: no
#  - 1: yes
#  abodtrn: tranquilizer
#  - 0: no
#  - 1: yes
# 
# opioid use disorder: (combination)
#  abodanl: Rx opioid
#  - 0: no
#  - 1: yes
#  abodher: heroin
#  - 0: no
#  - 1: yes
#
# opioid-specific treatment use: 
#  txlther2: received last/current treatment for heroin
#  - 0: no
#  - 1: yes
#  txltanl2: received last/current treatment for pain relievers
#  - 0: no
#  - 1: yes
# 
# study population
#  - opioid use disorder
#

response_columns <- c(
    "txlther2", # heroin
    "txltanl2"  # Rx opioid
)
opioid_disorder_columns <- c(
    "abodher",  # heroin
    "abodanl"   # Rxopioid
)
other_disorder_columns <- c(
    "mdeyr",                           # past year major depression episode
      "amdeyr",                         # adults
      "ymdeyr",                         # youths
    "dnicnsp",                         # past month nicotine dependence
    "abodalc",                          # past year alcohol use disorder
    # other drug use disorder in the past year
    "abodcoc",                          # cocaine
    "abodhal",                          # hallucinogen
    "abodinh",                          # inhalant
    "abodmrj",                          # marijuana
    "abodsed",                          # sedative
    "abodstm",                          # stimulant
    "abodtrn"                           # tranquilizer
)
demographic_columns <- c(
    "newrace2",                        # race
    "catag3",                          # age
      "age2", "catage", "catag2", "catag6", "catag7", 
    "irsex",                           # sex
    "income",                          # income
    "coutyp2", "pden00",               # population density
    "irinsur4", "irprvhlt"             # insurance
)

nsduh_data_cols <- sapply(nsduh_data, colnames)

sapply(
    nsduh_data_cols, 
    function(cols) table(response_columns %in% cols)
)
sapply(
    nsduh_data_cols, 
    function(cols) table(opioid_disorder_columns %in% cols)
)
sapply(
    nsduh_data_cols, 
    function(cols) other_disorder_columns[!other_disorder_columns %in% cols]
    # function(cols) table(other_disorder_columns %in% cols)
)
sapply(
    nsduh_data_cols, 
    function(cols) table(demographic_columns %in% cols)
)

nsduh_data_subset <- lapply(
    seq_along(nsduh_data),
    function(ind, dfs, names) dfs[[ind]] |>
        select(
            "analwt_c", # survey weight
            any_of(response_columns),
            any_of(opioid_disorder_columns),
            any_of(other_disorder_columns),
            any_of(demographic_columns)
        ) |>
        mutate(
            year = names[[ind]]
        ),
    dfs = nsduh_data, names = names(nsduh_data)
) |>
    bind_rows()
# counts reported by Wu, Zhu, and Swartz
# - 503,101 total
# - 6,125 OUD
# counts loaded
# - 502,471 total
# - 6,121 OUD

nsduh_data_subset_opioid <- nsduh_data_subset |>
    filter((abodher == 1) | (abodanl == 1))

# save data -------
write.csv(
    nsduh_data_subset_opioid, 
    "data/nsduh_opioid_2005_2013.csv", 
    row.names = FALSE
)
