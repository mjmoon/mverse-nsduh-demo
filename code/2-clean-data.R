library(dplyr)
# read data ----------
nsduh_data <- read.csv("data/nsduh_opioid_2005_2013.csv")
# clean data ---------
# the file includes only non-branching data cleaning steps
# - encoding of categorical variables using R's factor()
#   using consistent ordering for boolean variables
# - saving major depression episode indicator to 
#   a single variable `mdeyr` for compatibility 
#   between different encoding over years (before and after 2008)
factor_boolean <- function(x, sorted = TRUE) {
    if (!sorted) x <- 2 - x
    factor(x, labels = c("No", "Yes"))
}
cleaned_data <- nsduh_data |>
    # fix compatibity of major depression episode
    mutate( 
        mdeyr = if_else(
            as.numeric(year) >= 2008,
            if_else(is.na(amdeyr), ymdeyr, amdeyr),
            mdeyr
        )
    ) |>
    select(-c(amdeyr, ymdeyr)) |>
    # encode categorical variable contexts with factor labels
    mutate(
        newrace2 = factor(newrace2, labels = c(
            "White", 
            "Black",
            "Native American",
            "Native Hawaiian/Pacific Islander",
            "Asian American",
            "Mixed race",
            "Hispanic"
        )),
        catag3 = factor(catag3, labels = c(
            "12-17",
            "18-25",
            "26-34",
            "35-49",
            "50+"
        )),
        catage = factor(catage, labels = c(
            "12-17",
            "18-25",
            "26-34",
            "35+"
        )),
        catag2 = factor(catag2, labels = c(
            "12-17",
            "18-25",
            "26+"
        )),
        catag6 = factor(catag6, labels = c(
            "12-17",
            "18-25",
            "26-34",
            "34-49",
            "50-64",
            "65+"
        )),
        catag7 = factor(catag7, labels = c(
            "12-13",
            "14-15",
            "16-17",
            "18-20",
            "21-25",
            "26-34",
            "35+"
        )),
        age2 = factor(age2, labels = c(
            as.character(12:21),
            "22-23",
            "24-25",
            "26-29",
            "30-34",
            "35-49",
            "50-64",
            "65+"
        )),
        irsex = factor(irsex, labels = c(
            "Male", 
            "Female"
        )),
        income = factor(income, labels = c(
            "<$20,000",
            "$20,000-$49,999",
            "$50,000-$74,999",
            "$75,000+"
        )),
        coutyp2 = factor(coutyp2, labels = c(
            "Large metropolitan",
            "Small metropolitan",
            "Non-metropolitan"
        )),
        pden00 = factor(pden00, labels = c(
            "CBSA; 1M+ persons",
            "CBSA; <1M persons",
            "Not a CBSA"
        )),
        across(
            c(irinsur4, irprvhlt, mdeyr),
            ~ factor_boolean(.x, sorted = FALSE)
        ),
        across(
            c(dnicnsp, starts_with("abod"), txlther2, txltanl2),
            factor_boolean
        )
    )
# save data ----------
saveRDS(cleaned_data, "data/nsduh_opioid_2005_2013.rds")

