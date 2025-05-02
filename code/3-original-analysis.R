library(dplyr)
library(forcats)
# read data ----------
nsduh_data <- readRDS("data/nsduh_opioid_2005_2013.rds")
# clean data ---------
fit_data <- nsduh_data |>
    # remove 148 with missing data on major depressive episode
    filter(!is.na(mdeyr)) |>
    mutate(
        opioid_use_treatment = (txlther2 == "Yes") | (txltanl2 == "Yes"),
        race = if_else( # lump native hawaiian/pacific islanders with asians
            as.integer(newrace2) %in% c(4, 5),
            "Native Hawaiian/Pacific Islander/Asian American",
            newrace2
        ) |> factor(
            levels = c(
                "White",
                "Black",
                "Native American",
                "Native Hawaiian/Pacific Islander/Asian American",
                "Mixed race",
                "Hispanic"
            )
        ),
        age = catag3,
        sex = irsex,
        income = if_else( # lump first 2 tiers
            as.integer(income) <= 2,
            "<$50,000",
            income
        ),
        pop_density = coutyp2,
        health_insurance = case_when(
            irinsur4 == "No" ~ 0,
            (irinsur4 == "Yes") & (irprvhlt == "Yes") ~ 1,
            (irinsur4 == "Yes") & (irprvhlt == "No") ~ 2
        ) |>
            factor(labels = c("None", "Private", "Public")),
        heroin_use_disorder = abodher,
        major_depression_episode = mdeyr,
        nicotine_dependence = dnicnsp,
        alcohol_use_disorder = abodalc,
        other_drug_use_disorder = factor(
            (abodcoc == "Yes")
            | (abodhal == "Yes")
            | (abodinh == "Yes") 
            | (abodmrj == "Yes")
            | (abodsed == "Yes")
            | (abodstm == "Yes")
            | (abodtrn == "Yes"),
            labels = c("No", "Yes")
        ),
        year = factor(year)
    )
# define model -------
formula_fit <- formula(
    opioid_use_treatment ~ race + age + sex + 
        income + pop_density + health_insurance +
        heroin_use_disorder + major_depression_episode + 
        nicotine_dependence + alcohol_use_disorder + other_drug_use_disorder +
        year
)
# fit model ----------
fit <- glm(formula = formula_fit, data = fit_data,
           family = "binomial")
# inspect ------------
View(broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE))
