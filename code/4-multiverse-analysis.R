if(!require(mverse)) {
    install.packages("mverse")
    library(mverse)
}
library(future)
library(dplyr)
library(ggplot2)
# read data ----------
nsduh_data <- readRDS("data/nsduh_opioid_2005_2013.rds") |>
    mutate(
        opioid_use_treatment = (txlther2 == "Yes") | (txltanl2 == "Yes"),
        sex = irsex,
        family_income = if_else(as.integer(income) <= 2, "<$50,000", income),
        pop_density = coutyp2,
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
# define multiverse ----------
## define mutate branches ----
race <- mutate_branch(
    # original: lump native hawaiian/pacific islanders with asians
    if_else(
        as.integer(newrace2) %in% c(4, 5),
        "Native Hawaiian/Pacific islander/Asian American",
        newrace2
    ) |> factor(levels = c(
        "White",
        "Black",
        "Native American",
        "Native Hawaiian/Pacific islander/Asian American",
        "Mixed Race",
        "Hispanic"
    )),
    # lump native hawaiian/pacific islanders, native americans, and asians
    if_else(
        as.integer(newrace2) %in% c(3, 4, 5),
        "Native American/Native Hawaiian/Pacific islander/Asian American",
        newrace2
    ) |> factor(levels = c(
        "White",
        "Black",
        "Native American/Native Hawaiian/Pacific islander/Asian American",
        "Mixed Race",
        "Hispanic"
    )),
    # lump all but white, black and hispanic
    if_else(
        as.integer(newrace2) %in% c(3, 4, 5, 6),
        "Others",
        newrace2
    ) |> factor(levels = c(
        "White",
        "Black",
        "Hispanic",
        "Others"
    ))
)
age <- mutate_branch(
    # original: use '12-17', '18-25', '26-35', '35-49', '50+' grouping
    catag3, 
    # use other age groupings
    catage, catag2, catag6, catag7, age2
)
## define the formula branch ---
formulae_logistic <- formula_branch(
    opioid_use_treatment ~ race,
    opioid_use_treatment ~ race + age + sex + 
        family_income + pop_density + health_insurance + heroin_use_disorder +
        nicotine_dependence + alcohol_use_disorder + other_drug_use_disorder,
    opioid_use_treatment ~ race + age + sex + 
        family_income + pop_density + health_insurance + heroin_use_disorder +
        nicotine_dependence + alcohol_use_disorder + other_drug_use_disorder + 
        major_depression_episode,
    covariates = c("year")
)
## define a filter branch ----
filter_missing_mdeyr <- filter_branch(
    # original: filter those missing major depression episode variable
    !is.na(mdeyr),
    # no filter
    TRUE
)
## define the family branch ----
family_logistic <- family_branch(binomial)
## define branch conditions to remove redundant branch options ---
filter_cond <- branch_condition(
    opioid_use_treatment ~ race + age + sex + 
        family_income + pop_density + health_insurance + heroin_use_disorder +
        nicotine_dependence + alcohol_use_disorder + other_drug_use_disorder + 
        major_depression_episode,
    !is.na(mdeyr)
)
age_cond_1 <- branch_condition(
    catage, opioid_use_treatment ~ race, reject = TRUE)
age_cond_2 <- branch_condition(
    catag2, opioid_use_treatment ~ race, reject = TRUE)
age_cond_3 <- branch_condition(
    catag6, opioid_use_treatment ~ race, reject = TRUE)
age_cond_4 <- branch_condition(
    catag7, opioid_use_treatment ~ race, reject = TRUE)
age_cond_5 <- branch_condition(
    age2, opioid_use_treatment ~ race, reject = TRUE)
## define multiverse
mv_nsduh <- mverse(nsduh_data) |>
    add_filter_branch(filter_missing_mdeyr) |>
    add_mutate_branch(race, age) |>
    add_family_branch(family_logistic) |>
    add_formula_branch(formulae_logistic) |>
    add_branch_condition(
        filter_cond, 
        age_cond_1, age_cond_2, age_cond_3, age_cond_4, age_cond_5
    )
# inspect multiverse ------
execute_multiverse(mv_nsduh)

multiverse_tree(
    mv_nsduh, 
    label = "code",
    branches = c("formulae_logistic", "age"),
    label_size = 2
    ) +
    ggraph::scale_edge_colour_brewer(
        palette = "Dark2",
        breaks = c("formulae_logistic", "age"),
        labels = c( "Model Specification", "Age")
    ) +
    ggtitle("Multiverse tree diagram") +
    theme(plot.title = element_text(hjust = 0.5))


mutiverse_table <- summary(mv_nsduh)
# fit multiverse ---
plan(multisession, workers = availableCores())
glm_mverse(mv_nsduh)
multiverse_result <- spec_summary(mv_nsduh, var = "raceBlack")
plan(sequential)

write.csv(multiverse_result, "data/multiverse_result.csv", row.names = FALSE)

multiverse_result_formatted <- multiverse_result |>
    select(-starts_with("family_logistic")) |>
    mutate(
        estimate = exp(estimate),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high),
        is_original = (
            filter_missing_mdeyr_branch == "filter_missing_mdeyr_1" &
                age_branch == "age_1" &
                race_branch == "race_1" &
                covariate_year_branch == "include_year" &
                formulae_logistic_branch == "formulae_logistic_2" 
        ),
        formulae_logistic_branch = case_match(
            formulae_logistic_branch,
            "formulae_logistic_1" ~ "No covariate",
            "formulae_logistic_2" ~ "All but major depression episode",
            "formulae_logistic_3" ~ "All"
        ),
        race_branch = case_match(
            race_branch,
            "race_1" ~ "1. Group Native Hawaiian/Pacific Islander with Asian American",
            "race_2" ~ "2. Further group Native American",
            "race_3" ~ "3. Further group mixed races"
        ),
        filter_missing_mdeyr_branch = case_match(
            filter_missing_mdeyr_branch,
            "filter_missing_mdeyr_1" ~ "Remove",
            "filter_missing_mdeyr_2" ~ "Keep"
        ),
        covariate_year_branch = case_match(
            covariate_year_branch,
            "exclude_year" ~ "Exclude",
            "include_year" ~ "Include"
        ),
        age_branch = age_branch_code
    ) |>
    rename(
        `Race grouping_branch` = "race_branch",
        `Covariates (others)_branch` = "formulae_logistic_branch",
        `Missing major depression episode data_branch` = "filter_missing_mdeyr_branch",
        `Covariate 'year'_branch` = "covariate_year_branch",
        `Age (variable name in NSDUH PUF)_branch` = "age_branch"
    )

colour_palette <- c(`FALSE` = "darkgrey", `TRUE` = "coral3")
spec_curve_nsduh <- multiverse_result_formatted |>
    spec_curve(
        order_by = c("estimate", "race_branch_code"), 
        colour_by = "is_original",
        palette_common = colour_palette,
        spec_matrix_spacing = 5
    ) +
    scale_colour_manual(
        values = colour_palette,
        breaks = TRUE,
        labels = "Original analysis",
        name = NULL
    ) +
    labs(
        y = NULL,
        title = paste0(
            "Adjusted odd ratio of opioid use treatment service use ",
            "for non-Hispanic black vs. non-Hispanic white populations"
        ),
        caption = paste0(
            "(n=", nrow(nsduh_data), 
            "; n=", nrow(filter(nsduh_data, !is.na(mdeyr))),
            " with data missing major depression espisode removed)"
        )
    ) +
    theme(
        legend.position = "top",
        legend.justification = "right",
        plot.title.position = "plot"
    )

ggsave("man/spec_curve_nsduh_ex.png", spec_curve_nsduh, device = "png",
       width = 10, height = 10, units = "in", dpi = 400)

