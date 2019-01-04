library(validate)
library(magrittr)
library(tibble)
library(dplyr)
library(purrr)

# -----------------------------------------------------------------------------
# helpers
test_group <- function(.validator_list) {
  .validator_list %>% purrr::reduce(`+`)
}

test_run <- function(.test_data, .validator) {
  confront(.test_data, .validator, key="INDEX", raise = "all") %>%
  as.data.frame
}

add_index <- function(.data, .index = "INDEX") {
  assertthat:: assert_that(!(.index %in% names(.data)))
  .data_names <- names(.data)
  .data$INDEX <- c(1:nrow(.data))
  .data <- .data[,c(.index, .data_names)]
  return(.data)
}

# validation helpers

filter_failing <- function(.data) {
  filter(.data, value == FALSE)
}

# -----------------------------------------------------------------------------
# test data
df <- tibble(
  gender = c("male", "female", "male", "vemale","male", "female"),
  animal = c("dog", "cat", "cat", "dog", "donkey", "dog")
) %>% add_index 

# reference data
cats_gender = c("male", "female")
cats_animal = c("dog", "cat")

# -----------------------------------------------------------------------------
# collector
results <- list()

results$test_na <- list(
  validator(
    !is.na(gender)) %>%
      set_names("test_na_gender"),
  validator(
    !is.na(animal)) %>%
      set_names("test_na_animal")
)%>% 
  test_group %>%
  test_run(.test_data = df)

results$test_cats <- list(
  validator(
    gender %in% cats_gender) %>%
    set_names("test_gender"),
  validator(
    animal %in% cats_animal) %>%
    set_names("test_animals")
  ) %>% 
  test_group %>%
  test_run(.test_data = df)

# collector
results %<>% flatten

# -----------------------------------------------------------------------------
# collector

failing <- filter_failing(results)



