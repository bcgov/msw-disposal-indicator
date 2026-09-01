# Copyright 2026 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(dplyr)
library(tidyr)
library(httr)

match_rd_names <- function(names_to_fix, reference_names, max_distance = 1) {
  reference_names <- sort(unique(reference_names))
  name_matches <- lapply(names_to_fix, function(x) {
    agrep(x, reference_names, max.distance = max_distance)
  })
  many_matches <- vapply(name_matches, function(x) length(x) > 1, FUN.VALUE = logical(1))
  name_matches <- vapply(name_matches, `[[`, 1,  FUN.VALUE = integer(1))
  if (any(many_matches)) {
    warning("Name(s) matched by multiple values. Choosing the first:\n", 
            paste0(names_to_fix[many_matches], ": ", 
                  reference_names[name_matches[many_matches]], 
                  collapse = "\n"), 
            call. = FALSE)
  }
  fixed_names <- reference_names[name_matches]
  fixed_names
}

fill_years <- function(df) {
  years <- dplyr::tibble(Year = min(df$Year):max(df$Year))
  
  df2 <- dplyr::left_join(years, df, by = "Year")
  
  out <- tidyr::complete(df2, Regional_District, Year)
  out <- dplyr::filter(out, !is.na(Regional_District))
  out
}

check_url <- function(x) {
  res <- vector(mode = "list", length = length(x))
  
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      res[i] <- NA_character_
    } else {
      res[[i]] <- tryCatch(GET(x[i], timeout(5)), error = function(e) return(NA))
    }
  }
  
  status <- vapply(res, function(z) {
    if (length(z) == 1 && is.na(z)) return(NA)
    status_code(z)
  },
  numeric(1))
  
  ret <- ifelse(substr(status, 1,1) == 2, "good",
                ifelse(substr(status, 1,1) == 3, "redirect", "bad"))
  ret[is.na(x)] <- NA
  for (r in seq_along(ret)) {
    if (ret[r] %in% c("bad", "redirect")) {
      warning(x[r], " was bad or redirected")
    }
  }
  ret
}
