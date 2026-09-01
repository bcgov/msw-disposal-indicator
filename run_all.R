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

source("internal.R")

mon_year <- format(Sys.Date(), "%b%Y")
#mon_year <- "Sept2021"
outfile <- paste0("envreportbc_municipal_solid_waste_", mon_year, ".pdf")

rmarkdown::render("print_ver/Municipal_Solid_Waste_print_ver.Rmd",
                  output_file = outfile, params = list(input_source = "local"))
extrafont::embed_fonts(file.path("print_ver", outfile))
