# Copyright 2025 Province of British Columbia
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

shinyUI(
  fluidPage(
    includeCSS("www/style.css"),
    tags$head(
      tags$meta(name = "robots", content = "noindex")
    ),
    fixedRow(align = "center",
             div(style = div_css(p1.w, p1.h + 50),
                 conditionalPanel("output.plot_rd",
                   h2(paste(max_year, "Regional District Disposal Rates")),
                 div(class = "div-link", style = paste0("width:", translate_px(p1.w - 38), ";"),
                     HTML(paste0(div("Sort by: ", class = 'msw-label'),
                                 actionButton("sort_name", "Name", class = 'msw-button'),
                                 "|",
                                 actionButton("sort_rate", "Disposal Rate", class = 'msw-button'),
                                 "|",
                                 actionButton("sort_population", "Population", class = 'msw-button')
                     ))
                 )),
                 girafeOutput(outputId = 'plot_rd', height = p1.h))
    ),
    fixedRow(align = "center",
             div(style = div_css(p1.w, p1.h),
             uiOutput("ui_info"),
             girafeOutput(outputId = 'plot_year', height = p2.h),
             br(),
             uiOutput("ui_resources", style = "display: inline-block;"),
             uiOutput("ui_dl", style = "display: inline-block;"))
    ))
)
