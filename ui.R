################################################################################

#                       DDES Shiny Application: UI                             #

################################################################################


## Created by: Wim Trypsteen
## Debugged by: Emile Roose
## Interface redesign: October 2026
## v1.0.0 June 2025

################################################################################

library(shiny)
library(bslib)


## Create a consistent heading for each page of the application.
pageHeading <- function(iconName, title, description) {
  div(
    class = "mb-4",
    h2(icon(iconName), title, class = "h3"),
    p(description, class = "text-muted mb-0")
  )
}


## Present tabular output in a responsive, scrollable container.
tableShell <- function(...) {
  div(class = "table-responsive", ...)
}

shinyUI(
  page_navbar(
    id = "conditionedPanels",
    title = "DDES Converter & Annotation",
    theme = bs_theme(bootswatch = "simplex"),
    fillable = FALSE,
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$meta(
          name = "description",
          content = paste(
            "Convert, annotate, review, and export digital polymerase chain",
            "reaction data in the DDES format."
          )
        )
      ),
      useShinyFeedback(),
      div(
        class = "container-fluid pt-3",
        uiOutput("dataStatusBanner")
      )
    ),

    # Step 1: Select a demo data set or upload platform files.
    nav_panel(
      title = tagList(icon("folder-open"), "1 Import"),
      value = "import",
      div(
        class = "container-fluid py-4",
        pageHeading(
          "folder-open",
          "Import data",
          "Start with an example data set or upload files from your platform."
        ),
        layout_columns(
          col_widths = c(5, 7),
          card(
            class = "h-100",
            card_header(
              div(
                class = "d-flex align-items-start gap-2",
                icon("flask-vial"),
                div(
                  h3("Explore demo data"),
                  p("Load a prepared data set to explore the complete workflow.")
                )
              )
            ),
            card_body(
              selectInput(
                "ddes_demo_datasets",
                "Demo data set",
                choices = c("QIAcuity", "Select"),
                selected = "Select"
              ),
              div(
                class = "d-flex align-items-start gap-2 text-muted small mb-3",
                icon("circle-info"),
                span("The demo includes run, assay, sample, and intensity data.")
              ),
              actionButton(
                "start_demo",
                label = "Start DDES demo",
                icon = icon("play"),
                class = "btn-primary w-100"
              )
            )
          ),
          card(
            class = "h-100",
            card_header(
              div(
                class = "d-flex align-items-start gap-2",
                icon("cloud-arrow-up"),
                div(
                  h3("Upload your own data"),
                  p("Choose a platform to display its required input files.")
                )
              )
            ),
            card_body(
              selectInput(
                "dpcr_platform",
                "Digital polymerase chain reaction platform",
                choices = c(
                  "QIAcuity",
                  "DDES",
                  "QX200_Quantasoft",
                  "Select"
                ),
                selected = "Select"
              ),
              conditionalPanel(
                condition = "input.dpcr_platform == 'QX200_Quantasoft'",
                div(
                  class = "border rounded p-3 mb-3",
                  fileInput(
                    "qx200_head",
                    "Head file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    multiple = FALSE
                  ),
                  fileInput(
                    "qx200_amplitude",
                    "Amplitude files",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    multiple = TRUE
                  ),
                  div(
                    class = "d-flex align-items-start gap-2 text-muted small",
                    icon("file-csv"),
                    span("Upload one head file and one or more amplitude files.")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.dpcr_platform == 'QIAcuity'",
                div(
                  class = "border rounded p-3 mb-3",
                  fileInput(
                    "qq_current_results",
                    "Analysis file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    multiple = TRUE
                  ),
                  fileInput(
                    "qq_plate_layout",
                    "Plate layout",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    multiple = TRUE
                  ),
                  fileInput(
                    "qq_RFU",
                    "Relative fluorescence unit files",
                    accept = ".zip",
                    multiple = FALSE
                  ),
                  div(
                    class = "d-flex align-items-start gap-2 text-muted small",
                    icon("file-zipper"),
                    span("Upload the relative fluorescence unit files as one ZIP archive.")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.dpcr_platform == 'Nio'",
                div(
                  class = "border rounded p-3 mb-3",
                  fileInput(
                    "nio_results",
                    "Results table",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    multiple = FALSE
                  ),
                  fileInput(
                    "nio_fluo_data",
                    "Fluorescence data files",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    multiple = TRUE
                  ),
                  fileInput(
                    "nio_exp_details",
                    "Experiment details file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    ),
                    multiple = FALSE
                  )
                )
              ),
              conditionalPanel(
                condition = "input.dpcr_platform == 'DDES'",
                div(
                  class = "border rounded p-3 mb-3",
                  fileInput(
                    "ddes_zip_upload",
                    "DDES archive",
                    accept = c(".zip", ".ddes"),
                    multiple = FALSE
                  ),
                  div(
                    class = "d-flex align-items-start gap-2 text-muted small",
                    icon("file-zipper"),
                    span("Upload an existing .ddes or .zip archive.")
                  )
                )
              ),
              actionButton(
                "start",
                label = "Start DDES conversion",
                icon = icon("arrows-rotate"),
                class = "btn-primary w-100"
              )
            )
          )
        )
      )
    ),

    # Step 2: Review and update run metadata.
    nav_panel(
      title = tagList(icon("circle-info"), "2 Run info"),
      value = 1,
      div(
        class = "container-fluid py-4",
        pageHeading(
          "circle-info",
          "Run information",
          "Review the metadata retrieved from the imported platform files."
        ),
        card(
          card_header(h3("Experiment metadata")),
          card_body(
            accordion(
              id = "runHelp",
              open = FALSE,
              accordion_panel(
                title = tagList(icon("circle-question"), "Instructions"),
                value = "runInstructions",
                uiOutput("helptext_runinfo1")
              )
            ),
            div(
              class = "row row-cols-1 row-cols-md-2",
              uiOutput("exp_id_user_ui"),
              uiOutput("run_id_user_ui"),
              uiOutput("instrument_user_ui"),
              uiOutput("instrument_software_version_user_ui"),
              uiOutput("plate_type_user_ui")
            ),
            div(
              class = "d-flex flex-wrap gap-2 mt-2",
              uiOutput("update_ddes_main_head")
            )
          )
        )
      )
    ),

    # Step 3: Edit probe and assay annotations side by side.
    nav_panel(
      title = tagList(icon("dna"), "3 Probes & assays"),
      value = 2,
      div(
        class = "container-fluid py-4",
        pageHeading(
          "dna",
          "Probe and assay annotation",
          "Define targets and assemble their probe combinations into assays."
        ),
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header(h3("Probe information")),
            card_body(
              accordion(
                id = "probeHelp",
                open = FALSE,
                accordion_panel(
                  title = tagList(icon("circle-question"), "Instructions"),
                  value = "probeInstructions",
                  uiOutput("helptext_probeinfo1")
                )
              ),
              div(
                class = "border rounded p-3 my-3",
                textInput("probe_name_tmp", "Probe name", value = ""),
                div(
                  class = "d-flex flex-wrap gap-2",
                  actionButton(
                    "add_probe",
                    label = "Add",
                    icon = icon("plus"),
                    class = "btn-primary"
                  ),
                  actionButton(
                    "delete_probe",
                    label = "Delete selected",
                    icon = icon("trash-can"),
                    class = "btn-outline-danger"
                  ),
                  actionButton(
                    "update_ddes_probe",
                    label = "Save probes",
                    icon = icon("floppy-disk"),
                    class = "btn-success"
                  )
                )
              ),
              tableShell(
                rHandsontableOutput("ddes_probe2", height = "500px")
              )
            )
          ),
          card(
            card_header(h3("Assay information")),
            card_body(
              accordion(
                id = "assayHelp",
                open = FALSE,
                accordion_panel(
                  title = tagList(icon("circle-question"), "Instructions"),
                  value = "assayInstructions",
                  uiOutput("helptext_assayinfo1")
                )
              ),
              div(
                class = "border rounded p-3 my-3",
                textInput("assay_name_tmp", "Assay name", value = ""),
                div(
                  class = "d-flex flex-wrap gap-2",
                  actionButton(
                    "add_assay",
                    label = "Add",
                    icon = icon("plus"),
                    class = "btn-primary"
                  ),
                  actionButton(
                    "delete_assay",
                    label = "Delete selected",
                    icon = icon("trash-can"),
                    class = "btn-outline-danger"
                  ),
                  actionButton(
                    "update_ddes_assay",
                    label = "Save assays",
                    icon = icon("floppy-disk"),
                    class = "btn-success"
                  )
                )
              ),
              tableShell(
                rHandsontableOutput("ddes_assay2", height = "500px")
              )
            )
          )
        )
      )
    ),

    # Step 4: Apply assays and update sample identifiers.
    nav_panel(
      title = tagList(icon("table-cells"), "4 Samples"),
      value = 3,
      div(
        class = "container-fluid py-4",
        pageHeading(
          "table-cells",
          "Sample annotation",
          "Assign assays to wells and update the sample identifiers."
        ),
        card(
          card_header(h3("Well and sample information")),
          card_body(
            accordion(
              id = "sampleHelp",
              open = FALSE,
              accordion_panel(
                title = tagList(icon("circle-question"), "Instructions"),
                value = "sampleInstructions",
                uiOutput("helptext_annotinfo1")
              )
            ),
            div(
              class = "d-flex flex-wrap align-items-end gap-3 mb-3",
              div(class = "flex-grow-1", uiOutput("assay_selector")),
              div(
                class = "d-flex flex-wrap gap-2 mb-3",
                actionButton(
                  "apply_assay_to_well",
                  label = "Apply assay",
                  icon = icon("check"),
                  class = "btn-primary"
                ),
                actionButton(
                  "update_sample_id",
                  label = "Save samples",
                  icon = icon("floppy-disk"),
                  class = "btn-success"
                )
              )
            ),
            tableShell(
              rHandsontableOutput("ddes_sample_annotation", height = "580px")
            )
          )
        )
      )
    ),

    # Step 5: Inspect the generated DDES files in one navigation group.
    nav_menu(
      title = tagList(icon("magnifying-glass-chart"), "5 Review"),
      nav_panel(
        title = "Main file",
        value = 4,
        div(
          class = "container-fluid py-4",
          pageHeading(
            "table-list",
            "DDES main file",
            "Review the generated header and target overview for every well."
          ),
          card(
            card_header(h3("Main header")),
            card_body(tableShell(tableOutput("ddes_main_head")))
          ),
          card(
            card_header(h3("Targets per well")),
            card_body(tableShell(tableOutput("ddes_main")))
          )
        )
      ),
      nav_panel(
        title = "Assays file",
        value = 5,
        div(
          class = "container-fluid py-4",
          pageHeading(
            "vials",
            "DDES assays file",
            "Review the generated assay header and assay definitions."
          ),
          card(
            card_header(h3("Assays header")),
            card_body(tableShell(tableOutput("ddes_assay_head")))
          ),
          card(
            card_header(h3("Assay information")),
            card_body(tableShell(tableOutput("ddes_assay")))
          )
        )
      ),
      nav_panel(
        title = "Intensity files",
        value = 6,
        div(
          class = "container-fluid py-4",
          pageHeading(
            "chart-line",
            "DDES intensity files",
            "Select a sample and inspect its fluorescence intensity data."
          ),
          card(
            card_body(
              div(
                class = "d-flex flex-wrap align-items-end gap-3",
                div(class = "flex-grow-1", uiOutput("sample_selector")),
                actionButton(
                  "view_ddes_intensity_file",
                  label = "View intensity",
                  icon = icon("eye"),
                  class = "btn-primary"
                )
              )
            )
          ),
          card(
            card_header(h3("Selected sample header")),
            card_body(tableShell(tableOutput("ddes_intensity_head")))
          ),
          card(
            card_header(h3("Intensity data: first 10 rows")),
            card_body(tableShell(tableOutput("ddes_intensity_data")))
          )
        )
      )
    ),

    # Step 6: Validate and download the completed archive.
    nav_panel(
      title = tagList(icon("download"), "6 Download"),
      value = 7,
      div(
        class = "container py-4",
        pageHeading(
          "download",
          "Download DDES files",
          "Complete the workflow by exporting the converted and annotated data."
        ),
        card(
          card_body(
            div(class = "fs-1 text-success mb-3", icon("file-zipper")),
            h3("Your DDES archive"),
            p(
              "The archive contains the main file, assay definitions, and",
              "intensity files for the complete run."
            ),
            tags$ul(
              class = "list-unstyled",
              tags$li(icon("check"), "Run and sample annotations"),
              tags$li(icon("check"), "Probe and assay definitions"),
              tags$li(icon("check"), "Per-well intensity data")
            ),
            actionButton(
              "validate_download",
              label = "Download DDES archive",
              icon = icon("download"),
              class = "btn-success btn-lg"
            )
          )
        )
      )
    ),

    # Reference documentation remains available without interrupting the workflow.
    nav_panel(
      title = tagList(icon("circle-question"), "Help"),
      value = 8,
      div(
        class = "container py-4",
        pageHeading(
          "circle-question",
          "How to use DDES",
          "Follow the sections below in sequence to complete a conversion."
        ),
        accordion(
          id = "workflowHelp",
          open = "helpImport",
          accordion_panel(
            title = "1. Import data",
            value = "helpImport",
            p(
              "Choose a prepared demo data set and select \"Start DDES demo\",",
              "or select your digital polymerase chain reaction platform, upload",
              "the requested files, and select \"Start DDES conversion\"."
            )
          ),
          accordion_panel(
            title = "2. Review run information",
            p(
              "Inspect the metadata retrieved from the platform output. Update any",
              "missing or incorrect values and select \"Save run information\"."
            )
          ),
          accordion_panel(
            title = "3. Annotate probes and assays",
            tags$ul(
              tags$li("Add a probe by entering its name, selecting \"Add\", and assigning a channel."),
              tags$li("Delete probes by selecting their checkboxes and choosing \"Delete selected\"."),
              tags$li("Create an assay by selecting probes, entering an assay name, and choosing \"Add\"."),
              tags$li("Select \"Save probes\" or \"Save assays\" after making changes.")
            )
          ),
          accordion_panel(
            title = "4. Annotate samples",
            p(
              "Choose an assay, select the relevant wells, and select \"Apply assay\".",
              "Sample names can be entered, pasted, or extended with the table fill",
              "handle. Select \"Save samples\" when finished."
            )
          ),
          accordion_panel(
            title = "5. Review generated files",
            p(
              "Use the Review menu to inspect the DDES main file, assay file, and",
              "the first 10 rows of an individual sample's intensity file. These",
              "views are read-only."
            )
          ),
          accordion_panel(
            title = "6. Download the archive",
            p(
              "Open Download and select \"Download DDES archive\". The application",
              "checks that the required DDES objects are available before preparing",
              "the file."
            )
          )
        ),
        card(
          card_body(
            icon("envelope"),
            div(
              h3("Questions or feedback?"),
              p(
                "Contact wim.trypsteen@ugent.be or the DIGPCR UGent Center."
              )
            )
          )
        )
      )
    ),
    footer = div(
      class = "container-fluid d-flex justify-content-between py-3",
      span("v1.0.0"),
      tags$a(
        "DIGPCR - Ghent University",
        href = "https://digpcr.ugent.be",
        target = "_blank",
        rel = "noopener noreferrer"
      )
    )
  )
)
