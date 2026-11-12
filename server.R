################################################################################

#                         DDES Shiny Application: server                       #

################################################################################


## Created by: Wim Trypsteen
## Debugged by: Emile Roose
## v1.0.0 June 2025

################################################################################

shinyServer(function(input, output, session) {

  ## 0. Set general options
  
  #  File size increase for upload
  options(shiny.maxRequestSize=50*1024^2) 

  # Store the imported data set details for the status banner.
  dataStatus <- reactiveVal(NULL)

  output$dataStatusBanner <- renderUI({
    currentStatus <- dataStatus()

    if (is.null(currentStatus)) {
      div(
        class = "alert alert-secondary d-flex align-items-center gap-2 mb-0",
        icon("circle-info"),
        div(
          strong("No data set loaded"),
          span("Open Import to load demo data or upload platform files.")
        )
      )
    } else {
      div(
        class = "alert alert-success d-flex align-items-center gap-2 mb-0",
        icon("circle-check"),
        div(
          strong("Data set ready"),
          span(
            paste(
              currentStatus$source,
              "· Experiment:", currentStatus$experimentId,
              "· Run:", currentStatus$runId
            )
          )
        )
      )
    }
  })
  
  # Information text 
  
  # Probes
  output$helptext_probeinfo1 <- renderUI({
    tags$ul(
      class = "mb-0",
      tags$li("Enter a target identifier, select \"Add\", and choose its channel."),
      tags$li("Select probe checkboxes before choosing \"Delete selected\"."),
      tags$li("Select \"Save probes\" after making changes."),
      tags$li("Names may contain letters, numbers, hyphens, or underscores.")
    )
  })
  output$helptext_probeinfo2 <- renderUI({
    HTML(" <br> ")
  })
  
  # Assays
  output$helptext_assayinfo1 <- renderUI({
    tags$ul(
      class = "mb-0",
      tags$li("Select probes, enter an assay identifier, and choose \"Add\"."),
      tags$li("Select assay checkboxes before choosing \"Delete selected\"."),
      tags$li("Select \"Save assays\" after making changes."),
      tags$li("Names may contain letters, numbers, hyphens, or underscores.")
    )
  })
  output$helptext_assayinfo2 <- renderUI({
    HTML(" <br> ")
  })
  
  # Sample Annotation
  output$helptext_annotinfo1 <- renderUI({
    tags$ul(
      class = "mb-0",
      tags$li("Enter, paste, or extend sample names with the table fill handle."),
      tags$li("Choose an assay, select wells, and then choose \"Apply assay\"."),
      tags$li("Select \"Save samples\" after making changes."),
      tags$li("Names may contain letters, numbers, hyphens, or underscores.")
    )
  })
  output$helptext_annotinfo2 <- renderUI({
    HTML(" <br> ")
  })
  
  ## 1. PERFORM DDES CONVERSION WITH EDITS

  observe({
    if (input$dpcr_platform != "Select") {
      hideFeedback("dpcr_platform")
    }
  })
  
  observe({
    if (input$ddes_demo_datasets != "Select") {
      hideFeedback("ddes_demo_datasets")
    }
  })
  
  observeEvent(input$start_demo | input$start,{

    # Record the source shown after a successful import.
    dataSourceLabel <- NULL
    
    ### Start demo specific processing
    if(input$start_demo > demo_clicks){
      
      ## Hide feedback for upload set
      hideFeedback("dpcr_platform")
      hideFeedback("qq_RFU")
      hideFeedback("qq_current_results")
      hideFeedback("qq_plate_layout")
      hideFeedback("ddes_zip_upload")
      
      ## Test if a data set is selected
      if(input$ddes_demo_datasets == "Select"){
        
        ## Show warning
        showFeedbackDanger(inputId = "ddes_demo_datasets",
                           text = "Please select a data set") 
        ## Set status for shared processing to not run
        shared_processing <<- "not_OK"
        
      } else {
        
        ## Suppress warning
        hideFeedback("ddes_demo_datasets")
        
        ## Retrieve demo data set name
        demo_dataset_selected <- input$ddes_demo_datasets
        dataSourceLabel <- paste("Demo data:", demo_dataset_selected)
        print(paste("Using DDES Demo data set: ", demo_dataset_selected))
        
        ## Run function to get the DDES demo data set
        retrieve_demo_dataset_ddes(input$ddes_demo_datasets)
        
        ## Set status for shared code to run
        shared_processing <<- "OK"
        
        ## Close demo data set retrieval (if loop test for select)
      }
      
      ## Increase the demo click counter
      demo_clicks <<- demo_clicks + 1
      
    }
    ### End of demo specific processing
    
    ### Start upload specific processing
    if(input$start > upload_clicks){
      
      ## Hide feedback for demo data set
      hideFeedback("ddes_demo_datasets")
      
      ## Set status for shared processing to not run
      shared_processing <<- "not_OK"
      
      ## Test if a platform is selected
      if(input$dpcr_platform == "Select"){
        
        showFeedbackDanger(inputId = "dpcr_platform",
                           text = "Please select a platform")
      } else {
        
        ## Suppress warning
        hideFeedback("dpcr_platform")
        
        ## Retrieve selected platform
        platform_selected <<- input$dpcr_platform
        dataSourceLabel <- paste("Uploaded platform:", platform_selected)
        print(paste("Using Uploaded Data Set: ", platform_selected))
        
        ## Process DDES uploads (.ddes or .zip)
        if(platform_selected == "DDES"){
          
          ddes_zip_folder <<- input$ddes_zip_upload
          
          if (is.null(ddes_zip_folder)){
            
            showFeedbackDanger(inputId = "ddes_zip_upload",
                               text = "Nothing to upload")
            
            ## Set status for shared processing to not run
            shared_processing <<- "not OK"
            
          } else {
            
            hideFeedback("ddes_zip_upload")
            retrieve_upload_dataset_ddes()
            
            ## Set status for shared processing to run
            shared_processing <<- "OK"
          }
        }
        
        if(platform_selected == "QIAcuity"){
          
          # Get the filename info of the uploads
          qq_cur_res_file <<- input$qq_current_results
          qq_RFU_files <<- input$qq_RFU
          qq_plate_layout_file <<- input$qq_plate_layout
          
          # Test if (RFU) files are uploaded
          if (is.null(qq_RFU_files)|is.null(qq_cur_res_file)|is.null(qq_plate_layout_file)){
            
            if(is.null(qq_RFU_files)){
            showFeedbackDanger(inputId = "qq_RFU",
                               text = "RFU files needed")
            }
            if(is.null(qq_cur_res_file)){
              showFeedbackDanger(inputId = "qq_current_results",
                                 text = "Analysis file needed")
            }
            if(is.null(qq_plate_layout_file)){
              showFeedbackDanger(inputId = "qq_plate_layout",
                                 text = "Plate Layout file needed")
            }
            
            ## Set status for shared processing to not run
            shared_processing <<- "not OK"
            
          } else {
            
            
            hideFeedback("qq_RFU")
            hideFeedback("qq_current_results")
            hideFeedback("qq_plate_layout")
            retrieve_upload_dataset_qiacuity()
            
            ## Set status for shared processing to run
            shared_processing <<- "OK"
          
            }
        }
        
        if(platform_selected == "QX200_Quantasoft"){
          
          # Get the filename info of the uploads
          qx200_head_file <<- input$qx200_head
          qx200_amplitude_files <<- input$qx200_amplitude

          # Test if files are uploaded
          # To do: also check number of amplitude with unique well names?
          if (is.null(qx200_head_file)|is.null(qx200_amplitude_files)){
            
            if(is.null(qx200_head_file)){
              showFeedbackDanger(inputId = "qx200_head",
                                 text = "Head file needed")
            }
            if(is.null(qx200_amplitude_files)){
              showFeedbackDanger(inputId = "qx200_amplitude",
                                 text = "Amplitude files needed")
            }

            
            ## Set status for shared processing to not run
            shared_processing <<- "not OK"
            
          } else {
            
            
            hideFeedback("qx200_head")
            hideFeedback("qx200_amplitude")

            retrieve_upload_dataset_qx200_quantasoft()
            
            ## Set status for shared processing to run
            shared_processing <<- "OK"
            
          }
        }
        ## To do: Process Platform upload in one function
        #retrieve_upload_dataset(platform_selected)
        
        ## Run functions to get to the uploaded files
        #retrieve_upload_dataset_ddes()
        #retrieve_upload_dataset_qiacuity()
 
        ## Close
        
      }
      
      ## Increase the upload click counter
      upload_clicks <<- upload_clicks + 1
      
    }
    ### End upload specific processing
    
    ### Execute shared processing
    if(shared_processing == "OK"){

      dataStatus(
        list(
          source = dataSourceLabel,
          experimentId = experimentid,
          runId = runid
        )
      )
      
      ## Start of shared processing
      ## Initiate User Input Fields for minimal DDES info
      output$exp_id_user_ui <- renderUI({
        textInput("exp_id_user", "Experiment ID", value = experimentid)
      })
      output$run_id_user_ui <- renderUI({
        textInput("run_id_user", "Run ID", value = runid)
      })
      output$instrument_user_ui <- renderUI({
        selectInput("instrument_user", "Instrument", choices = c("NA", instruments), 
                    selected = instrument)
      })
      output$instrument_software_version_user_ui <- renderUI({
        textInput("instrument_software_version_user", "Instrument Software Version", 
                  value = instrument_software_version)
      })
      output$plate_type_user_ui <- renderUI({
        selectInput("plate_type_user", "Plate or Chip Type", 
                    choices = c("NA", plate_types), selected = plate_type)
      })
      output$update_ddes_main_head <- renderUI({
        actionButton(
          "update_ddes_main_head",
          "Save run information",
          icon = icon("floppy-disk"),
          class = "btn-success"
        )
      })
      
      output$helptext_runinfo1 <- renderUI({
        tags$ul(
          class = "mb-0",
          tags$li("Verify the imported values and update them where needed."),
          tags$li("Names may contain letters, numbers, hyphens, or underscores."),
          tags$li("The software version may also contain periods."),
          tags$li("Select \"Save run information\" to retain your changes.")
        )
      })
      output$helptext_runinfo2 <- renderUI({
        HTML(" <br> ")
      })
      
      ## Generate Reactive View of the DDES MAIN Header
      
      # Grab The Reactive value to store in the ddes main header
      ddes_import_main_head_df_tmp <- reactiveValues(data = ddes_import_main_head_df)
      
      # Update the ddes headers when Update button is clicked
      observeEvent(input$update_ddes_main_head,{
        
        # Integrity Checks for special characters, tabs and spaces 
        if(grepl(special_chars, input$exp_id_user)==FALSE){
          
          showFeedbackDanger(
            
            inputId = "exp_id_user",
            #text = "Error: Special characters #%&{}<>*?/$!:@+\\\\'\"|= and spaces are not allowed."
            text = "Error: Only letters, numbers, hyphens or underscores are allowed"
            
          )} else if(grepl(special_chars, input$run_id_user)==FALSE) {
            
            showFeedbackDanger(
              
              inputId = "run_id_user",
              #text = "Error: Special characters #%&{}<>*?/$!:@+\\\\'\"|= and spaces are not allowed."
              text = "Error: Only letters, numbers, hyphens or underscores are allowed"
              
            )} else if(grepl(special_chars_3, input$instrument_software_version_user)==FALSE) {
              
              showFeedbackDanger(
                
                inputId = "instrument_software_version_user",
                #text = "Error: Special characters #%&{}<>*?/$!:@+\\\\'\"|= and spaces are not allowed."
                text = "Error: Only letters, numbers, dots, hyphens or underscores are allowed"
                
              )}else {
                
                # Suppress error
                hideFeedback("exp_id_user")
                hideFeedback("run_id_user")
                hideFeedback("instrument_software_version_user")
                
                # Update ddes_import_main_head_df_tmp and df
                ddes_import_main_head_df_tmp$data[3, 2] <- datetime
                ddes_import_main_head_df_tmp$data[4, 2] <- input$exp_id_user
                ddes_import_main_head_df_tmp$data[5, 2] <- input$run_id_user
                ddes_import_main_head_df_tmp$data[7, 2] <- input$instrument_user
                ddes_import_main_head_df_tmp$data[9, 2] <- input$instrument_software_version_user
                ddes_import_main_head_df_tmp$data[8, 2] <- input$plate_type_user
                
                ddes_import_main_head_df <<- ddes_import_main_head_df_tmp$data
                
                ddes_import_main_head <- paste(ddes_import_main_head_df[,1], ddes_import_main_head_df[,2], sep="\t")
                ddes_import_main_head[1] <- "#header"
                ddes_import_main_head[10] <- "#data"
                ddes_import_main_head <<- ddes_import_main_head
                
                # update main table and viewing
                ddes_import_main <<- ddes_import_main
                ddes_import_main_table <- reactiveVal(ddes_import_main)
                output$ddes_main <- renderTable(ddes_import_main_table())
                
                # Update objects changed by enduser
                experiment_id <<- input$exp_id_user
                run_id <<- input$run_id_user
                instrument <<- input$instrument_user
                instrument_software_version <<- input$instrument_software_version_user
                plate_type <<- input$plate_type_user
                
                # Update Assay Header in df
                ddes_import_assay_head_df[3,2] <- datetime
                ddes_import_assay_head_df[4,2] <- experimentid
                ddes_import_assay_head_df[5,2] <- runid
                ddes_import_assay_head_df <<- ddes_import_assay_head_df
                
                # Update Assay Header for DDES export
                ddes_import_assay_head[4] <- paste("#experiment_ID",experimentid, sep="\t")
                ddes_import_assay_head[5] <- paste("#run_ID",runid, sep="\t")
                ddes_import_assay_head <<- ddes_import_assay_head
                
                # Update Intensity Files Header
                for(i in 1:length(intensity_files_header_list)){
                  
                  #Update header info with exp/runid
                  intensity_files_header_list[[i]][4] <- paste("#experiment_ID", experimentid, sep = "\t")
                  intensity_files_header_list[[i]][5] <- paste("#run_ID", runid, sep = "\t")
                  
                }
                
                intensity_files_header_list <<- intensity_files_header_list

                # Refresh the banner with the user-confirmed identifiers.
                currentStatus <- dataStatus()
                dataStatus(
                  list(
                    source = currentStatus$source,
                    experimentId = input$exp_id_user,
                    runId = input$run_id_user
                  )
                )
                
                # Confirm that the run information was saved.
                showNotification("DDES information saved successfully", type = "message", 
                                 duration = 5)
                
                # Update table views ddes main and assay
                output$ddes_assay2 <- renderRHandsontable({
                  
                  rhandsontable(ddes_import_assay, editable = TRUE, selectCallback = TRUE) %>%
                    hot_col(col = 'probe_concentrations_nM', readOnly = TRUE) %>%
                    hot_col(col = 'probe_combination', readOnly = TRUE) %>%
                    hot_col(col = 'target_number', readOnly = TRUE) %>%
                    hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                      "function(value, callback) {
                          if (value === null) {
                     callback(false);  // Prevent deletion by returning false
                    } else {
                      callback(true);   // Allow check/uncheck
                 }
                     }"
                    ))
                  
                })
              
                
                
                
              }
        
        
      })
      

      ### END OF TAB 1 DDES MINIMAL INFO
      
      
      ### START OF TAB 2 PROBE AND ASSAY INFO
      
      ## DDES PROBES

      # Dynamic view of probe info
      ddes_import_probe_table <- reactiveVal(ddes_import_probe)
      output$ddes_probe2 <- renderRHandsontable({
        
        rhandsontable(ddes_import_probe, editable = TRUE, selectCallback = TRUE,
                      overflow = "visible" ) %>%
          hot_col("channel_ID", type = "dropdown", source = channel_id, strict = TRUE, allowInvalid = FALSE)%>%
          hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
            "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
          ))
        
      })
      
      # Validate probe combinations
      validate_probe_combinations <- function(probe_df) {
        # Convert inputs to appropriate types
        probe_df$target_ID <- as.character(probe_df$target_ID)
        probe_df$channel_ID <- as.character(probe_df$channel_ID)
        
        # Remove empty rows
        probe_df <- probe_df[probe_df$target_ID != "" & probe_df$channel_ID != "", ]
        
        # Check for duplicates using base R
        has_duplicates <- FALSE
        if(nrow(probe_df) > 0) {
          channels <- unique(probe_df$channel_ID)
          for(chan in channels) {
            targets_in_channel <- probe_df$target_ID[probe_df$channel_ID == chan]
            if(length(targets_in_channel) != length(unique(targets_in_channel))) {
              has_duplicates <- TRUE
              break
            }
          }
        }
        
        return(!has_duplicates)
      }
      
      # Update Probe info when edited
      observeEvent(input$update_ddes_probe, {
        req(input$ddes_probe2)
        
        # Get the updated probe data
        updated_probe_data <- hot_to_r(input$ddes_probe2)
        
        # Check for missing channel assignments
        missing_channels <- updated_probe_data[ 
                                                 (is.na(updated_probe_data$channel_ID) | 
                                                    updated_probe_data$channel_ID == ""), ]
        missing_probenames <- updated_probe_data[updated_probe_data$target_ID == "",]
        
        if(nrow(missing_probenames) > 0) {
          shinyalert(
            title = "Warning",
            text = paste("There are probe names missing. Please provide a name"),
            type = "warning"
          )
          return()
        }
        if(nrow(missing_channels) > 0) {
          shinyalert(
            title = "Warning",
            text = paste("Please assign channels to the following probes:", 
                         paste(missing_channels$target_ID, collapse = ", ")),
            type = "warning"
          )
          return()
        }
        
        # Validate the probe combinations
        if (!validate_probe_combinations(updated_probe_data)) {
          shinyalert(
            title = "Warning",
            text = "Invalid probe combination: Same target ID cannot exist in the same channel",
            type = "warning"
          )
          return()
        }
        
        # If valid, update the probe data
        tryCatch({
          # Reset all selectors to FALSE
          updated_probe_data$selector <- FALSE
          ddes_import_probe <<- updated_probe_data
          
          # Update views
          ddes_import_probe_table(ddes_import_probe)
          output$ddes_probe2 <- renderRHandsontable({
            rhandsontable(ddes_import_probe, editable = TRUE, selectCallback = TRUE,
                          overflow = "visible") %>%
              hot_col("channel_ID", type = "dropdown", source = channel_id, strict = TRUE, allowInvalid = FALSE) %>%
              hot_col(col = 'selector', type = 'checkbox')
          })
          
        showNotification("Probe information saved successfully", type = "message", duration = 5)
        }, error = function(e) {
          shinyalert(
            title = "Error",
            text = paste("Error updating probe data:", e$message),
            type = "error"
          )
        })
      })
     
      # Add Probe when clicked
      observeEvent(input$add_probe,{
        
        # Test whether probe table exists
        if(exists("ddes_import_probe") == TRUE){
          
          if(dim(ddes_import_probe)[1]>0){
            
            probe_name_tmp <- input$probe_name_tmp
            
            if(is.null(probe_name_tmp)){
            
            add_probe_line <- c("", "", "FALSE")} else {add_probe_line <- c(probe_name_tmp, "", "FALSE")}
            ddes_import_probe <<- rbind(ddes_import_probe, add_probe_line)
            rownames(ddes_import_probe) <- 1:dim(ddes_import_probe)[1]
            
            # Check if probe table changes and update static+editable view
            ddes_import_probe_table <- reactiveVal(ddes_import_probe)
            output$ddes_probe <- renderTable(ddes_import_probe_table())
            
            # Editable table of probe info
            output$ddes_probe2 <- renderRHandsontable({
              
              rhandsontable(ddes_import_probe_table(), editable = TRUE, selectCallback = TRUE,
                            overflow = "visible") %>%
                hot_col("channel_ID", type = "dropdown", source = channel_id, strict = TRUE, allowInvalid = FALSE)%>%
                hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                  "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
                ))
              
            })
            
          }
          
          
        } else {
          
          # Create ddes_import_probe object
          ddes_import_probe <- as.data.frame(matrix(ncol=3, nrow=1))
          ddes_import_probe[1,] <- c("","", "FALSE")
          colnames(ddes_import_probe) <- c("target_ID", "channel_ID", "selector")
          ddes_import_probe <<- ddes_import_probe
          
          # Check if probe table changes and update static+editable view
          ddes_import_probe_table <- reactiveVal(ddes_import_probe)
          output$ddes_probe <- renderTable(ddes_import_probe_table())
          output$ddes_probe2 <- renderRHandsontable({
            
            rhandsontable(ddes_import_probe_table(), editable = TRUE, selectCallback = TRUE,
                          overflow = "visible" ) %>%
              hot_col("channel_ID", type = "dropdown", source = channel_id, strict = TRUE, allowInvalid = FALSE)%>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
          })
          
            showNotification("Probe information initialized", type = "message", duration = 3)
          
        }
        
        # Clear the input field
        updateTextInput(session, "probe_name_tmp", value = "")
        
      })
      
      # Delete Probe(s) based on selector
      observeEvent(input$delete_probe,{
        
        probe_in_use <- 0

          # Retrieve assay edit info table to see selection
          ddes_import_probe_tmp <<- hot_to_r(input$ddes_probe2)

          for(i in 1:dim(ddes_import_probe_tmp)[1]){
            
            if(ddes_import_probe_tmp[i,]$selector == TRUE){
              
              ddes_probe_to_delete <- paste(ddes_import_probe_tmp[i,]$target_ID,ddes_import_probe_tmp[i,]$channel_ID,sep=".")
              
              if(ddes_probe_to_delete=="."){
                
                ddes_import_probe <<- ddes_import_probe[-i,] 
                
              } else {
              
                if(any(str_detect(ddes_import_assay$probe_combination, ddes_probe_to_delete))==TRUE){
              
                  probe_in_use <- 1
                
                
                  }  else {
                
                      ddes_import_probe <<- ddes_import_probe[-i,] 
                
                      }
                  }
              }
          }

          if(probe_in_use == 1){
            
            shinyalert(
              title = "Warning",
              text = "Warning: Probe(s) detected that are in use in an assay (see assay table).
              Probes that are not in use were deleted if these were selected.",
              type = "warning"
            )
          }
          
          
          # To update probe/assay for viewing
          #ddes_import_probe <<- ddes_probe_info_delete[ddes_probe_info_delete$selector!=TRUE,]
          
          # If all probes are deleted, retain one empty probe
          if(dim(ddes_import_probe)[1]==0){
            
            ddes_import_probe[1,] <- c("","", "FALSE")
            
          showNotification("Add at least one probe with a target and channel identifier", type = "message", duration = 3)
            
          }
          
          rownames(ddes_import_probe) <- 1:dim(ddes_import_probe)[1]
          
          ddes_import_probe <<- ddes_import_probe
          
          # Static view of changes
          ddes_import_probe_table <- reactiveVal(ddes_import_probe)
         
          # editable table
          output$ddes_probe2 <-  renderRHandsontable({
            
            rhandsontable(ddes_import_probe, editable = TRUE, selectCallback = TRUE,
                          overflow = "visible")%>%
              hot_col("channel_ID", type = "dropdown", source = channel_id, strict = TRUE, allowInvalid = FALSE)%>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
            
          })
       # }
          # set probe in use back to 0
          probe_in_use <<- 0
        
      })
      
      
      ## DDES ASSAYS
      # TO DO: Make probe column clickable to select probes (see concentrations)
      
      # Editable view of assay info
      output$ddes_assay2 <- renderRHandsontable({
        
        rhandsontable(ddes_import_assay, editable = TRUE, selectCallback = TRUE) %>%
          hot_col(col = 'probe_concentrations_nM', readOnly = TRUE) %>%
          hot_col(col = 'probe_combination', readOnly = TRUE) %>%
          hot_col(col = 'target_number', readOnly = TRUE) %>%
          hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
            "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
          ))
        
      })
      
      # Update assay info when edited
      observeEvent(input$update_ddes_assay,{
        
        # Retrieve updates to assay table
        ddes_import_assay_tmp <<- hot_to_r(input$ddes_assay2)
        assay_names_old <<- ddes_import_assay$assay_ID
        assay_names <<- ddes_import_assay_tmp$assay_ID

        # Check for empty assay names
        
        if(any(trimws(ddes_import_assay_tmp$assay_ID) == "")){
          
          shinyalert(
            title = "Warning",
            text = "Please Check Assay Naming. There Seems To Be An Assay Name Missing Or Only Containing Spaces",
            type = "warning"
          )
          

        } else if(any(grepl(special_chars_3, ddes_import_assay_tmp$assay_ID))==FALSE){
          
          
          # Show alert
          shinyalert(
            title = "Warning",
            text = "Only letters, numbers, hyphens, spaces or underscores are allowed.",
            type = "warning"
          )
          
          # Go back to original data
          # Update Editable table of assay info for viewing
          output$ddes_assay2 <- renderRHandsontable({
            
            rhandsontable(ddes_import_assay, editable = TRUE, selectCallback = TRUE ) %>%
              hot_col(col = 'probe_concentrations_nM', readOnly = TRUE) %>%
              hot_col(col = 'probe_combination', readOnly = TRUE) %>%
              hot_col(col = 'target_number', readOnly = TRUE) %>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
          })         
       
          
          } else {
          
          # Update assay table
          ddes_import_assay <<- ddes_import_assay_tmp
          
          # Update assay for viewing
          ddes_import_assay_table(hot_to_r(input$ddes_assay2))
          
          # Update assay static view
          output$ddes_assay <- renderTable(ddes_import_assay[,-5])
          
          # Update assay choices for selector in samples annotation
          updateSelectInput(session, 'assay_well_selector', choices = unique(assay_names))
          
          # Prepare assay matching object for updating intensity headers
          assaynamesmatching <- as.data.frame(cbind(assay_names_old,assay_names))
          colnames(assaynamesmatching) <- c("assayOld","assayNew")
          assaynamesmatching$assayNew <- paste("#assay_ID",assaynamesmatching$assayNew,sep="\t")
          assaynamesmatching$assayOld <- paste("#assay_ID",assaynamesmatching$assayOld,sep="\t")
          assaynamesmatching <<- assaynamesmatching
          
          # Update intensity file headers
          for(i in 1:length(intensity_files_header_list)){
            
            intensity_files_header_list[[i]][8] <- ifelse(is.na(assaynamesmatching$assayNew[
              match(intensity_files_header_list[[i]][8] , assaynamesmatching$assayOld)]),
              intensity_files_header_list[[i]][8],assaynamesmatching$assayNew[
                match(intensity_files_header_list[[i]][8] , assaynamesmatching$assayOld)])
            
          }
          
          intensity_files_header_list <<- intensity_files_header_list     
          
          # Prepare assay name matching for main table
          assaynamesmatching_main <- assaynamesmatching
          assaynamesmatching_main$assayOld <- gsub(assaynamesmatching_main$assayOld,pattern="#assay_ID\t",replacement = "")
          assaynamesmatching_main$assayNew <- gsub(assaynamesmatching_main$assayNew,pattern="#assay_ID\t",replacement = "")
          
          # Update main table and make available
          ddes_import_main$assay_ID<- ifelse(is.na(assaynamesmatching_main$assayNew[
            match(ddes_import_main$assay_ID , assaynamesmatching_main$assayOld)]),
            ddes_import_main$assay_ID,assaynamesmatching_main$assayNew[
              match(ddes_import_main$assay_ID , assaynamesmatching_main$assayOld)])
          
          ddes_import_main <<- ddes_import_main
          
          # Update main table view
          ddes_import_main_table <- reactiveVal(ddes_import_main)
          output$ddes_main <- renderTable(ddes_import_main_table())
          
          # Update sample annotation table
          ddes_import_sample_annotation <- unique(ddes_import_main[,c("well_ID","assay_ID","sample_ID")])
          ddes_import_sample_annotation$selector <- "FALSE"
          rownames(ddes_import_sample_annotation) <- 1:dim(ddes_import_sample_annotation)[1]
          
          ddes_import_sample_annotation <<- ddes_import_sample_annotation
          
          ddes_import_sample_table <- reactiveVal(ddes_import_sample_annotation)
          
          output$ddes_sample_annotation <- renderRHandsontable({
            
            rhandsontable(ddes_import_sample_annotation, editable = TRUE, selectCallback = TRUE) %>%
              hot_col(col = 'well_ID', readOnly = TRUE) %>%
              hot_col(col = 'assay_ID', readOnly = TRUE) %>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
          })
          
        showNotification("Assay information saved successfully", type = "message", duration = 3)
        }
        
      })
      
      # Add assay when clicked
      observeEvent(input$add_assay,{
        
        # Retrieve assay edit info table to see selection
        assay_probes_selected <- hot_to_r(input$ddes_probe2)
        assay_probes_selected <- assay_probes_selected[assay_probes_selected$selector==TRUE,]
        
        # Prepare Info for assay table
        assay_probes_selected_short <<- paste(paste(assay_probes_selected$target_ID,
                                                    assay_probes_selected$channel_ID, sep="."),
                                              collapse = ";")
        

        # Summarize the probes selected for the new assay.
        target_number_tmp <- length(unique(assay_probes_selected$target_ID))
        probe_concentrations_nM_tmp <- paste(rep(NA,length(assay_probes_selected$target_ID)), collapse = ";")
        assay_name_tmp <<- input$assay_name_tmp

        if (nrow(assay_probes_selected) == 0) {

          shinyalert(
            title = "No probes selected",
            text = "Select at least one probe before adding an assay.",
            type = "warning"
          )

        } else if (trimws(assay_name_tmp) == "" |
                   grepl(special_chars_3, assay_name_tmp) == FALSE) {
          
          shinyalert(
            title = "Warning",
            text = "Warning: Please Provide (Valid) Assay Name. Only letters, numbers, hyphens or underscores are allowed.",
            type = "warning"
          )
          ddes_import_probe$selector <- FALSE
          
          # Reset the Probe table
          ddes_import_probe_table <- reactiveVal(ddes_import_probe)
          output$ddes_probe2 <-  renderRHandsontable({
            
            rhandsontable(ddes_import_probe, editable = TRUE, selectCallback = TRUE,
                          overflow = "visible")%>%
              hot_col("channel_ID", type = "dropdown", source = channel_id, strict = TRUE, allowInvalid = FALSE)%>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
            
          })
            
        } else if(any(duplicated(c(assay_name_tmp, ddes_import_assay$assay_ID)))==TRUE){
          
          shinyalert(
            title = "Warning",
            text = "Warning: Assay Name already exists",
            type = "warning"
          )
          
          
        } else {
          
          # Test whether assay table exists and add new assay
          if(exists("ddes_import_assay") == TRUE){
            
            add_assay_line <- c(assay_name_tmp,assay_probes_selected_short,
                                probe_concentrations_nM_tmp,target_number_tmp,"FALSE")
            ddes_import_assay <<- rbind(ddes_import_assay, add_assay_line)
            rownames(ddes_import_assay) <- 1:dim(ddes_import_assay)[1]
            colnames(ddes_import_assay) <-  c("assay_ID", "probe_combination","probe_concentrations_nM",
                                              "target_number","selector")
            
          } else {
            
            # Create ddes_import_assay object
            ddes_import_assay <- as.data.frame(matrix(ncol=5))
            colnames(ddes_import_assay) <- c("assay_ID", "probe_combination","probe_concentrations_nM",
                                             "target_number","selector")
            add_assay_line <- c(assay_name_tmp,assay_probes_selected_short,
                                probe_concentrations_nM_tmp,target_number_tmp,"FALSE")
            ddes_import_assay <- rbind(ddes_import_assay, add_assay_line)
            rownames(ddes_import_assay) <- 1:dim(ddes_import_assay)[1]
            colnames(ddes_import_assay) <-  c("assay_ID", "probe_combination","probe_concentrations_nM",
                                              "target_number","selector")
            
            
          showNotification("Assay information initialized",
                             type = "message", duration = 3)
            
          }
          
          # Update assay static view
          output$ddes_assay <- renderTable(ddes_import_assay[,-5])
          
          # Update Editable table of assay info for viewing
          output$ddes_assay2 <- renderRHandsontable({
            
            rhandsontable(ddes_import_assay, editable = TRUE, selectCallback = TRUE ) %>%
              hot_col(col = 'probe_concentrations_nM', readOnly = TRUE) %>%
              hot_col(col = 'probe_combination', readOnly = TRUE) %>%
              hot_col(col = 'target_number', readOnly = TRUE) %>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
          })
          
          # Reset the Probe table
          ddes_import_probe_table <- reactiveVal(ddes_import_probe)
          output$ddes_probe2 <-  renderRHandsontable({
            
            rhandsontable(ddes_import_probe, editable = TRUE, selectCallback = TRUE,
                          overflow = "visible")%>%
              hot_col("channel_ID", type = "dropdown", source = channel_id, strict = TRUE, allowInvalid = FALSE)%>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
            
          })
          
          # Reset the assay textinput
          updateTextInput(session, "assay_name_tmp", value = "")
          
          # Make asssay table and assay names available
          ddes_import_assay <<- ddes_import_assay
          assay_names <<- ddes_import_assay$assay_ID
          
          # Update the assay selection for sample annotation
          updateSelectInput(session, 'assay_well_selector', choices = unique(assay_names))
          
          # To do: check whether assay names exist (uniqueness) and other error catching
          
        }
        
      })
      
      # Delete Assay(s) based on selector
      observeEvent(input$delete_assay,{
        
        # Retrieve assay edit info table to see selection
        ddes_assay_info_delete <- hot_to_r(input$ddes_assay2)   
        
        # Retrieve assay names to be deleted
        assays_to_delete <<- ddes_assay_info_delete[ddes_assay_info_delete$selector==TRUE,]$assay_ID
        assay_in_use <<- 0
        
        # start the if with check for existing name, include resorting back to initial assay table
        if(length(assays_to_delete)==0){
          
          # Show alert
          shinyalert(
            title = "Warning",
            text = "No assay selected to delete.",
            type = "warning"
          )
          
          } else {
            
              for(i in 1:length(assays_to_delete)){
               
                if(grepl(assays_to_delete[i], unique(ddes_import_main$assay_ID))==FALSE){
                
                  # Update assay table based on selection for deleting
                  ddes_import_assay <- ddes_assay_info_delete[ddes_assay_info_delete[["assay_ID"]] != assays_to_delete[i], ]
                  
                } else { assay_in_use <- assay_in_use + 1}
                
              }
            
            if(assay_in_use >= 1){
          
          
          shinyalert(
            title = "Warning",
            text = "Warning: Assay(s) detected that are in use.
              Assays that are not in use were deleted.",
            type = "warning"
          )
          assay_in_use <- 0
          ddes_import_assay$selector <- FALSE
        }
            
        # Only allocate row numbers if there is at least one assay
        if(dim(ddes_import_assay)[1]>0){
          
          rownames(ddes_import_assay) <- 1:dim(ddes_import_assay)[1]
          
        } 
        
        # Make assay table available
        ddes_import_assay <<- ddes_import_assay
        
        # Update assay static view
        output$ddes_assay <- renderTable(ddes_import_assay[,-5])
        
        # Update editable table viewing
        output$ddes_assay2 <-  renderRHandsontable({
          
          rhandsontable(ddes_import_assay, editable = TRUE, selectCallback = TRUE)%>%
            hot_col(col = 'probe_concentrations_nM', readOnly = TRUE) %>%
            hot_col(col = 'probe_combination', readOnly = TRUE) %>%
            hot_col(col = 'target_number', readOnly = TRUE) %>%
            hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
              "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
            ))
          
          
        })
        
        }
      })
      
      # Make probe concentrations editable via pop-u
      # TO DO: if probes are not selected, give warning to select probes first
      observeEvent(input$ddes_assay2_select, {
        
        selected <<- input$ddes_assay2_select
        ddes_import_assay_tmp <<- hot_to_r(input$ddes_assay2)
        
        if (!is.null(selected) && selected$select$c == 3) {  # Check if the selected column is Column3
          
          # Add check for prior probe selection
          
          #introduce warning if there are no probes
          #is.null(ddes_import_assay_tmp[selected$select$r,]$probe_combination)
          if(ddes_import_assay_tmp[selected$select$r,]$probe_combination==""){
            
            shinyalert(
              title = "Warning",
              text = "Warning: There are no probes. Please select first",
              type = "warning"
            )
            
            
          } else {
          
          text <- ddes_import_assay_tmp[selected$select$r, selected$select$c]
          numInputs <<- length(strsplit(text, ";")[[1]])
          nameInputs <<- strsplit(ddes_import_assay_tmp[selected$select$r,2], split = ";")[[1]]
            
          showModal(modalDialog(
            title = "Input required",
            lapply(1:numInputs, function(i) {
              textInput(inputId = paste0("input", i), label = nameInputs[i])
            }),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("ok_concentrations", "OK")
            )
          ))
        }
        }
  
        
      })

      observeEvent(input$ok_concentrations, {
        inputs <<- sapply(1:numInputs, function(i) {
          input[[paste0("input", i)]]
        })
        
        if (all(grepl("^[0-9]+$", inputs)==TRUE)) {
        newData <- ddes_import_assay_tmp
        newData[selected$select$r, selected$select$c] <- paste(inputs, collapse = ";")
        ddes_import_assay_tmp <<- newData
        removeModal()
        
        ddes_import_assay <<- ddes_import_assay_tmp
        output$ddes_assay2 <-  renderRHandsontable({
          
          rhandsontable(ddes_import_assay, editable = TRUE, selectCallback = TRUE)%>%
            hot_col(col = 'probe_concentrations_nM', readOnly = TRUE) %>%
            hot_col(col = 'probe_combination', readOnly = TRUE) %>%
            hot_col(col = 'target_number', readOnly = TRUE) %>%
            hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
              "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
            ))
          
          
        })
        
        } else {
          
          shinyalert("Error", "Please enter numeric values and fill in all concentrations", type = "error")
        }
      })
      
      # Make probe selection editable

      observeEvent(input$ddes_assay2_select, {
        
        selected_probes <<- input$ddes_assay2_select
        ddes_import_assay_tmp <<- hot_to_r(input$ddes_assay2)
        assay_tmp_name <- ddes_import_assay_tmp[selected_probes$select$r,]$assay_ID
        
        if (!is.null(selected_probes) && selected_probes$select$c == 2) {  # Check if the selected column is Column2
          
          #introduce warning if assay is in use
          if(grepl(assay_tmp_name, unique(ddes_import_main$assay_ID))==TRUE){
            
            shinyalert(
              title = "Warning",
              text = "Warning: This assay is in use (Sample annotation) and cannot be changed.",
              type = "warning"
            )
            
          
          } else {
          
          text_probes <<- paste0(ddes_import_probe$target_ID,".",ddes_import_probe$channel_ID, collapse=";")
          numInputs_probes <<- length(strsplit(text_probes, ";")[[1]])
          nameInputs_probes <<- strsplit(text_probes, split = ";")[[1]]
          
          
          showModal(modalDialog(
            title = "Input required",
            lapply(1:numInputs_probes, function(i) {
              checkboxInput(inputId = paste0("checkbox", i), label = nameInputs_probes[i])
            }),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("ok_probe_selection", "OK")
            )
          ))
          }
        }
        
        
        
      })
      
      observeEvent(input$ok_probe_selection, {
        
        selected_probes_inputs <<- sapply(1:numInputs_probes, function(i) {
          if (input[[paste0("checkbox", i)]]) {
            nameInputs_probes[i]
          }
        })
        selected_probes_inputs <<- selected_probes_inputs[!is.na(selected_probes_inputs)]
        
        # Change probe information for that row
        newData_probes <- ddes_import_assay_tmp
        
        probe_names_new <<- paste(unlist(selected_probes_inputs), collapse = ";")
        probe_names_fortarget <<- strsplit(probe_names_new, split=";")
        
        targets <<- unique(do.call(rbind, strsplit(probe_names_fortarget[[1]], split= "\\."))[,1])
        target_number_tmp <<- length(targets)
        probe_concentrations_nM_tmp <<- paste(rep(NA,length(probe_names_fortarget[[1]])), collapse = ";")
        
        newData_probes[selected_probes$select$r, 4] <- target_number_tmp
        newData_probes[selected_probes$select$r, selected_probes$select$c] <- probe_names_new
        newData_probes[selected_probes$select$r, 3] <- probe_concentrations_nM_tmp
      
        ddes_import_assay_tmp <<- newData_probes
        removeModal()
        
        
        ddes_import_assay <<- ddes_import_assay_tmp
        output$ddes_assay2 <-  renderRHandsontable({
          
          rhandsontable(ddes_import_assay, editable = TRUE, selectCallback = TRUE)%>%
            hot_col(col = 'probe_concentrations_nM', readOnly = TRUE) %>%
            hot_col(col = 'probe_combination', readOnly = TRUE) %>%
            hot_col(col = 'target_number', readOnly = TRUE) %>%
            hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
              "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
            ))
          
          
        })
      })
      
      ### START OF TAB 3 SAMPLE/WELL ANNOTATION
      
      ## DDES SAMPLE/WELL ANNOTATION
      ## Sample Select + Assay Select or vice versa
      
      # Dynamic view of samples info (construct from ddes_import_main)
      ddes_import_sample_table <- reactiveVal(ddes_import_sample_annotation)
      output$ddes_sample_annotation <- renderRHandsontable({
        
        rhandsontable(ddes_import_sample_annotation, editable = TRUE, selectCallback = TRUE) %>%
          hot_col(col = 'well_ID', readOnly = TRUE) %>%
          hot_col(col = 'assay_ID', readOnly = TRUE) %>%
          hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
            "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
          ))
        
      })
      
      # Update sample annotation with assay naming
      observeEvent(input$apply_assay_to_well,{
        
        # Retrieve assay name for updating
        assayNameUpdate <- input$assay_well_selector
        
        # Retrieve sample edit info table to see selection
        sampleAnnotation <- hot_to_r(input$ddes_sample_annotation)
        selectedRows <- which(sampleAnnotation$selector %in% TRUE)

        if (length(selectedRows) == 0) {

          shinyalert(
            title = "No wells selected",
            text = "Select at least one well before applying an assay.",
            type = "warning"
          )

          return()
        }

        sampleAnnotation$assay_ID[selectedRows] <- assayNameUpdate
        
        # Update intensity header info with new assay name
        wellsToChange <- sampleAnnotation$well_ID[selectedRows]
        wellPattern <- paste(wellsToChange, collapse = "|")
        
        updateIntensityHeaders <- function(inputList) {
          
          for (name in names(inputList)) {
            if (grepl(wellPattern, name)) {
              
              inputList[[name]][8] <- paste0("#assay_ID\t", assayNameUpdate)
              
            }
          }
          return(inputList)
        }
        intensity_files_header_list <<- updateIntensityHeaders(intensity_files_header_list)
        
        # Update main table with assay and targets change
        for (i in wellsToChange) {
          
          # Retrieve the sample identifier and combined well-sample name.
          sampleId <- sampleAnnotation[sampleAnnotation$well_ID == i,]$sample_ID
          wellSample <- paste(i, sampleId, sep = "_")
          
          # Retrieve the selected assay name.
          assayName <- sampleAnnotation[sampleAnnotation$well_ID == i,]$assay_ID
          
          # Retrieve the targets to add.
          targetsToAdd <- ddes_import_assay[
            ddes_import_assay$assay_ID == assayName,
          ]$probe_combination
          
          if (length(targetsToAdd) > 0) {
            
            targetsToAdd <- strsplit(targetsToAdd, split = ";")
            targetsToAdd <- unique(
              do.call(rbind, strsplit(targetsToAdd[[1]], split = "\\."))[,1]
            )
            
          }
          
          # Remove the rows for the well that is being changed.
          ddes_import_main <- ddes_import_main[-which(ddes_import_main$well_ID %in% i),]
          
          for (j in targetsToAdd) {
            
            addTargetLine <- c(
              i,
              sampleId,
              assayName,
              j,
              "NA",
              "NA",
              "NA",
              "NA",
              wellSample
            )
            
            ddes_import_main <- rbind(ddes_import_main, addTargetLine)
            rownames(ddes_import_main) <- 1:dim(ddes_import_main)[1]
            colnames(ddes_import_main) <-  c("well_ID","sample_ID","assay_ID", "target_ID","counts_positive",
                                             "counts_negative","concentration_reaction_cp_µL","threshold_if_threshold","well_samplename")
          }
          
        }
        ddes_import_main <- ddes_import_main[order(ddes_import_main$well_ID),]
        
        # update main table and viewing
        ddes_import_main <<- ddes_import_main
        ddes_import_main_table <- reactiveVal(ddes_import_main)
        output$ddes_main <- renderTable(ddes_import_main_table())
        
        # update sample annotation object and viewing
        sampleAnnotation$selector <- FALSE
        ddes_import_sample_annotation <<- sampleAnnotation
        output$ddes_sample_annotation <- renderRHandsontable({
          
          rhandsontable(ddes_import_sample_annotation, editable = TRUE, selectCallback = TRUE) %>%
            hot_col(col = 'well_ID', readOnly = TRUE) %>%
            hot_col(col = 'assay_ID', readOnly = TRUE) %>%
            hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
              "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
            ))
          
        })
        
        # Make sample annotation available
        ddes_import_sample_annotation <<- ddes_import_sample_annotation
        
        showNotification("Sample information saved successfully", type = "message", duration = 3)
        
      })
      
      # Update sample IDs in main when changed in sample annotation
      observeEvent(input$update_sample_id,{
   
        # Retrieve sample edit info table to see sample ID changes
        ddes_import_sample_annotation_tmp <<- hot_to_r(input$ddes_sample_annotation)
        
        # check for name integrity
        if(any(grepl(special_chars_3, ddes_import_sample_annotation_tmp$sample_ID))==FALSE){
      
        # Show alert
        shinyalert(
          title = "Warning",
          text = "Only letters, numbers, hyphens, dots, spaces or underscores are allowed.",
          type = "warning"
        )
        
        # Go back to original data
        # Update Editable table of assay info for viewing
          output$ddes_sample_annotation <- renderRHandsontable({
            
            rhandsontable(ddes_import_sample_annotation, editable = TRUE, selectCallback = TRUE) %>%
              hot_col(col = 'well_ID', readOnly = TRUE) %>%
              hot_col(col = 'assay_ID', readOnly = TRUE) %>%
              hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
                "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
              ))
            
          })
          
        } else {
        # Make new sampleID column
        occurences <- merge(ddes_import_sample_annotation_tmp, 
                            ddes_import_assay[,c("assay_ID","target_number")],
                            by = "assay_ID")
        
        sampleids_new <- rep(occurences$sample_ID, occurences$target_number)
        ddes_import_main$sample_ID <- sampleids_new
        
        # update main table and viewing
        ddes_import_main$well_samplename <- paste0(ddes_import_main$well_ID,"_",ddes_import_main$sample_ID)
        ddes_import_main <<- ddes_import_main
        ddes_import_main_table <- reactiveVal(ddes_import_main)
        output$ddes_main <- renderTable(ddes_import_main_table())
        
        # update sample annotation object and viewing
        ddes_import_sample_annotation <<- ddes_import_sample_annotation_tmp
        output$ddes_sample_annotation <- renderRHandsontable({
          
          rhandsontable(ddes_import_sample_annotation, editable = TRUE, selectCallback = TRUE) %>%
            hot_col(col = 'well_ID', readOnly = TRUE) %>%
            hot_col(col = 'assay_ID', readOnly = TRUE) %>%
            hot_col(col = 'selector', type = 'checkbox', validator = htmlwidgets::JS(
              "function(value, callback) {
              if (value === null) {
                  callback(false);  // Prevent deletion by returning false
              } else {
                  callback(true);   // Allow check/uncheck
              }
             }"
            ))
          
        })
        
        # Make sample annotation available
        ddes_import_sample_annotation <<- ddes_import_sample_annotation
        
        # Update intensity file dropdown list
        output$sample_selector <- renderUI({
          
          selectInput('sample_well_selector', 
                      'Choose sample',
                      choices = unique(ddes_import_main$well_samplename)
          )
        })
        
        showNotification("Sample information saved successfully", type = "message", duration = 3)
        
        }
      })
      
      
      # To do: when only fluovalues, this needs main table = number of samples need to be extracted in global
      
      ### END OF TAB 3 SAMPLE/WELL ANNOTATION
      
      
      ### START OF TAB 4 MAIN FILE VIEWING
      
      ## Static view of ddes_main_head
      output$ddes_main_head <- renderTable({
        
        sapply(ddes_import_main_head_df[-c(1,10),], function(x) gsub("#", "", x))
        
      })
      
      ## Static view of main table
      ddes_import_main_table <- reactiveVal(ddes_import_main[,-9])
      output$ddes_main <- renderTable(ddes_import_main_table())
      
      ### END OF TAB 4
      
      
      ### START OF TAB 5 ASSAYS FILE VIEWING
      
      ## Static view of ddes_assays_head
      output$ddes_assay_head <- renderTable({
        
        sapply(ddes_import_assay_head_df[-c(1,7,8),], function(x) gsub("#", "", x))
      })
      
      ddes_import_assay_table <- reactiveVal(ddes_import_assay)
      output$ddes_assay <- renderTable(ddes_import_assay[,-c(4,5)])
      
      ### END OF TAB 5
      
      ### START OF TAB 6 INTENSITY FILES
      
      ## Intensity File coding here? Now outside of start_demo
      
      ### END OF TAB 6 INTENSITY FILES
      
    
      
      #})
      
      output$assay_selector <- renderUI({
        
        selectInput('assay_well_selector', 
                      'Choose assay',
                    choices = unique(assay_names)
        )
      })
      
      output$sample_selector <- renderUI({

        selectInput('sample_well_selector', 
                      'Choose sample',
                    choices = unique(ddes_import_main$well_samplename)
        )
      })
      
      ### Intensity file viewing
      # Show checks differently? Number channels?
      # necessary to view this?
      observeEvent(input$view_ddes_intensity_file,{
        
        intensity_tmp_name <<- input$sample_well_selector
        intensity_tmp_well <<- strsplit(intensity_tmp_name, split = "_")[[1]][1]
        
        if(input$dpcr_platform=="DDES"){
          intensity_tmp_filename <<- grep(ddes_intensity_files, pattern = intensity_tmp_well, value = TRUE)
          
          ddes_import_intensity_tmp <<- read.table(file = intensity_tmp_filename,
                                                   sep = "\t", stringsAsFactors = FALSE,
                                                   comment.char = "#", header = TRUE)
          
          ddes_import_intensity_head_tmp <<- readLines(con = intensity_tmp_filename,
                                                       n = 7)
        }
        if(input$dpcr_platform=="Nio"){
          
          
          intensity_tmp_filename <<- grep(names(intensity_files_data_list), pattern = intensity_tmp_well)
          
          ddes_import_intensity_tmp <<- intensity_files_data_list[[intensity_tmp_filename]]
          
          ddes_import_intensity_head_tmp <<- intensity_files_header_list[[intensity_tmp_filename]]
        }
        if(input$dpcr_platform=="QIAcuity"){

          intensity_tmp_filename <<- grep(names(intensity_files_data_list), pattern = intensity_tmp_well)
          
          ddes_import_intensity_tmp <<- intensity_files_data_list[[intensity_tmp_filename]]
          
          ddes_import_intensity_head_tmp <<- intensity_files_header_list[[intensity_tmp_filename]]
          
        }
        if(input$dpcr_platform=="QX200_Quantasoft"){
          
          intensity_tmp_filename <<- grep(names(intensity_files_data_list), pattern = intensity_tmp_well)
          
          ddes_import_intensity_tmp <<- intensity_files_data_list[[intensity_tmp_filename]]
          
         ddes_import_intensity_head_tmp <<- intensity_files_header_list[[intensity_tmp_filename]]
          
        }
        if(input$dpcr_platform=="Select"){
          
          intensity_tmp_filename <<- grep(names(intensity_files_data_list), pattern = intensity_tmp_well)
          
          ddes_import_intensity_tmp <<- intensity_files_data_list[[intensity_tmp_filename]]
          
          ddes_import_intensity_head_tmp <<- intensity_files_header_list[[intensity_tmp_filename]]
          
        }
        
        
        ddes_import_intensity_head_tmp_df <- data.frame(do.call(rbind,str_split(ddes_import_intensity_head_tmp, "\t", n = 2)))
        colnames(ddes_import_intensity_head_tmp_df) <- c("header","header_content")
        
        output$ddes_intensity_head <- renderTable({
          
          sapply(ddes_import_intensity_head_tmp_df[-c(1,9),], function(x) gsub("#", "", x))
          
        })
        output$ddes_intensity_data <- renderTable(
          head(as.data.frame(ddes_import_intensity_tmp), 10)
        )
        
      })
      
      ### TAB 7 DOWNLOAD
      ### Construct DDES download object if opted for direct download

      
    ### End of Shared processing
    }
    

  ## End of DDES start/start demo
  }) 

  
  ## 2. Downloading DDES
  
  observeEvent(input$validate_download, {
    if (!all(
      exists("ddes_import_main", envir = .GlobalEnv),
      exists("ddes_import_assay", envir = .GlobalEnv),
      exists("intensity_files_data_list", envir = .GlobalEnv),
      exists("intensity_files_header_list", envir = .GlobalEnv)
    )) {

      showModal(modalDialog(title = "Error",
                            "One or more required DDES objects are missing.",
                            easyClose = TRUE
      ))
    } else {
      hideFeedback("validate_download")
      showModal(modalDialog(
        title = "Download ready",
        "Your DDES data is ready to download.",
        downloadButton("downloadDDES", "Download archive", class = "btn-success"),
        easyClose = TRUE
      ))
    }
  })
  
  output$downloadDDES <- downloadHandler( 
    
    filename = function() { 
      
      paste0("DDES_", experimentid, "_", runid, "_", datetime, "_DDES_files.zip")
      
    },
    content = function(file) {
      
      withProgress(message = "Preparing DDES Files Download", value = 0,{
        
        # temp dir to store files for download
        files <- NULL;
        tmpdir <- tempdir()
        
        # DDES Intensity files
        for (i in 1:length(well_ids)) {
          print(i)
          print(files)
          if(i == 1){
            
            # DDES main file
            main_filename <- file.path(tmpdir,paste0("DDES_", experimentid, "_", runid, "_", datetime, "_main.tsv"))
            
            main_filenametmp <- paste0("DDES_", experimentid, "_", runid, "_", datetime, "_main.tsv")
            
            ddes_import_main_head[3] <- paste("#datetime", datetime, sep = "\t")
            
            writeLines(ddes_import_main_head, main_filename)
            
            write.table(ddes_import_main[,!names(ddes_import_main) %in% c("well_samplename")], file = main_filename, 
                        sep = "\t", quote = FALSE, append = TRUE, row.names = FALSE,
                        col.names = TRUE)
            
            files <- c(main_filenametmp, files)
            
            # DDES assay file
            assay_details_filename <- file.path(tmpdir,paste0("DDES_", experimentid, "_", runid, "_", datetime, "_assays.tsv"))
            
            assay_details_filenametmp <- paste0("DDES_", experimentid, "_", runid, "_", datetime, "_assays.tsv")
            ddes_import_assay_head[3] <- paste("#datetime", datetime, sep = "\t")
            
            writeLines(ddes_import_assay_head, assay_details_filename)
            
            write.table(ddes_import_assay[,1:3], file = assay_details_filename, 
                        sep = "\t", quote = FALSE, append = TRUE, row.names = FALSE,
                        col.names = TRUE)
            
            
            files <- c(assay_details_filenametmp, files)
            
            
          }
          
          
          
          names(intensity_files_header_list)[i] <- paste0("DDES_", experimentid, "_", runid,"_", datetime, 
                                                          "_intensity_", well_ids[i])
          
          intensity_files_header_list[[i]][3] <- paste("#datetime", datetime, sep = "\t")
          
          fileNameOut <- file.path(tmpdir,paste0(names(intensity_files_header_list)[i], ".tsv"))
          
          fileNameOuttmp <- paste0(names(intensity_files_header_list)[i], ".tsv")
          
          
          # Export DDES intensity files
          writeLines(intensity_files_header_list[[i]], fileNameOut)
          write.table(intensity_files_data_list[[i]], file = fileNameOut, 
                      sep = "\t", quote = FALSE, append = TRUE, row.names = FALSE,
                      col.names = TRUE)
          
          files <- c(fileNameOuttmp, files) # store written file name
          print(files)
        }
        
        # create archive from written files
        zip::zip(file, files, root = tmpdir)
      })
    },
    contentType = "application/zip"
    
  )

  ## End of Session
  
})



#EOS#
