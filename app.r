library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(reactable)
library(htmltools)
library(sodium)
library(rsconnect)
library(jsonlite)

# Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1Sjx9ETDXCIeTnuDJ_aUokqzt-mOfGyKg_HTljzunkOc/edit?gid=1726487847#gid=1726487847"

### use locally
 #gs4_auth(path = "service-account.json")

service_account <- tempfile(fileext = ".json")

 writeLines(
   Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON"),
   service_account
 )

 gs4_auth(path = service_account)

# Convert a column name to its spreadsheet letter, based on current df column order.
# Used so writes target the exact cell/row that changed instead of rewriting
# the whole sheet (avoids clobbering concurrent edits from other users).
col_letter <- function(df, colname) {
  idx <- which(names(df) == colname)
  if (length(idx) == 0) stop(paste("Column not found:", colname))
  LETTERS[idx] # works up to column Z; fine for this sheet's width
}

# Helper for password input with show/hide eye toggle
showablePasswordInput <- function(inputId, label, placeholder = "") {
  div(
    style = "position: relative; margin-bottom: 15px;",
    passwordInput(inputId, label, placeholder = placeholder),
    span(
      icon("eye"),
      style = "position: absolute; right: 12px; top: 32px; cursor: pointer; color: #666; font-size: 14px; padding: 4px; z-index: 10;",
      title = "Toggle password visibility",
      onclick = sprintf("
        var el = document.getElementById('%s');
        var icon = this.querySelector('i');
        if (el.type === 'password') {
          el.type = 'text';
          icon.className = 'fa fa-eye-slash';
        } else {
          el.type = 'password';
          icon.className = 'fa fa-eye';
        }
      ", inputId)
    )
  )
}

# ─── UI ───────────────────────────────────────────────────────────────────────
# We use a simple fluidPage shell; the login form or dashboard is rendered
# dynamically on the server side based on authentication state.
ui <- fluidPage(
  uiOutput("page_ui")
)

# ─── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # ── Auth state ─────────────────────────────────────────────────────────────
  rv <- reactiveValues(
    logged_in    = FALSE,
    current_user = NULL
  )

  # ── Fetch user/password data from PassCodes sheet ──────────────────────────
  users_df <- reactiveVal(NULL)

  observe({
    tryCatch(
      {
        df <- read_sheet(sheet_url, sheet = "PassCodes")
        # Ensure PasswordHash column exists
        if (!"PasswordHash" %in% names(df)) {
          df$PasswordHash <- NA_character_
        } else {
          df$PasswordHash <- as.character(df$PasswordHash)
          df$PasswordHash[!nzchar(df$PasswordHash)] <- NA_character_
        }
        users_df(df)
      },
      error = function(e) {
        showNotification(paste("Could not load PassCodes sheet:", e$message),
          type = "error", duration = 10
        )
      }
    )
  }) |> bindEvent(TRUE, once = TRUE) # run once at startup

  # ── Main page router ───────────────────────────────────────────────────────
  output$page_ui <- renderUI({
    if (!rv$logged_in) {
      # ── LOGIN PAGE ─────────────────────────────────────────────────────────
      login_ui()
    } else {
      # ── DASHBOARD ──────────────────────────────────────────────────────────
      dashboard_ui()
    }
  })

  # ── Login UI builder ───────────────────────────────────────────────────────
  login_ui <- function() {
    udf <- users_df()
    if (is.null(udf)) {
      return(div(
        style = "text-align:center; margin-top:100px;",
        h3("Loading users…"),
        icon("spinner", class = "fa-spin fa-2x")
      ))
    }

    fluidPage(
      div(
        style = "max-width: 400px; margin: 80px auto; padding: 30px;
                 border: 1px solid #ddd; border-radius: 8px;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
        h2("Wish List Login", style = "text-align:center; color:#605ca8;"),
        hr(),
        selectInput("login_user", "Select your name",
          choices = sort(unique(na.omit(udf$Name)))
        ),
        uiOutput("login_fields"),
        br(),
        uiOutput("login_message")
      )
    )
  }

  # ── Dynamic login fields: password-create vs. password-enter ───────────────
  output$login_fields <- renderUI({
    udf <- users_df()
    req(udf, input$login_user)

    row <- udf[udf$Name == input$login_user, ]
    has_password <- nrow(row) > 0 && !is.na(row$PasswordHash[1]) && nzchar(row$PasswordHash[1])

    if (has_password) {
      tagList(
        showablePasswordInput("pw", "Password"),
        actionButton("login_btn", "Log In",
          icon = icon("sign-in-alt"),
          class = "btn-primary btn-block",
          style = "width:100%;"
        )
      )
    } else {
      tagList(
        showablePasswordInput("new_pw", "Create Password"),
        showablePasswordInput("confirm_pw", "Confirm Password"),
        actionButton("create_btn", "Create Account",
          icon = icon("user-plus"),
          class = "btn-success btn-block",
          style = "width:100%;"
        )
      )
    }
  })

  # ── Create password ────────────────────────────────────────────────────────
  observeEvent(input$create_btn, {
    req(input$new_pw, input$confirm_pw)

    if (input$new_pw != input$confirm_pw) {
      output$login_message <- renderUI(
        div(class = "alert alert-danger", "Passwords do not match.")
      )
      return()
    }

    if (nchar(input$new_pw) < 4) {
      output$login_message <- renderUI(
        div(class = "alert alert-danger", "Password must be at least 4 characters.")
      )
      return()
    }

    hash <- password_store(input$new_pw)

    # Update local copy
    udf <- users_df()
    row_idx <- which(udf$Name == input$login_user)
    req(length(row_idx) == 1)
    udf$PasswordHash[row_idx] <- hash
    users_df(udf)

    # Persist only this user's PasswordHash cell to the Google Sheet, rather
    # than rewriting the whole PassCodes sheet — avoids wiping out another
    # user's password if they saved between when this session loaded and now.
    sheet_row <- row_idx + 1 # +1 for header row
    pw_col <- col_letter(udf, "PasswordHash")

    tryCatch(
      {
        range_write(
          ss = sheet_url,
          data = data.frame(x = hash, stringsAsFactors = FALSE),
          sheet = "PassCodes",
          range = paste0(pw_col, sheet_row),
          col_names = FALSE,
          reformat = FALSE
        )
        showNotification("Password created! You can now log in.", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error saving password:", e$message),
          type = "error", duration = 10
        )
      }
    )
  })

  # ── Log in ─────────────────────────────────────────────────────────────────
  observeEvent(input$login_btn, {
    req(input$pw, input$login_user)

    udf <- users_df()
    row <- udf[udf$Name == input$login_user, ]

    if (nrow(row) == 0 || is.na(row$PasswordHash[1])) {
      output$login_message <- renderUI(
        div(class = "alert alert-danger", "No account found. Please create a password first.")
      )
      return()
    }

    if (password_verify(row$PasswordHash[1], input$pw)) {
      rv$logged_in <- TRUE
      rv$current_user <- input$login_user
    } else {
      output$login_message <- renderUI(
        div(class = "alert alert-danger", "Incorrect password. Please try again.")
      )
    }
  })

  # ── DASHBOARD (rendered after login) ───────────────────────────────────────
  dashboard_ui <- function() {
    dashboardPage(
      skin = "purple",
      dashboardHeader(
        title = "Wish Lists",
        tags$li(
          class = "dropdown",
          style = "padding: 8px 12px; display: flex; align-items: center; gap: 8px;",
          span(paste("Logged in as:", rv$current_user), style = "color: white; font-weight: bold; margin-right: 4px;"),
          actionButton("info_btn", "Info",
            icon = icon("info-circle"),
            class = "btn-sm btn-info"
          ),
          actionButton("refresh", "Refresh Data",
            icon = icon("sync"),
            class = "btn-sm btn-primary"
          ),
          actionButton("logout_btn", "Log Out",
            icon = icon("sign-out-alt"),
            class = "btn-sm btn-default"
          )
        )
      ),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        tags$head(
          tags$style(HTML("
            .nav-pills > li.active > a, 
            .nav-pills > li.active > a:focus, 
            .nav-pills > li.active > a:hover {
              background-color: #605ca8 !important;
              color: #ffffff !important;
              font-weight: bold;
            }
            .nav-pills > li > a {
              font-size: 15px;
              font-weight: 600;
              border-radius: 6px;
              padding: 8px 18px;
              color: #444;
              background-color: #e4e4e4;
              margin-right: 8px;
              margin-bottom: 8px;
            }
            .nav-pills > li > a:hover {
              background-color: #d6d6d6;
            }
          "))
        ),
        tabsetPanel(
          id = "top_tabs",
          type = "pills",
          # Tab 1: My Wish List
          tabPanel(
            title = tagList(icon("user"), "My Wish List"),
            value = "my_list",
            br(),
            fluidRow(
              valueBoxOutput("my_name_box", width = 6),
              valueBoxOutput("my_item_count", width = 6)
            ),
            fluidRow(
              box(
                title = "Add New Wish List Item",
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                fluidRow(
  column(4,
                    selectizeInput(
                      "add_category",
                      "Category",
                      choices  = c(
                        "" ,
                        "Accessory", "Activity", "Beauty Product", "Car",
                        "Clothes", "Decorations", "Food", "Games", "Gardening",
                        "Gift Card", "Household Item", "Luggage", "Miscellaneous",
                        "Shoes", "Stocking", "Tools", "Toy", "Treats", "Vinyl"
                      ),
                      selected = "",
                      options  = list(
                        placeholder = "Select or type a category...",
                        create      = TRUE
                      )
                    )
                  ),
                  column(5, textInput("add_item", "Item Name*", placeholder = "e.g., Sweater, Shoes")),
                  column(3, textInput("add_size", "Size / Options", placeholder = "e.g., Medium, Size 8")),
                  column(12, textInput("add_link", "URL / Product Link", placeholder = "e.g., https://..."))
              
                ),
                div(
                  style = "text-align: right;",
                  actionButton("submit_item", "Add Item", icon = icon("plus"), class = "btn-success")
                )
              )
            ),
            fluidRow(
              box(
                title = "My Wish List Items",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("my_wishlist_table"),
                hr(),
                div(
                  style = "text-align: right;",
                  actionButton("save_my_edits", "Save Changes", icon = icon("save"), class = "btn-success btn-lg")
                )
              )
            )
          ),

          # Tab 2: Others Wish List
          tabPanel(
            title = tagList(icon("users"), "Other's Wish Lists"),
            value = "others_list",
            br(),
            fluidRow(
              box(
                title = "Filter by Person",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                selectInput("selected_other", "Select Person:", choices = NULL)
              )
            ),
            fluidRow(
              valueBoxOutput("other_item_count", width = 6),
              valueBoxOutput("other_bought_count", width = 6)
            ),
            fluidRow(
              box(
                title = "Add Item to Their Wish List",
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                width = 12,
                fluidRow(
                      column(4,
                    selectizeInput(
                      "add_other_category",
                      "Category",
                      choices  = c(
                        "",
                        "Accessory", "Activity", "Beauty Product", "Car",
                        "Clothes", "Decorations", "Food", "Games", "Gardening",
                        "Gift Card", "Household Item", "Luggage", "Miscellaneous",
                        "Shoes", "Stocking", "Tools", "Toy", "Treats", "Vinyl"
                      ),
                      selected = "",
                      options  = list(
                        placeholder = "Select or type a category...",
                        create      = TRUE
                      )
                    )
                  ),
                  column(5, textInput("add_other_item", "Item Name*", placeholder = "e.g., Sweater, Shoes")),
                  column(3, textInput("add_other_size", "Size / Options", placeholder = "e.g., Medium, Size 8")),
                  column(12, textInput("add_other_link", "URL / Product Link", placeholder = "e.g., https://..."))
                ),
                div(
                  style = "text-align: right;",
                  uiOutput("submit_other_item_btn_ui")
                )
              )
            ),
            fluidRow(
              box(
                title = "Wish List Items & Purchase Status",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                reactableOutput("others_wishlist_table"),
                hr(),
                div(
                  style = "text-align: right;",
                  actionButton("save_purchases", "Save Updates", icon = icon("save"), class = "btn-success btn-lg")
                )
              )
            )
          )
        )
      )
    )
  }

  # ── Info Modal Popup ───────────────────────────────────────────────────────
  observeEvent(input$info_btn, {
    showModal(
      modalDialog(
        title = tagList(icon("info-circle", class = "text-info"), " How Wish Lists Work"),
        div(
          style = "line-height: 1.6; font-size: 14px;",
          h4(tagList(icon("user"), " My Wish List Tab"), style = "margin-top: 0; color: #605ca8; font-weight: bold;"),
          tags$ul(
            tags$li(tags$b("Your Editable Wish List: "), "This is your personal wish list. Any items and edits you add here will be seen by everyone else."),
            tags$li(tags$b("Protected Items: "), "No one else can edit or delete your pre-existing wish list items.")
          ),
          hr(),
          h4(tagList(icon("users"), " Other's Wish Lists Tab"), style = "color: #00c0ef; font-weight: bold;"),
          tags$ul(
            tags$li(tags$b("Browse Wish Lists: "), "Select any individual from the dropdown menu to view their wish list items."),
            tags$li(tags$b("Add Secret Items: "), "You can add items to someone else's list! Items added by others remain hidden from that person, so you can share ideas or coordinate gifts with everyone else while keeping it a surprise."),
            tags$li(tags$b("Group Transparency: "), "Mark items as bought when you purchase them to make gift buying easier and prevent duplicate purchases.")
          ),
          div(
            class = "alert alert-danger",
            style = "margin-top: 15px; margin-bottom: 0; padding: 10px 15px;",
            tagList(
              icon("exclamation-triangle"),
              tags$b(" Important Rule: "),
              "Once an item is marked as bought, it turns ",
              tags$b("red"),
              ". ",
              tags$b("DO NOT buy items highlighted in red!")
            )
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

  # ── Log out ────────────────────────────────────────────────────────────────
  observeEvent(input$logout_btn, {
    rv$logged_in <- FALSE
    rv$current_user <- NULL
  })

  # ── WISH LIST DATA ─────────────────────────────────────────────────────────
  # Main in-memory reactive data frame for wish list items
  local_df <- reactiveVal(NULL)

  # Function to fetch latest data from Google Sheet
  fetch_sheet_data <- function() {
    df <- read_sheet(sheet_url, sheet = "WishLists")

    # Ensure 'Bought' column exists and defaults to "No"
    if (!"Bought" %in% names(df)) {
      df$Bought <- "No"
    } else {
      df$Bought <- ifelse(is.na(df$Bought) | !nzchar(as.character(df$Bought)), "No", as.character(df$Bought))
    }

    # Ensure 'Who Bought' column exists
    if (!"Who Bought" %in% names(df)) {
      df$`Who Bought` <- NA_character_
    } else {
      df$`Who Bought` <- ifelse(is.na(df$`Who Bought`) | !nzchar(as.character(df$`Who Bought`)), NA_character_, as.character(df$`Who Bought`))
    }

    # Ensure 'Entered By' column exists
    if (!"Entered By" %in% names(df)) {
      df$`Entered By` <- NA_character_
    } else {
      df$`Entered By` <- ifelse(is.na(df$`Entered By`) | !nzchar(as.character(df$`Entered By`)), NA_character_, as.character(df$`Entered By`))
    }

    # Ensure 'Category' column exists
    if (!"Category" %in% names(df)) {
      df$Category <- NA_character_
    } else {
      df$Category <- ifelse(is.na(df$Category) | !nzchar(trimws(as.character(df$Category))), NA_character_, as.character(df$Category))
    }

    df
  }

  # Load data on login and on manual refresh
  observe({
    req(rv$logged_in)
    input$refresh
    data <- fetch_sheet_data()
    local_df(data)
  })

  # Get list of unique non-NA names
  available_names <- reactive({
    df <- local_df()
    req(df)
    unique(na.omit(df$Name))
  })

  # Define group memberships
  group_1_members <- c(
    "Hannah", "Jacob", "Sinatra/Versailles",
    "Sara", "Charlie", "Emma", "Michael", "Ethan", "Violet", "Pepper", "Mark",
    "Dixie", "Wrenley", "Dixie/Wrenley", "Dixie / Wrenley"
  )

  group_2_members <- c(
    "Hannah", "Jacob", "Sinatra/Versailles",
    "Kim", "Rob", "Jackie", "Nick", "Sal"
  )

  # Filter available names based on currently logged in user's group(s)
  get_allowed_names <- function(current_user, names_vec) {
    if (is.null(current_user) || !nzchar(current_user)) {
      return(names_vec)
    }

    allowed <- character(0)

    if (any(tolower(group_1_members) == tolower(current_user))) {
      allowed <- c(allowed, group_1_members)
    }

    if (any(tolower(group_2_members) == tolower(current_user))) {
      allowed <- c(allowed, group_2_members)
    }

    if (length(allowed) == 0) {
      return(names_vec)
    }

    allowed_clean <- unique(allowed)
    matched_names <- names_vec[tolower(names_vec) %in% tolower(allowed_clean)]
    return(matched_names)
  }

  # Keep dropdown choices updated for Tab 2 while preserving current selection & group permissions
  observe({
    names <- available_names()
    user <- rv$current_user
    req(names)

    allowed_names <- get_allowed_names(user, names)
    other_names <- setdiff(allowed_names, user)
    if (length(other_names) == 0) other_names <- setdiff(names, user)

    # Sort names alphabetically
    other_names <- sort(unique(other_names))

    current_sel <- isolate(input$selected_other)
    selected_val <- if (!is.null(current_sel) && current_sel %in% other_names) current_sel else other_names[1]

    updateSelectInput(session, "selected_other", choices = other_names, selected = selected_val)
  })

  # Listen to 'Bought' dropdown changes from JavaScript
  observeEvent(input$bought_change, {
    info <- input$bought_change
    df <- local_df()
    req(df, info$id, info$value)

    row_idx <- as.integer(info$id)
    if (row_idx >= 1 && row_idx <= nrow(df)) {
      df$Bought[row_idx] <- info$value

      # Auto-fill Who Bought with the logged-in user when marked Yes;
      # clear it when switched back to No
      if (info$value == "Yes" && !is.null(rv$current_user)) {
        df$`Who Bought`[row_idx] <- rv$current_user
      } else if (info$value == "No") {
        df$`Who Bought`[row_idx] <- NA_character_
      }

      local_df(df)

      # Write only this row's Bought/Who Bought cells immediately, rather than
      # waiting for a "Save" click that rewrites the whole sheet — prevents
      # one person's save from clobbering another person's concurrent edits.
      sheet_row <- row_idx + 1
      col_start <- col_letter(df, "Bought")
      col_end <- col_letter(df, "Who Bought")

      tryCatch(
        {
          range_write(
            ss = sheet_url,
            data = df[row_idx, c("Bought", "Who Bought")],
            sheet = "WishLists",
            range = paste0(col_start, sheet_row, ":", col_end, sheet_row),
            col_names = FALSE,
            reformat = FALSE
          )
        },
        error = function(e) {
          showNotification(paste("Error saving purchase status:", e$message),
            type = "error", duration = 10
          )
        }
      )
    }
  })

  # Listen to 'Who Bought' dropdown changes from JavaScript
  observeEvent(input$buyer_change, {
    info <- input$buyer_change
    df <- local_df()
    req(df, info$id)

    row_idx <- as.integer(info$id)
    val <- if (nzchar(info$value)) info$value else NA_character_

    if (row_idx >= 1 && row_idx <= nrow(df)) {
      df$`Who Bought`[row_idx] <- val
      # Automatically set Bought="Yes" if a buyer is selected
      if (!is.na(val)) {
        df$Bought[row_idx] <- "Yes"
      }
      local_df(df)

      # Write only this row's Bought/Who Bought cells immediately.
      sheet_row <- row_idx + 1
      col_start <- col_letter(df, "Bought")
      col_end <- col_letter(df, "Who Bought")

      tryCatch(
        {
          range_write(
            ss = sheet_url,
            data = df[row_idx, c("Bought", "Who Bought")],
            sheet = "WishLists",
            range = paste0(col_start, sheet_row, ":", col_end, sheet_row),
            col_names = FALSE,
            reformat = FALSE
          )
        },
        error = function(e) {
          showNotification(paste("Error saving buyer:", e$message),
            type = "error", duration = 10
          )
        }
      )
    }
  })

  # ── Helper: align new_row column order to match the live sheet ───────────
  # sheet_append writes by POSITION, so new_row must match the sheet's
  # column order exactly. This reorders new_row to match local_df(), and
  # appends any extra columns (e.g. Category not yet in sheet) at the end.
  align_new_row <- function(new_row) {
    sheet_cols  <- setdiff(names(local_df()), "row_id")       # live sheet order
    common      <- intersect(sheet_cols, names(new_row))       # cols in both
    extras      <- setdiff(names(new_row), sheet_cols)         # new cols not in sheet yet
    missing     <- setdiff(sheet_cols, names(new_row))         # sheet cols not in new_row
    # Fill any sheet columns we didn't supply with NA
    for (col in missing) new_row[[col]] <- NA_character_
    new_row[, c(intersect(sheet_cols, names(new_row)), extras), drop = FALSE]
  }

  # Add New Item to Google Sheet
  observeEvent(input$submit_item, {
    req(rv$current_user)

    item_name <- trimws(input$add_item)
    if (!nzchar(item_name)) {
      showNotification("Please enter an Item Name before adding.", type = "warning")
      return()
    }

    item_size     <- if (nzchar(trimws(input$add_size)))     trimws(input$add_size)     else NA_character_
    item_link     <- if (nzchar(trimws(input$add_link)))     trimws(input$add_link)     else NA_character_
    item_category <- if (nzchar(trimws(input$add_category))) trimws(input$add_category) else NA_character_

    new_row <- data.frame(
      Name         = rv$current_user,
      Category     = item_category,
      Item         = item_name,
      Size         = item_size,
      Link         = item_link,
      Bought       = "No",
      `Who Bought` = NA_character_,
      `Entered By` = rv$current_user,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    new_row <- align_new_row(new_row)

    tryCatch(
      {
        sheet_append(ss = sheet_url, data = new_row, sheet = "WishLists")
        showNotification(paste("Successfully added", item_name), type = "message")

        updateTextInput(session,      "add_item",     value = "")
        updateTextInput(session,      "add_size",     value = "")
        updateTextInput(session,      "add_link",     value = "")
        updateSelectizeInput(session, "add_category", selected = "")

        # Re-fetch dataset
        local_df(fetch_sheet_data())
      },
      error = function(e) {
        showNotification(
          paste("Write error:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # Dynamic button label for adding item on others wish list tab
  output$submit_other_item_btn_ui <- renderUI({
    req(input$selected_other)
    actionButton("submit_other_item", paste("Add Item for", input$selected_other), icon = icon("plus"), class = "btn-success")
  })

  # Add Item to Someone Else's Wish List
  observeEvent(input$submit_other_item, {
    req(rv$current_user, input$selected_other)

    item_name <- trimws(input$add_other_item)
    if (!nzchar(item_name)) {
      showNotification("Please enter an Item Name before adding.", type = "warning")
      return()
    }

    item_size     <- if (nzchar(trimws(input$add_other_size)))     trimws(input$add_other_size)     else NA_character_
    item_link     <- if (nzchar(trimws(input$add_other_link)))     trimws(input$add_other_link)     else NA_character_
    item_category <- if (nzchar(trimws(input$add_other_category))) trimws(input$add_other_category) else NA_character_

    new_row <- data.frame(
      Name         = input$selected_other,
      Category     = item_category,
      Item         = item_name,
      Size         = item_size,
      Link         = item_link,
      Bought       = "No",
      `Who Bought` = NA_character_,
      `Entered By` = rv$current_user,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    new_row <- align_new_row(new_row)

    tryCatch(
      {
        sheet_append(ss = sheet_url, data = new_row, sheet = "WishLists")
        showNotification(paste("Successfully added", item_name, "for", input$selected_other, "!"), type = "message")

        updateTextInput(session,      "add_other_item",     value = "")
        updateTextInput(session,      "add_other_size",     value = "")
        updateTextInput(session,      "add_other_link",     value = "")
        updateSelectizeInput(session, "add_other_category", selected = "")

        # Re-fetch dataset
        local_df(fetch_sheet_data())
      },
      error = function(e) {
        showNotification(
          paste("Write error:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # "Save Updates" button: Bought/Who Bought changes now save immediately
  # per-cell as they're toggled (see bought_change/buyer_change above), so
  # this button just re-fetches the latest data from the sheet — useful if
  # someone else has made changes since this session loaded.
  observeEvent(input$save_purchases, {
    local_df(fetch_sheet_data())
    showNotification("Refreshed with latest data", type = "message")
  })

  # ── Helper: Convert column number to Google Sheets column letter ───────────
  int2col <- function(n) {
    letters <- ""
    while (n > 0) {
      r <- (n - 1) %% 26
      letters <- paste0(LETTERS[r + 1], letters)
      n <- floor((n - r) / 26)
    }
    letters
  }

  # ── Tab 1: My Wish List Table (editable) ───────────────────────────────────
  output$my_wishlist_table <- renderReactable({
    df <- local_df()
    req(df, rv$current_user)

    my_df <- df %>%
  mutate(row_id = row_number()) %>%
  filter(
    Name == rv$current_user &
      (is.na(`Entered By`) | `Entered By` == rv$current_user)
  ) %>%
  arrange(
    tolower(ifelse(is.na(Category), "", Category)),
    tolower(ifelse(is.na(Item), "", Item))
  )

    reactable(
      my_df %>% select(Category, Item, Size, Link, row_id),
      pagination = FALSE,
      wrap = TRUE,
      filterable = FALSE,
      searchable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultColDef = colDef(
        style = list(
          whiteSpace = "pre-wrap",
          wordBreak = "break-word"
        )
      ),
      columns = list(Category = colDef(
          name = "Category",
          headerStyle = list(fontWeight = "bold"),
  minWidth = 85,

  style = list(
    whiteSpace = "pre-wrap",
    wordBreak = "break-word"
  ),

  cell = function(value, index) {

    row_id <- my_df$row_id[index]

    cur_val <- ifelse(
      is.na(value),
      "",
      as.character(value)
    )


    tags$textarea(
      value = cur_val,

      placeholder = "Item name...",

      onblur = sprintf(
  "Shiny.setInputValue(
    'my_edit',
    {
      row:%d,
      col:'Category',
      value:this.value
    },
    {priority:'event'}
  )",
  row_id
),
      onkeydown = "event.stopPropagation();",

      onclick = "event.stopPropagation();",

      class = "form-control",

      style =
        "padding:4px 6px;
         font-size:13px;
         font-weight:bold;
         width:100%;
         min-height:38px;
         resize:vertical;
         white-space:pre-wrap;
         overflow-wrap:break-word;"
    )
  }
),

        Item = colDef(
          name = "Item",
          headerStyle = list(fontWeight = "bold"),
  minWidth = 85,

  style = list(
    whiteSpace = "pre-wrap",
    wordBreak = "break-word"
  ),

  cell = function(value, index) {

    row_id <- my_df$row_id[index]

    cur_val <- ifelse(
      is.na(value),
      "",
      as.character(value)
    )


    tags$textarea(
      value = cur_val,

      placeholder = "Item name...",

      onblur = sprintf(
        "Shiny.setInputValue(
          'my_edit',
          {
            row:%d,
            col:'Item',
            value:this.value
          },
          {priority:'event'}
        )",
        row_id
      ),

      onkeydown = "event.stopPropagation();",

      onclick = "event.stopPropagation();",

      class = "form-control",

      style =
        "padding:4px 6px;
         font-size:13px;
         font-weight:bold;
         width:100%;
         min-height:55px;
         resize:vertical;
         white-space:pre-wrap;
         overflow-wrap:break-word;"
    )
  }
),
        Size = colDef(
          name = "Size / Options",
          minWidth = 50,
          cell = function(value, index) {
            row_id <- my_df$row_id[index]
            cur_val <- ifelse(is.na(value), "", as.character(value))

            tags$input(
              type = "text",
              value = cur_val,
              placeholder = "e.g. Medium",
              onblur = sprintf(
                "Shiny.setInputValue('my_edit', {row:%d, col:'Size', value:this.value}, {priority:'event'})",
                row_id
              ),
              onkeydown = "event.stopPropagation();",
              onclick = "event.stopPropagation();",
              class = "form-control",
style =
        "padding:4px 6px;
         font-size:13px;
         font-weight:bold;
         width:100%;
         min-height:38px;
         resize:vertical;
         white-space:pre-wrap;
         overflow-wrap:break-word;"            )
          }
        ),
        Link = colDef(
          name = "Link",
          minWidth = 100,
          cell = function(value, index) {
            row_id <- my_df$row_id[index]
            cur_val <- ifelse(is.na(value), "", as.character(value))

            tagList(
              tags$input(
                type = "text",
                value = cur_val,
                placeholder = "https://...",
                onblur = sprintf(
                  "Shiny.setInputValue('my_edit', {row:%d, col:'Link', value:this.value}, {priority:'event'})",
                  row_id
                ),
                onkeydown = "event.stopPropagation();",
                onclick = "event.stopPropagation();",
                class = "form-control",
                style =  "padding:4px 6px;
         font-size:13px;
         font-weight:bold;
         width:100%;
         min-height:38px;
         resize:vertical;
         white-space:pre-wrap;
         overflow-wrap:break-word;" 
              ),
              if (nzchar(cur_val)) {
                tags$a(
                  href = cur_val,
                  target = "_blank",
                  rel = "noopener noreferrer",
                  icon("external-link-alt"),
                  syle = "margin-left:6px; vertical-align:tmiddle;"
                )
              }
            )
          }
        ),
        row_id = colDef(
          name = "",
          sortable = FALSE,
          filterable = FALSE,
          width = 85,
          cell = function(value) {
            tags$button(
              onclick = sprintf(
                "Shiny.setInputValue('delete_my_row', {row:%d, nonce:Math.random()}, {priority:'event'})",
                value
              ),
              class = "btn btn-danger btn-sm",
              style = "padding:2px 8px; font-size:12px;",
              icon("trash"),
              " Delete"
            )
          }
        )
      )
    )
  })

  # ── Capture edits without refreshing table ────────────────────────────────
  observeEvent(input$my_edit, {
    info <- input$my_edit
    req(info$row, info$col, !is.null(info$value))

    if (info$col %in% c("Category", "Item", "Size", "Link")) {
      if (is.null(rv$my_edits)) {
        rv$my_edits <- list()
      }
      key <- paste(as.integer(info$row), info$col, sep = "_")
      rv$my_edits[[key]] <- info$value
    }
  })

  # ── Save ONLY changed cells ────────────────────────────────────────────────
  observeEvent(input$save_my_edits, {
    edits <- rv$my_edits
    req(edits, length(edits) > 0)

    tryCatch(
      {
        df <- local_df()

        for (key in names(edits)) {
          parts <- strsplit(key, "_")[[1]]
          row_id <- as.integer(parts[1])
          col_name <- parts[2]

          col_num <- match(col_name, names(df))
          req(!is.na(col_num))

          col_letter_val <- int2col(col_num)

          # +1 because row 1 in Google Sheets is headers
          sheet_cell <- paste0(col_letter_val, row_id + 1)
          new_value <- edits[[key]]

          if (!nzchar(new_value)) {
            new_value <- NA_character_
          }

          print(paste("Saving", new_value, "to", sheet_cell))

          range_write(
            ss = sheet_url,
            sheet = "WishLists",
            range = sheet_cell,
            data = data.frame(value = new_value),
            col_names = FALSE,
            reformat = FALSE
          )

          # update local memory
          new_value <- as.character(new_value)
        }

        local_df(df)
        rv$my_edits <- list()
        showNotification("Saved changes", type = "message")
      },
      error = function(e) {
        showNotification(
          paste("Error saving changes:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # ── Delete Wish List Row ───────────────────────────────────────────────────
  # Stores the row waiting for confirmation
  pending_delete_row <- reactiveVal(NULL)

  # Step 1: User clicks Delete button
  observeEvent(input$delete_my_row, {
    req(input$delete_my_row$row)
    pending_delete_row(as.integer(input$delete_my_row$row))

    showModal(
      modalDialog(
        title = "Confirm Delete",
        "Are you sure you want to delete this item?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete", "Delete", class = "btn btn-danger")
        ),
        easyClose = TRUE
      )
    )
  })

  # Step 2: Confirm deletion
  observeEvent(input$confirm_delete, {
    row_idx <- pending_delete_row()
    df <- local_df()
    req(df, row_idx)

    tryCatch(
      {
        # Google Sheets has a header row
        sheet_row <- row_idx + 1

        # Last used column
        last_col <- int2col(ncol(df))

        delete_range <- paste0("A", sheet_row, ":", last_col, sheet_row)

        range_delete(
          ss = sheet_url,
          sheet = "WishLists",
          range = delete_range,
          shift = "up"
        )

        # Clear pending state
        pending_delete_row(NULL)
        removeModal()
        showNotification("Row deleted", type = "message")

        # Reload data because rows shifted upward
        local_df(fetch_sheet_data())
      },
      error = function(e) {
        showNotification(
          paste("Error deleting row:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # ── Dashboard Summary Boxes ────────────────────────────────────────────────
  # Tab 1: My Item Count Box
  output$my_item_count <- renderValueBox({
    df <- local_df()
    req(df, rv$current_user)
    count <- sum(df$Name == rv$current_user & (is.na(df$`Entered By`) | df$`Entered By` == rv$current_user) & !is.na(df$Item), na.rm = TRUE)
    valueBox(count, "Items on Your Wish List", icon = icon("gift"), color = "teal")
  })

  # Tab 1: My Name Box
  output$my_name_box <- renderValueBox({
    req(rv$current_user)
    valueBox(rv$current_user, "Active Profile", icon = icon("user-check"), color = "purple")
  })

  # Tab 2: Other Person Item Count Box
  output$other_item_count <- renderValueBox({
    df <- local_df()
    req(df, input$selected_other, rv$current_user)
    allowed_names <- get_allowed_names(rv$current_user, available_names())
    req(input$selected_other %in% allowed_names)
    bought_count <- sum(df$Name == input$selected_other & df$Bought == "Yes", na.rm = TRUE)
    valueBox(bought_count, paste("Total Items Bought for", input$selected_other), icon = icon("gift"), color = "red")
  })

  # Tab 2: Other Person Bought Count Box
  output$other_bought_count <- renderValueBox({
    df <- local_df()
    req(df, input$selected_other, rv$current_user)
    allowed_names <- get_allowed_names(rv$current_user, available_names())
    req(input$selected_other %in% allowed_names)
    bought_count <- sum(df$Name == input$selected_other & df$Bought == "No", na.rm = TRUE)
    valueBox(bought_count, paste("Items Available"), icon = icon("shopping-cart"), color = "green")
  })

  # ── Tab 2: Others Wish List Table ──────────────────────────────────────────
  output$others_wishlist_table <- renderReactable({
    df <- local_df()
    req(df, input$selected_other, rv$current_user)

    allowed_names <- get_allowed_names(rv$current_user, available_names())
    req(input$selected_other %in% allowed_names)

    df_with_id <- df %>% mutate(row_id = row_number())

    if (!"Category" %in% names(df_with_id)) {
      df_with_id$Category <- "Uncategorized"
    }

    df_with_id <- df_with_id %>%
      mutate(
        Category = ifelse(
          is.na(Category) | !nzchar(trimws(as.character(Category))),
          "Uncategorized",
          trimws(as.character(Category))
        )
      )

    others_df <- df_with_id %>%
      filter(Name == input$selected_other) %>%
      arrange(
        Category == "Uncategorized",
        Category,
        Bought == "Yes",
        tolower(ifelse(is.na(Item), "", Item))
      ) %>%
      # Flatten any list-columns that googlesheets4 may produce so bind_rows works
      mutate(across(where(is.list), ~ sapply(., function(x) {
        if (is.null(x) || length(x) == 0) NA_character_
        else as.character(x[[1]])
      })))

    names_choices <- sort(unique(allowed_names))

    # ── Build grouped_df: inject a header row before each category ──────────
    if (nrow(others_df) == 0) {
      grouped_df <- others_df %>% mutate(is_header = logical(0))
    } else {
      categories <- unique(others_df$Category)
      grouped_list <- lapply(categories, function(cat_name) {
        sub_df <- others_df %>% filter(Category == cat_name)
        sub_df$is_header <- FALSE

        # Build one header row by copying structure from first data row
        hdr <- sub_df[1, , drop = FALSE]
        hdr$is_header   <- TRUE
        hdr$row_id      <- NA_integer_
        hdr$Item        <- cat_name 
        hdr$Size        <- ""
        hdr$Bought      <- ""
        hdr$`Who Bought`<- ""
        hdr$`Entered By`<- ""
        hdr$Link        <- ""

        bind_rows(hdr, sub_df)
      })
      grouped_df <- bind_rows(grouped_list)
    }

    # ── Helper used inside every cell function ──────────────────────────────
    is_hdr <- function(index) isTRUE(grouped_df$is_header[index])

    reactable(
      grouped_df %>% select(Item, Size, Bought, `Who Bought`, `Entered By`),
      pagination    = FALSE,
      wrap          = TRUE,
      filterable    = TRUE,
      searchable    = TRUE,
      striped       = TRUE,
      highlight     = TRUE,
      bordered      = TRUE,
      sortable      = FALSE,

      rowStyle = function(index) {
        if (is_hdr(index)) {
          return(list(
            backgroundColor = "#f399dfff",
            fontWeight      = "bold",
            fontSize        = "14px",
            color           = "#1d032dff"
          ))
        }
        if (isTRUE(grouped_df$Bought[index] == "Yes")) {
          return(list(backgroundColor = "#fce8e6"))
        }
        NULL
      },

      language = reactableLang(
        searchPlaceholder = "Search items...",
        filterPlaceholder = "Filter..."
      ),

      columns = list(

        Item = colDef(
          headerStyle = list(fontWeight = "bold"),
          style       = list(fontWeight = "bold", whiteSpace = "pre-wrap", wordBreak = "break-word"),
          cell = function(value, index) {
            if (is_hdr(index)) {
              return(tags$span(
                style = "font-weight: bold; font-size: 14px; color: #872ee1ff;",
                value
              ))
            }

            link_url <- grouped_df$Link[index]
            has_link <- !is.na(link_url) && nzchar(trimws(as.character(link_url)))

            if (has_link) {
              url <- trimws(as.character(link_url))
              if (!grepl("^https?://", url, ignore.case = TRUE)) url <- paste0("https://", url)
              tags$a(
                href   = url, target = "_blank", rel = "noopener noreferrer",
                style  = "color: #3c8dbc; font-weight: bold; text-decoration: underline;
                          white-space: pre-wrap; word-break: break-word;",
                value, " ",
                icon("external-link-alt",
                     style = "font-size: 11px; margin-left: 3px; vertical-align: baseline;")
              )
            } else {
              tags$div(
                style = "white-space: pre-wrap; word-break: break-word;
                         overflow-wrap: break-word; font-weight: bold;",
                value
              )
            }
          }
        ),

        Size = colDef(
          style = list(whiteSpace = "pre-wrap", wordBreak = "break-word"),
          cell  = function(value, index) {
            if (is_hdr(index)) return("")
            value
          }
        ),

        Bought = colDef(
          name = "Bought?",
          cell = function(value, index) {
            if (is_hdr(index)) return("")

            row_id      <- grouped_df$row_id[index]
            current_val <- ifelse(is.na(value) | !nzchar(as.character(value)), "No", as.character(value))
            is_yes      <- (current_val == "Yes")

            select_style <- if (is_yes) {
              "height:32px; padding:2px 6px; font-size:13px; min-width:90px;
               background-color:#d9534f; color:white; font-weight:bold; border-color:#d43f3a;"
            } else {
              "height:32px; padding:2px 6px; font-size:13px; min-width:90px;"
            }

            tags$select(
              onchange = sprintf(
                "Shiny.setInputValue('bought_change', {id: %d, value: this.value}, {priority: 'event'})",
                row_id
              ),
              class = "form-control",
              style = select_style,
              tags$option(value = "No",  selected = if (!is_yes) "selected" else NULL, "No"),
              tags$option(value = "Yes", selected = if (is_yes)  "selected" else NULL, "Yes")
            )
          }
        ),

        `Who Bought` = colDef(
          name = "Who Bought It",
          cell = function(value, index) {
            if (is_hdr(index)) return("")

            row_id      <- grouped_df$row_id[index]
            current_val <- ifelse(is.na(value) | !nzchar(as.character(value)), "", as.character(value))

            opt_list <- list(
              tags$option(value = "",
                          selected = if (!nzchar(current_val)) "selected" else NULL,
                          "-- Select --")
            )
            for (nm in names_choices) {
              opt_list[[length(opt_list) + 1]] <- tags$option(
                value    = nm,
                selected = if (current_val == nm) "selected" else NULL,
                nm
              )
            }

            tags$select(
              onchange = sprintf(
                "Shiny.setInputValue('buyer_change', {id: %d, value: this.value}, {priority: 'event'})",
                row_id
              ),
              class = "form-control",
              style = "height:32px; padding:2px 6px; font-size:13px; min-width:140px;",
              opt_list
            )
          }
        ),

        `Entered By` = colDef(
          name = "Entered By",
          cell = function(value, index) {
            if (is_hdr(index)) return("")
            owner_name <- grouped_df$Name[index]
            if (is.na(value) || !nzchar(as.character(value))) owner_name else as.character(value)
          }
        )

      )
    )
  })
}

shinyApp(ui = ui, server = server)
