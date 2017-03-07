library(dplyr)
library(RMySQL)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(xlsx)


ui <- dashboardPage(
  dashboardHeader(title = "On Demand SKU Health Check", titleWidth = 375),
  dashboardSidebar(
    width = 375,
    
    radioButtons(
      "inputType_Input",
      label = "Choose Input Type",
      choices = list("Enter IDs" = 1, "Upload File" = 2),
      selected = 1,
      inline = TRUE
    ),
    
    # Only show this panel if Enter IDs is selected
    conditionalPanel(condition = "input.inputType_Input == '1'",
                     textInput("item_list", "Enter IDs")),
    
    # Only show this panel if Custom is selected
    conditionalPanel(
      # option to choose file type csv or excel
      condition = "input.inputType_Input == '2'",
      radioButtons(
        "fileType_Input",
        label = "Choose File type",
        choices = list(".xlsx" = 1, ".csv/txt" = 2),
        selected = 1,
        inline = TRUE
      ),
      fileInput(
        'file1',
        'Upload Items List',
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv',
          '.xlsx'
        )
      )
    ),
    
    
    tags$head(tags$script(
      HTML(
        'Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'
      )
    )),
    
    radioButtons(
      "perfBasis_Input",
      label = "Choose Performance based on",
      choices = list(
        "7 Days" = 1,
        "30 Days" = 2,
        "90 Days" = 3
      ),
      selected = 1,
      inline = TRUE
    ),
    shinyjs::useShinyjs(),
    fluidRow(column(
      width = 2,
      offset = 1,
      actionButton("goButton", "Submit")
    )),
    br(),
    br(),
    uiOutput("SupDeptOutput"),
    uiOutput("DeptOutput"),
    uiOutput("CategOutput"),
    uiOutput("wm_mpOutput"),
    uiOutput("ItemPublishOutput"),
    uiOutput("MerchActOutput"),
    uiOutput("ItemPerfOutput")
  ),
  dashboardBody(
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    tabsetPanel(
      type = "tabs",
      
      tabPanel("Dashboard",
               
               fluidRow(
                 box(height = 470,
                     plotlyOutput("pie1")),
                 box(height = 470,
                     plotlyOutput("barplot1"))
               ),
               fluidRow(
                 box(height = 470, plotlyOutput(outputId = "barplot2")),
                 
                 box(
                   height = 470,
                   h3("Reference Sheet"),
                   tags$ol(
                     h4(
                       tags$li(
                         "Choose input type 'Enter IDs' and enter a list of base or variant Item IDs, or a combination of both, separated by space. Alternatively, cut paste a list directly from excel."
                       ),
                       br(),
                       tags$li(
                         "Or choose input type 'Upload File' and upload a list as .xlsx or .csv/.txt."
                       ),
                       br(),
                       tags$li(
                         "File should contain Item/Base Item IDs in the first column with or without a header. File should not contain more than 50,000 rows."
                       ),
                       br(),
                       tags$li("Click 'Submit' to process the entered data or uploaded file."),
                       br(),
                       tags$li("Choose the number of days you want the sku performance based on."),
                       br(),
                       tags$li(
                         "'Dashboard' tab gives a quick view of item performance and recommended actions."
                       ),
                       br(),
                       tags$li(
                         "'Item Details' tab displays the item table with selective columns."
                       ),
                       br(),
                       tags$li(
                         "Click on 'Download' button on 'Item Details' tab to download the detailed table in csv format."
                       )
                     )
                   ),
                   br(),
                   textOutput("rpt_dt"),
                   
                   tags$style("#rpt_dt{color: red;
                              font-size: 20px;
                              font-style: italic;
                              }")

                   )
                   )),
      
      tabPanel(
        "Item Details",
        fluidRow(
          column(
            width = 2,
            textOutput("rpt_dt2"),
            
            tags$style("#rpt_dt2{color: black;
                       font-size: 18px;
                       }")
                        ),
          
          column(
            width = 5,
            offset = 2,
            checkboxGroupInput(
              "column_Input",
              label = "Choose column groups to download",
              choices = list(
                "Inventory" = 1,
                "Pricing" = 2,
                "Sales" = 3,
                "Traffic" = 4,
                "Amazon Info" = 5,
                "BuyBox Info" = 6
              ),
              selected = c(1, 2, 3, 4, 5, 6),
              inline = TRUE
            )
          ),
          
          column(
            width = 2,
            offset = 1,
            strong('Download Detailed File'),
            downloadButton('downloadData', 'Download')
          )
          
            ),
        
        fluidRow(
          div(style = 'overflow-x: scroll;margin-left: 20px', dataTableOutput("contents"))
        )
        ),
      
      tabPanel("Glossary",
               
               fluidRow(
                 column(width = 5,
                        h3("Item performance"),
                        tags$ol(h4(
                          tags$li(
                            strong("High Performers:"),
                            "Items contributing to the top 80% towards sales",
                            strong("OR"),
                            "unit sales within their sub-category in the last 7, 30 or 90 days depending on the selection."
                          ),
                          br(),
                          tags$li(
                            strong("Undperformers:"),
                            "Items contributing to the bottom 20% towards sales",
                            strong("AND"),
                            "unit sales within their sub-category in the last 7, 30 or 90 days depending on the selection."
                          ),
                          br(),
                          tags$li(
                            strong("No Sales:"),
                            "Items that did not get any sales in last 7, 30 or 90 days depending on the selection."
                          )
                        ))),
                 
                 
                 column(
                   width = 6,
                   offset = 1,
                   h3("Recommended Actions"),
                   tags$ol(
                     h4(
                       tags$li(strong("Publish Item:"), "In stock, but not published."),
                       br(),
                       "In stock, not published, item end date in future, non-bundle.",
                       br(),
                       br(),
                       tags$li(
                         strong("Set Item on Replen:"),
                         "Owned item not on replen and is out of stock, but is getting high site traffic."
                       ),
                       br(),
                       "OWN only item, is not on clearance or rollback, non-bundle, OOS, has no on order coming, and has product page views or total
                       search/browse impressions similar to that of High Performers within the same category, or is an Amazon best seller. ",
                       br(),
                       br(),
                       tags$li(
                         strong("Contact DSV to Replenish Inventory:"),
                         "DSV only item or DSV/OWN shared item currently out of stock, but is getting high
                         site traffic, or is an Amazon best seller."
                       ),
                       br(),
                       "DSV only or DSV/OWN shared, future item end date, is not on clearance or rollback, is not on promotion, non-bundle,
                       and has product page views or total search/browse impressions similar to that of High Performers
                       within the same category.",
                       br(),
                       br(),
                       tags$li(
                         strong("Turn on Auto Pricing:"),
                         "Auto pricing not turned on for Underperformer or No Sales item."
                       ),
                       br(),
                       "Underperformer or No Sales, in stock, is set up in smart pricing basket, and is not on clearance or rollback, is not on promotion, non-map, non-bundle.",
                       br(),
                       br(),
                       tags$li(
                         strong("Setup in Smart Pricing Basket:"),
                         "Underperforming item not set up in Smart Pricing basket, but have CIA competitor pricing available."
                       ),
                       br(),
                       
                       "Item is in stock, is set up in Smart Pricing basket, has lowest competitor pricing info in CIA, and is not on clearance or rollback, is not on promotion,  non-map, non-bundle.",
                       br(),
                       br(),
                       
                       tags$li(
                         strong("Price More Competitively:"),
                         "Underperformer or No Sales item is priced higher than the competitor price, and is getting high site traffic."
                       ),
                       br(),
                       "In stock, auto price update flag is on, set up in Smart Pricing basket, not on clearance or rollback, is not on promotion, non-map, non-bundle, Walmart price currently
                       above the lowest competitor, and item is getting total impressions and item views greater than the median values of High Performers within the category,
                       or is an Amazon best seller. ",
                       br(),
                       br(),
                       
                       tags$li(
                         strong("Elevate on Site or Invest SEM:"),
                         "Underperformer or No Sales items which have low discoverability, but high click-through rate and high conversion rate."
                       ),
                       br(),
                       "In stock, auto price update flag is on, set up in Smart Pricing basket, priced competitively, not on clearance or rollback, non-map, non-bundle,
                       low traffic, but has CTR and Conversion rates similar to that of High Performers within the same category."
                       
                       )
                   )
                   )
                 
                 
               ))
    )
)
)

server <- function(input, output, session) {
  # Disable submit button if file not uploaded or ids not entered
  observe({
    if (input$inputType_Input == 1) {
      shinyjs::toggleState("goButton", input$item_list != "")
    } else {
      shinyjs::toggleState("goButton",!is.null(input$file1) &&
                             input$file1 != "")
      
    }
  })
  
  # Get the uploaded file function
  get_item_list <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    # upload .csv or .xlsx based on input selection
    if (input$fileType_Input == "1") {
      read.xlsx(
        inFile$datapath,
        sheetIndex = 1,
        colIndex = 1,
        header = FALSE,
        stringsAsFactors = FALSE
      )
      
    } else {
      read.csv(inFile$datapath,
               header = FALSE,
               stringsAsFactors = FALSE)
    }
    
  })
  
  # read the file and assign the header name
  observeEvent(input$goButton, {
    # Create a progress bar
    progress <- Progress$new(session, min = 1, max = 100)
    
    on.exit(progress$close())
    
    #update progress
    progress$set(message = 'Processing...please wait', value = 20)
    
    
    if (input$inputType_Input == 1) {
      item_list <- data.frame(unlist(strsplit(input$item_list, " ")))
      
      #assign coloumn to the first column of the dataset
      colnames(item_list)[1] <- "item_id"
      
      item_list$item_id <- as.character(item_list$item_id)
      
      #assign coloumn to the first column of the dataset
      colnames(item_list)[1] <- "item_id"
      
    } else {
      item_list <- get_item_list()
      
      #assign coloumn to the first column of the dataset
      colnames(item_list)[1] <- "item_id"
      
    }
    
    
    # check if vals has values, if not, throw a popup error
    if (nrow(item_list) > 50000) {
      file_test <-
        "Input file has more than 50,000 rows. Please split input file into smaller files "
      js_string <- 'alert("Oversized File");'
      js_string <- sub("Oversized File", file_test, js_string)
      session$sendCustomMessage(type = 'jsCode', list(value = js_string))
      return()
    }
    # update prgoress
    progress$inc(40)
    
    # loop on item ids, validate and move them to vals separated by commas
    flag <- 0
    for (i in 1:nrow(item_list)) {
      item_list$item_id[i] <- as.numeric(item_list$item_id[i])
      
      if (is.na(item_list$item_id[i]) == FALSE) {
        if (flag == 0) {
          vals <- item_list$item_id[i]
          flag <- 1
          next
        }
        
        vals <- paste0(vals, ",", item_list$item_id[i])
      }
      
    }
    
    #update progress
    progress$inc(50)
    #    validate item ids in the file
    
    # check if vals has values, if not, throw a popup error
    if (exists("vals") == FALSE) {
      file_test <-
        "Input file should be a comma separated, text or an excel file containing item ids in the first column with or without a header. Please check input file"
      js_string <- 'alert("Incorrect Input");'
      js_string <- sub("Incorrect Input", file_test, js_string)
      session$sendCustomMessage(type = 'jsCode', list(value = js_string))
      return()
    }
    
    browser()
    # prepare item ids list for query
    item_ids <- paste0("(", vals, ")")
    
    # create channel fo mySQL db
    channel <-
      dbConnect(
        MySQL(),
        user = 'root',
        password = 'agithftw',
        dbname = 'CDPROD',
        host = '172.29.105.128'
      )
    
    # update progress
    progress$inc(60)
    
    query <-
      paste(
        "select catlg_item_id as 'Item ID', base_item_id as 'Base Item', upc as UPC, prod_nm as Product, item_performance_7d,
        action_7d, item_performance_30d, action_30d, item_Performance as item_performance_90d, action_90d,
        super_dept_nm as 'Super.Department', dept_nm as Department, categ_nm as Category,
        sub_categ_nm as 'Sub.Category', brand_nm as Brand, vendor_name as Vendor, is_marketplace as 'WM.MP',
        seller_count as 'No Of Sellers', online_store_flag as 'Online/Store', own_dsv_type as 'Owned/DSV',
        store_cnt as 'Store Count', freight_flag as 'Freight Flag', replenishable_ind as 'Replenishable Ind',
        item_display_status as 'Item.Display.Status', wm_display_status as 'WM Item Disp Status', mp_display_status as
        'MP Item Disp Status' , avg_overall_rating as 'Avg Overall Rating', num_appr_reviews as 'No Of Reviews',
        qty_in_stock as 'Current Qty in Stock', curr_oh_qty as 'Current On Hand Qty', store_inventory as 'Store Inventory',
        on_order_qty as 'On Order Quantity', min_mabd as 'Min Must Arrive by Date', max_mabd as 'Max Must Arrive by Date',
        mp_qty_in_stock as 'MP Quantity in Stock', perc_oos_variant_grp as 'Variants Stock Status', total_inventory as
        'Total Inventory', street_date as 'Street date', wm_item_start_date as 'WM Item Start Date', mp_item_start_date
        as 'MP Item Start Data', wm_item_end_dt as 'WM Item End Date', mp_item_end_dt as 'MP Item End Date', first_sold_date
        as 'First Sold Date', last_sold_date as 'Last Sold Date', first_published_dt as 'First Published Date', curr_price as
        'Current Price', curr_avg_cost as 'Current Avg Cost', curr_price_change_desc as 'Special Price Desc', map_price as MAP,
        curr_wm_price as 'Current WM Price', min_mp_price as 'Minimum MP Price', max_mp_price as 'Maximum MP Price',
        avg_mp_price as 'Avg MP Price', store_price as 'Store Price',lowest_competitor_price as 'Current Lowest Competitor Price', lowest_competitor_name as
        'Current Lowest Priced Competitor Name', smart_basket_id as 'Smart Basket ID', smart_basket_name as 'Smart Basket Name', 'objective' as
        Objective, is_auto_price_update as 'Is Auto Price on?', avg_ship_days_out_item_pg as 'Avg Ship Days',
        retail_sales as 'Retail Sales Yesterday', unit_sales as 'Unit Sales Yesterday', mp_sales as
        'Market Place Sales Yesterday', mp_unit_sales as 'Market Place Unit Sales Yesterday', s2s_sales as
        'Ship to Store Sales Yesterday', s2s_unit_sales as 'Ship to Store Unit Sales Yesterday', s2h_sales as
        'Ship to Home Sales Yesterday', s2h_unit_sales as 'Ship to Home Unit Sales Yesterday', sfs_sales as
        'Ship from Store Sales Yesterday', sfs_unit_sales as 'Ship from Store Unit Sales Yesterday', put_sales as
        'PUT Sales Yesterday', put_unit_sales as 'PUT Unit Sales Yesterday', prev_retail_sales_7d as 'Retail Sales 7 Days',
        prev_unit_sales_7d as 'Unit Sales 7 Days', prev_mp_sales_7d as 'Market Place Sales 7 Days', prev_mp_unit_sales_7d
        as 'Market Place Unit Sales 7 Days', prev_s2s_sales_7d as 'Ship to Store Sales 7 Days', prev_s2s_unit_sales_7d as
        'Ship to Store Unit Sales 7 Days', prev_s2h_sales_7d as 'Ship to Home Sales 7 Days', prev_s2h_unit_sales_7d as
        'Ship to Home Unit Sales 7 Days', prev_sfs_sales_7d as 'Ship from Store Sales 7 Days', prev_sfs_unit_sales_7d as
        'Ship from Store Unit Sales 7 Days', prev_put_sales_7d as 'PUT Sales 7 Days', prev_put_unit_sales_7d as
        'PUT Unit Sales 7 Days', 100*imu_perc as 'IMU Item % Yesterday', 100*prev_imu_perc_7d as 'IMU Item % 7 Days',
        imu_sales as 'IMU Sales Yesterday', prev_imu_sales_7d as 'IMU Sales 7 Days', prev_retail_sales_30d as 'Retail Sales 30 Days',
        prev_unit_sales_30d as 'Unit Sales 30 Days', prev_mp_sales_30d as 'Market Place Sales 30 Days', prev_mp_unit_sales_30d
        as 'Market Place Unit Sales 30 Days', prev_s2s_sales_30d as 'Ship to Store Sales 30 Days', prev_s2s_unit_sales_30d as
        'Ship to Store Unit Sales 30 Days', prev_s2h_sales_30d as 'Ship to Home Sales 30 Days', prev_s2h_unit_sales_30d as
        'Ship to Home Unit Sales 30 Days', prev_sfs_sales_30d as 'Ship from Store Sales 30 Days', prev_sfs_unit_sales_30d as
        'Ship from Store Unit Sales 30 Days', prev_put_sales_30d as 'PUT Sales 30 Days', prev_put_unit_sales_30d as
        'PUT Unit Sales 30 Days', 100*prev_imu_perc_30d as 'IMU Item % 30 Days', prev_imu_sales_30d as 'IMU Sales 30 Days',
        prev_retail_sales_90d as 'Retail Sales 90 Days',prev_unit_sales_90d as 'Unit Sales 90 Days', prev_mp_sales_90d as
        'Market Place Sales 90 Days', prev_mp_unit_sales_90d as 'Market Place Unit Sales 90 Days', prev_s2s_sales_90d as
        'Ship to Store Sales 90 Days', prev_s2s_unit_sales_90d as 'Ship to Store Unit Sales 90 Days', prev_s2h_sales_90d as
        'Ship to Home Sales 90 Days', prev_s2h_unit_sales_90d as 'Ship to Home Unit Sales 90 Days', prev_sfs_sales_90d as
        'Ship from Store Sales 90 Days', prev_sfs_unit_sales_90d as 'Ship from Store Unit Sales 90 Days',
        prev_put_sales_90d as 'PUT Sales 90 Days', prev_put_unit_sales_90d as 'PUT Unit Sales 90 Days',
        100*prev_imu_perc_90d as 'IMU Item % 90 Days', prev_imu_sales_90d as 'IMU Sales 90 Days',  bbx_winner_1 as
        'BuyBox Winner 1 Yesterday', bbx_pcts_1 as 'Buy Box Winner 1 % Times Won', bbx_winner_2 as 'BuyBox Winner 2 Yesterday', bbx_pcts_2 as
        'Buy Box Winner 2 % Times Won', bbx_winner_3 as 'BuyBox Winner 3 Yesterday', bbx_pcts_3 as 'Buy Box Winner 3 % Times Won',
        amzn_item_name as 'Amazon Item Name', amzn_num_reviews as 'Amazon Number of Reviews', amzn_rating as 'Amazon Avg Rating',
        amzn_list_price as 'Amazon List price',amzn_best_seller as 'Amazon Best Seller?', item_views as 'Item Views Yesterday',
        oos_item_views as 'Out of Stock Item Views Yesterday', ttl_impressions as 'Total Impressions Yesterday',
        prev_item_views_7d as 'Item Views 7 Days', prev_oos_item_views_7d as 'Out Of Stock Item Views 7 Days',
        prev_ttl_impressions_7d as 'Total Impressions 7 Days', categ_med_item_views_7d as 'Category High Perf Median Item Views 7 Days',
        categ_med_ttl_impressions_7d as 'Category High Perf Median Total Impressions 7 Days', prev_ctr_7d as 'Click Through Rate 7 Days',
        categ_med_ctr_7d as 'Category High Perf Median Click Through Rate 7 Days', prev_conversion_7d as 'Conversion Rate 7 days',
        categ_med_conversion_7d as 'Category High Perf Median Conversion Rate 7 Days', prev_item_views_30d as 'Item Views 30 Days',
        prev_oos_item_views_30d as 'Out Of Stock Item Views 30 Days', prev_ttl_impressions_30d as 'Total Impressions 30 Days',
        categ_med_item_views_30d as 'Category High Perf Median Item Views 30 Days', categ_med_ttl_impressions_30d as
        'Category High Perf Median Total Impressions 30 Days', prev_ctr_30d as 'Click Through Rate 30 Days',
        categ_med_ctr_30d as 'Category High Perf Median Click Through Rate 30 Days', prev_conversion_30d as
        'Conversion Rate 30 days',  categ_med_conversion_30d as 'Category High Perf Median Conversion Rate 30 Days',
        prev_item_views_90d as 'Item Views 90 Days', prev_oos_item_views_90d as 'Out Of Stock Item Views 90 Days',
        prev_ttl_impressions_90d as 'Total Impressions 90 Days', categ_med_item_views_90d as 'Category High Perf Median Item Views 90 Days',
        categ_med_ttl_impressions_90d as 'Category High Perf Median Total Impressions 90 Days', prev_ctr_90d as 'Click Through Rate 90 Days',
        categ_med_ctr_90d as 'Category High Perf Median Click Through Rate 90 Days', prev_conversion_90d as 'Conversion Rate 90 days',
        categ_med_conversion_90d as 'Category High Perf Median Conversion Rate 90 Days',
        is_promo as 'On Promotions', rpt_dt as 'Last Updated On' from sku_audit_daily where base_item_id in ",
        item_ids,
        " or catlg_item_id in",
        item_ids
      )
    
    
    rs <- dbSendQuery(channel, query)
    
    # update progress
    progress$inc(80)
    
    # fetch data from mySQL
    data <- fetch(rs, n = -1)
    
    
    # disconnect mySQL
    dbDisconnect(channel)
    
    
    data$'Item ID' <- as.integer(data$'Item ID')
    data$'Base Item' <- as.integer(data$'Base Item')
    
    
    drop_perf_7d <-  c("item_performance_7d",
                       "action_7d")
    
    drop_perf_30d <-  c("item_performance_30d",
                        "action_30d")
    
    drop_perf_90d <-  c("item_performance_90d",
                        "action_90d")
    
    drop_price <- c(
      "Current Price",
      "Current Avg Cost",
      "Special Price Desc",
      "MAP",
      "Current WM Price",
      "Minimum MP Price",
      "Maximum MP Price",
      "Avg MP Price",
      "Store Price",
      "Current Lowest Competitor Price",
      "Current Lowest Priced Competitor Name",
      "Smart Basket ID",
      "Smart Basket Name",
      "Objective",
      "Is Auto Price on?"
    )
    
    
    drop_inventory <- c(
      "Current Qty in Stock",
      "Current On Hand Qty",
      "Store Inventory",
      "On Order Quantity",
      "Min Must Arrive by Date",
      "Max Must Arrive by Date",
      "MP Quantity in Stock",
      "Variants Stock Status",
      "Total Inventory"
    )
    
    
    drop_sales <- c(
      "Retail Sales Yesterday",
      "Unit Sales Yesterday",
      "Market Place Sales Yesterday",
      "Market Place Unit Sales Yesterday",
      "Ship to Store Sales Yesterday",
      "Ship to Store Unit Sales Yesterday",
      "Ship to Home Sales Yesterday",
      "Ship to Home Unit Sales Yesterday",
      "Ship from Store Sales Yesterday",
      "Ship from Store Unit Sales Yesterday",
      "PUT Sales Yesterday",
      "PUT Unit Sales Yesterday",
      "IMU Item % Yesterday",
      "IMU Sales Yesterday"
    )
    
    
    drop_sales_7d <- c(
      "Retail Sales 7 Days",
      "Unit Sales 7 Days",
      "Market Place Sales 7 Days",
      "Market Place Unit Sales 7 Days",
      "Ship to Store Sales 7 Days",
      "Ship to Store Unit Sales 7 Days",
      "Ship to Home Sales 7 Days",
      "Ship to Home Unit Sales 7 Days",
      "Ship from Store Sales 7 Days",
      "Ship from Store Unit Sales 7 Days",
      "PUT Sales 7 Days",
      "PUT Unit Sales 7 Days",
      "IMU Item % 7 Days",
      "IMU Sales 7 Days"
    )
    
    drop_sales_30d <- c(
      "Retail Sales 30 Days",
      "Unit Sales 30 Days",
      "Market Place Sales 30 Days",
      "Market Place Unit Sales 30 Days",
      "Ship to Store Sales 30 Days",
      "Ship to Store Unit Sales 30 Days",
      "Ship to Home Sales 30 Days",
      "Ship to Home Unit Sales 30 Days",
      "Ship from Store Sales 30 Days",
      "Ship from Store Unit Sales 30 Days",
      "PUT Sales 30 Days",
      "PUT Unit Sales 30 Days",
      "IMU Item % 30 Days",
      "IMU Sales 30 Days"
    )
    
    drop_sales_90d <- c(
      "Retail Sales 90 Days",
      "Unit Sales 90 Days",
      "Market Place Sales 90 Days",
      "Market Place Unit Sales 90 Days",
      "Ship to Store Sales 90 Days",
      "Ship to Store Unit Sales 90 Days",
      "Ship to Home Sales 90 Days",
      "Ship to Home Unit Sales 90 Days",
      "Ship from Store Sales 90 Days",
      "Ship from Store Unit Sales 90 Days",
      "PUT Sales 90 Days",
      "PUT Unit Sales 90 Days",
      "IMU Item % 90 Days",
      "IMU Sales 90 Days"
    )
    
    drop_buybox <-  c(
      "BuyBox Winner 1 Yesterday",
      "Buy Box Winner 1 % Times Won",
      "BuyBox Winner 2 Yesterday",
      "Buy Box Winner 2 % Times Won",
      "BuyBox Winner 3 Yesterday",
      "Buy Box Winner 3 % Times Won"
    )
    
    drop_amazon <- c(
      "Amazon Item Name",
      "Amazon Number of Reviews",
      "Amazon Avg Rating",
      "Amazon List price",
      "Amazon Best Seller?"
    )
    
    drop_traffic <- c(
      "Item Views Yesterday",
      "Out of Stock Item Views Yesterday",
      "Total Impressions Yesterday"
    )
    
    drop_traffic_7d <- c(
      "Item Views 7 Days",
      "Out Of Stock Item Views 7 Days",
      "Total Impressions 7 Days",
      "Category High Perf Median Item Views 7 Days",
      "Category High Perf Median Total Impressions 7 Days",
      "Click Through Rate 7 Days",
      "Category High Perf Median Click Through Rate 7 Days",
      "Conversion Rate 7 days",
      "Category High Perf Median Conversion Rate 7 Days"
    )
    
    
    drop_traffic_30d <- c(
      "Item Views 30 Days",
      "Out Of Stock Item Views 30 Days",
      "Total Impressions 30 Days",
      "Category High Perf Median Item Views 30 Days",
      "Category High Perf Median Total Impressions 30 Days",
      "Click Through Rate 30 Days",
      "Category High Perf Median Click Through Rate 30 Days",
      "Conversion Rate 30 days",
      "Category High Perf Median Conversion Rate 30 Days"
    )
    
    drop_traffic_90d <- c(
      "Item Views 90 Days",
      "Out Of Stock Item Views 90 Days",
      "Total Impressions 90 Days",
      "Category High Perf Median Item Views 90 Days",
      "Category High Perf Median Total Impressions 90 Days",
      "Click Through Rate 90 Days",
      "Category High Perf Median Click Through Rate 90 Days",
      "Conversion Rate 90 days",
      "Category High Perf Median Conversion Rate 90 Days"
    )
    
    
    if (input$perfBasis_Input == 1) {
      names(data)[names(data) == "item_performance_7d"] <-
        "Item.Performance"
      names(data)[names(data) == "action_7d"] <-
        "Recommended.Action"

      # data <- data[,!(names(data) %in% drop_perf_30d)]
      # data <- data[,!(names(data) %in% drop_perf_90d)]

    } else if (input$perfBasis_Input == 2) {
      names(data)[names(data) == "item_performance_30d"] <-
        "Item.Performance"
      names(data)[names(data) == "action_30d"] <-
        "Recommended.Action"

      # data <- data[,!(names(data) %in% drop_perf_7d)]
      # data <- data[,!(names(data) %in% drop_perf_90d)]

    } else {
      names(data)[names(data) == "item_performance_90d"] <-
        "Item.Performance"
      names(data)[names(data) == "action_90d"] <-
        "Recommended.Action"

      # data <- data[,!(names(data) %in% drop_perf_7d)]
      # data <- data[,!(names(data) %in% drop_perf_30d)]

    }
    
    
    # create filtered table based on inputs
    filtered <-
      reactive({
        if (is.null(input$SupDeptInput)) {
          return(NULL)
        }
        
        if (is.null(input$DeptInput)) {
          return(NULL)
        }
        
        if (is.null(input$CategInput)) {
          return(NULL)
        }
        
        if (is.null(input$ItemPerfInput)) {
          return(NULL)
        }
        
        if (is.null(input$ItemPublishInput)) {
          return(NULL)
        }
        
        if (is.null(input$MerchActInput)) {
          return(NULL)
        }
        
        
        if (input$SupDeptInput != "All") {
          data <- data[data$Super.Department == input$SupDeptInput,]
          
        }
        
        if (input$DeptInput != "All") {
          data <- data[data$Department == input$DeptInput,]
          
        }
        
        if (input$CategInput != "All") {
          data <- data[data$Category == input$CategInput,]
          
        }
        
        data <- data[data$WM.MP %in% input$wm_mpInput,]
        
        if (input$ItemPublishInput != "All") {
          data <- data[data$Item.Display.Status %in% input$ItemPublishInput,]
          
        }
        
        if (input$MerchActInput != "All") {
          data <- data[data$Recommended.Action %in% input$MerchActInput,]
          
        }
        
        data <-
          data[data$Item.Performance %in% input$ItemPerfInput,]
        
        
        if (input$perfBasis_Input == 1) {
          names(data)[names(data) == "item_performance_7d"] <-
            "Item.Performance"
          names(data)[names(data) == "action_7d"] <-
            "Recommended.Action"
          
          # data <- data[,!(names(data) %in% drop_perf_30d)]
          # data <- data[,!(names(data) %in% drop_perf_90d)]
          data <- data[,!(names(data) %in% drop_sales_30d)]
          data <- data[,!(names(data) %in% drop_sales_90d)]
          data <- data[,!(names(data) %in% drop_traffic_30d)]
          data <- data[,!(names(data) %in% drop_traffic_90d)]
          
          
          if (!(1 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_inventory)]
            
          }
          
          
          if (!(2 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_price)]
            
          }
          
          
          if (!(3 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_sales)]
            data <- data[,!(names(data) %in% drop_sales_7d)]
          }
          
          if (!(4 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_traffic)]
            data <- data[,!(names(data) %in% drop_traffic_7d)]
            
          }
          
          if (!(5 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_amazon)]
            
          }
          
          if (!(6 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_buybox)]
            
          }
          
          data <- data
          
        } else if (input$perfBasis_Input == 2) {
          
          browser()
          names(data)[names(data) == "item_performance_30d"] <-
            "Item.Performance"
          names(data)[names(data) == "action_30d"] <-
            "Recommended.Action"
          
          # data <- data[,!(names(data) %in% drop_perf_7d)]
          # data <- data[,!(names(data) %in% drop_perf_90d)]
          data <- data[,!(names(data) %in% drop_sales_7d)]
          data <- data[,!(names(data) %in% drop_sales_90d)]
          data <- data[,!(names(data) %in% drop_traffic_7d)]
          data <- data[,!(names(data) %in% drop_traffic_90d)]
          
          if (!(1 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_inventory)]
            
          }
          
          
          if (!(2 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_price)]
            
          }
          
          
          if (!(3 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_sales)]
            data <- data[,!(names(data) %in% drop_sales_30d)]
          }
          
          if (!(4 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_traffic)]
            data <- data[,!(names(data) %in% drop_traffic_30d)]
            
          }
          
          if (!(5 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_amazon)]
            
          }
          
          if (!(6 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_buybox)]
            
          }
          
          data <- data
          
        } else {
          names(data)[names(data) == "item_performance_90d"] <-
            "Item.Performance"
          names(data)[names(data) == "action_90d"] <-
            "Recommended.Action"
          
          # data <- data[,!(names(data) %in% drop_perf_7d)]
          # data <- data[,!(names(data) %in% drop_perf_30d)]
          data <- data[,!(names(data) %in% drop_sales_7d)]
          data <- data[,!(names(data) %in% drop_sales_30d)]
          data <- data[,!(names(data) %in% drop_traffic_7d)]
          data <- data[,!(names(data) %in% drop_traffic_30d)]
          
          
          if (!(1 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_inventory)]
            
          }
          
          
          if (!(2 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_price)]
            
          }
          
          
          if (!(3 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_sales)]
            data <- data[,!(names(data) %in% drop_sales_90d)]
          }
          
          if (!(4 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_traffic)]
            data <- data[,!(names(data) %in% drop_traffic_90d)]
            
          }
          
          if (!(5 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_amazon)]
            
          }
          
          if (!(6 %in% input$column_Input)) {
            data <- data[,!(names(data) %in% drop_buybox)]
            
          }
          
          data <- data
          
        }
      })
    
    # Render super department select input
    output$SupDeptOutput <- renderUI({
      selectInput("SupDeptInput",
                  "Super Department",
                  c("All", sort(unique(
                    data$Super.Department
                  ))),
                  selected = 'All')
    })
    
    
    # Render department select input
    output$DeptOutput <- renderUI({
      if (input$SupDeptInput != "All") {
        data <- data[data$Super.Department == input$SupDeptInput,]
        
      }
      
      selectInput("DeptInput",
                  "Department",
                  c("All", sort(unique(
                    data$Department
                  ))),
                  selected = "All")
      
    })
    
    
    # Render category select input
    
    output$CategOutput <- renderUI({
      if (input$DeptInput != "All") {
        data <- data[data$Department == input$DeptInput,]
        
      }
      
      selectInput("CategInput",
                  "Category",
                  c("All", sort(unique(
                    data$Category
                  ))),
                  selected = "All")
      
    })
    
    # Render WM or MP checkbox input
    output$wm_mpOutput <- renderUI({
      if (input$CategInput != "All") {
        data <- data[data$Category == input$CategInput,]
        
      }
      
      wm_mpOptions <-
        sort(unique(data$WM.MP))
      
      checkboxGroupInput(
        "wm_mpInput",
        "WM MP Indicator",
        wm_mpOptions,
        selected = wm_mpOptions,
        inline = TRUE
      )
    })
    
    # Render Item Publish input
    
    output$ItemPublishOutput <- renderUI({
      data <- data[data$WM.MP %in% input$wm_mpInput,]
      
      selectizeInput(
        "ItemPublishInput",
        "Item Publish Status",
        c("All", sort(unique(
          data$Item.Display.Status
        ))),
        selected = "All",
        multiple = TRUE,
        options = list(maxOptions = 6)
      )
      
    })
    
    # Render merchant action select input
    output$MerchActOutput <- renderUI({
      if (input$ItemPublishInput != "All") {
        data <- data[data$Item.Display.Status %in% input$ItemPublishInput,]
        
      }
      
      selectizeInput(
        "MerchActInput",
        "Recommended Actions",
        c("All", sort(unique(
          data$Recommended.Action
        ))),
        selected = "All",
        multiple = TRUE,
        options = list(maxOptions = 7)
      )
    })
    
    # Render item performance checkbox input
    
    output$ItemPerfOutput <- renderUI({
      if (input$MerchActInput != "All") {
        data <- data[data$Recommended.Action %in% input$MerchActInput,]
        
      }
      
      ItemPerfOptions <-
        sort(unique(data$Item.Performance))
      
      
      checkboxGroupInput(
        "ItemPerfInput",
        "Item Performance",
        ItemPerfOptions,
        selected = ItemPerfOptions,
        inline = TRUE
      )
      
    })
    # generate item performance pie chart
    output$pie1 <- renderPlotly({
      if (is.null(filtered())) {
        return()
      }
      browser()
      data <- filtered()
      
      # create table of counts
      tab <- count(data, Item.Performance)
      
      tab$item_perf_ord <-
        factor(
          tab$Item.Performance,
          levels = c('High Performers', 'Underperformers', 'No Sales')
        )
      
      # sort the factor table
      tab <- arrange(tab, item_perf_ord)
      
      # create axis property variables
      ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      
      title_main <-
        if (input$perfBasis_Input == 1) {
          "7-day Performance by SKU Count"
        }
      else if (input$perfBasis_Input == 2) {
        "30-day Performance by SKU Count"
      }
      else if (input$perfBasis_Input == 3) {
        "90-day Performance by SKU Count"
      }
      
      tab$n1 <- formatC(tab$n, format = "d", big.mark = ',')
      
      # generate plot
      plot_ly(
        labels = tab$item_perf_ord,
        values = tab$n,
        text = tab$n1,
        textinfo = "percent+text",
        hoverinfo = "percent+text",
        textfont = list(size = 18, color = "white"),
        marker = list(
          colors = ifelse(
            tab$item_perf_ord == "High Performers",
            "olivedrab",
            ifelse(
              tab$item_perf_ord == "Underperformers",
              "darkorange",
              "steelblue"
            )
          )
        ),
        sort = FALSE,
        type = "pie"
      ) %>%
        layout(
          title = title_main,
          xaxis = ax,
          yaxis = ax,
          legend = list(font = list(size = 18)),
          titlefont = list(size = 18),
          margin = list(t = 50)
        )
      
    })
    
    
    # generate department/category item performance
    output$barplot1 <- renderPlotly({
      if (is.null(filtered())) {
        return()
      }
      
      data <- filtered()
      
      if (input$CategInput != "All") {
        # prepare count table for subcategories
        tab <-
          data %>%  group_by(Sub.Category, Item.Performance) %>%  summarise(Count = n())
        
        tab$item_perf_ord <-
          factor(
            tab$Item.Performance,
            levels = c('High Performers', 'Underperformers', 'No Sales')
          )
        
        
        # sort the factor table
        tab <- arrange(tab, item_perf_ord)
        
        # wrap the labels
        tab$Sub.Category <- sapply(
          tab$Sub.Category,
          FUN = function(x) {
            paste(strwrap(x, width = 30), collapse = "<br>")
          }
        )
        
        title_subcat <-
          if (input$perfBasis_Input == 1) {
            "7-day Sub-Category Performance"
          }
        else if (input$perfBasis_Input == 2) {
          "30-day Sub-Category Performance"
        }
        else if (input$perfBasis_Input == 3) {
          "90-day Sub-Category Performance"
        }
        
        tab$Sub.Category <-
          factor(tab$Sub.Category, levels = unique(tab$Sub.Category)[order(tab$Count, decreasing = FALSE)])
        # generate plot
        plot_ly(
          x = tab$Count,
          y = tab$Sub.Category,
          color = tab$item_perf_ord,
          marker = list(
            color = ifelse(
              tab$item_perf_ord == "High Performers",
              "olivedrab",
              ifelse(
                tab$item_perf_ord == "Underperformers",
                "darkorange",
                "steelblue"
              )
            )
          ),
          type = "bar",
          orientation = 'h'
        ) %>%
          layout(
            title = title_subcat,
            titlefont = list(size = 18),
            barmode = "stack",
            xaxis = list(
              title = "SKU Count",
              titlefont = list(size = 18),
              tickfont = list(size = 14)
            ),
            yaxis = list(title = "",
                         tickfont = list(size = 16)),
            showlegend = FALSE,
            margin = list(l = 240, t = 50)
          )
        
      } else if (input$DeptInput != "All") {
        # prepare count table for categories
        tab <-
          data %>%  group_by(Category, Item.Performance) %>%  summarise(Count = n())
        
        tab$item_perf_ord <-
          factor(
            tab$Item.Performance,
            levels = c('High Performers', 'Underperformers', 'No Sales')
          )
        # sort the factor table
        tab <- arrange(tab, item_perf_ord)
        
        # wrap the labels
        tab$Category <- sapply(
          tab$Category,
          FUN = function(x) {
            paste(strwrap(x, width = 30), collapse = "<br>")
          }
        )
        
        title_cat <-
          if (input$perfBasis_Input == 1) {
            "7-day Category Performance"
          }
        else if (input$perfBasis_Input == 2) {
          "30-day Category Performance"
        }
        else if (input$perfBasis_Input == 3) {
          "90-day Category Performance"
        }
        
        
        tab$Category <-
          factor(tab$Category, levels = unique(tab$Category)[order(tab$Count, decreasing = FALSE)])
        # generate plot
        plot_ly(
          x = tab$Count,
          y = tab$Category,
          color = tab$item_perf_ord,
          marker = list(
            color = ifelse(
              tab$item_perf_ord == "High Performers",
              "olivedrab",
              ifelse(
                tab$item_perf_ord == "Underperformers",
                "darkorange",
                "steelblue"
              )
            )
          ),
          type = "bar",
          orientation = 'h'
        ) %>%
          layout(
            title = title_cat,
            titlefont = list(size = 18),
            barmode = "stack",
            xaxis = list(
              title = "SKU Count",
              titlefont = list(size = 18),
              tickfont = list(size = 14)
            ),
            yaxis = list(title = "",
                         tickfont = list(size = 16)),
            showlegend = FALSE,
            margin = list(l = 240, t = 50)
          )
        
      } else if (input$SupDeptInput != "All") {
        # prepare the count table
        tab <-
          data %>%  group_by(Department, Item.Performance) %>%  summarise(Count = n())
        
        
        tab$item_perf_ord <-
          factor(
            tab$Item.Performance,
            levels = c('High Performers', 'Underperformers', 'No Sales')
          )
        # sort the factor table
        tab <- arrange(tab, item_perf_ord)
        
        # wrap the labels
        tab$Department <- sapply(
          tab$Department,
          FUN = function(x) {
            paste(strwrap(x, width = 30), collapse = "<br>")
          }
        )
        title_dept <-
          if (input$perfBasis_Input == 1) {
            "7-day Department Performance"
          }
        else if (input$perfBasis_Input == 2) {
          "30-day Department Performance"
        }
        else if (input$perfBasis_Input == 3) {
          "90-day Department Performance"
        }
        
        tab$Department <-
          factor(tab$Department, levels = unique(tab$Department)[order(tab$Count, decreasing = FALSE)])
        # generate plot
        plot_ly(
          x = tab$Count,
          y = tab$Department,
          color = tab$item_perf_ord,
          marker = list(
            color = ifelse(
              tab$item_perf_ord == "High Performers",
              "olivedrab",
              ifelse(
                tab$item_perf_ord == "Underperformers",
                "darkorange",
                "steelblue"
              )
            )
          ),
          type = "bar",
          orientation = 'h'
        ) %>%
          layout(
            title = title_dept,
            titlefont = list(size = 18),
            barmode = "stack",
            xaxis = list(
              title = "SKU Count",
              titlefont = list(size = 18),
              tickfont = list(size = 14)
            ),
            yaxis = list(title = "",
                         tickfont = list(size = 16)),
            showlegend = FALSE,
            margin = list(l = 240, t = 50)
          )
        
      }
      else {
        # prepare the count table
        tab <-
          data %>%  group_by(Super.Department, Item.Performance) %>%  summarise(Count = n())
        
        # create ordered factor for item performance
        tab$item_perf_ord <-
          factor(
            tab$Item.Performance,
            levels = c('High Performers', 'Underperformers', 'No Sales')
          )
        
        # sort the table
        tab <- arrange(tab, item_perf_ord)
        
        # wrap the labels
        tab$Super.Department <- sapply(
          tab$Super.Department,
          FUN = function(x) {
            paste(strwrap(x, width = 30), collapse = "<br>")
          }
        )
        
        title_supdept <-
          if (input$perfBasis_Input == 1) {
            "7-day Super-Department Performance"
          }
        else if (input$perfBasis_Input == 2) {
          "30-day Super-Department Performance"
        }
        else if (input$perfBasis_Input == 3) {
          "90-day Super-Department Performance"
        }
        
        tab$Super.Department <-
          factor(tab$Super.Department,
                 levels = unique(tab$Super.Department)[order(tab$Count, decreasing = FALSE)])
        
        # generate plot
        plot_ly(
          x = tab$Count,
          y = tab$Super.Department,
          color = tab$item_perf_ord,
          marker = list(
            color = ifelse(
              tab$item_perf_ord == "High Performers",
              "olivedrab",
              ifelse(
                tab$item_perf_ord == "Underperformers",
                "darkorange",
                "steelblue"
              )
            )
          ),
          type = "bar",
          orientation = 'h'
        ) %>%
          layout(
            title = title_supdept,
            titlefont = list(size = 18),
            barmode = "stack",
            xaxis = list(
              title = "SKU Count",
              titlefont = list(size = 18),
              tickfont = list(size = 16)
            ),
            yaxis = list(title = "",
                         tickfont = list(size = 16)),
            showlegend = FALSE,
            margin = list(l = 240, t = 50)
          )
      }
      
    })
    
    
    # generate Actions barplot
    output$barplot2 <- renderPlotly({
      if (is.null(filtered())) {
        return()
      }
      
      data <- filtered()
      
      # sort ordered table
      tab <- count(data, Recommended.Action)
      
      # we do not want to show 'no action' item count on the chart
      tab <- filter(tab, Recommended.Action != "No Action")
      
      # wrap action labels for display
      tab$Recommended.Action <- sapply(
        tab$Recommended.Action,
        FUN = function(x) {
          paste(strwrap(x, width = 26), collapse = "<br>")
        }
      )
      
      tab$Recommended.Action <-
        factor(tab$Recommended.Action,
               levels = unique(tab$Recommended.Action)[order(tab$n, decreasing = FALSE)])
      
      title_actions <-
        if (input$perfBasis_Input == 1) {
          "Recommended Actions (excludes No Action Items)"
        }
      else if (input$perfBasis_Input == 2) {
        "Recommended Actions (excludes No Action Items)"
      }
      else if (input$perfBasis_Input == 3) {
        "Recommended Actions (excludes No Action Items)"
      }
      cols <-
        c("darkcyan")
      
      # Generate plot
      plot_ly(
        x = tab$n,
        y = tab$Recommended.Action,
        marker = list(color = cols),
        type = 'bar',
        orientation = 'h'
      ) %>%
        layout(
          title = title_actions,
          titlefont = list(size = 18),
          xaxis = list(
            title = "SKU Count",
            titlefont = list(size = 18),
            tickfont = list(size = 16)
          ),
          yaxis = list(
            title = "",
            titlefont = list(size = 18),
            tickfont = list(size = 16)
          ),
          margin = list(l = 220, t = 50)
        )
      
    })
    
    output$rpt_dt <- renderText({
      if (is.null(filtered())) {
        return()
      }
      
      
      data <- filtered()
      
      paste("Last Updated on:", data$'Last Updated On'[1])
      
    })
    
    output$rpt_dt2 <- renderText({
      if (is.null(filtered())) {
        return()
      }
      
      
      data <- filtered()
      
      paste("Last Updated on:", data$'Last Updated On'[1])
      
    })
    
    # generate filtered table for display
    output$contents <- renderDataTable({
      if (is.null(filtered())) {
        return()
      }
      Sys.setlocale('LC_ALL', 'C')
      
      if (input$perfBasis_Input == 1) {
        data_tmp <- filtered()
        
        # rename some of the columns for better view
        names(data_tmp)[names(data_tmp) == "Recommended.Action"] <-
          "Recommended Action"
        names(data_tmp)[names(data_tmp) == "Super.Department"] <-
          "Super Department"
        names(data_tmp)[names(data_tmp) == "Sub.Category"] <-
          "Sub-Category"
        names(data_tmp)[names(data_tmp) == "Item.Performance"] <-
          "Item Performance Last 7 Days"
        names(data_tmp)[names(data_tmp) == "WM.MP"] <-
          "WM/MP/Shared"
        names(data_tmp)[names(data_tmp) == "Item.Display.Status"] <-
          "Item Publish Status"
        
        # generate final item table display
        data_display <-
          data_tmp[c(
            "Item ID",
            "Base Item",
            "Product",
            "Item Performance Last 7 Days",
            "Recommended Action",
            "Item Publish Status",
            "Super Department",
            "Department",
            "Category",
            "Sub-Category",
            "WM/MP/Shared"
          )]
      } else if (input$perfBasis_Input == 2) {
        data_tmp <- filtered()
        
        # rename some of the columns for better view
        names(data_tmp)[names(data_tmp) == "Recommended.Action"] <-
          "Recommended Action"
        names(data_tmp)[names(data_tmp) == "Super.Department"] <-
          "Super Department"
        names(data_tmp)[names(data_tmp) == "Sub.Category"] <-
          "Sub-Category"
        names(data_tmp)[names(data_tmp) == "Item.Performance"] <-
          "Item Performance Last 30 Days"
        names(data_tmp)[names(data_tmp) == "WM.MP"] <-
          "WM/MP/Shared"
        names(data_tmp)[names(data_tmp) == "Item.Display.Status"] <-
          "Item Publish Status"
        
        # generate final item table display
        data_display <-
          data_tmp[c(
            "Item ID",
            "Base Item",
            "Product",
            "Item Performance Last 30 Days",
            "Recommended Action",
            "Item Publish Status",
            "Super Department",
            "Department",
            "Category",
            "Sub-Category",
            "WM/MP/Shared"
          )]
      } else if (input$perfBasis_Input == 3) {
        data_tmp <- filtered()
        
        # rename some of the columns for better view
        names(data_tmp)[names(data_tmp) == "Recommended.Action"] <-
          "Recommended Action"
        names(data_tmp)[names(data_tmp) == "Super.Department"] <-
          "Super Department"
        names(data_tmp)[names(data_tmp) == "Sub.Category"] <-
          "Sub-Category"
        names(data_tmp)[names(data_tmp) == "Item.Performance"] <-
          "Item Performance Last 90 Days"
        names(data_tmp)[names(data_tmp) == "WM.MP"] <-
          "WM/MP/Shared"
        names(data_tmp)[names(data_tmp) == "Item.Display.Status"] <-
          "Item Publish Status"
        
        # generate final item table display
        data_display <-
          data_tmp[c(
            "Item ID",
            "Base Item",
            "Product",
            "Item Performance Last 90 Days",
            "Recommended Action",
            "Item Publish Status",
            "Super Department",
            "Department",
            "Category",
            "Sub-Category",
            "WM/MP/Shared"
          )]
      }
    })
    
    # download button
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (input$perfBasis_Input == 1) {
          paste("SKU Health Data 7 Days ", Sys.time(), '.csv', sep = '')
        } else if (input$perfBasis_Input == 2) {
          paste("SKU Health Data 30 Days ", Sys.time(), '.csv', sep = '')
        } else {
          paste("SKU Health Data 90 Days ", Sys.time(), '.csv', sep = '')
        }
      },
      
      content = function(file) {
        write.csv(filtered(), file)
      }
    )
  })
}

shinyApp(ui, server)
