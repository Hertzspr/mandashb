header <- dashboardHeader(title = "| MAN 3 Lebak | Indonesian Language Class | 2020/2021 | 2nd Semester |",
                          titleWidth = 700)

sidebar <- dashboardSidebar(collapsed = F, 
  sidebarMenu(
    menuItem(text = "Minor Test Exploration",
             tabName = "analysis_1"),
    menuItem(text = "Major Test Exploration",
             tabName = "analysis_2"),
    menuItem(text = "Unsupervised Learning",
             tabName = "unspv_learning")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "analysis_1",
            fluidPage(valueBox(value = "Classes",
                              subtitle = "XI IPS 1 - XI IPS 2 - XI MIPA", 
                              icon = icon("book-open-readerbusiness-time"), 
                              color = "olive",
                              width = 3),
                     valueBox(value = 82,
                              subtitle = "Students",
                              icon = icon("person-chalkboard"), 
                              color = "olive",
                              width = 3),
                     valueBox(subtitle = "4 Minor Tests", 
                              value = "Harian 1 - 4", 
                              icon = icon("calendar-days"), 
                              color = "olive",
                              width = 3),
                     valueBox(subtitle ="2 Major Tests", 
                              value = "PHBO & PAT",  
                              icon = icon("school")
                              , color = "olive",
                              width = 3),
                     # 1st plot
                     box(selectInput(inputId = "harian.input", 
                                     choices = unique(data_mean_ph$penilaian_h), 
                                     label = "Minor Test",
                                     selected = "Harian 1"),
                         plotlyOutput(outputId = "rata.plot"),
                         width = 4),
                     # 2nd plot
                     box(selectInput(inputId = "class.input", 
                                     choices = unique(data$kelas), 
                                     label = "Class",
                                     selected = "XI IPS 1"),
                         plotlyOutput(outputId = "ppp.plot"),
                         width = 8),
                     # 3rd plot
                     box(selectInput(inputId = "materi.input", 
                                     choices = unique(corp3ph.kelas.materi$materi), 
                                     label = "Minor Test Material",
                                     selected = "Drama"),
                         plotlyOutput(outputId = "korplot.materi"),
                         width = 4),
                     # 4th plot
                     box(title = "Student Assignment Scores & Minor Test Results",
                         plotlyOutput(outputId = "pvh.plot"),
                         width = 4),
                     
                     # 5th plot
                     box(title = "Correlation Value of Student Assignment Scores and Minor Test Results",
                         plotlyOutput(outputId = "korplot.kp"),
                         width = 4)
                     )),
    tabItem(tabName = "analysis_2",
            fluidPage(
              box(checkboxGroupInput(inputId = "semester.input",
                                     label = "Major Tests",
                                     choices = unique(data$penilaian_s),
                                     selected = "PHBO",
                                     inline = T),
                  width = 12,
                  height = 65), # height in pixel
              # 6th plot
              box(title = "Major Test Scores Distribution",
                plotlyOutput(outputId = "dist.semester.plot"),
                width = 6),
              
              # 7th plot
              box(title = "Relationship of Correlation Values",
                  plotlyOutput(outputId = "s.p3.h.s.plot"),
                  width = 6),
              box(radioButtons(inputId = "radio.class.input",
                               label = "Class",
                               choices = unique(data$kelas),
                               selected = "XI IPS 1",
                               inline = T),
                  width = 6,
                  height = 65), # height in pixel
              box(title = "Correlation Tests",
                  width = 6,
                  height = 65),
              
              # plot 8
              box("Parallel Plot of Assignment Scores and Test Results",
                  plotlyOutput(outputId = "parkord.plot")),
              
              # plot 9
              tabBox(width = 6,
                     height = 445,
                tabPanel(title = "Student Assignment Scores & Major Test Results",
                         dataTableOutput(outputId = "ppp.s.kortest.table"),
                         ),
                tabPanel(title = "Minor Test Results & Major Test Results",
                         dataTableOutput(outputId = "ph.s.kortest.table")))
            )),
    tabItem(tabName = "unspv_learning",
            fluidPage(
              column(width = 6,
                     box(
                         sliderInput(inputId = "pc.slider",
                                     label = "Principal Component Analysis",
                                     min = 1,
                                     max = 6,
                                     value = 6),
                         width = NULL,
                         height = 100),
                     box(title = "Variance Explained by Principal Component",
                         width = NULL,
                         plotOutput(outputId = "varexp.plot")),
                     box(title = "Individual Factor",
                         width = NULL,
                         plotOutput(outputId = "pc.innards.plot"))
                     ),
              column(width = 6,
                     box(width = NULL,
                         checkboxGroupInput(inputId = "cluster.input",
                                            label = "K-means Clustering",
                                            choices = c(1, 2, 3),
                                            selected = c(1, 2, 3),
                                            inline = T)),
                     box(title = "Cluster Profile",
                         width = NULL, 
                         plotOutput(outputId = "kmean.profile")),
                     box(width = NULL,
                         plotOutput(outputId = "kmean.pca.plot"))
                
              )
              
            )
            )
  )
)

dashboardPage(header, sidebar, body, skin = "black")