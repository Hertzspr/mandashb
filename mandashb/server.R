server <- 
  function(input, output){
    
    # reactive
    
    data_class_input <- reactive({
      data <- 
        data %>% filter(kelas == input$class.input)
    })
    
    cor_class_input <- reactive({
      corp3ph.kelas.p3<-
        corp3ph.kelas.p3 %>% filter(kelas == input$class.input)
    })
    
    data_semester_input1 <- reactive({
      data <- 
        data %>% filter(penilaian_s == input$semester.input)
    })
    
    data_semester_input2 <- reactive({
      df.imputed.s.p3.h.s2 <-
        df.imputed.s.p3.h.s2 %>% filter(penilaian_s %in% input$semester.input)
    })
    
    datacol.class <- reactive({
      
      data_edit_col <-
        data_edit_col %>% filter(Class == input$radio.class.input)
      
    })
    
    cluster <- reactive({
      student_score_profile <- 
        d4kmeans %>% 
        filter(cluster %in% input$cluster.input) %>% 
        group_by(cluster) %>% 
        summarise_all(mean)
    })
    
    cluster_pca <- reactive({

      d4kmeans <- 
      d4kmeans %>% 
        filter(cluster %in% input$cluster.input)
      
      km_x_pca <- PCA(X = d4kmeans, # this table has already added with the cluster
                      quali.sup = 10, 
                      graph=F) 
    })
    
    # 1st plot
    
    output$rata.plot <- renderPlotly({
      
      data_mean_ph <- 
        data_mean_ph %>% 
        filter(penilaian_h == input$harian.input)
      
      rata.plot <- ggplot(data_mean_ph, aes(y = penilaian_h, x = rata2_ph, fill = kelas))+
        geom_col(aes(text = avg), position = position_dodge(width = 1.1))+
        theme_minimal()+
        theme(axis.title.y = element_blank())+
        labs(title = "Average Minor Test Results",
             subtitle = "2020/2021 2nd Semester",
             x = NULL,
             y = NULL,
             fill = NULL)+
        scale_fill_manual(
          values = fcols      
        )+
        xlim(0, 100)+
        coord_flip()
      
      ggplotly(rata.plot, tooltip = "text")
    })
    
    # 2nd plot
    
    
    output$ppp.plot <- renderPlotly({
      
      ppp.plot <- ggplot(data_class_input(), aes(y = materi, 
                                   x = nilai_ppp, 
                                   fill = kelas))+
        geom_col(position = position_dodge(width = 1.2),
                 aes(text = exp))+
        facet_wrap(~ppp)+
        theme_minimal()+
        theme(legend.position = "none",
              axis.title.y = element_blank())+
        labs(title = "Student Assignment Scores in Specific Subject",
             subtitle = "2020/2021 Semester Genap",
             x = "Nilai")+
        scale_fill_manual(
          values = fcols      
        )
      
      ggplotly(ppp.plot, tooltip = "text")
    })
    
    # 3rd plot
    
    output$korplot.materi <- renderPlotly({
      
      corp3ph.kelas.materi <- 
        corp3ph.kelas.materi %>% filter(materi == input$materi.input)
      
      cor.plot.k.m <- 
        ggplot(corp3ph.kelas.materi, 
               aes(x = materi, 
                   y = correlation,
                   text = kelas))+
        geom_col(aes(fill = kelas), 
                 position = position_dodge()
        )+
        theme_minimal()+
        labs(title = "Correlation Value of Material and Minor Test Results",
             x = "Material")+
        scale_fill_manual(
          values = fcols      
        )+
        theme(legend.position = "none",
              axis.title.y = element_blank())+
        coord_flip()+
        ylim(0,1)
      
      ggplotly(cor.plot.k.m, tooltip = "text")
    })
    
    # 4th plot
    
    output$pvh.plot <- renderPlotly({
      
      hxp.plot <- 
        ggplot(data_class_input(), 
               aes(x = nilai_ppp, 
                   y = nilai_ph))+
        geom_jitter(aes(col = kelas,
                        text = exp),
                    width = 0.3, 
                    height = 0.3, 
                    alpha = 0.7)+
        theme_minimal()+
          labs(
             x = "Student Assignment Scores",
             y = "Minor Test Results")+
        scale_color_manual(
          values = fcols      
        )+
        theme(legend.position = "none")
        
      ggplotly(hxp.plot, tooltip = "text")
    })
    
    
    
    # 5th plot
    
    output$korplot.kp <- renderPlotly({
    
      cor.plot.k.p <- 
        ggplot(cor_class_input(), aes(ppp, correlation))+
        geom_col(aes(fill = kelas, text = correlation), 
                 position = position_dodge()
        )+
        theme_minimal()+
        labs(
             subtitle = "Berdasarkan Proyek/Praktek/Portfolio",
             x = "Student Assignment Scores")+
        scale_fill_manual(
          values = fcols      
        )+
        theme(legend.position = "none",
              axis.title.y = element_blank())
      
      ggplotly(cor.plot.k.p, tooltip = "text")
      
    })
    
    # 6th plot
    
    output$dist.semester.plot <- renderPlotly({
      
      plot.semester.k <-
        ggplot(data_semester_input1(), aes(x = nilai, text = exp))+
        geom_histogram(aes(fill = kelas),
                       binwidth = 1,
                       position = "identity")+
        facet_wrap(~penilaian_s + kelas)+
        theme_minimal()+
        theme(legend.position = "none",
              panel.spacing = unit(4, "lines"))+
        labs(
             x = "Scores",
             y = "Count")+
        scale_fill_manual(
          values = fcols      
        )
      
      ggplotly(plot.semester.k, tooltip = "text")
    })
    
    # 7th plot
    
    output$s.p3.h.s.plot <- renderPlotly({
      
      pcols = c(
        "Portofolio" = "#BEEF9E",
        "Praktek" = "#3292C3",
        "Proyek" = "#BCAB79"
      )
      
      p.dens <-
        ggplot(data_semester_input2(), aes(kor_ppp_ph, kor_ph_s)) +
        stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
        geom_point(size = 6, color = "white")+
        geom_point(aes(col=ppp, text = penilaian_s), size = 3)+
        labs(
             x = "Cor. of Student Assignment Scores & Minor Test Results",
             y = "Cor. of Minor and Major Results",
             color = "Student Assignment Scores",
             fill = "Density"
        )+
        theme_minimal()+
        scale_color_manual(values = pcols)+
        scale_fill_gradient(low = "black", high = "white")+
        facet_wrap(~penilaian_s_f)
      
      ggplotly(p.dens, tooltip = "text")
    })
    
    # 8th plot
    
    output$parkord.plot <- renderPlotly({
      
    p.par.nilai <- 
      ggparcoord(datacol.class(),
                 columns = c("Assignment Scores", 
                             "Minor Test Results", 
                             "Major Test Results"),
                 groupColumn = "Class",
                 scale = "uniminmax",
                 order = c(11,12,13),
                 showPoints = TRUE,
                 splineFactor = 4,
                 alphaLines = 0.4)+ 
      scale_color_manual(values = fcols) +
      theme(
        axis.title.x = element_blank()
      )+
      theme_minimal()+
      labs(y = "Scaled with UniMinMax",
           x = NULL,
           col = "Class")
    
    ggplotly(p.par.nilai, tooltip = c("y", "x", "colour") )
    })
    
    # 9th output
    
    output$ppp.s.kortest.table <- DT::renderDataTable({
      uk.ppp.s %>% select(kelas, estimate, p.value, method)
    })
    
    output$ph.s.kortest.table <- DT::renderDataTable({
      uk.ph.s %>% select(kelas, estimate, p.value, method)
    })
    
    # 10th plot
    
    output$varexp.plot <- renderPlot({
      var_df %>%
        mutate(PC = fct_inorder(PC),
               var_rounded = round(var_explained, digits = 2),
               var_cum = cumsum(var_rounded)) %>%
        filter(PC %in% paste0("PC", 1:input$pc.slider)) %>% 
        ggplot(aes(x = PC,
                   y= var_explained, 
                   fill = PC))+
        theme_minimal()+
        geom_col()+ 
        scale_fill_manual(
          values = c(
            "#C6E0FF",
            "#579A9E",
            "#3292C3",
            "#BCAB79",
            "maroon",
            "white",
            "violet",
            "grey",
            "brown",
            "lightyellow"
          )
        )+
        geom_label(aes(label = var_cum))+
        labs(#title = "PC Contributions to Variance Explained",
             x = NULL,
             y = "Var Explained")+
        theme(legend.position = "none")
    })
    
    # 11th plot
    
    output$pc.innards.plot <- renderPlot({

      tidied_pca %>%
      filter(component %in% paste0("PC", 1:input$pc.slider)) %>%
      group_by(component) %>%
      top_n(8, abs(value)) %>%
      ungroup() %>%
      mutate(terms = reorder_within(terms, abs(value), component)) %>%
      ggplot(aes(abs(value), terms, fill = value > 0)) +
      geom_col() +
      facet_wrap(~component, scales = "free_y") +
      scale_y_reordered() +
      labs(
        x = "Absolute Value of Contribution",
        y = NULL, fill = "Positive?"
      )+
      scale_fill_manual(
        values = c("#BEEF9E", "#3292C3")
      )
    })
    
    # 12th plot
    
    output$kmean.profile <- renderPlot({
      ggRadar(
        data=cluster(),
        mapping = aes(colours = cluster),
        ylim = 0.8
      )+theme_minimal()+
        facet_grid(~cluster)+
        theme(legend.position = "none")+
        scale_color_manual(values = c("#BEEF9E", 
                                      "#3292C3",
                                      "#BCAB79"))
    })
    
    # 13th plot
    
    output$kmean.pca.plot <- renderPlot({
      factoextra::fviz_pca_biplot(cluster_pca(),
                      habillage = "cluster",
                      geom.ind = "point", # menampilkan titik observasi saja
                      addEllipses = T, # membuat elips disekitar cluster
                      col.var = "navy")+
        theme(legend.position = "none")+
        labs(title = "Clustering + PCA")+
        scale_color_manual(values = c("#BEEF9E", 
                                      "#3292C3",
                                      "#BCAB79"))
      
    })
    
  }