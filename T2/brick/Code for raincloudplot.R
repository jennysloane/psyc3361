  ###############################################
  
  library(readr)
  library(Hmisc)
  library(RColorBrewer)
  library(reshape2)
  library(here)
  library(assertr)
  library(haven)
  library(psych)
  library(data.table)
  library(tidyverse)
  
  # =========================== creation data =========================== 
  load("merged.rda")
  
  
  merged$smoking_status_others<-as.factor(merged$smoking_status_others)
  merged$smoking_status_othersdi <- revalue(merged$smoking_status_others, c("ext_non"="Nonsmokers", "ext_former"="Nonsmokers", "ext_light"="Smokers", "ext_heavy"="Smokers", "ext_mod"="Smokers", "nonsmoker"="Nonsmokers", "smoker" = "Smokers"))
  
  mergedplot <- merged  %>% 
    select (id, study, estimate_S4, smoking_status_othersdi) %>% 
    filter (study == "MTurk" & !is.na(estimate_S4) & !is.na(smoking_status_othersdi)) %>% 
    distinct()
  # ==================== Plot for Estimated Donation Willingness (%) by Payment ======================
  
  ## define function of geom_flat_volin
  # devtools::install_github(repo = "IndrajeetPatil/ggstatsplot")
  
  "%||%" <- function(a, b) {
    if (!is.null(a))
      a
    else
      b
  }
  
  geom_flat_violin <-
    function(mapping = NULL,
             data = NULL,
             stat = "ydensity",
             position = "dodge",
             trim = FALSE,
             scale = "area",
             show.legend = NA,
             inherit.aes = TRUE,
             ...) {
      ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomFlatViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(trim = trim,
                      scale = scale,
                      ...)
      )
    }
  
  GeomFlatViolin <-
    ggproto(
      "GeomFlatViolin",
      Geom,
      setup_data = function(data, params) {
        data$width <- data$width %||%
          params$width %||% (resolution(data$x, FALSE) * 0.9)
        
        # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
        data %>%
          dplyr::group_by(.data = ., group) %>%
          dplyr::mutate(
            .data = .,
            ymin = min(y),
            ymax = max(y),
            xmin = x,
            xmax = x + width / 2
          )
      },
      
      draw_group = function(data, panel_scales, coord)
      {
        # Find the points for the line to go all the way around
        data <- base::transform(data,
                                xminv = x,
                                xmaxv = x + violinwidth * (xmax - x))
        
        # Make sure it's sorted properly to draw the outline
        newdata <-
          base::rbind(
            dplyr::arrange(.data = base::transform(data, x = xminv), y),
            dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
          )
        
        # Close the polygon: set first and last point the same
        # Needed for coord_polar and such
        newdata <- rbind(newdata, newdata[1,])
        
        ggplot2:::ggname("geom_flat_violin",
                         GeomPolygon$draw_panel(newdata, panel_scales, coord))
      },
      
      draw_key = draw_key_polygon,
      
      default_aes = ggplot2::aes(
        weight = 1,
        colour = "grey20",
        fill = "white",
        size = 0.5,
        alpha = NA,
        linetype = "solid"
      ),
      
      required_aes = c("x", "y")
    )
  # =========================== end function definition ===========================
  
  # define theme
  raincloud_theme = theme(
    text = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32),
    axis.text = element_text(size = 32),
    axis.text.x = element_text(angle = 0, vjust = 0.6),
    legend.title=element_text(size=32),
    legend.text=element_text(size=32),
    legend.position = "right",
    plot.title = element_text(lineheight=.8, face="bold", size = 32),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
  
  # summary stats
  lb <- function(x) mean(x) - sd(x)
  ub <- function(x) mean(x) + sd(x)
  
  sumld<- plyr::ddply(mergedplot, ~smoking_status_othersdi, summarise, mean = mean(estimate_S4), 
                      median = median(estimate_S4), lower = lb(estimate_S4), upper = ub(estimate_S4))
  head(sumld)
  
  ### raincloud plot
  
  # behavior
  png(filename="plot policy support others.png", width = 1100, height = 660)
  g <- ggplot(data = mergedplot, aes(y = estimate_S4, x = smoking_status_othersdi, fill = smoking_status_othersdi)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = 1, adjust = .5) +
    geom_point(aes(y = estimate_S4, color = smoking_status_othersdi), 
               position = position_jitter(width = .10), size = 1, alpha = 1) +
    geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.4) +
    #  geom_hline(aes(yintercept = .66), colour="black") + 
    theme(legend.position = "none") +
    expand_limits(x = 3.25) +
    scale_y_continuous(breaks = pretty(c(0,100), n = 5), limits = c(0,100.10)) +
    xlab(label = "") + 
    ylab(label = "\nEstimated policy support in others (%)") +
    guides(fill = FALSE) +  guides(color = FALSE) +
    scale_color_brewer(type = "div", palette = "Set2") +
    scale_fill_brewer(type = "div", palette = "Set2") +
    coord_flip() +
    theme_bw() +
    raincloud_theme
  g
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # =========================== Plot for paid vs. unpaid =========================== 
  ###############################################
  
  library(readr)
  library(Hmisc)
  library(RColorBrewer)
  library(reshape2)
  library(here)
  library(assertr)
  library(haven)
  library(psych)
  library(data.table)
  library(tidyverse)
  
  # =========================== creation data =========================== 
  load("merged.rda")
  
  merged$donation_payment_others <- revalue(merged$donation_payment_others, c("paid"="Paid", "unpaid"="Unpaid"))
  
  mergedplot2 <- merged  %>% 
    select (id, estimate_S1, donation_payment_others) %>% 
    filter ( !is.na(estimate_S1) & !is.na(donation_payment_others)) %>% 
    distinct()
  # =========================== function definition ===========================
  
  ## define function of geom_flat_volin
  # devtools::install_github(repo = "IndrajeetPatil/ggstatsplot")
  
  "%||%" <- function(a, b) {
    if (!is.null(a))
      a
    else
      b
  }
  
  geom_flat_violin <-
    function(mapping = NULL,
             data = NULL,
             stat = "ydensity",
             position = "dodge",
             trim = FALSE,
             scale = "area",
             show.legend = NA,
             inherit.aes = TRUE,
             ...) {
      ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomFlatViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(trim = trim,
                      scale = scale,
                      ...)
      )
    }
  
  GeomFlatViolin <-
    ggproto(
      "GeomFlatViolin",
      Geom,
      setup_data = function(data, params) {
        data$width <- data$width %||%
          params$width %||% (resolution(data$x, FALSE) * 0.9)
        
        # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
        data %>%
          dplyr::group_by(.data = ., group) %>%
          dplyr::mutate(
            .data = .,
            ymin = min(y),
            ymax = max(y),
            xmin = x,
            xmax = x + width / 2
          )
      },
      
      draw_group = function(data, panel_scales, coord)
      {
        # Find the points for the line to go all the way around
        data <- base::transform(data,
                                xminv = x,
                                xmaxv = x + violinwidth * (xmax - x))
        
        # Make sure it's sorted properly to draw the outline
        newdata <-
          base::rbind(
            dplyr::arrange(.data = base::transform(data, x = xminv), y),
            dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
          )
        
        # Close the polygon: set first and last point the same
        # Needed for coord_polar and such
        newdata <- rbind(newdata, newdata[1,])
        
        ggplot2:::ggname("geom_flat_violin",
                         GeomPolygon$draw_panel(newdata, panel_scales, coord))
      },
      
      draw_key = draw_key_polygon,
      
      default_aes = ggplot2::aes(
        weight = 1,
        colour = "grey20",
        fill = "white",
        size = 0.5,
        alpha = NA,
        linetype = "solid"
      ),
      
      required_aes = c("x", "y")
    )
  # =========================== end function definition ===========================
  
  # define theme
  raincloud_theme = theme(
    text = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32),
    axis.text = element_text(size = 32),
    axis.text.x = element_text(angle = 0, vjust = 0.6),
    legend.title=element_text(size=32),
    legend.text=element_text(size=32),
    legend.position = "right",
    plot.title = element_text(lineheight=.8, face="bold", size = 32),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
  
  # summary stats
  lb <- function(x) mean(x) - sd(x)
  ub <- function(x) mean(x) + sd(x)
  
  sumld<- plyr::ddply(mergedplot2, ~donation_payment_others, summarise, mean = mean(estimate_S1), 
                      median = median(estimate_S1), lower = lb(estimate_S1), upper = ub(estimate_S1))
  head(sumld)
  
  ### raincloud plot
  
  # behavior
  png(filename="plot donation others.png", width = 1100, height = 660)
  g <- ggplot(data = mergedplot2, aes(y = estimate_S1, x = donation_payment_others, fill = donation_payment_others)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = 1, adjust = .5) +
    geom_point(aes(y = estimate_S1, color = donation_payment_others), 
               position = position_jitter(width = .10), size = 1, alpha = 1) +
    geom_boxplot(width = .15, outlier.shape = NA, alpha = 0.4) +
    #  geom_hline(aes(yintercept = .66), colour="black") + 
    theme(legend.position = "none") +
    expand_limits(x = 3.25) +
    scale_y_continuous(breaks = pretty(c(0,100), n = 5), limits = c(0,100.10)) +
    xlab(label = "") + 
    ylab(label = "\nEstimated donation in others (%)") +
    guides(fill = FALSE) +  guides(color = FALSE) +
    scale_color_brewer(type = "div", palette = "Set2") +
    scale_fill_brewer(type = "div", palette = "Set2") +
    coord_flip() +
    theme_bw() +
    raincloud_theme
  g
  dev.off()
  