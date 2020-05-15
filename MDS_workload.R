#load packages
library(tidyverse)
library(gganimate)
library(lubridate)
library(magick)

#load dataframe
workload <- read_csv("data/workload_v2.csv")
workload <- workload %>% select(-`Days from today`)

#' Trim & wrangle the large workload to a given work week, and remove the NA columns
#'
#' @param df 
#' @param startday 
#' @param endday 
#'
#' @return trimmed df
#' @export
#'
#' @examples
trim_week <- function(df, startday, endday){
  df <- slice(df, startday:endday)
  df <- df[,colSums(is.na(df))<nrow(df)]
  df <- gather(df, "course", "progress", -date)
  df <- df %>% 
    separate(date, c("wday", "md", "year"), ",") %>%
    separate(md, c("garbage","month", "day"), " ") %>% 
    select(-garbage) %>% 
    mutate(year = trimws(year)) %>% 
    mutate(date = ymd(paste(year, month, day)))
}

#wrangle and clean the data frame into different weeks
workload_week1 <- trim_week(workload, 1, 7)
workload_week2 <- trim_week(workload, 8, 14)
workload_week3 <- trim_week(workload, 15, 21)
workload_week4 <- trim_week(workload, 22, 28)
workload_week5 <- trim_week(workload, 29, 33)

#change course col to factors 
workload_week1$course <- as.factor(workload_week1$course)
workload_week2$course <- as.factor(workload_week2$course)
workload_week3$course <- as.factor(workload_week3$course)
workload_week4$course <- as.factor(workload_week4$course)
workload_week5$course <- as.factor(workload_week5$course)

#break up the course space into a new line for plot
levels(workload_week1$course) <- gsub(" ", "\n", levels(workload_week1$course))
levels(workload_week2$course) <- gsub(" ", "\n", levels(workload_week2$course))
levels(workload_week3$course) <- gsub(" ", "\n", levels(workload_week3$course))
levels(workload_week4$course) <- gsub(" ", "\n", levels(workload_week4$course))
levels(workload_week5$course) <- gsub(" ", "\n", levels(workload_week5$course))

#load ubc logo
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

logo <- get_png("img/UBC_logo_3.png")
t <- grid::roundrectGrob()


#' Build a ggplot anamation of a kanban board for a given 
#' week in the MDS program
#'
#' @param df Trimmed df from trim_week
#' @param week week of the program
#'
#' @return
#' @export ggplot animation 
#'
#' @examples
time_kanban <- function(df, week=1){
  p <- ggplot(df, aes(x=course, y=progress, label = wday))+
    geom_point(shape=19, size=12, aes(color =progress))+
    geom_text(x=.45, y=3.59, hjust=0)+
    labs(title = 'Date: {frame_time} MDS Block 5 Kanban Board by Thomas Pin', 
         subtitle = paste0("Week: ", week), x = 'Course', y = '')+
    transition_time(date)+
    ease_aes('linear')+
    annotation_custom(logo, xmin = -.3, xmax = .3, ymin = 3.25, ymax = 4.25) +
    coord_cartesian(clip = "off") +
    scale_colour_manual(values = c("#4CBB17", "#FF7800", "#E60000"))+
    theme_minimal()+
    theme(axis.line = element_line(colour = 'black', size = 1), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "none")
  animate(p, start_pause=10, end_pause=10, duration=2, height=600)
  anim_save(paste0("gifs/0", week, "_week_kanaban.gif"))
}

#test kanban
# time_kanban(workload_week4, 4)

#make the animations for week 1-5
time_kanban(workload_week1, 1)
time_kanban(workload_week2, 2)
time_kanban(workload_week3, 3)
time_kanban(workload_week4, 4)
time_kanban(workload_week5, 5)

