library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(forcats)
library(magrittr)

# Create a calendar for your syllabus ----
# Source: http://svmiller.com/blog/2020/08/a-ggplot-calendar-for-your-semester/

# 1) what is the first Monday of the semester?
# Any number of ways to identify dates in R, but we'll use {lubridate} and the ymd() function here.
# Format: YYYYMMDD. In this example, 4 January 2022.

# What are the full dates of the semester? Here, I'll exclude exam week as I like to do.
# In this case: 6 January to 23 April
sem_boundary <- c(ymd(20240826), ymd(20241220))

semester_dates <- seq(sem_boundary[1], sem_boundary[2], by=1)

exam_week <- seq(ymd(20241216), ymd(20241220), by = 1)


# Weekday(s) of class
class_wdays <- c("Thu")

not_here_dates <- c(
  ymd(20240902), # Labor Day
  ymd(20241021:20241022), # Fall Break
  c(ymd(20241127:20241129), ymd(20241129)) # Thanksgiving
)

# Days where class is scheduled outside of normal times
extra_days <- ymd(20241126)

# You can adjust this as you see fit.
# Basically: add assignment types (e.g. papers, quizzes).
# My intro class was fairly simple: just exams.
weekly_papers <- c(ymd(20240829), seq(ymd(20240904), ymd(20241211), by = 7))
weekly_papers[weekly_papers==ymd(20241127)] <- ymd(20241126)

paper_due_dates <- c(ymd(20240927), ymd(20241018), ymd(20241108), ymd(20241213))


# Custom function for treating the first day of the month as the first week
# of the month up until the first Sunday (unless Sunday was the start of the month)
wom <- function(date) {
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}

# Create a data frame of dates, assign to Cal
Cal <- tibble(date = seq(floor_date(sem_boundary[1], "month"),
                         ceiling_date(sem_boundary[2], "month") - days(1),
                         by=1))  |>
  mutate(mon = lubridate::month(date, label=T, abbr=F), # get month label
         wkdy = weekdays(date, abbreviate=T), # get weekday label
         wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu","Fri","Sat"), # make sure Sunday comes first
         semester = date %in% semester_dates, # is date part of the semester?
         assignment = date %in% weekly_papers, # assignment due?
         paper = date %in% paper_due_dates, # is it an exam?
         not_here = date %in% not_here_dates, # is it a day off?
         exam_wk = date %in% exam_week,
         day = lubridate::mday(date), # get day of month to add later as a label
         # Below: our custom wom() function
         week = wom(date))

# Create a category variable, for filling.
# I can probably make this a case_when(), but this will work.

Cal <- Cal |>
  mutate(category = case_when(
    paper ~ "Paper Due Date",
    not_here ~ "UNL holiday",
    semester & (wkdy %in% class_wdays | date %in% extra_days) &
      !not_here & !exam_wk ~ "Class Day",
    semester & exam_wk ~ "Finals",
    semester ~ "Semester",
    TRUE ~ "NA"
  ))
# mutate(category = NA,
#        category = ifelse(semester == 1, "Semester", category),
#        category = ifelse(semester == 1 & wkdy %in% c("Wed"), "Class Day", category),
#        category = ifelse(exams == 1, "Exams", category),
#        category = ifelse(is.na(category) | (semester == 1 & not_here == 1), "NA", category)) -> Cal

my_scales <- tribble(
  ~category, ~fill,
  "Class Day",      "purple",
  "Semester",       "white",
  "UNL holiday",    "grey10",
  "NA",             "white",
  "Paper Due Date", "orange",
  "Finals",         "grey")

fill_scale <- c(my_scales$fill) |> magrittr::set_names(my_scales$category)

plot_Cal <-  Cal |>
  left_join(my_scales) |>
  mutate(
    shape = if_else(assignment, "Assignment Due", NA_character_),
    color = if_else(semester & (!not_here),
                    "black", "white"))

class_cal <- plot_Cal |>
  ggplot(aes(wkdy, week)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(1, 0), legend.justification = c(1,0), legend.direction = "vertical", legend.title = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  # geom_tile and facet_wrap will do all the heavy lifting
  geom_tile(alpha=0.8, aes(fill=category), color="black", linewidth=.45) +
  facet_wrap(~mon, scales = "free", ncol=3) +
  # fill in tiles to make it look more "calendary" (sic)
  geom_text(aes(label=day, color = color)) +
  geom_point(aes(y = week + .3, shape = shape)) +
  # put your y-axis down, flip it, and reverse it
  scale_y_reverse(breaks=NULL) +
  # manually fill scale colors to something you like...
  scale_color_identity() +
  scale_shape_manual("", values = 8, breaks = unique(na.omit(plot_Cal$shape))) +
  scale_fill_manual(values = fill_scale,
                    breaks=c("Semester", "UNL holiday", "Class Day",
                             "Assignment Due", "Paper Due Date", "Finals"))
# class_cal

exam_days <- filter(Cal, category == "Paper Due Date") |>
  mutate(topic = c("992 Paper #1 Due", "892 Paper #1 Due", "992 Paper #2 Due", "Final Paper Due (892/992)"),
         time = c("12pm", "12pm", "12pm", "12pm"))

class_days <- filter(Cal, category == "Class Day") |>
  mutate(topic = c(
    "Syllabus, Introduction",
    "Grammar of Graphics",
    "Implementations of the Grammar",
    "Extending the Grammar",
    "Evaluating Graphics",
    "Graphical Testing",
    "Graphical Testing",
    "Audience Considerations",
    "Visual Statistics",
    "Visual Inference",
    "Visual Inference",
    "High Dimensional Vis",
    "Tours",
    "Dimension Reduction",
    "High Dimensional Vis",
    "Data Art/Infographics")) |>
  bind_rows(exam_days) |>
  arrange(date)

