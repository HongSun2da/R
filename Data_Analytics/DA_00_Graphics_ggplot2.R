# R Graphics  ##################################################################
# 1. ggplot2
#   - gg = grammar of graphics


################################################################################

#install.packages("ggplot2")
library(ggplot2)


# ggplot 집단별  ###############################################################
# - ggplot2::facet_wrap()
# - ggplot2::facet_grid()

#install.packages("lattice")
library(lattice)

head(singer, n=20)
str(singer)

# 전체(기본) - facet_wrap
ggplot2::ggplot(singer,
                aes(x = height)) +
  ggplot2::geom_histogram()


ggplot2::ggplot(singer,
                aes(x = height)) +
  ggplot2::geom_histogram() +
  ggplot2::facet_wrap(~ voice.part, 
                      nrow = 4)



# 전체(기본) - facet_grid
ggplot2::ggplot(singer,
                aes(x = height)) +
  ggplot2::geom_density()


ggplot2::ggplot(singer,
                aes(x = height, fill = voice.part)) +
  ggplot2::geom_density() +
  ggplot2::facet_grid(voice.part ~ .)



#install.packages("car")
library(car)

str(Salaries)

# 전체(기본) - facet_grid
ggplot2::ggplot(Salaries,
                aes(x = yrs.since.phd, y = salary)) +
  ggplot2::geom_point()


ggplot2::ggplot(Salaries,
                aes(x = yrs.since.phd, y = salary)) +
  ggplot2::geom_point() + 
  ggplot2::facet_grid(sex ~ rank)


ggplot2::ggplot(Salaries,
                aes(x = yrs.since.phd, y = salary, color = rank, shape = rank)) +
  ggplot2::geom_point() + 
  ggplot2::facet_grid(. ~ sex)



# ggplot scale  ################################################################

ggplot2::ggplot(Salaries,
                aes(x = rank, y = salary, fill = sex)) +
  ggplot2::geom_boxplot() +
  
  # x축 항목명- 범주형
  ggplot2::scale_x_discrete(breaks = c("AsstProf", "AssocProf", "Prof"),
                            labels = c("Asst\nProf", "Assoc\nProf", "Proessorf")) + 
  
  # y축 항목명 - 연속형
  ggplot2::scale_y_continuous(breaks = c(50000, 100000, 150000, 200000),
                              labels = c("$50K", "$100K", "$150K", "$200K")) +
  
  # 범례 - aes(fill = sex) 연결됨
  # ggplot2::labs(fill = "Gender") 
  ggplot2::scale_fill_discrete(name ="Gender") +
  
  # 범례 위치
  # ggplot2::theme(legend.position = "top")
  # ggplot2::theme(legend.position = "none")
  ggplot2::theme(legend.position = c(0.15, 0.85))
  

ggplot2::ggplot(mtcars,
                aes(x = wt, y = mpg, shape = factor(cyl), color = factor(cyl))) +
  ggplot2::geom_point() + 
  
  # 범례 - aes(shape = factor(cyl), color = factor(cyl)) 연결됨
  #ggplot2::labs(shape = "Cylinder", color = "Cylinder")
  
  ggplot2::scale_shape_discrete(name = "Cylinder") +
  ggplot2::scale_color_discrete(name = "Cylinder")



ggplot2::ggplot(mtcars,
                aes(x = wt, y = mpg, size = disp)) + 
  ggplot2::geom_point(shape = 21, color = "black", fill = "wheat") +
  
  # 범례 - aes(size = disp) 연결됨
  # ggplot2::labs(size = "Engine\nDisplacement")
  
  ggplot2::scale_size_continuous(name = "Engine\nDisplacement") 
  
  

ggplot2::ggplot(Salaries,
                aes(x = rank, fill = sex)) +
  ggplot2::geom_bar() + 

  # 색상 - 범주형
  ggplot2::scale_fill_manual(values = c("tomato", "cornflowerblue"))
  
  
  
ggplot2::ggplot(Salaries,
                aes(x = yrs.since.phd, y = salary, color = rank)) +
  ggplot2::geom_point(size = 2) +
    
  # 색상 - 범주형
  ggplot2::scale_color_manual(values = c("orange", "violetred", "steelblue"))


ggplot2::ggplot(Salaries,
                aes(x = yrs.since.phd, y = salary, color = rank)) +
  ggplot2::geom_point(size = 2) +
  
  # 색상(palette 사용) - 범주형
  ggplot2::scale_color_brewer(palette = "Accent") 

#-------------------------------------------------------------------------------
?RColorBrewer # {RColorBrewer} ColorBrewer palettes

# Accent	8
# Dark2	  8
# Paired	12
# Pastel1	9
# Pastel2	8
# Set1	  9
# Set2	  8
# Set3	  12
#-------------------------------------------------------------------------------

ggplot2::ggplot(mtcars,
                aes(x = wt, y = mpg, color = disp)) + 
  ggplot2::geom_point() + 

  # 색상 - 연속형
  scale_color_gradient2()


ggplot2::ggplot(Salaries,
                aes(x = yrs.since.phd, y = salary, color = rank, shape = rank)) +
  ggplot2::geom_point(size = 2) + 
  
  # 모양 순서 aes(shape = rank) 연결됨
  ggplot2::scale_shape_manual(values=c(15, 17, 19))





# ggplot 테마  #################################################################


ggplot2::ggplot(Salaries,
                aes(x = yrs.since.phd, y = salary, color = rank, shape = rank)) +
  ggplot2::geom_point() +
  
  # 그룹
  ggplot2::facet_grid(. ~ sex) +

  # 테마(기본)
  # ggplot2::theme_gray()
  
  ggplot2::theme_classic()

#-------------------------------------------------------------------------------
?theme_gray # {ggplot2} Complete themes

# theme_gray
# theme_bw
# theme_linedraw
# theme_light
# theme_dark
# theme_minimal
# theme_classic
# theme_void
# theme_test
#-------------------------------------------------------------------------------



# theme() 함수 - 사용자 정의


ggplot2::ggplot(Salaries,
                aes(x = rank, y = salary, fill = sex)) +
  ggplot2::geom_boxplot()

?theme














