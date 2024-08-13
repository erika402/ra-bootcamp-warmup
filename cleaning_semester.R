################################################################################
################################################################################
#Semester Dataの整形
#生データの読み込み
semester_data_1 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/semester_data_1.csv", header=FALSE)
head(semester_data_1)
semester_data_2 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/semester_data_2.csv", header=FALSE)
head(semester_data_2)

#semester_data_1については、1行目を列名としなさい。
#参考サイト：https://ides.hatenablog.com/entry/2022/02/04/205703
#参考サイト：https://mi-chan-nel.com/r-dataframe-delete/
semester_data_1 <- semester_data_1[-1,]

#2つのデータを適切に結合しなさい。
#参考サイト：https://hira-labo.com/archives/2600
#参考サイト：https://datasciencemore.com/dplyr-bind/
#install.packages("dplyr")
#上記のパッケージの中にbind_rowsとbind_colsの関数が含まれている。
library("dplyr")
#semester_data_1_new：100654～182290
#semester_data_2：182634～243780
semester_data_joint <- bind_rows(semester_data_1, semester_data_2)

#Y列を削除
semester_data_joint <- semester_data_joint[,-6]

#semester制が導入された年の列を作成しなさい。
#semester制が導入された年は大学によって様々。大学すべての導入年をメモするのは厳しいので、
#簡単にクリーニングできる方法はないか。
#重複しているデータがあればそれを出力してくれるコマンド
#library("dplyr")
#duplicate <- semester_data_joint_new%>%group_by(V1)%>%filter(n()>1)%>%arrange(V1)
#duplicate <- duplicate[order(duplicate$V1),]
#print(duplicate)

#重複しているデータに同じ番号を振り、全体として連番になるように番号を振るコマンド
library("dplyr")
duplicate <- semester_data_joint%>%group_by(V1)%>%mutate(id = cur_group_id())%>%ungroup() %>%arrange(V1)
print(duplicate)
#0から1に切り替わったタイミングを新しい列に表示(例えば、2001年に導入されたという特徴をもつ機関は全体として2001年ダミーを持つ)
switch_points <- duplicate %>% group_by(id) %>%  mutate(lead_semester = lead(V3)) %>% filter(V3 == 0 & lead_semester == 1) %>% select(id, switch_points = V5)
print(switch_points)

switch_points_1 <- duplicate %>% group_by(id) %>%  mutate(lead_semester = lead(V3)) %>% filter(V3 == 0 & lead_semester == 1) %>% select(id, switch_points_1 = V5)
print(switch_points_1)

#switch_pointsをmerge (switch_points_1)との違いをメモ
#例えば2001年にセメスターが導入された場合、その期間全体に2001と表示されるようになっている。
duplicate <- duplicate %>%  left_join(switch_points, by = "id") %>% mutate(switch_points = ifelse(V3 == 1, switch_points, switch_points))
print(duplicate)

#switch_points_1をmerge
#2001年にセメスター導入後のダミー変数を作成した。
duplicate <- duplicate %>%  left_join(switch_points_1, by = "id") %>% mutate(switch_points_1 = ifelse(V3 == 1, switch_points_1, 0))
print(duplicate)

#エラーでswitch_pointsが数値じゃないと出たので、数値に直した。
semester <- duplicate %>%mutate(switch_points = as.numeric(switch_points))

semester <- duplicate %>%mutate(switch_points_1 = as.numeric(switch_points_1))

# データ型を確認
str(semester$switch_points)
str(semester$switch_points_1)

#switch_pointsを数値に直したので、1を足す作業を行った#動かなかった
#semester <- semester%>%mutate(seme_year = switch_points + 1)
#print(semester)

#semester <- semester%>%mutate(seme_year_1 = switch_points_1 + 1)
#print(semester)

#以下のコードを使用すれば計算式を数値で足すことができる
semester <- semester %>%
  mutate(
    switch_points = as.numeric(switch_points),
    switch_points_1 = as.numeric(switch_points_1),
    seme_year = switch_points + 1,
    seme_year_1 = switch_points_1 + 1
  )
print(semester)

#NAをゼロに置き換え analysisの部分でNAの数を数えなきゃいけないので、これを実行するのはやめた
semester <- mutate_all(semester, ~replace(., is.na(.), 0))
print(semester)

#seme_year_1に1を足すと、以前0だったところが1になっているので0に戻したい。
#つまり、条件式で観測地が1であるところを0にするという条件式を作成する。
library("dplyr")
semester <- semester%>%mutate(seme_year_1=if_else(seme_year_1==1, 0, seme_year_1))


#switch_pointsを削除
semester <- semester[,-7]

#変数の中のデータの数値の種類をすべて報告するコマンド
#報告された数値に1を足したものがセメスター制が導入された年になる
unique_values <- unique(semester$switch_points_1)
print(unique_values)

#unique_values <- unique(semester$seme_year_1)
#print(unique_values)

#変数作成#変数は数値のみでは生成できない
library("dplyr")
semester <- semester%>%mutate(year_1992=if_else(seme_year==1992, 1, 0))
semester <- semester%>%mutate(year_1993=if_else(seme_year==1993, 1, 0))
semester <- semester%>%mutate(year_1994=if_else(seme_year==1994, 1, 0))
semester <- semester%>%mutate(year_1996=if_else(seme_year==1996, 1, 0))
semester <- semester%>%mutate(year_1998=if_else(seme_year==1998, 1, 0))
semester <- semester%>%mutate(year_1999=if_else(seme_year==1999, 1, 0))
semester <- semester%>%mutate(year_2000=if_else(seme_year==2000, 1, 0))
semester <- semester%>%mutate(year_2001=if_else(seme_year==2001, 1, 0))
semester <- semester%>%mutate(year_2002=if_else(seme_year==2002, 1, 0))
semester <- semester%>%mutate(year_2005=if_else(seme_year==2005, 1, 0))

#2001年にセメスター導入後のダミー変数作成
library("dplyr")
semester <- semester%>%mutate(aft_seme1992=if_else(switch_points_1==1991, 1, 0))
semester <- semester%>%mutate(aft_seme1993=if_else(switch_points_1==1992, 1, 0))
semester <- semester%>%mutate(aft_seme1994=if_else(switch_points_1==1993, 1, 0))
semester <- semester%>%mutate(aft_seme1996=if_else(switch_points_1==1995, 1, 0))
semester <- semester%>%mutate(aft_seme1998=if_else(switch_points_1==1997, 1, 0))
semester <- semester%>%mutate(aft_seme1999=if_else(switch_points_1==1998, 1, 0))
semester <- semester%>%mutate(aft_seme2000=if_else(switch_points_1==1999, 1, 0))
semester <- semester%>%mutate(aft_seme2001=if_else(switch_points_1==2000, 1, 0))
semester <- semester%>%mutate(aft_seme2002=if_else(switch_points_1==2001, 1, 0))
semester <- semester%>%mutate(aft_seme2005=if_else(switch_points_1==2004, 1, 0))

#列名の変更
#参考サイト:https://note.com/mitti1210/n/n5bfe951e74f5#dcf9e806-fa98-4d58-957c-e1133c3483f0
semester <- rename(semester, united=V1)
semester <- rename(semester, instnm=V2)
semester <- rename(semester, semester=V3)
semester <- rename(semester, quarter=V4)
semester <- rename(semester, year=V5)

#余分な行を削除
semester <- semester[-13890, ]
semester <- semester[-13891, ]

################################################################################
################################################################################
#Graduate Dataの整形
#outcomeデータを整理した
#生データの読み込み
#install.packages("readxl")
#install.packages("writexl")
#install.packages("dplyr")
#install.packages("purrr")

library(readxl)
library(writexl)
library(dplyr)
library(purrr)
xlsx_files <- c("C:/作業フォルダ/RA_CAMP_semester/1991.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/1992.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/1993.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/1995.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/1996.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/1997.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/1998.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/1999.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2000.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2001.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2002.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2003.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2004.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2005.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2006.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2007.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2008.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2009.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2010.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2011.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2012.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2013.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2014.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2015.xlsx",
                "C:/作業フォルダ/RA_CAMP_semester/2016.xlsx")

output_directory <- "C:/作業フォルダ/RA_CAMP_semester/"

convert_to_csv <- function(xlsx_file) {
  file_name <- tools::file_path_sans_ext(basename(xlsx_file))
  output_file <- file.path(output_directory, paste0(file_name, ".csv"))
  data <- read_excel(xlsx_file)
  write.csv(data, output_file, row.names = FALSE)
  return(paste("Converted:", xlsx_file, "to", output_file))
}
results <- map_chr(xlsx_files, convert_to_csv)
print(results)

year1991 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1991.csv", header=FALSE)
year1992 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1992.csv", header=FALSE)
year1993 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1993.csv", header=FALSE)
year1995 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1995.csv", header=FALSE)
year1996 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1996.csv", header=FALSE)
year1997 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1997.csv", header=FALSE)
year1998 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1998.csv", header=FALSE)
year1999 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/1999.csv", header=FALSE)
year2000 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2000.csv", header=FALSE)
year2001 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2001.csv", header=FALSE)
year2002 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2002.csv", header=FALSE)
year2003 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2003.csv", header=FALSE)
year2004 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2004.csv", header=FALSE)
year2005 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2005.csv", header=FALSE)
year2006 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2006.csv", header=FALSE)
year2007 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2007.csv", header=FALSE)
year2008 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2008.csv", header=FALSE)
year2009 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2009.csv", header=FALSE)
year2010 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2010.csv", header=FALSE)
year2011 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2011.csv", header=FALSE)
year2012 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2012.csv", header=FALSE)
year2013 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2013.csv", header=FALSE)
year2014 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2014.csv", header=FALSE)
year2015 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2015.csv", header=FALSE)
year2016 <- read.csv("C:/作業フォルダ/RA_CAMP_semester/2016.csv", header=FALSE)

year1992 <- year1992[-1,]
year1993 <- year1993[-1,]
year1995 <- year1995[-1,]
year1996 <- year1996[-1,]
year1997 <- year1997[-1,]
year1998 <- year1998[-1,]
year1999 <- year1999[-1,]
year2000 <- year2000[-1,]
year2001 <- year2001[-1,]
year2002 <- year2002[-1,]
year2003 <- year2003[-1,]
year2004 <- year2004[-1,]
year2005 <- year2005[-1,]
year2006 <- year2006[-1,]
year2007 <- year2007[-1,]
year2008 <- year2008[-1,]
year2009 <- year2009[-1,]
year2010 <- year2010[-1,]
year2011 <- year2011[-1,]
year2012 <- year2012[-1,]
year2013 <- year2013[-1,]
year2014 <- year2014[-1,]
year2015 <- year2015[-1,]
year2016 <- year2016[-1,]

graderate <- bind_rows(year1991, year1992, year1993, year1995, year1996, year1997, year1998, year1999, year2000, year2001, year2002, year2003, year2004, year2005, year2006, year2007, year2008, year2009, year2010, year2011, year2012, year2013, year2014, year2015, year2016)

#rename作業
graderate <- rename(graderate, united=V1)
graderate <- rename(graderate, year=V2)
graderate <- rename(graderate, totcohortsize=V3)
graderate <- rename(graderate, w_cohortsize=V4)
graderate <- rename(graderate, m_cohortsize=V5)
graderate <- rename(graderate, tot4yrgrads=V6)
graderate <- rename(graderate, m_4yrgrads=V7)
graderate <- rename(graderate, w_4yrgrads=V8)
graderate <- rename(graderate, women_gradrate_4yr=V9)

#1行目を削除
graderate <- graderate[-1,]

#女子学生の4年卒業率に0.01をかけて、0から1のスケールに変更しなさい。
graderate <- graderate %>%
  mutate(
    women_gradrate_4yr = as.numeric(women_gradrate_4yr),
    wgra_4yr = women_gradrate_4yr * 0.01,
  )

#男女の合計の4年生卒業率を計算しなさい
graderate <- graderate %>%
  mutate(
    w_cohortsize = as.numeric(w_cohortsize),
    m_cohortsize = as.numeric(m_cohortsize),
    w_4yrgrads = as.numeric(w_4yrgrads),
    m_4yrgrads = as.numeric(m_4yrgrads),
    cohortsize = w_cohortsize + m_cohortsize,
    graderate = w_4yrgrads + m_4yrgrads,
    tosemester_graderate= graderate/cohortsize
  )

#男子学生の4年生卒業率を計算し、新たな列として追加しなさい。
graderate <- graderate %>%
  mutate(
    m_4yrgrads = as.numeric(m_4yrgrads),
    m_cohortsize = as.numeric(m_cohortsize),
    mgra_4yr = m_4yrgrads / m_cohortsize,
  )

#卒業率変数：tosemester_graderate, mgra_4yr, wgra_4yr この3つを有効数字三桁に調整する
#0で始まる場合は、それ以外の数字が出てきてからカウントをスタートします。
graderate <- graderate %>% mutate(tosemester_graderate = signif(tosemester_graderate, digits = 3))
graderate <- graderate %>% mutate(mgra_4yr = signif(mgra_4yr, digits = 3))
graderate <- graderate %>% mutate(wgra_4yr = signif(wgra_4yr, digits = 3))

#1991年から2010年までのデータフレームに変形しなさい
library(dplyr)

# 1991年から2010年までのデータを抽出
graderate <- graderate %>% filter(year >= 1991 & year <= 2010)

#unitedを使用してソートする。つまり、番号を小さい順に並べ替える
graderate <- graderate %>% arrange(united)

#余分な行を削除
graderate <- graderate[-13890, ]
graderate <- graderate[-13891, ]

semester <- semester[-13890, ]
################################################################################
################################################################################
#Covariates Dataの整形
library(readxl)
library(writexl)
library(dplyr)
library(purrr)
xlsx_files <- c("C:/作業フォルダ/RA_CAMP_semester/covariates.xlsx")

output_directory <- "C:/作業フォルダ/RA_CAMP_semester/"

convert_to_csv <- function(xlsx_file) {
  file_name <- tools::file_path_sans_ext(basename(xlsx_file))
  output_file <- file.path(output_directory, paste0(file_name, ".csv"))
  data <- read_excel(xlsx_file)
  write.csv(data, output_file, row.names = FALSE)
  return(paste("Converted:", xlsx_file, "to", output_file))
}
results <- map_chr(xlsx_files, convert_to_csv)
print(results)

covariates <- read.csv("C:/作業フォルダ/RA_CAMP_semester/covariates.csv", header=FALSE)
head(covariates)

covariates <- rename(covariates, united=V1)
covariates <- rename(covariates, year=V2)
covariates <- rename(covariates, category=V3)
covariates <- rename(covariates, value=V4)

#1行目を削除
covariates <- covariates[-1,]

#100654aaaaから100654に変換する作業：第1引数[^0-9]は数字以外のすべての文字、第2引数""（空欄）は
#置換する文字。つまり、数値以外の文字を削除する意味。unitedは置換対象の文字列ベクトル。
covariates <- covariates %>%mutate(united = gsub("[^0-9]", "", united))

#category列に含まれるinstitution, cost, faculty, white_cohortsizeを別の列として追加しなさい
#wide型に変更しなさい。以下のURLはwide型とlong型の違いについて解説しているサイトである。
#https://beginner-r.com/r_from_wide_to_long/ 
#long型からwide型へ変換する方法は以下のサイト
#https://izunyan.hatenablog.com/entry/2016/12/27/205432
library(tidyr)
library(dplyr)
covariates <- covariates %>% spread(category,value)
#1991から2010の範囲にデータをそろえる
#次のサイトを使用：https://ides.hatenablog.com/entry/2022/02/04/205703#:~:text=www.datasciencemadesimple.com
covariates <- covariates[!(covariates$year==1987|covariates$year==1988|covariates$year==1989|covariates$year==1990|covariates$year==1994|covariates$year==2011|covariates$year==2012|covariates$year==2013|covariates$year==2014|covariates$year==2015|covariates$year==2016),]

################################################################################
################################################################################
#今まで作成したデータフレーム：semester, graderate, covariates
#semester: 1991～2010　semesterのデータに合わせる必要がある
#graderate: 1991～2016
#各年は、732個
#covariates: 1987～2016
#各年は、4個

#Master Dataの作成
#3つのデータを結合する
#今回の結合するときの基準のとなるデータセットはsemesterである。配布されたデータセットによると、
#semesterのunitedとinstnmを基にしてgraderateとcovariatesを結合している。
#https://mom-neuroscience.com/r-join/
#library("dplyr")
#master_file <- left_join("semester", "graderate", by="year")
#↳次のエラーが出た：UseMethod("left_join") でエラー: 
#'left_join' をクラス "character" のオブジェクトに適用できるようなメソッドがありません

#データフレームの""を削除した。指定する変数を1つから2にした。そして、by=c("", "")という形に書き直した。
library(dplyr)
master_file <- left_join(semester, graderate, by=c("united", "year"))
