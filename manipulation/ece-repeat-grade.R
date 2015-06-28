# Set working directory
# setwd()

new_data <- read.table('ece-repeat-grade.dat', sep=' ')
names(new_data) <- c('C0000100','C0000200','C0005300','C0005400','C0005700','C0592000','C0592200','C0592500','C0784700','C0811400','C0811600','C0811700','C0811800','C0987300','C1001400','C1001500','C1001600','C1001700','C1001800','C1185600','C1205000','C1205100','C1205200','C1205300','C1205400','C1438000','C1439400','C1525000','C1525100','C1525200','C1525300','C1525400','C1639700','C1639800','C1639900','C1640000','C1640100','C1640200','C1640300','C1640400','C1640500','C1640600','C1640700','C1640800','C1640900','C1641100','C1641200','C1641300','C1641400','C1641500','C1641600','C1641700','C1771400','C1771500','C1771600','C1771700','C1771800','C1982000','C2244400','C2244600','C2244700','C2244800','C2244900','C2245000','C2245100','C2248300','C2687900','C2688000','C2688100','C2688200','C2688300','C2688400','C2688500','C2688600','C2688700','C2733300','C2969400','C2969500','C2969600','C2969700','C2969800','C2969900','C2970000','C3004500','C3549400','C3549500','C3549600','C3549700','C3549800','C3549900','C3550000','C3576700','C3893800','C3893900','C3894000','C3894100','C3894200','C3894300','C3894400','C3949200','C5142800','C5142900','C5143000','C5143100','C5143200','C5143300','C5143400','C5193300','C5720000','C5720200','C5720300','C5720400','C5720500','C5720600','C5720700','C5767700','S0008700','S0021200','Y0034400','Y0419400','Y0709300','Y1015000','Y1257400','Y1491100','Y1740900','Y2021700','Y2346800','Y2676300')

# Handle missing values
  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -7] = NA  # Missing 

# If there are values not categorized they will be represented as NA
vallabels = function(data) {
  data$C0000100 <- cut(data$C0000100, c(1.0,9999999.0), labels=c("1 TO 9999999: See Min & Max values below for range as of this release"), right=FALSE)
  data$C0000200 <- cut(data$C0000200, c(1.0,12686.0), labels=c("1 TO 12686: NLSY79 Public ID"), right=FALSE)
  data$C0005300 <- factor(data$C0005300, levels=c(1.0,2.0,3.0), labels=c("HISPANIC","BLACK","NON-BLACK, NON-HISPANIC"))
  data$C0005400 <- factor(data$C0005400, levels=c(1.0,2.0), labels=c("MALE","FEMALE"))
  data$C0005700 <- cut(data$C0005700, c(1970.0,1979.0,1980.0,1981.0,1982.0,1983.0,1984.0,1985.0,1986.0,1987.0,1988.0,1989.0,1990.0,1991.0,1992.0,1993.0,1994.0,1995.0,1996.0,1997.0,1998.0,1999.0,2000.0,2001.0,2002.0,2003.0,2004.0,2005.0,2006.0,2007.0,2008.0,2009.0,2010.0,2011.0,2012.0,1978.0), labels=c("1970 TO 1978: < before 1979","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012"), right=FALSE)
  data$C0592000 <- factor(data$C0592000, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C0592200 <- factor(data$C0592200, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("LESS THAN 3 MONTHS","3 - 11 MONTHS","1 YEAR - 23 MONTHS","2 YEARS OR MORE","STILL ENROLLED"))
  data$C0592500 <- factor(data$C0592500, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C0784700 <- factor(data$C0784700, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C0811400 <- factor(data$C0811400, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C0811600 <- factor(data$C0811600, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("LESS THAN 3 MONTHS","3 - 11 MONTHS","1 YEAR - 23 MONTHS","2 YEARS OR MORE","STILL ENROLLED"))
  data$C0811700 <- factor(data$C0811700, levels=c(1.0,2.0,3.0,4.0), labels=c("VERY DISSATISFIED","SOMEWHAT DISSATISFIED","SOMEWHAT SATISFIED","VERY SATISFIED"))
  data$C0811800 <- factor(data$C0811800, levels=c(1.0,2.0,3.0,4.0), labels=c("VERY DISSATISFIED","SOMEWHAT DISSATISFIED","SOMEWHAT SATISFIED","VERY SATISFIED"))
  data$C0987300 <- factor(data$C0987300, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C1001400 <- factor(data$C1001400, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C1001500 <- cut(data$C1001500, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,99999.0), labels=c("0: < 1","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17 TO 99999: 17+"), right=FALSE)
  data$C1001600 <- factor(data$C1001600, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("LESS THAN 3 MONTHS","3 - 11 MONTHS","1 YEAR - 23 MONTHS","2 YEARS OR MORE","STILL ENROLLED"))
  data$C1001700 <- factor(data$C1001700, levels=c(4.0,3.0,2.0,1.0), labels=c("VERY SATISFIED","SOMEWHAT SATISFIED","SOMEWHAT DISSATISFIED","VERY DISSATISFIED"))
  data$C1001800 <- factor(data$C1001800, levels=c(4.0,3.0,2.0,1.0), labels=c("VERY SATISFIED","SOMEWHAT SATISFIED","SOMEWHAT DISSATISFIED","VERY DISSATISFIED"))
  data$C1185600 <- factor(data$C1185600, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C1205000 <- factor(data$C1205000, levels=c(1.0,0.0), labels=c("YES","NO"))
  data$C1205100 <- cut(data$C1205100, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,99999.0), labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16 TO 99999: 16+"), right=FALSE)
  data$C1205200 <- factor(data$C1205200, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("LESS THAN 3 MONTHS","3 - 11 MONTHS","1 YEAR - 23 MONTHS","2 YEARS OR MORE","STILL ENROLLED"))
  data$C1205300 <- factor(data$C1205300, levels=c(1.0,2.0,3.0,4.0), labels=c("VERY DISSATISFIED","SOMEWHAT DISSATISFIED","SOMEWHAT SATISFIED","VERY SATISFIED"))
  data$C1205400 <- factor(data$C1205400, levels=c(1.0,2.0,3.0,4.0), labels=c("VERY DISSATISFIED","SOMEWHAT DISSATISFIED","SOMEWHAT SATISFIED","VERY SATISFIED"))
  data$C1438000 <- factor(data$C1438000, levels=c(1.0), labels=c("YES"))
  data$C1439400 <- factor(data$C1439400, levels=c(1.0), labels=c("YES"))
  data$C1525000 <- factor(data$C1525000, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C1525100 <- cut(data$C1525100, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,999999.0), labels=c("0","1","2","3","4","5","6","7 TO 999999: > 6"), right=FALSE)
  data$C1525200 <- factor(data$C1525200, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C1525300 <- factor(data$C1525300, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C1525400 <- factor(data$C1525400, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C1639700 <- factor(data$C1639700, levels=c(1.0), labels=c("YES"))
  data$C1639800 <- factor(data$C1639800, levels=c(1.0), labels=c("YES"))
  data$C1639900 <- factor(data$C1639900, levels=c(1.0), labels=c("YES"))
  data$C1640000 <- factor(data$C1640000, levels=c(1.0), labels=c("YES"))
  data$C1640100 <- factor(data$C1640100, levels=c(1.0), labels=c("YES"))
  data$C1640200 <- factor(data$C1640200, levels=c(1.0), labels=c("YES"))
  data$C1640300 <- factor(data$C1640300, levels=c(1.0), labels=c("YES"))
  data$C1640400 <- factor(data$C1640400, levels=c(1.0), labels=c("YES"))
  data$C1640500 <- factor(data$C1640500, levels=c(1.0), labels=c("YES"))
  data$C1640600 <- factor(data$C1640600, levels=c(1.0), labels=c("YES"))
  data$C1640700 <- factor(data$C1640700, levels=c(1.0), labels=c("YES"))
  data$C1640800 <- factor(data$C1640800, levels=c(1.0), labels=c("YES"))
  data$C1640900 <- factor(data$C1640900, levels=c(1.0), labels=c("YES"))
  data$C1641100 <- factor(data$C1641100, levels=c(1.0), labels=c("YES"))
  data$C1641200 <- factor(data$C1641200, levels=c(1.0), labels=c("YES"))
  data$C1641300 <- factor(data$C1641300, levels=c(1.0), labels=c("YES"))
  data$C1641400 <- factor(data$C1641400, levels=c(1.0), labels=c("YES"))
  data$C1641500 <- factor(data$C1641500, levels=c(1.0), labels=c("YES"))
  data$C1641600 <- factor(data$C1641600, levels=c(1.0), labels=c("YES"))
  data$C1641700 <- factor(data$C1641700, levels=c(1.0), labels=c("YES"))
  data$C1771400 <- factor(data$C1771400, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C1771500 <- cut(data$C1771500, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,9999.0), labels=c("0","1","2","3","4","5","6","7 TO 9999"), right=FALSE)
  data$C1771600 <- factor(data$C1771600, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C1771700 <- factor(data$C1771700, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C1771800 <- factor(data$C1771800, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C1982000 <- factor(data$C1982000, levels=c(1.0,2.0), labels=c("Selected","Not Selected"))
  data$C2244400 <- factor(data$C2244400, levels=c(1.0,0.0), labels=c("1","0"))
  data$C2244600 <- factor(data$C2244600, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C2244700 <- factor(data$C2244700, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C2244800 <- cut(data$C2244800, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,9999.0), labels=c("0","1","2","3","4","5","6","7 TO 9999"), right=FALSE)
  data$C2244900 <- factor(data$C2244900, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C2245000 <- factor(data$C2245000, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C2245100 <- factor(data$C2245100, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C2248300 <- factor(data$C2248300, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C2687900 <- factor(data$C2687900, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C2688000 <- factor(data$C2688000, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C2688100 <- factor(data$C2688100, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C2688200 <- factor(data$C2688200, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C2688300 <- factor(data$C2688300, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C2688400 <- cut(data$C2688400, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,9999.0), labels=c("0","1","2","3","4","5","6","7 TO 9999"), right=FALSE)
  data$C2688500 <- factor(data$C2688500, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C2688600 <- factor(data$C2688600, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C2688700 <- factor(data$C2688700, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C2733300 <- factor(data$C2733300, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C2969400 <- factor(data$C2969400, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C2969500 <- factor(data$C2969500, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C2969600 <- factor(data$C2969600, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C2969700 <- cut(data$C2969700, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,9999.0), labels=c("0","1","2","3","4","5","6","7 TO 9999"), right=FALSE)
  data$C2969800 <- factor(data$C2969800, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C2969900 <- factor(data$C2969900, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C2970000 <- factor(data$C2970000, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C3004500 <- factor(data$C3004500, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C3549400 <- factor(data$C3549400, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C3549500 <- factor(data$C3549500, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C3549600 <- factor(data$C3549600, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C3549700 <- cut(data$C3549700, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,9999.0), labels=c("0","1","2","3","4","5","6","7 TO 9999"), right=FALSE)
  data$C3549800 <- factor(data$C3549800, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C3549900 <- factor(data$C3549900, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C3550000 <- factor(data$C3550000, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C3576700 <- factor(data$C3576700, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C3893800 <- factor(data$C3893800, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C3893900 <- factor(data$C3893900, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C3894000 <- factor(data$C3894000, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C3894100 <- cut(data$C3894100, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,9999.0), labels=c("0","1","2","3","4","5","6","7 TO 9999"), right=FALSE)
  data$C3894200 <- factor(data$C3894200, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C3894300 <- factor(data$C3894300, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C3894400 <- factor(data$C3894400, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C3949200 <- factor(data$C3949200, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C5142800 <- factor(data$C5142800, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C5142900 <- factor(data$C5142900, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C5143000 <- factor(data$C5143000, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C5143100 <- cut(data$C5143100, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,999.0), labels=c("0: Less than 1 year old","1: year old","2: years old","3: years old","4: years old","5: years old","6: years old","7: years old","8: years old","9: years old","10: years old","11: years old","12: years old","13: years old","14: years old","15 TO 999: over 15 yrs old"), right=FALSE)
  data$C5143200 <- factor(data$C5143200, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C5143300 <- factor(data$C5143300, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C5143400 <- factor(data$C5143400, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C5193300 <- factor(data$C5193300, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C5720000 <- factor(data$C5720000, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C5720200 <- factor(data$C5720200, levels=c(1.0,0.0), labels=c("1: Yes/Condition applies","0: No/Condition does not apply"))
  data$C5720300 <- factor(data$C5720300, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$C5720400 <- cut(data$C5720400, c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,999.0), labels=c("0: Less than 1 year old","1: year old","2: years old","3: years old","4: years old","5: years old","6: years old","7: years old","8: years old","9: years old","10: years old","11: years old","12: years old","13: years old","14: years old","15 TO 999: over 15 yrs old"), right=FALSE)
  data$C5720500 <- factor(data$C5720500, levels=c(1.0,2.0,3.0,4.0,5.0), labels=c("Less than 3 months","3 - 11 months","1 year - 23 months","2 years or more","Still enrolled"))
  data$C5720600 <- factor(data$C5720600, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C5720700 <- factor(data$C5720700, levels=c(1.0,2.0,3.0,4.0), labels=c("Very satisfied","Somewhat satisfied","Somewhat dissatisfied","Very dissatisfied"))
  data$C5767700 <- factor(data$C5767700, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$S0008700 <- factor(data$S0008700, levels=c(1.0,2.0,3.0), labels=c("1: Yes","2: No","3: Data not available"))
  data$S0021200 <- factor(data$S0021200, levels=c(1.0,2.0,3.0), labels=c("1: Yes","2: No","3: Data not available"))
  data$Y0034400 <- factor(data$Y0034400, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y0419400 <- factor(data$Y0419400, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y0709300 <- factor(data$Y0709300, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y1015000 <- factor(data$Y1015000, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y1257400 <- factor(data$Y1257400, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y1491100 <- factor(data$Y1491100, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y1740900 <- factor(data$Y1740900, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y2021700 <- factor(data$Y2021700, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y2346800 <- factor(data$Y2346800, levels=c(1.0,0.0), labels=c("Yes","No"))
  data$Y2676300 <- factor(data$Y2676300, levels=c(1.0,0.0), labels=c("Yes","No"))
  return(data)
}

varlabels <- c(    "ID CODE OF CHILD",
    "ID CODE OF MOTHER OF CHILD",
    "RACE OF CHILD (FROM MOTHERS SCREENER 79)",
    "SEX OF CHILD",
    "DATE OF BIRTH OF CHILD - YEAR",
    "CHILD EVER ENROLLED IN HEAD START",
    "HOW LONG CHILD WAS IN HEAD START",
    "WAS MOM IN HEAD START AS A CHILD",
    "HAS CHILD REPEATED GRADE FOR ANY REASN",
    "CHILD EVER ENROLLED IN HEAD START PROGRM",
    "HOW LONG DID CHILD ATTEND HEAD START",
    "MOM'S SATISFACTN W/HEAD START AND CHILD",
    "MOM'S SATISFACTN WITH HEAD START AND HER",
    "HAS CHILD REPEATED ANY GRADE",
    "CHILD EVER ENRLD IN HEAD START PROGRM-92",
    "CHILD'S AGE WHEN 1ST ATTD HEAD START-92",
    "HOW LONG DID CHILD ATTEND HEAD START-92",
    "MOM'S SATIS W/ HEAD START FOR CHILD-92",
    "MOM'S SATIS W/ HEAD START FOR SELF-92",
    "HAS CHILD REPEATED ANY GRADE?-92",
    "CHILD EVER ENRLD IN HEAD START PROGRM 94",
    "CHILD*S AGE WHEN 1ST ATTD HEAD START 94",
    "HOW LONG DID CHILD ATTEND HEAD START 94",
    "MOM*S SATIS W/ HEAD START FOR CHILD 94",
    "MOM*S SATIS W/ HEAD START FOR SELF 94",
    "MS CHILD REPEATED PRE-K/KINDERG/PRE-1?94",
    "MS NEVER REPEATED ANY GRADE? 94",
    "CHILD EVER ENRLD IN HEAD START PROGRM 96",
    "CHILD AGE WHEN 1ST ATTD HEAD START 96",
    "HOW LONG DID CHILD ATTEND HEAD START 96",
    "MOM SATISFACTN W/ HEADSTART FOR CHILD 96",
    "MOM SATISFACTN W/ HEADSTART FOR SELF 96",
    "MS CHILD REPEATED 1ST GRADE 96",
    "MS CHILD REPEATED 2ND GRADE 96",
    "MS CHILD REPEATED 3RD GRADE 96",
    "MS CHILD REPEATED 4TH GRADE 96",
    "MS CHILD REPEATED 5TH GRADE 96",
    "MS CHILD REPEATED 6TH GRADE 96",
    "MS CHILD REPEATED 7TH GRADE 96",
    "MS CHILD REPEATED 8TH GRADE 96",
    "MS CHILD REPEATED 9TH GRADE 96",
    "MS CHILD REPEATED 10TH GRADE 96",
    "MS CHILD REPEATED 11TH GRADE 96",
    "MS CHILD REPEATED 12TH GRADE 96",
    "MS CHILD NEVER REPEATED A GRADE 96",
    "MS REPEATED DUE TO ACADEMIC FAILURE 96",
    "MS REPEATED DUE TO IMMATURITY 96",
    "MS REPEATED DUE TO FREQUENTLY ABSENT 96",
    "MS REPEATED DUE TO TRUANCY 96",
    "MS REPEATED DUE TO HEALTH REASONS 96",
    "MS REPEAT DUE TO MORE DIFFICULT SCHL 96",
    "MS REPEATED DUE TO OTHER REASON 96",
    "CHILD EVER ENROLLED IN HEAD START 1998",
    "CHILD AGE WHEN 1ST ATTD HEAD START 1998",
    "HOW LONG CHILD ATTENDED HEAD START 1998",
    "MOM SATIS W/ HEAD START FOR CHILD 1998",
    "MOM SATIS W/ HEAD START FOR SELF 1998",
    "MS CHILD NEVER REPEATED A GRADE 1998",
    "MACH CHK: CHECK FOR HEADSTRT FLAG 2000",
    "MACH CHK: PREV HEADSTRT INFO EXIST? 2000",
    "CHILD EVER ENROLLED IN HEAD START 2000",
    "CHILD AGE WHEN 1ST ATTD HEAD START 2000",
    "HOW LONG CHILD ATTENDED HEAD START 2000",
    "MOM SATIS W/ HEAD START FOR CHILD 2000",
    "MOM SATIS W/ HEAD START FOR SELF 2000",
    "HAS CHILD REPEATED A GRADE 2000",
    "MACH CHK: CHECK FOR HEADSTRT FLAG 2002",
    "MACH CHK: CHECK DATA HEADSTRT FLAG 2002",
    "MACH CHK: SET HEADSTRT FLAG DEFAULT 2002",
    "MACH CHK: PREV HEADSTRT INFO EXIST? 2002",
    "CHILD EVER ENROLLED IN HEAD START 2002",
    "CHILD AGE WHEN 1ST ATTD HEAD START 2002",
    "HOW LONG CHILD ATTENDED HEAD START 2002",
    "MOM SATIS W/ HEAD START FOR CHILD 2002",
    "MOM SATIS W/ HEAD START FOR SELF 2002",
    "HAS CHILD REPEATED A GRADE 2002",
    "MACH CHK: CHECK FOR HEADSTRT FLAG 2004",
    "MACH CHK: PREV HEADSTRT INFO EXIST? 2004",
    "CHILD EVER ENROLLED IN HEAD START 2004",
    "CHILD AGE WHEN 1ST ATTD HEAD START 2004",
    "HOW LONG CHILD ATTENDED HEAD START 2004",
    "MOM SATIS W/ HEAD START FOR CHILD 2004",
    "MOM SATIS W/ HEAD START FOR SELF 2004",
    "HAS CHILD REPEATED A GRADE 2004",
    "MACH CHK: CHECK FOR HEADSTRT FLAG 2006",
    "MACH CHK: PREV HEADSTRT INFO EXIST? 2006",
    "CHILD EVER ENROLLED IN HEAD START 2006",
    "CHILD AGE WHEN 1ST ATTD HEAD START 2006",
    "HOW LONG CHILD ATTENDED HEAD START 2006",
    "MOM SATIS W/ HEAD START FOR CHILD 2006",
    "MOM SATIS W/ HEAD START FOR SELF 2006",
    "HAS CHILD REPEATED A GRADE 2006",
    "MACH CHK: CHECK FOR HEADSTRT FLAG 2008",
    "MACH CHK: PREV HEADSTRT INFO EXIST? 2008",
    "CHILD EVER ENROLLED IN HEAD START 2008",
    "CHILD AGE WHEN 1ST ATTD HEAD START 2008",
    "HOW LONG CHILD ATTENDED HEAD START 2008",
    "MOM SATIS W/ HEAD START FOR CHILD 2008",
    "MOM SATIS W/ HEAD START FOR SELF 2008",
    "HAS CHILD REPEATED A GRADE 2008",
    "MACH CHK: CHECK FOR HEADSTRT FLAG 2010",
    "MACH CHK: PREV HEADSTRT INFO EXIST? 2010",
    "CHILD EVER ENROLLED IN HEAD START 2010",
    "CHILD AGE WHEN 1ST ATTD HEAD START 2010",
    "HOW LONG CHILD ATTENDED HEAD START 2010",
    "MOM SATIS W/ HEAD START FOR CHILD 2010",
    "MOM SATIS W/ HEAD START FOR SELF 2010",
    "HAS CHILD REPEATED A GRADE 2010",
    "MACH CHK: CHECK FOR HEADSTRT FLAG 2012",
    "MACH CHK: PREV HEADSTRT INFO EXIST? 2012",
    "CHILD EVER ENROLLED IN HEAD START 2012",
    "CHILD AGE WHEN 1ST ATTD HEAD START 2012",
    "HOW LONG CHILD ATTENDED HEAD START 2012",
    "MOM SATIS W/ HEAD START FOR CHILD 2012",
    "MOM SATIS W/ HEAD START FOR SELF 2012",
    "HAS CHILD REPEATED A GRADE 2012",
    "SCH#1 EVER REPEATED A GRADE OR HELD BACK",
    "SCH#2 EVER REPEATED A GRADE OR HELD BACK",
    "HAS R EVER REPEATED A GRADE?          94 1994",
    "HAS R EVER REPEATED A GRADE?          96 1996",
    "HAS R EVER REPEATED A GRADE? 1998",
    "HAS R REPEATED A GRADE IN SCHOOL EVER/SINCE DLI 2000",
    "HAS R REPEATED A GRADE IN SCHOOL EVER/SINCE DLI 2002",
    "HAS R REPEATED A GRADE IN SCHOOL EVER/SINCE DLI 2004",
    "HAS R REPEATED A GRADE IN SCHOOL EVER/SINCE DLI 2006",
    "HAS R REPEATED A GRADE IN SCHOOL EVER/SINCE DLI 2008",
    "HAS R REPEATED A GRADE IN SCHOOL EVER/SINCE DLI 2010",
    "HAS R REPEATED A GRADE IN SCHOOL EVER/SINCE DLI 2012"
)

# Use qnames rather than rnums
qnames = function(data) {
  names(data) <- c("CPUBID_XRND","MPUBID_XRND","CRACE_XRND","CSEX_XRND","CYRB_XRND","CS880974_1988","CS880977_1988","CS881012_1988","MS880844_1988","CS901619_1990","CS901623_1990","CS901625_1990","CS901627_1990","MS901429_1990","CS921746_1992","CS921748_1992","CS921750_1992","CS921752_1992","CS921754_1992","MS921429_1992","CS94-14_1994","CS94-15_1994","CS94-16_1994","CS94-17_1994","CS94-18_1994","MS941529_1994","MS941557_1994","CS96-14_1996","CS96-15_1996","CS96-16_1996","CS96-17_1996","CS96-18_1996","MS961531_1996","MS961533_1996","MS961535_1996","MS961537_1996","MS961539_1996","MS961541_1996","MS961543_1996","MS961545_1996","MS961547_1996","MS961549_1996","MS961551_1996","MS961553_1996","MS961555_1996","MS961559_1996","MS961561_1996","MS961563_1996","MS961565_1996","MS961567_1996","MS961569_1996","MS961571_1996","CS98-14_1998","CS98-15_1998","CS98-16_1998","CS98-17_1998","CS98-18_1998","MS985005N_1998","BKGN-9_2000","BKGN-11_2000","BKGN-12_2000","BKGN-13_2000","BKGN-14_2000","BKGN-15_2000","BKGN-16_2000","BKGN-37_2000","BKGN-9A_2002","BKGN-9B_2002","BKGN-10_2002","BKGN-11_2002","BKGN-12_2002","BKGN-13_2002","BKGN-14_2002","BKGN-15_2002","BKGN-16_2002","BKGN-37_2002","BKGN-9A_2004","BKGN-11_2004","BKGN-12_2004","BKGN-13_2004","BKGN-14_2004","BKGN-15_2004","BKGN-16_2004","BKGN-37_2004","MS-BKGN-9A_2006","MS-BKGN-11_2006","MS-BKGN-12_2006","MS-BKGN-13_2006","MS-BKGN-14_2006","MS-BKGN-15_2006","MS-BKGN-16_2006","MS-BKGN-37_2006","MS-BKGN-9A_2008","MS-BKGN-11_2008","MS-BKGN-12_2008","MS-BKGN-13_2008","MS-BKGN-14_2008","MS-BKGN-15_2008","MS-BKGN-16_2008","MS-BKGN-37_2008","MS-BKGN-9A_2010","MS-BKGN-11_2010","MS-BKGN-12_2010","MS-BKGN-13_2010","MS-BKGN-14_2010","MS-BKGN-15_2010","MS-BKGN-16_2010","MS-BKGN-37_2010","MS-BKGN-9A_2012","MS-BKGN-11_2012","MS-BKGN-12_2012","MS-BKGN-13_2012","MS-BKGN-14_2012","MS-BKGN-15_2012","MS-BKGN-16_2012","MS-BKGN-37_2012","C1Q40A_1995","C2Q40A_1995","Q4-14_1994","Q4-14_1996","Q4-14_1998","Q4-14_2000","Q4-14_2002","Q4-14_2004","Q4-14_2006","Q4-14_2008","Q4-14_2010","Q4-14_2012")
  return(data)
}

********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels. 
#categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
#new_data <- qnames(new_data)
#categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#summary(categories)

************************************************************************************************************
