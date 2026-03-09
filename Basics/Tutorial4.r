
R version 4.4.3 (2025-02-28 ucrt) -- "Trophy Case"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

During startup - Warning message:
Using locale code page other than 65001 ("UTF-8") may cause problems. 
>  foo <- "This is a character string!"
> foo
[1] "This is a character string!"
>  substr(x=foo,start=21,stop=27)
[1] "string!"
>  substr(x=foo,start=1,stop=4) <- "Here"
> foo
[1] "Here is a character string!"
>  bar <- "How much wood could a woodchuck chuck"
> bar
[1] "How much wood could a woodchuck chuck"
>  bar <- "How much wood could a woodchuck chuck"
> bar
[1] "How much wood could a woodchuck chuck"
> gsub(pattern="chuck",replacement="hurl",x=bar)
[1] "How much wood could a woodhurl hurl"
> #Factors
> # Identifying Categories
> firstname <- c("Liz","Jolene","Susan","Boris","Rochelle","Tim","Simon",
+ "Amy"
+ 
+ 

+ > firstname <- c("Liz","Jolene","Susan","Boris","Rochelle","Tim","Simon",
+ "Amy")
> sex.num <- c(0,0,0,1,0,1,1,0)
> sex.char <- c("female","female","female","male","female","male","male",
+ "female")
> sex.num.fac <- factor(x=sex.num)
> sex.num.fac
[1] 0 0 0 1 0 1 1 0
Levels: 0 1
>  sex.char.fac <- factor(x=sex.char)
> sex.char.fac
[1] female female female male   female male   male   female
Levels: female male
> levels(x=sex.num.fac)
[1] "0" "1"
> levels(x=sex.char.fac)
[1] "female" "male"  
> levels(x=sex.num.fac) <- c("1","2")
> sex.num.fac
[1] 1 1 1 2 1 2 2 1
Levels: 1 2
>  sex.char.fac[2:5]
[1] female female male   female
Levels: female male
> sex.char.fac[c(1:3,5,8)]
[1] female female female female female
Levels: female male
> sex.num.fac=="2"
[1] FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE
>  firstname[sex.char.fac=="male"]
[1] "Boris" "Tim"   "Simon"
> #Defining and Ordering Levels
>  mob <- c("Apr","Jan","Dec","Sep","Nov","Jul","Jul","Jun")
> mob[2]
[1] "Jan"
> mob[3]
[1] "Dec"
> mob[2]<mob[3]
[1] FALSE
>  ms <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov",
+ "Dec")
> mob.fac <- factor(x=mob,levels=ms,ordered=TRUE)
>  mob.fac
[1] Apr Jan Dec Sep Nov Jul Jul Jun
12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < ... < Dec
> mob.fac[2]
[1] Jan
12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < ... < Dec
> mob.fac[2]
[1] Jan
12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < ... < Dec
>  mob.fac[3]
[1] Dec
12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < ... < Dec
> #Combining and Cutting
> foo <- c(5.1,3.3,3.1,4)bar <- c(4.5,1.2)
Error: unexpected symbol in "foo <- c(5.1,3.3,3.1,4)bar"
> foo <- c(5.1,3.3,3.1,4)
> bar<-c(4.5,1.2)
> c(foo,bar)
[1] 5.1 3.3 3.1 4.0 4.5 1.2
> new.values <- factor(x=c("Oct","Feb","Feb"),levels=levels(mob.fac),
+ ordered=TRUE)
> new.values
[1] Oct Feb Feb
12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < ... < Dec
> c(mob.fac,new.values)
 [1] Apr Jan Dec Sep Nov Jul Jul Jun Oct Feb Feb
12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < ... < Dec
>  levels(mob.fac)
 [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
[12] "Dec"
>  levels(mob.fac)[c(mob.fac,new.values)]
 [1] "Apr" "Jan" "Dec" "Sep" "Nov" "Jul" "Jul" "Jun" "Oct" "Feb" "Feb"
>  mob.new <- levels(mob.fac)[c(mob.fac,new.values)]
> mob.new.fac <- factor(x=mob.new,levels=levels(mob.fac),ordered=TRUE)
> mob.new.fac
 [1] Apr Jan Dec Sep Nov Jul Jul Jun Oct Feb Feb
12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < ... < Dec
> br <- c(0,2,4,6)
> cut(x=Y,breaks=br)
Error: object 'Y' not found
> br <- c(0,2,4,6)
> br
[1] 0 2 4 6
> cut(x=Y,breaks=br)
Error: object 'Y' not found
>  Y <- c(0.53,5.4,1.5,3.33,0.45,0.01,2,4.2,1.99,1.01)
> cut(x=Y,breaks=br)
 [1] (0,2] (4,6] (0,2] (2,4] (0,2] (0,2] (0,2] (4,6] (0,2] (0,2]
Levels: (0,2] (2,4] (4,6]
> lab <- c("Small","Medium","Large")
> cut(x=Y,breaks=br,right=F,include.lowest=T,labels=lab)
 [1] Small  Large  Small  Medium Small  Small  Medium Large  Small 
[10] Small 
Levels: Small Medium Large
> 
