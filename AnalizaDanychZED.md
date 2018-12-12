---
title: "Zaawansowana Eksploracja Danych"
author: "Wojciech Tulej"
date: "12 grudzieñ, 2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
### Spis treœci

1. [Podsumowanie analizy](#1)
2. [Kod wyliczaj¹cy wykorzystane biblioteki](#2)
3. [Kod zapewniaj¹cy powtarzalnoœæ wyników przy ka¿dym uruchomieniu raportu na tych samych danych](#3)
4. [Kod pozwalaj¹cy wczytaæ dane z pliku.](#4)
5. [Kod usuwaj¹cy z danych wiersze posiadaj¹ce wartoœæ zmiennej res_name](#5)
6. [Kod przetwarzaj¹cy brakuj¹ce dane.](#6)
7. [Sekcjê podsumowuj¹c¹ rozmiar zbioru i podstawowe statystyki](#7)
8. [Kod ograniczaj¹cy liczbê klas (res_name) do 50 najpopularniejszych wartoœci](#8)
9. [Sekcjê sprawdzaj¹c¹ korelacje miêdzy zmiennymi](#9)
10. [Okreœlenie ile przyk³adów ma ka¿da z klas (res_name)](#10)
11. [Wykresy rozk³adów liczby atomów](#11)
12. [Tabelê pokazuj¹c¹ 10 klas z najwiêksz¹ niezgodnoœci¹ liczby atomów i elektronów](#12)
13. [Sekcjê pokazuj¹c¹ rozk³ad wartoœci wszystkich kolumn zaczynaj¹cych siê od part_01 z zaznaczeniem (graficznym i liczbowym) œredniej wartoœci](#13)
14. [Interaktywny wykres lub animacjê](#14)
15. [Sekcjê sprawdzaj¹c¹ czy na podstawie wartoœci innych kolumn mo¿na przewidzieæ liczbê elektronów i atomów oraz z jak¹ dok³adnoœci¹ mo¿na dokonaæ takiej predykcji; trafnoœæ regresji powinna zostaæ oszacowana na podstawie miar R^2 i RMSE;](#15)
16. [Sekcjê próbuj¹c¹ stworzyæ klasyfikator przewiduj¹cy wartoœæ atrybutu res_name (w tej sekcji nale¿y wykorzystaæ wiedzê z pozosta³ych punktów oraz wykonaæ dodatkowe czynnoœci, które mog¹ poprawiæ trafnoœæ klasyfikacji); trafnoœæ klasyfikacji powinna zostaæ oszacowana na danych inne ni¿ ucz¹ce za pomoc¹ mechanizmu (stratyfikowanej!) oceny krzy¿owej lub (stratyfikowanego!) zbioru testowego.](#16)




#<a name="1"/>
###Podsumowanie analizy





#<a name="2"/>
###Kod wyliczaj¹cy wykorzystane biblioteki


```r
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(plotly)
library(caret)
library(knitr)
```


#<a name="3"/>
###Kod zapewniaj¹cy powtarzalnoœæ wyników przy ka¿dym uruchomieniu raportu na tych samych danych


#<a name="4"/>
###Kod pozwalaj¹cy wczytaæ dane z pliku

```r
all <- fread("all_summary.csv", header="auto", sep=";")
```

```
## Warning in fread("all_summary.csv", header = "auto", sep = ";"): Bumped
## column 6 to type character on data row 13920, field contains '260G'.
## Coercing previously read values in this column from logical, integer
## or numeric back to character which may not be lossless; e.g., if '00'
## and '000' occurred before they will now be just '0', and there may be
## inconsistencies with treatment of ',,' and ',NA,' too (if they occurred
## in this column before the bump). If this matters please rerun and set
## 'colClasses' to 'character' for this column. Please note that column type
## detection uses a sample of 1,000 rows (100 rows at 10 points) so hopefully
## this message should be very rare. If reporting to datatable-help, please
## rerun and include the output from verbose=TRUE.
```



#<a name="5"/>
###Kod usuwaj¹cy z danych wiersze posiadaj¹ce wartoœæ zmiennej res_name

```r
all_summary <- all_summary <- all%>% filter(!(res_name %in% c('UNK', 'UNX', 'UNL', 'DUM', 'N', 'BLOB', 'ALA', 'ARG' ,'ASN', 'ASP', 'CYS', 'GLN', 'GLU', 'GLY', 'HIS', 'ILE', 'LEU', 'LYS', 'MET', 'MSE','PHE', 'PRO', 'SEC', 'SER', 'THR', 'TRP', 'TYR', 'VAL', 'DA', 'DG', 'DT', 'DC', 'DU', 'A', 'G', 'T', 'C', 'U', 'HOH', 'H20', 'WAT', "NAN", "", "NA", NA)))
```


#<a name="6"/>
###Kod przetwarzaj¹cy brakuj¹ce dane


```r
all_summary %>% drop_na()
```


#<a name="7"/>
###Sekcjê podsumowuj¹c¹ rozmiar zbioru i podstawowe statystyki




#<a name="8"/>
###Kod ograniczaj¹cy liczbê klas (res_name) do 50 najpopularniejszych wartoœci

```
##     res_name     N
##  1:      SO4 56572
##  2:      GOL 40606
##  3:      EDO 30825
##  4:      NAG 26360
##  5:       CL 23223
##  6:       CA 21038
##  7:       ZN 19826
##  8:       MG 14779
##  9:      HEM 11192
## 10:      PO4 11090
## 11:      ACT  8096
## 12:      DMS  6633
## 13:      IOD  6317
## 14:      PEG  4987
## 15:      CLA  4784
## 16:        K  4706
## 17:      FAD  4555
## 18:      NAD  4501
## 19:       MN  4215
## 20:      ADP  3819
## 21:      MLY  3509
## 22:      NAP  3505
## 23:       CD  3242
## 24:      MPD  3221
## 25:      UNX  3155
## 26:      FMT  2918
## 27:      MAN  2841
## 28:      PG4  2768
## 29:      MES  2697
## 30:       CU  2353
## 31:      ATP  2296
## 32:      COA  2183
## 33:      1PE  2136
## 34:       BR  2127
## 35:      NDP  2106
## 36:      FMN  2084
## 37:      EPE  1933
## 38:      HEC  1917
## 39:      PGE  1905
## 40:      TRS  1656
## 41:      SF4  1647
## 42:       NI  1637
## 43:      ACY  1609
## 44:       FE  1602
## 45:      NO3  1596
## 46:      PLP  1594
## 47:      GDP  1589
## 48:      SAH  1587
## 49:      UNK  1570
## 50:      FE2  1560
##     res_name     N
```

#<a name="9"/>
###Sekcjê sprawdzaj¹c¹ korelacje miêdzy zmiennymi; sekcja ta powinna zawieraæ jak¹œ formê graficznej prezentacji korelacji
![](AnalizaDanychZED_files/figure-html/cor_data_set-1.png)<!-- -->


#<a name="10"/>
###Okreœlenie ile przyk³adów ma ka¿da z klas (res_name)



#<a name="11"/>
###Wykresy rozk³adów liczby atomów (local_res_atom_non_h_count) i elektronów (local_res_atom_non_h_electron_sum) 
#Rozk³ad atomów 
![](AnalizaDanychZED_files/figure-html/atomHist-1.png)<!-- -->

#Rozk³ad elektronów
![](AnalizaDanychZED_files/figure-html/elektronHist-1.png)<!-- -->

#<a name="12"/>
###Tabelê pokazuj¹c¹ 10 klas z najwiêksz¹ niezgodnoœci¹ liczby atomów (local_res_atom_non_h_count vs dict_atom_non_h_count) i tabelê pokazuj¹c¹ 10 klas z najwiêksz¹ niezgodnoœci¹ liczby elektronów (local_res_atom_non_h_electron_sum vs dict_atom_non_h_electron_sum;)
#Niezgodnoœæ atomów

```
## # A tibble: 10 × 2
##    res_name max_diff_atom
##       <chr>         <dbl>
## 1       BV4           102
## 2       15P           101
## 3       CDL            90
## 4       LHI            83
## 5       BV3            80
## 6       DR6            78
## 7       PEU            76
## 8       J96            67
## 9       BV2            60
## 10      SVR            60
```


#Niezgodnoœæ elektronów

```
## # A tibble: 10 × 2
##    res_name max_electron
##       <chr>        <dbl>
## 1       15P          674
## 2       BV4          671
## 3       CDL          564
## 4       BV3          527
## 5       LHI          522
## 6       DR6          520
## 7       PEU          506
## 8       J96          448
## 9       SVR          421
## 10      BV2          395
```

#<a name="13"/>
###Sekcjê pokazuj¹c¹ rozk³ad wartoœci wszystkich kolumn zaczynaj¹cych siê od part_01 z zaznaczeniem (graficznym i liczbowym) œredniej wartoœci
![](AnalizaDanychZED_files/figure-html/his_part_01-1.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-2.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-3.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-4.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-5.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-6.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-7.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-8.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-9.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-10.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-11.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-12.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-13.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-14.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-15.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-16.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-17.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-18.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-19.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-20.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-21.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-22.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-23.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-24.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-25.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-26.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-27.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-28.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-29.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-30.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-31.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-32.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-33.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-34.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-35.png)<!-- -->![](AnalizaDanychZED_files/figure-html/his_part_01-36.png)<!-- -->

#<a name="14"/>
###Interaktywny wykres
<!--html_preserve--><div id="htmlwidget-bab4c70c6acdcd07c5b1" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-bab4c70c6acdcd07c5b1">{"x":{"data":[{"x":[0,25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525,550,575,600,625,650,675,700,725,750,775,800,825,850,875,900,925,950,975,1000,1025,1050,1075,1100,1125,1150,1175,1200,1225,1250,1275,1300,1325,1350,1375,1400,1425,1450,1475,1500,1525,1550,1575,1600,1625,1650,1675,1700,1725,1750,1775,1800,1825,1850],"y":[15517,147046,149936,45723,55126,19573,21761,19433,15711,13629,12680,5801,17361,9006,7584,8828,5278,2310,1072,332,118,192,370,33,145,606,331,16,35,32,18,0,13,2,84,0,0,0,0,0,0,0,0,0,0,0,0,0,8,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5],"text":["count: 15517<br>local_res_atom_non_h_electron_sum: 0","count: 147046<br>local_res_atom_non_h_electron_sum: 25","count: 149936<br>local_res_atom_non_h_electron_sum: 50","count: 45723<br>local_res_atom_non_h_electron_sum: 75","count: 55126<br>local_res_atom_non_h_electron_sum: 100","count: 19573<br>local_res_atom_non_h_electron_sum: 125","count: 21761<br>local_res_atom_non_h_electron_sum: 150","count: 19433<br>local_res_atom_non_h_electron_sum: 175","count: 15711<br>local_res_atom_non_h_electron_sum: 200","count: 13629<br>local_res_atom_non_h_electron_sum: 225","count: 12680<br>local_res_atom_non_h_electron_sum: 250","count: 5801<br>local_res_atom_non_h_electron_sum: 275","count: 17361<br>local_res_atom_non_h_electron_sum: 300","count: 9006<br>local_res_atom_non_h_electron_sum: 325","count: 7584<br>local_res_atom_non_h_electron_sum: 350","count: 8828<br>local_res_atom_non_h_electron_sum: 375","count: 5278<br>local_res_atom_non_h_electron_sum: 400","count: 2310<br>local_res_atom_non_h_electron_sum: 425","count: 1072<br>local_res_atom_non_h_electron_sum: 450","count: 332<br>local_res_atom_non_h_electron_sum: 475","count: 118<br>local_res_atom_non_h_electron_sum: 500","count: 192<br>local_res_atom_non_h_electron_sum: 525","count: 370<br>local_res_atom_non_h_electron_sum: 550","count: 33<br>local_res_atom_non_h_electron_sum: 575","count: 145<br>local_res_atom_non_h_electron_sum: 600","count: 606<br>local_res_atom_non_h_electron_sum: 625","count: 331<br>local_res_atom_non_h_electron_sum: 650","count: 16<br>local_res_atom_non_h_electron_sum: 675","count: 35<br>local_res_atom_non_h_electron_sum: 700","count: 32<br>local_res_atom_non_h_electron_sum: 725","count: 18<br>local_res_atom_non_h_electron_sum: 750","count: 0<br>local_res_atom_non_h_electron_sum: 775","count: 13<br>local_res_atom_non_h_electron_sum: 800","count: 2<br>local_res_atom_non_h_electron_sum: 825","count: 84<br>local_res_atom_non_h_electron_sum: 850","count: 0<br>local_res_atom_non_h_electron_sum: 875","count: 0<br>local_res_atom_non_h_electron_sum: 900","count: 0<br>local_res_atom_non_h_electron_sum: 925","count: 0<br>local_res_atom_non_h_electron_sum: 950","count: 0<br>local_res_atom_non_h_electron_sum: 975","count: 0<br>local_res_atom_non_h_electron_sum: 1000","count: 0<br>local_res_atom_non_h_electron_sum: 1025","count: 0<br>local_res_atom_non_h_electron_sum: 1050","count: 0<br>local_res_atom_non_h_electron_sum: 1075","count: 0<br>local_res_atom_non_h_electron_sum: 1100","count: 0<br>local_res_atom_non_h_electron_sum: 1125","count: 0<br>local_res_atom_non_h_electron_sum: 1150","count: 0<br>local_res_atom_non_h_electron_sum: 1175","count: 8<br>local_res_atom_non_h_electron_sum: 1200","count: 11<br>local_res_atom_non_h_electron_sum: 1225","count: 0<br>local_res_atom_non_h_electron_sum: 1250","count: 0<br>local_res_atom_non_h_electron_sum: 1275","count: 0<br>local_res_atom_non_h_electron_sum: 1300","count: 0<br>local_res_atom_non_h_electron_sum: 1325","count: 0<br>local_res_atom_non_h_electron_sum: 1350","count: 0<br>local_res_atom_non_h_electron_sum: 1375","count: 0<br>local_res_atom_non_h_electron_sum: 1400","count: 0<br>local_res_atom_non_h_electron_sum: 1425","count: 0<br>local_res_atom_non_h_electron_sum: 1450","count: 0<br>local_res_atom_non_h_electron_sum: 1475","count: 0<br>local_res_atom_non_h_electron_sum: 1500","count: 0<br>local_res_atom_non_h_electron_sum: 1525","count: 0<br>local_res_atom_non_h_electron_sum: 1550","count: 0<br>local_res_atom_non_h_electron_sum: 1575","count: 0<br>local_res_atom_non_h_electron_sum: 1600","count: 0<br>local_res_atom_non_h_electron_sum: 1625","count: 0<br>local_res_atom_non_h_electron_sum: 1650","count: 0<br>local_res_atom_non_h_electron_sum: 1675","count: 0<br>local_res_atom_non_h_electron_sum: 1700","count: 0<br>local_res_atom_non_h_electron_sum: 1725","count: 0<br>local_res_atom_non_h_electron_sum: 1750","count: 0<br>local_res_atom_non_h_electron_sum: 1775","count: 0<br>local_res_atom_non_h_electron_sum: 1800","count: 0<br>local_res_atom_non_h_electron_sum: 1825","count: 5<br>local_res_atom_non_h_electron_sum: 1850"],"key":null,"type":"bar","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","name":""}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":60.6392694063927},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Histogram local_res_atom_non_h_electron_sum","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-106.25,1956.25],"ticktext":["0","500","1000","1500"],"tickvals":[0,500,1000,1500],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"local_res_atom_non_h_electron_sum","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-7496.8,157432.8],"ticktext":["0","50000","100000","150000"],"tickvals":[0,50000,100000,150000],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"count","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"barmode":"stack","bargap":0,"hovermode":"closest"},"source":"A"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


#<a name="15"/>
###Sekcjê sprawdzaj¹c¹ czy na podstawie wartoœci innych kolumn mo¿na przewidzieæ liczbê elektronów i atomów oraz z jak¹ dok³adnoœci¹ mo¿na dokonaæ takiej predykcji; trafnoœæ regresji powinna zostaæ oszacowana na podstawie miar R^2 i RMSE

#Dok³adnoœc atomów

```r
all_data_ML <- all
all_data_ML[is.na(all_data_ML)] <- 0
all_data_ML <- dplyr::select_if(all_data_ML, is.numeric)

atom_model <- lm(local_res_atom_non_h_count ~ ., all_data_ML)
atom_summary <- summary(atom_model)

elektron_model <- lm(local_res_atom_non_h_electron_sum ~ ., all_data_ML)
elektron_summary <- summary(elektron_model)
```

Miary dla liczby atomów:<br />
R^2: 0.9998666<br />
RMSEL 0.1761393<br />

Miary dla liczby elektronów:<br />
R^2: 0.9988405<br />
RMSEL 3.4922532<br />


#<a name="16"/>
###Sekcjê próbuj¹c¹ stworzyæ klasyfikator przewiduj¹cy wartoœæ atrybutu res_name (w tej sekcji nale¿y wykorzystaæ wiedzê z pozosta³ych punktów oraz wykonaæ dodatkowe czynnoœci, które mog¹ poprawiæ trafnoœæ klasyfikacji); trafnoœæ klasyfikacji powinna zostaæ oszacowana na danych inne ni¿ ucz¹ce za pomoc¹ mechanizmu (stratyfikowanej!) oceny krzy¿owej lub (stratyfikowanego!) zbioru testowego



```r
all <- subset(all, select=-c(blob_coverage,res_coverage,title,pdb_code,res_id,chain_id,skeleton_data,fo_col,fc_col,weight_col))
all <- all[complete.cases(all), ]
```


```r
all$res_name <- as.character(all$res_name)
all$res_name <- as.factor(all$res_name)
```


```r
inTraining <- createDataPartition(y = all$res_name, p = .8, list = FALSE)
```

```
## Warning in createDataPartition(y = all$res_name, p = 0.8, list = FALSE):
## Some classes have a single record ( 00C, 00J, 014, 016, 018, 01G, 01W, 020,
## 024, 027, 02P, 02Y, 02Z, 034, 03B, 03F, 03K, 03R, 03U, 03W, 03Z, 040, 043,
## 044, 048, 04K, 04L, 04Z, 059, 05S, 067, 069, 06F, 06H, 06J, 06P, 06T, 06Z,
## 071, 076, 07B, 07G, 07K, 07L, 07O, 07S, 07U, 07Z, 084, 085, 088, 08D, 08H,
## 08Z, 091, 094, 095, 09B, 09E, 09F, 09H, 09J, 09K, 09V, 09Z, 0A1, 0AS, 0B1,
## 0B3, 0B4, 0BB, 0BD, 0BM, 0BR, 0C0, 0C3, 0C8, 0C9, 0CA, 0CE, 0CO, 0CS, 0D2,
## 0DB, 0DI, 0EA, 0EN, 0EU, 0EV, 0EX, 0F5, 0F9, 0FK, 0FN, 0FO, 0FR, 0FS, 0FV,
## 0FY, 0FZ, 0G4, 0G8, 0GO, 0GS, 0GU, 0H2, 0H3, 0H4, 0H5, 0H6, 0HL, 0HX, 0I5,
## 0J3, 0J8, 0J9, 0JE, 0JG, 0JH, 0JJ, 0JK, 0JL, 0JP, 0JV, 0JW, 0K0, 0K1, 0K2,
## 0KB, 0KC, 0KD, 0KF, 0KG, 0KH, 0KJ, 0KL, 0KN, 0KO, 0KW, 0L4, 0L6, 0L7, 0LB,
## 0LF, 0LG, 0LK, 0LQ, 0LW, 0LX, 0LZ, 0M6, 0MA, 0MN, 0MP, 0MQ, 0MT, 0MW, 0MX,
## 0MY, 0N1, 0N2, 0N5, 0NJ, 0NK, 0NP, 0NR, 0NS, 0NX, 0NY, 0O1, 0O7, 0O8, 0O9,
## 0OA, 0OB, 0OF, 0OK, 0OL, 0OM, 0ON, 0OQ, 0OV, 0OW, 0OY, 0P4, 0P5, 0P6, 0P8,
## 0PF, 0PJ, 0PQ, 0Q1, 0Q5, 0QF, 0QJ, 0QV, 0QW, 0QX, 0R4, 0R5, 0R6, 0R7, 0R8,
## 0R9, 0RA, 0RB, 0RF, 0RJ, 0RN, 0RO, 0RP, 0RS, 0RT, 0RX, 0S0, 0S4, 0S5, 0S6,
## 0SB, 0SC, 0SE, 0SF, 0SK, 0SL, 0SR, 0SS, 0SU, 0SV, 0SX, 0T5, 0TA, 0TB, 0TE,
## 0TJ, 0TK, 0TM, 0TQ, 0TS, 0TU, 0TZ, 0U0, 0U3, 0U4, 0U9, 0UB, 0UM, 0UN, 0UO,
## 0UP, 0UQ, 0US, 0UU, 0UX, 0UZ, 0V0, 0V2, 0V4, 0V8, 0VD, 0VE, 0VF, 0VG, 0VH,
## 0VK, 0VM, 0VN, 0VO, 0VR, 0VX, 0VY, 0VZ, 0W7, 0W8, 0WA, 0WB, 0WH, 0WR, 0WT,
## 0WU, 0WX, 0WY, 0X6, 0XA, 0XC, 0XD, 0XF, 0XG, 0XH, 0XJ, 0XP, 0XS, 0XU, 0XZ,
## 0Y3, 0YG, 0YH, 0YJ, 0YO, 0ZA, 101, 103, 104, 10H, 10K, 10P, 10R, 10S, 10Z,
## 11G, 11J, 11K, 11P, 11R, 11S, 121, 123, 129, 12F, 12Z, 130, 132, 134, 135,
## 136, 13I, 13J, 13K, 13L, 13M, 13N, 14D, 14F, 14I, 14J, 14K, 14N, 14S, 14X,
## 14Z, 150, 156, 15C, 15I, 15N, 15O, 15W, 16J, 16K, 16M, 16Q, 16R, 17H, 17K,
## 17L, 17P, 17V, 17W, 17X, 180, 184, 186, 188, 18E, 18K, 18N, 18R, 18U, 18W,
## 18Z, 19A, 19B, 19C, 19E, 19J, 19K, 19L, 19N, 19P, 19Q, 19R, 19S, 19T, 19U,
## 1A0, 1A3, 1A4, 1A6, 1A7, 1A9, 1AD, 1AE, 1AJ, 1AM, 1AN, 1AO, 1AT, 1AX, 1B1,
## 1B4, 1B5, 1B6, 1B7, 1B8, 1B9, 1BA, 1BB, 1BC, 1BE, 1BF, 1BH, 1BI, 1BJ, 1BK,
## 1BM, 1BP, 1BQ, 1BS, 1BT, 1C2, 1C4, 1C7, 1CB, 1CH, 1CK, 1CQ, 1CR, 1CT, 1CV,
## 1CW, 1CX, 1D0, 1D1, 1DC, 1DG, 1DI, 1DR, 1DS, 1DT, 1E2, 1EA, 1EJ, 1EL, 1EY,
## 1EZ, 1F0, 1FC, 1FD, 1FE, 1FF, 1FK, 1FN, 1FR, 1FW, 1FX, 1FY, 1G0, 1G7, 1GA,
## 1GE, 1GG, 1GK, 1GO, 1GW, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1HD, 1HG,
## 1HH, 1HJ, 1HK, 1HL, 1HM, 1HN, 1HO, 1HQ, 1HR, 1HT, 1HU, 1J2, 1J3, 1J4, 1J9,
## 1JD, 1JH, 1JL, 1JM, 1JV, 1JZ, 1K3, 1K4, 1K9, 1KH, 1KO, 1L5, 1L7, 1LB, 1LC,
## 1LE, 1LF, 1LG, 1LI, 1LL, 1LM, 1LN, 1LR, 1LT, 1LU, 1LW, 1LX, 1LY, 1LZ, 1M0,
## 1M2, 1M4, 1M5, 1M6, 1M7, 1M9, 1MJ, 1MM, 1MN, 1MO, 1MQ, 1MR, 1MX, 1N6, 1NK,
## 1NL, 1NQ, 1NR, 1O5, 1O8, 1OA, 1OB, 1OC, 1OG, 1OH, 1OK, 1ON, 1OQ, 1OU, 1OV,
## 1P4, 1P5, 1P6, 1P7, 1P9, 1PF, 1PH, 1Q3, 1QA, 1QB, 1QP, 1QV, 1QW, 1R5, 1RH,
## 1RN, 1RP, 1RQ, 1RR, 1RT, 1S1, 1S7, 1S8, 1S9, 1SA, 1SE, 1SF, 1SG, 1SN, 1SO,
## 1ST, 1T4, 1T6, 1T7, 1TE, 1TM, 1TR, 1TT, 1TU, 1U2, 1U4, 1U7, 1UH, 1UJ, 1UK,
## 1UP, 1V5, 1VC, 1VK, 1VM, 1VN, 1VS, 1VV, 1VZ, 1W0, 1W2, 1W7, 1WF, 1WG, 1WJ,
## 1WK, 1WM, 1WN, 1WP, 1WW, 1WY, 1X8, 1XA, 1XL, 1XM, 1XV, 1XX, 1Y7, 1YC, 1YD,
## 1YF, 1YM, 1YO, 1YP, 1YQ, 1YR, 1YZ, 1Z0, 1Z6, 1Z8, 1ZB, 1ZG, 1ZQ, 207, 208,
## 20A, 20C, 20E, 20K, 20Q, 20U, 217, 21B, 21C, 21E, 21G, 21H, 21L, 21M, 21O,
## 21Q, 21S, 21U, 21V, 21Y, 21Z, 225, 227, 228, 22A, 22E, 22G, 22J, 22K, 22L,
## 22M, 22T, 22U, 237, 238, 239, 23B, 23F, 23H, 23K, 23L, 23M, 23R, 23U, 23X,
## 24D, 24I, 24P, 24T, 24U, 24W, 24X, 24Y, 254, 258, 259, 25K, 25O, 25Q, 25R,
## 25S, 25Y, 260, 264, 265, 266, 268, 26E, 26F, 26K, 26L, 26M, 26R, 26S, 26U,
## 26V, 26W, 26Y, 26Z, 271, 273, 275, 276, 278, 279, 27D, 27P, 27R, 27S, 27V,
## 27Z, 280, 281, 282, 284, 285, 286, 28A, 28E, 28F, 28G, 28N, 28S, 28U, 28Z,
## 29A, 29B, 29G, 29H, 29U, 29V, 29X, 2A4, 2A5, 2A7, 2A8, 2A9, 2AC, 2AE, 2AF,
## 2AH, 2AJ, 2AU, 2B2, 2B7, 2B8, 2BG, 2BQ, 2BR, 2BV, 2BW, 2BX, 2BY, 2BZ, 2C0,
## 2C5, 2C9, 2CB, 2CD, 2CM, 2CN, 2CU, 2D0, 2D1, 2D2, 2D3, 2D4, 2D7, 2DB, 2DD,
## 2DL, 2DR, 2DS, 2E3, 2E8, 2EB, 2EF, 2EJ, 2EK, 2EM, 2EQ, 2ET, 2EX, 2EZ, 2F2,
## 2F6, 2FB, 2FQ, 2FR, 2FY, 2G1, 2G7, 2GA, 2GD, 2GG, 2GJ, 2GM, 2GW, 2H2, 2H8,
## 2HI, 2HK, 2HQ, 2HR, 2HS, 2HT, 2HU, 2HV, 2HW, 2HX, 2HY, 2HZ, 2IA, 2IC, 2IM,
## 2J0, 2J1, 2J2, 2J5, 2J6, 2J7, 2JA, 2JL, 2JQ, 2JY, 2K4, 2K6, 2KC, 2KF, 2KL,
## 2KN, 2KU, 2KW, 2KZ, 2L1, 2L2, 2LF, 2LG, 2LN, 2LQ, 2LR, 2MD, 2ME, 2MN, 2MQ,
## 2MV, 2MZ, 2N0, 2N5, 2N6, 2N7, 2N8, 2NB, 2ND, 2NI, 2NJ, 2NK, 2NP, 2NQ, 2NR,
## 2NS, 2NW, 2NX, 2NY, 2O0, 2O1, 2O3, 2O4, 2O5, 2O7, 2OE, 2OO, 2OQ, 2OS, 2P4,
## 2P6, 2P7, 2P8, 2PB, 2PF, 2PK, 2PX, 2PZ, 2Q4, 2Q8, 2Q9, 2QA, 2QD, 2QM, 2QN,
## 2QO, 2QP, 2QR, 2QT, 2QU, 2R4, 2R6, 2R8, 2RF, 2RJ, 2RL, 2RS, 2RT, 2RU, 2RV,
## 2S0, 2S1, 2S2, 2S3, 2S4, 2S6, 2S7, 2S8, 2S9, 2SD, 2SE, 2SF, 2SH, 2SJ, 2SK,
## 2SM, 2SX, 2SY, 2T2, 2T7, 2T9, 2TC, 2TH, 2TS, 2U0, 2U2, 2U3, 2U4, 2U5, 2U6,
## 2UG, 2UP, 2UQ, 2UR, 2US, 2UT, 2V0, 2V2, 2V3, 2V9, 2VE, 2VK, 2VL, 2VP, 2VZ,
## 2W0, 2W6, 2WB, 2WD, 2WH, 2WJ, 2WK, 2WP, 2WQ, 2WR, 2X4, 2X5, 2X6, 2X7, 2X9,
## 2XC, 2XG, 2XP, 2XX, 2XY, 2Y2, 2Y3, 2Y4, 2YO, 2YP, 2YT, 2YU, 2Z4, 2ZO, 2ZQ,
## 2ZT, 2ZU, 2ZY, 304, 308, 30E, 30G, 30H, 30K, 30Q, 30V, 319, 31K, 31L, 31N,
## 31R, 31U, 31V, 31X, 31Y, 320, 32A, 32D, 32G, 32H, 32J, 32O, 32Q, 32V, 32W,
## 330, 333, 335, 337, 33F, 33H, 33J, 33N, 33P, 33U, 33Y, 341, 342, 344, 348,
## 349, 34A, 34I, 34N, 34Q, 353, 357, 358, 35B, 365, 368, 36C, 376, 379, 37B,
## 37F, 37G, 37H, 37L, 37N, 37U, 380, 381, 384, 387, 389, 38F, 38G, 38J, 38M,
## 38S, 38T, 38W, 390, 391, 392, 394, 396, 39D, 39F, 39O, 39S, 39U, 39Z, 3A3,
## 3A6, 3A8, 3A9, 3AC, 3AL, 3AO, 3AW, 3AX, 3BC, 3BH, 3BI, 3BK, 3BL, 3BM, 3BP,
## 3BQ, 3BW, 3BX, 3C3, 3CB, 3CC, 3CR, 3CY, 3D3, 3D7, 3D8, 3D9, 3DC, 3DK, 3DL,
## 3DT, 3DV, 3DY, 3E1, 3E4, 3EC, 3EH, 3EL, 3EV, 3EW, 3EX, 3F5, 3F6, 3F8, 3FE,
## 3FF, 3FH, 3FN, 3FO, 3FR, 3FV, 3FY, 3FZ, 3G1, 3G7, 3GE, 3GG, 3GT, 3GU, 3GY,
## 3GZ, 3H0, 3H2, 3H6, 3H8, 3HJ, 3HK, 3HN, 3HP, 3HQ, 3HY, 3I6, 3IB, 3J9, 3JA,
## 3JC, 3JE, 3JM, 3JW, 3JZ, 3K1, 3K6, 3KC, 3KL, 3KP, 3KZ, 3LI, 3LN, 3LO, 3LQ,
## 3LV, 3LZ, 3MB, 3MH, 3MO, 3MT, 3N2, 3N5, 3N7, 3N9, 3NA, 3NE, 3NF, 3NJ, 3NL,
## 3NS, 3NU, 3NV, 3NW, 3O0, 3O7, 3OA, 3OB, 3OK, 3OO, 3OP, 3OR, 3OV, 3OW, 3OX,
## 3OZ, 3P0, 3P2, 3PI, 3PQ, 3PS, 3PU, 3Q0, 3Q1, 3Q2, 3Q3, 3Q4, 3Q5, 3Q6, 3QD,
## 3QI, 3QK, 3QN, 3QO, 3QR, 3QU, 3QW, 3QX, 3QY, 3R0, 3R1, 3RC, 3RE, 3RF, 3RG,
## 3RS, 3RU, 3RZ, 3S1, 3SC, 3SE, 3SF, 3SJ, 3SS, 3SY, 3T8, 3T9, 3TA, 3TB, 3TG,
## 3TI, 3TN, 3TT, 3U1, 3U5, 3U6, 3UB, 3UD, 3UR, 3US, 3UT, 3UW, 3UX, 3UY, 3UZ,
## 3V0, 3V8, 3VC, 3VD, 3VE, 3VM, 3VO, 3VP, 3W5, 3WA, 3WN, 3WO, 3WP, 3WQ, 3X2,
## 3X5, 3XH, 3XS, 3XX, 3Y3, 3Y4, 3YN, 3YV, 3YX, 3YZ, 3Z1, 3Z2, 3Z3, 3Z4, 3Z5,
## 3Z6, 3Z7, 3Z8, 3Z9, 3ZB, 3ZG, 3ZW, 400, 403, 40L, 40M, 40W, 40X, 40Y, 40Z,
## 410, 412, 41A, 41L, 421, 427, 428, 42K, 42Q, 437, 439, 43C, 43D, 43F, 43K,
## 43M, 43P, 43S, 440, 442, 443, 445, 446, 449, 44F, 44U, 44X, 454, 455, 45C,
## 45I, 45K, 461, 46C, 46D, 46G, 46K, 46M, 472, 475, 47C, 47W, 480, 481, 484,
## 488, 48B, 48P, 48T, 497, 498, 49A, 49C, 4AL, 4AM, 4AN, 4AU, 4AW, 4AX, 4AZ,
## 4B3, 4BD, 4BG, 4BL, 4BO, 4BP, 4BQ, 4BS, 4BU, 4BY, 4BZ, 4C0, 4C4, 4CD, 4CK,
## 4CL, 4D7, 4D9, 4DC, 4DH, 4DP, 4E2, 4E3, 4F3, 4F8, 4FA, 4FC, 4FE, 4FH, 4FJ,
## 4FR, 4FY, 4G2, 4G5, 4G8, 4GE, 4H2, 4HB, 4HG, 4HK, 4HO, 4I4, 4IR, 4JG, 4JP,
## 4JR, 4K0, 4KE, 4KR, 4LI, 4ME, 4MK, 4MM, 4MP, 4MY, 4NB, 4ND, 4NI, 4NN, 4O8,
## 4O9, 4OX, 4PG, 4PO, 4PS, 4QC, 4QS, 4RB, 4TC, 4TN, 4TP, 4TT, 4UB, 4VQ, 4XX,
## 4ZE, 505, 506, 507, 509, 50Q, 50Z, 512, 517, 518, 51J, 51K, 51U, 526, 527,
## 530, 532, 533, 534, 53A, 53U, 53Z, 548, 54D, 54M, 552, 556, 55E, 561, 564,
## 566, 567, 568, 569, 570, 571, 574, 589, 58Z, 591, 593, 596, 598, 59A, 59C,
## 59G, 5AE, 5AN, 5AO, 5AZ, 5B1, 5B2, 5BE, 5BM, 5BR, 5CH, 5CO, 5DI, 5DS, 5EO,
## 5EZ, 5FB, 5FH, 5FO, 5FR, 5GS, 5HK, 5HT, 5IO, 5JZ, 5K3, 5LK, 5MB, 5MH, 5MP,
## 5MS, 5MX, 5MZ, 5ND, 5NH, 5NI, 5OB, 5P3, 5PS, 5PV, 5R1, 5RI, 5SC, 5SD, 5TF,
## 5TN, 5UM, 5UN, 5Y0, 5Z5, 5ZE, 607, 609, 610, 611, 612, 614, 616, 617, 61E,
## 61K, 61N, 624, 625, 62A, 62D, 62K, 62N, 62U, 631, 635, 637, 63C, 640, 643,
## 647, 64P, 64U, 656, 659, 662, 663, 664, 665, 667, 671, 673, 675, 676, 67Z,
## 682, 685, 68Z, 692, 69P, 6AC, 6AD, 6AM, 6BC, 6BG, 6C3, 6CM, 6CR, 6DS, 6FC,
## 6GS, 6HK, 6KZ, 6LI, 6LV, 6M0, 6ME, 6MH, 6MK, 6MN, 6NH, 6OC, 6PO, 6SC, 6SE,
## 6T6, 6TG, 6UP, 6W2, 6X1, 6XE, 6XP, 6XS, 6Z0, 704, 709, 70B, 70U, 711, 719,
## 728, 72B, 735,
```

```r
training <- all[ inTraining,]
testing  <- all[-inTraining,]
rfGrid <- expand.grid(mtry = 10:30)
```


```r
gridCtrl <- trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 3)
```


```r
fitTune <- train(res_name ~ .,
             data = training,
             method = "rf",
             trControl = gridCtrl,
             tuneGrid = rfGrid,
             ntree = 30)

fitTune
```

macierz pomy³ek i miary dla ka¿dej klasy

{r evaluateClassification }
   
predClasses <- predict(fitTune,
                         newdata = testing)
cm <- confusionMatrix(data = predClasses, 
                testing$res_name)

cm$overall
cm$byClass %>% kable()

