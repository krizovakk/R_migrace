
Ltxt <- list.files(paste0(path_ces, proj), pattern = "*.txt")  # Identify file names
df_filled <- data.frame()
colnam <- c("zmena","duvod","typ_cisla","cislo",
            "s5","s6", "s7", "s8",
            "s9","s10","s11","s12")


for(i in Ltxt) {
  a <- read.table(paste0(path_ces, proj, "/", i),
                  skip = 4, header = F, sep = ";", fill = T,
                  check.names = F, fileEncoding="cp1250")
  names(a) <- colnam
  rows_to_change <- a$zmena == "N"
  a[rows_to_change,3:12] <- a[rows_to_change,2:12] # shitf columns with N
  a[rows_to_change,2] <- NA
  a$zdroj <- paste(i) # add source txt file name
  a <- a %>% filter(str_detect(zmena, ("^N$|^V$|^R$")))  # filter only relevant rows !!!
  df_filled <- rbind(df_filled,a)
}

# FUNKCNI SMYCKA PRO NACITANI CES EXPORTNICH TXT ^^^

rm(a)
rm(i)
rm(colnam)
rm(Ltxt)
rm(rows_to_change)
```

## Check CES txt

```{r CES PROCESSING}

ces <- df_filled %>% discard(~all(is.na(.) | . =="")) # drop empy columns
ces[ces == ""] <- NA

sort(unique(ces$zmena))
sort(unique(ces$duvod))
sort(unique(ces$typ_cisla))

dupl <- ces %>% group_by(cislo) %>%
  filter(n() != 1)

ces_ic <- ces %>% filter(typ_cisla == "I")
ces_pr <- ces %>% filter(typ_cisla == "P")

count(ces_ic)+count(ces_pr) == count(ces) # TRUE or FALSE

```

# CES INVC

PODSBIRKY:

  Grafika: \{S\}\{####\}
    Kresby: \{S\}\{####\}
      Obrazy: \{S\}\{####\}
        Sochy: \{S\}\{####\}
          Uzite umeny: \{S\}\{####\}

            ## Modify ces_ic

            ```{r}

            ic <- ces_ic %>%
              mutate(Mrada = str_extract(cislo, "[:alpha:]"),
                     Mcislo = str_extract(cislo, "[:digit:]+"),
                     Mcislo_0 = str_pad(Mcislo, 4, pad = "0")) %>%
              unite(MINVC, c(Mrada, Mcislo_0), sep = "", na.rm = T, remove = F) %>%
              select(zdroj, typ_cisla, cislo, zmena, duvod, MINVC)


            # CHECK !!! IMPORTANT !!!

            sum(is.na(ic$zmena))
            sum(is.na(ic$duvod)) # pocet NA by mel sedet s poctem N
            sum(is.na(ic$duvod)) == sum(ic$zmena == "N")
            sum(is.na(ic$typ_cisla))
            sum(is.na(ic$cislo))
            sum(is.na(ic$MINVC))

            dupli <- ic %>% group_by(MINVC) %>% filter(n() != 1) %>%
              mutate(popis_chyby = "duplicitni cislo CES")

            clear <- ic %>% group_by(MINVC) %>% filter(n() == 1)

            # chyby kontrolovat rucne a rucne nastavit podminky pro nestandardni formaty
            chyby <- ic %>% filter(str_detect(cislo, ".*/.*")) %>%
              mutate(popis_chyby = "neni cislo CES ?")

            # LOAD MUSEION

            mus_ic <- read_excel(paste0(path_gmur, "MUSEION/gmur_exportCES_KAT.xlsx"),
                                 sheet = 1, col_types="text") %>%
              mutate(id = floor(as.numeric(id)),
                     sp_porcislo = trunc(as.numeric(sp_porcislo)))

            # P A I R

            rel <- left_join(clear, mus_ic, by = c("MINVC" = "sp_cislo"), keep = T) %>%
              select(-sp_porcislo, -sp_porcislosub, -sp_porcislodo, -sp_porcislosub_1, -cislo.y, -kod) %>%
              rename(cislo = cislo.x)

            pair <- rel %>% filter(!is.na(sp_cislo)) %>% mutate(status = "sparovano")
            pair_not <- rel %>% filter(is.na(sp_cislo)) %>%
              mutate(popis_chyby = "nenalezeno v MUSEIONU",
                     id = as.character(id))
