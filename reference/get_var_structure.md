# Get Variable Structure Summary from SL4 and HAR Objects

Generates a summary of the variables within one or more SL4 or HAR
objects, listing their dimension sizes, structures, and optionally,
column and observation counts.

## Usage

``` r
get_var_structure(variables = NULL, ..., include_col_size = FALSE)
```

## Arguments

- variables:

  Character vector. Variable names to summarize. Use `NULL` or `"ALL"`
  to summarize all variables.

- ...:

  One or more SL4 or HAR objects created using
  [`load_sl4x()`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md)
  or
  [`load_harx()`](https://bodysbobb.github.io/HARplus/reference/load_harx.md).

- include_col_size:

  Logical. If `TRUE`, includes column and observation counts. Default is
  `FALSE`.

## Value

A named list, where each element contains a data frame with:

- `Variable`: The variable name.

- `Dimensions`: The associated dimensions.

- `DimSize`: The number of dimensions.

- `DataShape`: The shape of the data (e.g., `10x20x30`).

- `No.Col`: (Optional) The number of columns.

- `No.Obs`: (Optional) The number of observations.

## Details

- Extracts dimension structures for variables in one or more SL4 or HAR
  datasets.

- If `include_col_size = TRUE`, adds column and observation counts.

- Supports multiple datasets and returns results as a named list, with
  each dataset’s summary stored separately.

- Can summarize specific variables or `"ALL"`.

## See also

[`get_dim_patterns`](https://bodysbobb.github.io/HARplus/reference/get_dim_patterns.md),
[`get_dim_elements`](https://bodysbobb.github.io/HARplus/reference/get_dim_elements.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Import data sample:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data1 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Get summary for all variables in a single dataset
get_var_structure(data_obj = sl4_data)
#> $data_obj
#>           Variable        Dimensions DimSize  DataShape
#> 1      CNTalleffcr          COMM*REG       2      10x10
#> 2       CNTalleffr          REG*CTAX       2       10x9
#> 3          CNTdpar               REG       1         10
#> 4          CNTendw          ENDW*REG       2       5x10
#> 5         CNTendwr               REG       1         10
#> 6            CNTkb               REG       1         10
#> 7          CNTpinv               REG       1         10
#> 8           CNTpop               REG       1         10
#> 9           CNTqca     COMM*ACTS*REG       3   10x10x10
#> 10           CNTqe          ENDW*REG       2       5x10
#> 11          CNTqea     ENDW*ACTS*REG       3    5x10x10
#> 12          CNTqfd     COMM*ACTS*REG       3   10x10x10
#> 13          CNTqfe     ENDW*ACTS*REG       3    5x10x10
#> 14        CNTqfeer          ENDW*REG       2       5x10
#> 15         CNTqfer               REG       1         10
#> 16          CNTqfm     COMM*ACTS*REG       3   10x10x10
#> 17          CNTqfr           REG*DIR       2       10x2
#> 18          CNTqgd          COMM*REG       2      10x10
#> 19          CNTqgm          COMM*REG       2      10x10
#> 20          CNTqgr           REG*DIR       2       10x2
#> 21          CNTqia           REG*DIR       2       10x2
#> 22          CNTqid          COMM*REG       2      10x10
#> 23          CNTqim          COMM*REG       2      10x10
#> 24          CNTqms      COMM*REG*REG       3   10x10x10
#> 25         CNTqmsr               REG       1         10
#> 26           CNTqo          ACTS*REG       2      10x10
#> 27          CNTqor               REG       1         10
#> 28          CNTqpd          COMM*REG       2      10x10
#> 29          CNTqpm          COMM*REG       2      10x10
#> 30          CNTqpr           REG*DIR       2       10x2
#> 31          CNTqxs      COMM*REG*REG       3   10x10x10
#> 32         CNTqxsr               REG       1         10
#> 33      CNTtech_af     COMM*ACTS*REG       3   10x10x10
#> 34     CNTtech_afe     ENDW*ACTS*REG       3    5x10x10
#> 35    CNTtech_aint          ACTS*REG       2      10x10
#> 36     CNTtech_ams      COMM*REG*REG       3   10x10x10
#> 37      CNTtech_ao          ACTS*REG       2      10x10
#> 38  CNTtech_atmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 39     CNTtech_ava          ACTS*REG       2      10x10
#> 40        CNTtechr      TECHTYPE*REG       2       7x10
#> 41         CNTtotr               REG       1         10
#> 42              EV               REG       1         10
#> 43          EV_ALT               REG       1         10
#> 44             afa     COMM*ACTS*REG       3   10x10x10
#> 45           afall     COMM*ACTS*REG       3   10x10x10
#> 46           afcom              COMM       1         10
#> 47             afe     ENDW*ACTS*REG       3    5x10x10
#> 48          afeall     ENDW*ACTS*REG       3    5x10x10
#> 49          afecom              ENDW       1          5
#> 50          afereg               REG       1         10
#> 51          afesec              ACTS       1         10
#> 52           afreg               REG       1         10
#> 53           afsec              ACTS       1         10
#> 54            aint          ACTS*REG       2      10x10
#> 55         aintall          ACTS*REG       2      10x10
#> 56         aintreg               REG       1         10
#> 57         aintsec              ACTS       1         10
#> 58             ams      COMM*REG*REG       3   10x10x10
#> 59              ao          ACTS*REG       2      10x10
#> 60           aoall          ACTS*REG       2      10x10
#> 61           aoreg               REG       1         10
#> 62           aosec              ACTS       1         10
#> 63           atall MARG*COMM*REG*REG       4 1x10x10x10
#> 64             atd               REG       1         10
#> 65             atf              COMM       1         10
#> 66             atm              COMM       1         10
#> 67          atmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 68             ats               REG       1         10
#> 69              au               REG       1         10
#> 70             ava          ACTS*REG       2      10x10
#> 71          avaall          ACTS*REG       2      10x10
#> 72          avareg               REG       1         10
#> 73          avasec              ACTS       1         10
#> 74           c1_cr          COMM*REG       2      10x10
#> 75            c1_r               REG       1         10
#> 76           c2_cr          COMM*REG       2      10x10
#> 77            c2_r               REG       1         10
#> 78           c3_cr          COMM*REG       2      10x10
#> 79            c3_r               REG       1         10
#> 80        cgdslack               REG       1         10
#> 81       compvalad          ACTS*REG       2      10x10
#> 82      contgdpexp         REG*GDPEX       2       10x6
#> 83     del_indtaxr               REG       1         10
#> 84     del_taxrexp               REG       1         10
#> 85      del_taxrfu               REG       1         10
#> 86      del_taxrgc               REG       1         10
#> 87      del_taxric               REG       1         10
#> 88     del_taxrimp               REG       1         10
#> 89     del_taxrinc               REG       1         10
#> 90      del_taxriu               REG       1         10
#> 91     del_taxrout               REG       1         10
#> 92      del_taxrpc               REG       1         10
#> 93        del_tbal               REG       1         10
#> 94       del_tbalc          COMM*REG       2      10x10
#> 95      del_tbalry               REG       1         10
#> 96       del_ttaxr               REG       1         10
#> 97            dpav               REG       1         10
#> 98          dpavev               REG       1         10
#> 99           dpgov               REG       1         10
#> 100         dppriv               REG       1         10
#> 101         dpsave               REG       1         10
#> 102          dpsum               REG       1         10
#> 103      endwslack          ENDW*REG       2       5x10
#> 104         expand         ENDWC*REG       2       1x10
#> 105        fincome               REG       1         10
#> 106    incomeslack               REG       1         10
#> 107             kb               REG       1         10
#> 108             ke               REG       1         10
#> 109              p               REG       1         10
#> 110             pb          ACTS*REG       2      10x10
#> 111            pca     COMM*ACTS*REG       3   10x10x10
#> 112           pcif      COMM*REG*REG       3   10x10x10
#> 113            pds          COMM*REG       2      10x10
#> 114            pdw               REG       1         10
#> 115             pe        ENDWMS*REG       2       4x10
#> 116            peb     ENDW*ACTS*REG       3    5x10x10
#> 117    pebfactreal        ENDWMS*REG       2       4x10
#> 118     pefactreal        ENDWMS*REG       2       4x10
#> 119            pes     ENDW*ACTS*REG       3    5x10x10
#> 120            pfa     COMM*ACTS*REG       3   10x10x10
#> 121        pfactor               REG       1         10
#> 122      pfactreal     ENDW*ACTS*REG       3    5x10x10
#> 123            pfd     COMM*ACTS*REG       3   10x10x10
#> 124            pfe     ENDW*ACTS*REG       3    5x10x10
#> 125            pfm     COMM*ACTS*REG       3   10x10x10
#> 126           pfob      COMM*REG*REG       3   10x10x10
#> 127            pga          COMM*REG       2      10x10
#> 128            pgd          COMM*REG       2      10x10
#> 129           pgdp               REG       1         10
#> 130            pgm          COMM*REG       2      10x10
#> 131           pgov               REG       1         10
#> 132            pia          COMM*REG       2      10x10
#> 133            pid          COMM*REG       2      10x10
#> 134            pim          COMM*REG       2      10x10
#> 135           pint          ACTS*REG       2      10x10
#> 136           pinv               REG       1         10
#> 137          pm_cr          COMM*REG       2      10x10
#> 138           pmds      COMM*REG*REG       3   10x10x10
#> 139            pms          COMM*REG       2      10x10
#> 140            pmw          COMM*REG       2      10x10
#> 141         pmwcom              COMM       1         10
#> 142         pmwreg               REG       1         10
#> 143             po          ACTS*REG       2      10x10
#> 144            pop               REG       1         10
#> 145            ppa          COMM*REG       2      10x10
#> 146            ppd          COMM*REG       2      10x10
#> 147            ppm          COMM*REG       2      10x10
#> 148          ppriv               REG       1         10
#> 149             pr          COMM*REG       2      10x10
#> 150    profitslack          ACTS*REG       2      10x10
#> 151             ps     COMM*ACTS*REG       3   10x10x10
#> 152          psave               REG       1         10
#> 153     psaveslack               REG       1         10
#> 154            psw               REG       1         10
#> 155             pt              MARG       1          1
#> 156         ptrans      COMM*REG*REG       3   10x10x10
#> 157            pva          ACTS*REG       2      10x10
#> 158             pw              COMM       1         10
#> 159            pwu              COMM       1         10
#> 160            pxw          COMM*REG       2      10x10
#> 161         pxwcom              COMM       1         10
#> 162         pxwreg               REG       1         10
#> 163             qc          COMM*REG       2      10x10
#> 164            qca     COMM*ACTS*REG       3   10x10x10
#> 165            qds          COMM*REG       2      10x10
#> 166             qe        ENDWMS*REG       2       4x10
#> 167            qes     ENDW*ACTS*REG       3    5x10x10
#> 168           qesf    ENDWF*ACTS*REG       3    1x10x10
#> 169            qfa     COMM*ACTS*REG       3   10x10x10
#> 170            qfd     COMM*ACTS*REG       3   10x10x10
#> 171            qfe     ENDW*ACTS*REG       3    5x10x10
#> 172            qfm     COMM*ACTS*REG       3   10x10x10
#> 173            qga          COMM*REG       2      10x10
#> 174            qgd          COMM*REG       2      10x10
#> 175           qgdp               REG       1         10
#> 176            qgm          COMM*REG       2      10x10
#> 177            qia          COMM*REG       2      10x10
#> 178            qid          COMM*REG       2      10x10
#> 179            qim          COMM*REG       2      10x10
#> 180           qint          ACTS*REG       2      10x10
#> 181           qinv               REG       1         10
#> 182            qms          COMM*REG       2      10x10
#> 183            qmw          COMM*REG       2      10x10
#> 184         qmwcom              COMM       1         10
#> 185         qmwreg               REG       1         10
#> 186             qo          ACTS*REG       2      10x10
#> 187            qow              COMM       1         10
#> 188           qowu              COMM       1         10
#> 189            qpa          COMM*REG       2      10x10
#> 190            qpd          COMM*REG       2      10x10
#> 191           qpev          COMM*REG       2      10x10
#> 192            qpm          COMM*REG       2      10x10
#> 193          qpriv               REG       1         10
#> 194          qsave               REG       1         10
#> 195        qsaveev               REG       1         10
#> 196            qst          MARG*REG       2       1x10
#> 197            qtm              MARG       1          1
#> 198         qtmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 199            qva          ACTS*REG       2      10x10
#> 200            qxs      COMM*REG*REG       3   10x10x10
#> 201            qxw          COMM*REG       2      10x10
#> 202         qxwcom              COMM       1         10
#> 203         qxwreg               REG       1         10
#> 204         rental               REG       1         10
#> 205           rorc               REG       1         10
#> 206           rore               REG       1         10
#> 207            tfd     COMM*ACTS*REG       3   10x10x10
#> 208            tfe     ENDW*ACTS*REG       3    5x10x10
#> 209            tfm     COMM*ACTS*REG       3   10x10x10
#> 210            tgd          COMM*REG       2      10x10
#> 211            tgm          COMM*REG       2      10x10
#> 212            tid          COMM*REG       2      10x10
#> 213            tim          COMM*REG       2      10x10
#> 214           tinc     ENDW*ACTS*REG       3    5x10x10
#> 215             tm          COMM*REG       2      10x10
#> 216            tms      COMM*REG*REG       3   10x10x10
#> 217             to     COMM*ACTS*REG       3   10x10x10
#> 218            tot               REG       1         10
#> 219           tot2               REG       1         10
#> 220            tpd          COMM*REG       2      10x10
#> 221         tpdall          COMM*REG       2      10x10
#> 222            tpm          COMM*REG       2      10x10
#> 223         tpmall          COMM*REG       2      10x10
#> 224          tpreg               REG       1         10
#> 225      tradslack          COMM*REG       2      10x10
#> 226             tx          COMM*REG       2      10x10
#> 227            txs      COMM*REG*REG       3   10x10x10
#> 228              u               REG       1         10
#> 229          uelas               REG       1         10
#> 230        uelasev               REG       1         10
#> 231         uepriv               REG       1         10
#> 232       ueprivev               REG       1         10
#> 233             ug               REG       1         10
#> 234           ugev               REG       1         10
#> 235             up               REG       1         10
#> 236           upev               REG       1         10
#> 237         valuew              COMM       1         10
#> 238        valuewu              COMM       1         10
#> 239           vgdp               REG       1         10
#> 240         vmwcif          COMM*REG       2      10x10
#> 241         vmwcom              COMM       1         10
#> 242         vmwreg               REG       1         10
#> 243         vxwcom              COMM       1         10
#> 244         vxwfob          COMM*REG       2      10x10
#> 245         vxwreg               REG       1         10
#> 246              y               REG       1         10
#> 247            yev               REG       1         10
#> 248             yg               REG       1         10
#> 249           ygev               REG       1         10
#> 250             yp               REG       1         10
#> 251           ypev               REG       1         10
#> 252        ysaveev               REG       1         10
#> 

# Get summary for specific variables
get_var_structure(c("gdp", "trade"), sl4_data)
#> $sl4_data
#> data frame with 0 columns and 0 rows
#> 

# Include column and observation counts
get_var_structure("ALL", sl4_data, include_col_size = TRUE)
#> $sl4_data
#>           Variable        Dimensions DimSize  DataShape No.Col No.Obs
#> 1      CNTalleffcr          COMM*REG       2      10x10     10     10
#> 2       CNTalleffr          REG*CTAX       2       10x9      9     10
#> 3          CNTdpar               REG       1         10      1     10
#> 4          CNTendw          ENDW*REG       2       5x10     10      5
#> 5         CNTendwr               REG       1         10      1     10
#> 6            CNTkb               REG       1         10      1     10
#> 7          CNTpinv               REG       1         10      1     10
#> 8           CNTpop               REG       1         10      1     10
#> 9           CNTqca     COMM*ACTS*REG       3   10x10x10    100     10
#> 10           CNTqe          ENDW*REG       2       5x10     10      5
#> 11          CNTqea     ENDW*ACTS*REG       3    5x10x10    100      5
#> 12          CNTqfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 13          CNTqfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 14        CNTqfeer          ENDW*REG       2       5x10     10      5
#> 15         CNTqfer               REG       1         10      1     10
#> 16          CNTqfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 17          CNTqfr           REG*DIR       2       10x2      2     10
#> 18          CNTqgd          COMM*REG       2      10x10     10     10
#> 19          CNTqgm          COMM*REG       2      10x10     10     10
#> 20          CNTqgr           REG*DIR       2       10x2      2     10
#> 21          CNTqia           REG*DIR       2       10x2      2     10
#> 22          CNTqid          COMM*REG       2      10x10     10     10
#> 23          CNTqim          COMM*REG       2      10x10     10     10
#> 24          CNTqms      COMM*REG*REG       3   10x10x10    100     10
#> 25         CNTqmsr               REG       1         10      1     10
#> 26           CNTqo          ACTS*REG       2      10x10     10     10
#> 27          CNTqor               REG       1         10      1     10
#> 28          CNTqpd          COMM*REG       2      10x10     10     10
#> 29          CNTqpm          COMM*REG       2      10x10     10     10
#> 30          CNTqpr           REG*DIR       2       10x2      2     10
#> 31          CNTqxs      COMM*REG*REG       3   10x10x10    100     10
#> 32         CNTqxsr               REG       1         10      1     10
#> 33      CNTtech_af     COMM*ACTS*REG       3   10x10x10    100     10
#> 34     CNTtech_afe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 35    CNTtech_aint          ACTS*REG       2      10x10     10     10
#> 36     CNTtech_ams      COMM*REG*REG       3   10x10x10    100     10
#> 37      CNTtech_ao          ACTS*REG       2      10x10     10     10
#> 38  CNTtech_atmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 39     CNTtech_ava          ACTS*REG       2      10x10     10     10
#> 40        CNTtechr      TECHTYPE*REG       2       7x10     10      7
#> 41         CNTtotr               REG       1         10      1     10
#> 42              EV               REG       1         10      1     10
#> 43          EV_ALT               REG       1         10      1     10
#> 44             afa     COMM*ACTS*REG       3   10x10x10    100     10
#> 45           afall     COMM*ACTS*REG       3   10x10x10    100     10
#> 46           afcom              COMM       1         10      1     10
#> 47             afe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 48          afeall     ENDW*ACTS*REG       3    5x10x10    100      5
#> 49          afecom              ENDW       1          5      1      5
#> 50          afereg               REG       1         10      1     10
#> 51          afesec              ACTS       1         10      1     10
#> 52           afreg               REG       1         10      1     10
#> 53           afsec              ACTS       1         10      1     10
#> 54            aint          ACTS*REG       2      10x10     10     10
#> 55         aintall          ACTS*REG       2      10x10     10     10
#> 56         aintreg               REG       1         10      1     10
#> 57         aintsec              ACTS       1         10      1     10
#> 58             ams      COMM*REG*REG       3   10x10x10    100     10
#> 59              ao          ACTS*REG       2      10x10     10     10
#> 60           aoall          ACTS*REG       2      10x10     10     10
#> 61           aoreg               REG       1         10      1     10
#> 62           aosec              ACTS       1         10      1     10
#> 63           atall MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 64             atd               REG       1         10      1     10
#> 65             atf              COMM       1         10      1     10
#> 66             atm              COMM       1         10      1     10
#> 67          atmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 68             ats               REG       1         10      1     10
#> 69              au               REG       1         10      1     10
#> 70             ava          ACTS*REG       2      10x10     10     10
#> 71          avaall          ACTS*REG       2      10x10     10     10
#> 72          avareg               REG       1         10      1     10
#> 73          avasec              ACTS       1         10      1     10
#> 74           c1_cr          COMM*REG       2      10x10     10     10
#> 75            c1_r               REG       1         10      1     10
#> 76           c2_cr          COMM*REG       2      10x10     10     10
#> 77            c2_r               REG       1         10      1     10
#> 78           c3_cr          COMM*REG       2      10x10     10     10
#> 79            c3_r               REG       1         10      1     10
#> 80        cgdslack               REG       1         10      1     10
#> 81       compvalad          ACTS*REG       2      10x10     10     10
#> 82      contgdpexp         REG*GDPEX       2       10x6      6     10
#> 83     del_indtaxr               REG       1         10      1     10
#> 84     del_taxrexp               REG       1         10      1     10
#> 85      del_taxrfu               REG       1         10      1     10
#> 86      del_taxrgc               REG       1         10      1     10
#> 87      del_taxric               REG       1         10      1     10
#> 88     del_taxrimp               REG       1         10      1     10
#> 89     del_taxrinc               REG       1         10      1     10
#> 90      del_taxriu               REG       1         10      1     10
#> 91     del_taxrout               REG       1         10      1     10
#> 92      del_taxrpc               REG       1         10      1     10
#> 93        del_tbal               REG       1         10      1     10
#> 94       del_tbalc          COMM*REG       2      10x10     10     10
#> 95      del_tbalry               REG       1         10      1     10
#> 96       del_ttaxr               REG       1         10      1     10
#> 97            dpav               REG       1         10      1     10
#> 98          dpavev               REG       1         10      1     10
#> 99           dpgov               REG       1         10      1     10
#> 100         dppriv               REG       1         10      1     10
#> 101         dpsave               REG       1         10      1     10
#> 102          dpsum               REG       1         10      1     10
#> 103      endwslack          ENDW*REG       2       5x10     10      5
#> 104         expand         ENDWC*REG       2       1x10     10      1
#> 105        fincome               REG       1         10      1     10
#> 106    incomeslack               REG       1         10      1     10
#> 107             kb               REG       1         10      1     10
#> 108             ke               REG       1         10      1     10
#> 109              p               REG       1         10      1     10
#> 110             pb          ACTS*REG       2      10x10     10     10
#> 111            pca     COMM*ACTS*REG       3   10x10x10    100     10
#> 112           pcif      COMM*REG*REG       3   10x10x10    100     10
#> 113            pds          COMM*REG       2      10x10     10     10
#> 114            pdw               REG       1         10      1     10
#> 115             pe        ENDWMS*REG       2       4x10     10      4
#> 116            peb     ENDW*ACTS*REG       3    5x10x10    100      5
#> 117    pebfactreal        ENDWMS*REG       2       4x10     10      4
#> 118     pefactreal        ENDWMS*REG       2       4x10     10      4
#> 119            pes     ENDW*ACTS*REG       3    5x10x10    100      5
#> 120            pfa     COMM*ACTS*REG       3   10x10x10    100     10
#> 121        pfactor               REG       1         10      1     10
#> 122      pfactreal     ENDW*ACTS*REG       3    5x10x10    100      5
#> 123            pfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 124            pfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 125            pfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 126           pfob      COMM*REG*REG       3   10x10x10    100     10
#> 127            pga          COMM*REG       2      10x10     10     10
#> 128            pgd          COMM*REG       2      10x10     10     10
#> 129           pgdp               REG       1         10      1     10
#> 130            pgm          COMM*REG       2      10x10     10     10
#> 131           pgov               REG       1         10      1     10
#> 132            pia          COMM*REG       2      10x10     10     10
#> 133            pid          COMM*REG       2      10x10     10     10
#> 134            pim          COMM*REG       2      10x10     10     10
#> 135           pint          ACTS*REG       2      10x10     10     10
#> 136           pinv               REG       1         10      1     10
#> 137          pm_cr          COMM*REG       2      10x10     10     10
#> 138           pmds      COMM*REG*REG       3   10x10x10    100     10
#> 139            pms          COMM*REG       2      10x10     10     10
#> 140            pmw          COMM*REG       2      10x10     10     10
#> 141         pmwcom              COMM       1         10      1     10
#> 142         pmwreg               REG       1         10      1     10
#> 143             po          ACTS*REG       2      10x10     10     10
#> 144            pop               REG       1         10      1     10
#> 145            ppa          COMM*REG       2      10x10     10     10
#> 146            ppd          COMM*REG       2      10x10     10     10
#> 147            ppm          COMM*REG       2      10x10     10     10
#> 148          ppriv               REG       1         10      1     10
#> 149             pr          COMM*REG       2      10x10     10     10
#> 150    profitslack          ACTS*REG       2      10x10     10     10
#> 151             ps     COMM*ACTS*REG       3   10x10x10    100     10
#> 152          psave               REG       1         10      1     10
#> 153     psaveslack               REG       1         10      1     10
#> 154            psw               REG       1         10      1     10
#> 155             pt              MARG       1          1      1      1
#> 156         ptrans      COMM*REG*REG       3   10x10x10    100     10
#> 157            pva          ACTS*REG       2      10x10     10     10
#> 158             pw              COMM       1         10      1     10
#> 159            pwu              COMM       1         10      1     10
#> 160            pxw          COMM*REG       2      10x10     10     10
#> 161         pxwcom              COMM       1         10      1     10
#> 162         pxwreg               REG       1         10      1     10
#> 163             qc          COMM*REG       2      10x10     10     10
#> 164            qca     COMM*ACTS*REG       3   10x10x10    100     10
#> 165            qds          COMM*REG       2      10x10     10     10
#> 166             qe        ENDWMS*REG       2       4x10     10      4
#> 167            qes     ENDW*ACTS*REG       3    5x10x10    100      5
#> 168           qesf    ENDWF*ACTS*REG       3    1x10x10    100      1
#> 169            qfa     COMM*ACTS*REG       3   10x10x10    100     10
#> 170            qfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 171            qfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 172            qfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 173            qga          COMM*REG       2      10x10     10     10
#> 174            qgd          COMM*REG       2      10x10     10     10
#> 175           qgdp               REG       1         10      1     10
#> 176            qgm          COMM*REG       2      10x10     10     10
#> 177            qia          COMM*REG       2      10x10     10     10
#> 178            qid          COMM*REG       2      10x10     10     10
#> 179            qim          COMM*REG       2      10x10     10     10
#> 180           qint          ACTS*REG       2      10x10     10     10
#> 181           qinv               REG       1         10      1     10
#> 182            qms          COMM*REG       2      10x10     10     10
#> 183            qmw          COMM*REG       2      10x10     10     10
#> 184         qmwcom              COMM       1         10      1     10
#> 185         qmwreg               REG       1         10      1     10
#> 186             qo          ACTS*REG       2      10x10     10     10
#> 187            qow              COMM       1         10      1     10
#> 188           qowu              COMM       1         10      1     10
#> 189            qpa          COMM*REG       2      10x10     10     10
#> 190            qpd          COMM*REG       2      10x10     10     10
#> 191           qpev          COMM*REG       2      10x10     10     10
#> 192            qpm          COMM*REG       2      10x10     10     10
#> 193          qpriv               REG       1         10      1     10
#> 194          qsave               REG       1         10      1     10
#> 195        qsaveev               REG       1         10      1     10
#> 196            qst          MARG*REG       2       1x10     10      1
#> 197            qtm              MARG       1          1      1      1
#> 198         qtmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 199            qva          ACTS*REG       2      10x10     10     10
#> 200            qxs      COMM*REG*REG       3   10x10x10    100     10
#> 201            qxw          COMM*REG       2      10x10     10     10
#> 202         qxwcom              COMM       1         10      1     10
#> 203         qxwreg               REG       1         10      1     10
#> 204         rental               REG       1         10      1     10
#> 205           rorc               REG       1         10      1     10
#> 206           rore               REG       1         10      1     10
#> 207            tfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 208            tfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 209            tfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 210            tgd          COMM*REG       2      10x10     10     10
#> 211            tgm          COMM*REG       2      10x10     10     10
#> 212            tid          COMM*REG       2      10x10     10     10
#> 213            tim          COMM*REG       2      10x10     10     10
#> 214           tinc     ENDW*ACTS*REG       3    5x10x10    100      5
#> 215             tm          COMM*REG       2      10x10     10     10
#> 216            tms      COMM*REG*REG       3   10x10x10    100     10
#> 217             to     COMM*ACTS*REG       3   10x10x10    100     10
#> 218            tot               REG       1         10      1     10
#> 219           tot2               REG       1         10      1     10
#> 220            tpd          COMM*REG       2      10x10     10     10
#> 221         tpdall          COMM*REG       2      10x10     10     10
#> 222            tpm          COMM*REG       2      10x10     10     10
#> 223         tpmall          COMM*REG       2      10x10     10     10
#> 224          tpreg               REG       1         10      1     10
#> 225      tradslack          COMM*REG       2      10x10     10     10
#> 226             tx          COMM*REG       2      10x10     10     10
#> 227            txs      COMM*REG*REG       3   10x10x10    100     10
#> 228              u               REG       1         10      1     10
#> 229          uelas               REG       1         10      1     10
#> 230        uelasev               REG       1         10      1     10
#> 231         uepriv               REG       1         10      1     10
#> 232       ueprivev               REG       1         10      1     10
#> 233             ug               REG       1         10      1     10
#> 234           ugev               REG       1         10      1     10
#> 235             up               REG       1         10      1     10
#> 236           upev               REG       1         10      1     10
#> 237         valuew              COMM       1         10      1     10
#> 238        valuewu              COMM       1         10      1     10
#> 239           vgdp               REG       1         10      1     10
#> 240         vmwcif          COMM*REG       2      10x10     10     10
#> 241         vmwcom              COMM       1         10      1     10
#> 242         vmwreg               REG       1         10      1     10
#> 243         vxwcom              COMM       1         10      1     10
#> 244         vxwfob          COMM*REG       2      10x10     10     10
#> 245         vxwreg               REG       1         10      1     10
#> 246              y               REG       1         10      1     10
#> 247            yev               REG       1         10      1     10
#> 248             yg               REG       1         10      1     10
#> 249           ygev               REG       1         10      1     10
#> 250             yp               REG       1         10      1     10
#> 251           ypev               REG       1         10      1     10
#> 252        ysaveev               REG       1         10      1     10
#> 

# Compare structures across multiple datasets
get_var_structure("ALL", sl4_data, sl4_data1)
#> $sl4_data
#>           Variable        Dimensions DimSize  DataShape
#> 1      CNTalleffcr          COMM*REG       2      10x10
#> 2       CNTalleffr          REG*CTAX       2       10x9
#> 3          CNTdpar               REG       1         10
#> 4          CNTendw          ENDW*REG       2       5x10
#> 5         CNTendwr               REG       1         10
#> 6            CNTkb               REG       1         10
#> 7          CNTpinv               REG       1         10
#> 8           CNTpop               REG       1         10
#> 9           CNTqca     COMM*ACTS*REG       3   10x10x10
#> 10           CNTqe          ENDW*REG       2       5x10
#> 11          CNTqea     ENDW*ACTS*REG       3    5x10x10
#> 12          CNTqfd     COMM*ACTS*REG       3   10x10x10
#> 13          CNTqfe     ENDW*ACTS*REG       3    5x10x10
#> 14        CNTqfeer          ENDW*REG       2       5x10
#> 15         CNTqfer               REG       1         10
#> 16          CNTqfm     COMM*ACTS*REG       3   10x10x10
#> 17          CNTqfr           REG*DIR       2       10x2
#> 18          CNTqgd          COMM*REG       2      10x10
#> 19          CNTqgm          COMM*REG       2      10x10
#> 20          CNTqgr           REG*DIR       2       10x2
#> 21          CNTqia           REG*DIR       2       10x2
#> 22          CNTqid          COMM*REG       2      10x10
#> 23          CNTqim          COMM*REG       2      10x10
#> 24          CNTqms      COMM*REG*REG       3   10x10x10
#> 25         CNTqmsr               REG       1         10
#> 26           CNTqo          ACTS*REG       2      10x10
#> 27          CNTqor               REG       1         10
#> 28          CNTqpd          COMM*REG       2      10x10
#> 29          CNTqpm          COMM*REG       2      10x10
#> 30          CNTqpr           REG*DIR       2       10x2
#> 31          CNTqxs      COMM*REG*REG       3   10x10x10
#> 32         CNTqxsr               REG       1         10
#> 33      CNTtech_af     COMM*ACTS*REG       3   10x10x10
#> 34     CNTtech_afe     ENDW*ACTS*REG       3    5x10x10
#> 35    CNTtech_aint          ACTS*REG       2      10x10
#> 36     CNTtech_ams      COMM*REG*REG       3   10x10x10
#> 37      CNTtech_ao          ACTS*REG       2      10x10
#> 38  CNTtech_atmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 39     CNTtech_ava          ACTS*REG       2      10x10
#> 40        CNTtechr      TECHTYPE*REG       2       7x10
#> 41         CNTtotr               REG       1         10
#> 42              EV               REG       1         10
#> 43          EV_ALT               REG       1         10
#> 44             afa     COMM*ACTS*REG       3   10x10x10
#> 45           afall     COMM*ACTS*REG       3   10x10x10
#> 46           afcom              COMM       1         10
#> 47             afe     ENDW*ACTS*REG       3    5x10x10
#> 48          afeall     ENDW*ACTS*REG       3    5x10x10
#> 49          afecom              ENDW       1          5
#> 50          afereg               REG       1         10
#> 51          afesec              ACTS       1         10
#> 52           afreg               REG       1         10
#> 53           afsec              ACTS       1         10
#> 54            aint          ACTS*REG       2      10x10
#> 55         aintall          ACTS*REG       2      10x10
#> 56         aintreg               REG       1         10
#> 57         aintsec              ACTS       1         10
#> 58             ams      COMM*REG*REG       3   10x10x10
#> 59              ao          ACTS*REG       2      10x10
#> 60           aoall          ACTS*REG       2      10x10
#> 61           aoreg               REG       1         10
#> 62           aosec              ACTS       1         10
#> 63           atall MARG*COMM*REG*REG       4 1x10x10x10
#> 64             atd               REG       1         10
#> 65             atf              COMM       1         10
#> 66             atm              COMM       1         10
#> 67          atmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 68             ats               REG       1         10
#> 69              au               REG       1         10
#> 70             ava          ACTS*REG       2      10x10
#> 71          avaall          ACTS*REG       2      10x10
#> 72          avareg               REG       1         10
#> 73          avasec              ACTS       1         10
#> 74           c1_cr          COMM*REG       2      10x10
#> 75            c1_r               REG       1         10
#> 76           c2_cr          COMM*REG       2      10x10
#> 77            c2_r               REG       1         10
#> 78           c3_cr          COMM*REG       2      10x10
#> 79            c3_r               REG       1         10
#> 80        cgdslack               REG       1         10
#> 81       compvalad          ACTS*REG       2      10x10
#> 82      contgdpexp         REG*GDPEX       2       10x6
#> 83     del_indtaxr               REG       1         10
#> 84     del_taxrexp               REG       1         10
#> 85      del_taxrfu               REG       1         10
#> 86      del_taxrgc               REG       1         10
#> 87      del_taxric               REG       1         10
#> 88     del_taxrimp               REG       1         10
#> 89     del_taxrinc               REG       1         10
#> 90      del_taxriu               REG       1         10
#> 91     del_taxrout               REG       1         10
#> 92      del_taxrpc               REG       1         10
#> 93        del_tbal               REG       1         10
#> 94       del_tbalc          COMM*REG       2      10x10
#> 95      del_tbalry               REG       1         10
#> 96       del_ttaxr               REG       1         10
#> 97            dpav               REG       1         10
#> 98          dpavev               REG       1         10
#> 99           dpgov               REG       1         10
#> 100         dppriv               REG       1         10
#> 101         dpsave               REG       1         10
#> 102          dpsum               REG       1         10
#> 103      endwslack          ENDW*REG       2       5x10
#> 104         expand         ENDWC*REG       2       1x10
#> 105        fincome               REG       1         10
#> 106    incomeslack               REG       1         10
#> 107             kb               REG       1         10
#> 108             ke               REG       1         10
#> 109              p               REG       1         10
#> 110             pb          ACTS*REG       2      10x10
#> 111            pca     COMM*ACTS*REG       3   10x10x10
#> 112           pcif      COMM*REG*REG       3   10x10x10
#> 113            pds          COMM*REG       2      10x10
#> 114            pdw               REG       1         10
#> 115             pe        ENDWMS*REG       2       4x10
#> 116            peb     ENDW*ACTS*REG       3    5x10x10
#> 117    pebfactreal        ENDWMS*REG       2       4x10
#> 118     pefactreal        ENDWMS*REG       2       4x10
#> 119            pes     ENDW*ACTS*REG       3    5x10x10
#> 120            pfa     COMM*ACTS*REG       3   10x10x10
#> 121        pfactor               REG       1         10
#> 122      pfactreal     ENDW*ACTS*REG       3    5x10x10
#> 123            pfd     COMM*ACTS*REG       3   10x10x10
#> 124            pfe     ENDW*ACTS*REG       3    5x10x10
#> 125            pfm     COMM*ACTS*REG       3   10x10x10
#> 126           pfob      COMM*REG*REG       3   10x10x10
#> 127            pga          COMM*REG       2      10x10
#> 128            pgd          COMM*REG       2      10x10
#> 129           pgdp               REG       1         10
#> 130            pgm          COMM*REG       2      10x10
#> 131           pgov               REG       1         10
#> 132            pia          COMM*REG       2      10x10
#> 133            pid          COMM*REG       2      10x10
#> 134            pim          COMM*REG       2      10x10
#> 135           pint          ACTS*REG       2      10x10
#> 136           pinv               REG       1         10
#> 137          pm_cr          COMM*REG       2      10x10
#> 138           pmds      COMM*REG*REG       3   10x10x10
#> 139            pms          COMM*REG       2      10x10
#> 140            pmw          COMM*REG       2      10x10
#> 141         pmwcom              COMM       1         10
#> 142         pmwreg               REG       1         10
#> 143             po          ACTS*REG       2      10x10
#> 144            pop               REG       1         10
#> 145            ppa          COMM*REG       2      10x10
#> 146            ppd          COMM*REG       2      10x10
#> 147            ppm          COMM*REG       2      10x10
#> 148          ppriv               REG       1         10
#> 149             pr          COMM*REG       2      10x10
#> 150    profitslack          ACTS*REG       2      10x10
#> 151             ps     COMM*ACTS*REG       3   10x10x10
#> 152          psave               REG       1         10
#> 153     psaveslack               REG       1         10
#> 154            psw               REG       1         10
#> 155             pt              MARG       1          1
#> 156         ptrans      COMM*REG*REG       3   10x10x10
#> 157            pva          ACTS*REG       2      10x10
#> 158             pw              COMM       1         10
#> 159            pwu              COMM       1         10
#> 160            pxw          COMM*REG       2      10x10
#> 161         pxwcom              COMM       1         10
#> 162         pxwreg               REG       1         10
#> 163             qc          COMM*REG       2      10x10
#> 164            qca     COMM*ACTS*REG       3   10x10x10
#> 165            qds          COMM*REG       2      10x10
#> 166             qe        ENDWMS*REG       2       4x10
#> 167            qes     ENDW*ACTS*REG       3    5x10x10
#> 168           qesf    ENDWF*ACTS*REG       3    1x10x10
#> 169            qfa     COMM*ACTS*REG       3   10x10x10
#> 170            qfd     COMM*ACTS*REG       3   10x10x10
#> 171            qfe     ENDW*ACTS*REG       3    5x10x10
#> 172            qfm     COMM*ACTS*REG       3   10x10x10
#> 173            qga          COMM*REG       2      10x10
#> 174            qgd          COMM*REG       2      10x10
#> 175           qgdp               REG       1         10
#> 176            qgm          COMM*REG       2      10x10
#> 177            qia          COMM*REG       2      10x10
#> 178            qid          COMM*REG       2      10x10
#> 179            qim          COMM*REG       2      10x10
#> 180           qint          ACTS*REG       2      10x10
#> 181           qinv               REG       1         10
#> 182            qms          COMM*REG       2      10x10
#> 183            qmw          COMM*REG       2      10x10
#> 184         qmwcom              COMM       1         10
#> 185         qmwreg               REG       1         10
#> 186             qo          ACTS*REG       2      10x10
#> 187            qow              COMM       1         10
#> 188           qowu              COMM       1         10
#> 189            qpa          COMM*REG       2      10x10
#> 190            qpd          COMM*REG       2      10x10
#> 191           qpev          COMM*REG       2      10x10
#> 192            qpm          COMM*REG       2      10x10
#> 193          qpriv               REG       1         10
#> 194          qsave               REG       1         10
#> 195        qsaveev               REG       1         10
#> 196            qst          MARG*REG       2       1x10
#> 197            qtm              MARG       1          1
#> 198         qtmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 199            qva          ACTS*REG       2      10x10
#> 200            qxs      COMM*REG*REG       3   10x10x10
#> 201            qxw          COMM*REG       2      10x10
#> 202         qxwcom              COMM       1         10
#> 203         qxwreg               REG       1         10
#> 204         rental               REG       1         10
#> 205           rorc               REG       1         10
#> 206           rore               REG       1         10
#> 207            tfd     COMM*ACTS*REG       3   10x10x10
#> 208            tfe     ENDW*ACTS*REG       3    5x10x10
#> 209            tfm     COMM*ACTS*REG       3   10x10x10
#> 210            tgd          COMM*REG       2      10x10
#> 211            tgm          COMM*REG       2      10x10
#> 212            tid          COMM*REG       2      10x10
#> 213            tim          COMM*REG       2      10x10
#> 214           tinc     ENDW*ACTS*REG       3    5x10x10
#> 215             tm          COMM*REG       2      10x10
#> 216            tms      COMM*REG*REG       3   10x10x10
#> 217             to     COMM*ACTS*REG       3   10x10x10
#> 218            tot               REG       1         10
#> 219           tot2               REG       1         10
#> 220            tpd          COMM*REG       2      10x10
#> 221         tpdall          COMM*REG       2      10x10
#> 222            tpm          COMM*REG       2      10x10
#> 223         tpmall          COMM*REG       2      10x10
#> 224          tpreg               REG       1         10
#> 225      tradslack          COMM*REG       2      10x10
#> 226             tx          COMM*REG       2      10x10
#> 227            txs      COMM*REG*REG       3   10x10x10
#> 228              u               REG       1         10
#> 229          uelas               REG       1         10
#> 230        uelasev               REG       1         10
#> 231         uepriv               REG       1         10
#> 232       ueprivev               REG       1         10
#> 233             ug               REG       1         10
#> 234           ugev               REG       1         10
#> 235             up               REG       1         10
#> 236           upev               REG       1         10
#> 237         valuew              COMM       1         10
#> 238        valuewu              COMM       1         10
#> 239           vgdp               REG       1         10
#> 240         vmwcif          COMM*REG       2      10x10
#> 241         vmwcom              COMM       1         10
#> 242         vmwreg               REG       1         10
#> 243         vxwcom              COMM       1         10
#> 244         vxwfob          COMM*REG       2      10x10
#> 245         vxwreg               REG       1         10
#> 246              y               REG       1         10
#> 247            yev               REG       1         10
#> 248             yg               REG       1         10
#> 249           ygev               REG       1         10
#> 250             yp               REG       1         10
#> 251           ypev               REG       1         10
#> 252        ysaveev               REG       1         10
#> 
#> $sl4_data1
#>           Variable        Dimensions DimSize  DataShape
#> 1      CNTalleffcr          COMM*REG       2      10x10
#> 2       CNTalleffr          REG*CTAX       2       10x9
#> 3          CNTdpar               REG       1         10
#> 4          CNTendw          ENDW*REG       2       5x10
#> 5         CNTendwr               REG       1         10
#> 6            CNTkb               REG       1         10
#> 7          CNTpinv               REG       1         10
#> 8           CNTpop               REG       1         10
#> 9           CNTqca     COMM*ACTS*REG       3   10x10x10
#> 10           CNTqe          ENDW*REG       2       5x10
#> 11          CNTqea     ENDW*ACTS*REG       3    5x10x10
#> 12          CNTqfd     COMM*ACTS*REG       3   10x10x10
#> 13          CNTqfe     ENDW*ACTS*REG       3    5x10x10
#> 14        CNTqfeer          ENDW*REG       2       5x10
#> 15         CNTqfer               REG       1         10
#> 16          CNTqfm     COMM*ACTS*REG       3   10x10x10
#> 17          CNTqfr           REG*DIR       2       10x2
#> 18          CNTqgd          COMM*REG       2      10x10
#> 19          CNTqgm          COMM*REG       2      10x10
#> 20          CNTqgr           REG*DIR       2       10x2
#> 21          CNTqia           REG*DIR       2       10x2
#> 22          CNTqid          COMM*REG       2      10x10
#> 23          CNTqim          COMM*REG       2      10x10
#> 24          CNTqms      COMM*REG*REG       3   10x10x10
#> 25         CNTqmsr               REG       1         10
#> 26           CNTqo          ACTS*REG       2      10x10
#> 27          CNTqor               REG       1         10
#> 28          CNTqpd          COMM*REG       2      10x10
#> 29          CNTqpm          COMM*REG       2      10x10
#> 30          CNTqpr           REG*DIR       2       10x2
#> 31          CNTqxs      COMM*REG*REG       3   10x10x10
#> 32         CNTqxsr               REG       1         10
#> 33      CNTtech_af     COMM*ACTS*REG       3   10x10x10
#> 34     CNTtech_afe     ENDW*ACTS*REG       3    5x10x10
#> 35    CNTtech_aint          ACTS*REG       2      10x10
#> 36     CNTtech_ams      COMM*REG*REG       3   10x10x10
#> 37      CNTtech_ao          ACTS*REG       2      10x10
#> 38  CNTtech_atmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 39     CNTtech_ava          ACTS*REG       2      10x10
#> 40        CNTtechr      TECHTYPE*REG       2       7x10
#> 41         CNTtotr               REG       1         10
#> 42              EV               REG       1         10
#> 43          EV_ALT               REG       1         10
#> 44             afa     COMM*ACTS*REG       3   10x10x10
#> 45           afall     COMM*ACTS*REG       3   10x10x10
#> 46           afcom              COMM       1         10
#> 47             afe     ENDW*ACTS*REG       3    5x10x10
#> 48          afeall     ENDW*ACTS*REG       3    5x10x10
#> 49          afecom              ENDW       1          5
#> 50          afereg               REG       1         10
#> 51          afesec              ACTS       1         10
#> 52           afreg               REG       1         10
#> 53           afsec              ACTS       1         10
#> 54            aint          ACTS*REG       2      10x10
#> 55         aintall          ACTS*REG       2      10x10
#> 56         aintreg               REG       1         10
#> 57         aintsec              ACTS       1         10
#> 58             ams      COMM*REG*REG       3   10x10x10
#> 59              ao          ACTS*REG       2      10x10
#> 60           aoall          ACTS*REG       2      10x10
#> 61           aoreg               REG       1         10
#> 62           aosec              ACTS       1         10
#> 63           atall MARG*COMM*REG*REG       4 1x10x10x10
#> 64             atd               REG       1         10
#> 65             atf              COMM       1         10
#> 66             atm              COMM       1         10
#> 67          atmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 68             ats               REG       1         10
#> 69              au               REG       1         10
#> 70             ava          ACTS*REG       2      10x10
#> 71          avaall          ACTS*REG       2      10x10
#> 72          avareg               REG       1         10
#> 73          avasec              ACTS       1         10
#> 74           c1_cr          COMM*REG       2      10x10
#> 75            c1_r               REG       1         10
#> 76           c2_cr          COMM*REG       2      10x10
#> 77            c2_r               REG       1         10
#> 78           c3_cr          COMM*REG       2      10x10
#> 79            c3_r               REG       1         10
#> 80        cgdslack               REG       1         10
#> 81       compvalad          ACTS*REG       2      10x10
#> 82      contgdpexp         REG*GDPEX       2       10x6
#> 83     del_indtaxr               REG       1         10
#> 84     del_taxrexp               REG       1         10
#> 85      del_taxrfu               REG       1         10
#> 86      del_taxrgc               REG       1         10
#> 87      del_taxric               REG       1         10
#> 88     del_taxrimp               REG       1         10
#> 89     del_taxrinc               REG       1         10
#> 90      del_taxriu               REG       1         10
#> 91     del_taxrout               REG       1         10
#> 92      del_taxrpc               REG       1         10
#> 93        del_tbal               REG       1         10
#> 94       del_tbalc          COMM*REG       2      10x10
#> 95      del_tbalry               REG       1         10
#> 96       del_ttaxr               REG       1         10
#> 97            dpav               REG       1         10
#> 98          dpavev               REG       1         10
#> 99           dpgov               REG       1         10
#> 100         dppriv               REG       1         10
#> 101         dpsave               REG       1         10
#> 102          dpsum               REG       1         10
#> 103      endwslack          ENDW*REG       2       5x10
#> 104         expand         ENDWC*REG       2       1x10
#> 105        fincome               REG       1         10
#> 106    incomeslack               REG       1         10
#> 107             kb               REG       1         10
#> 108             ke               REG       1         10
#> 109              p               REG       1         10
#> 110             pb          ACTS*REG       2      10x10
#> 111            pca     COMM*ACTS*REG       3   10x10x10
#> 112           pcif      COMM*REG*REG       3   10x10x10
#> 113            pds          COMM*REG       2      10x10
#> 114            pdw               REG       1         10
#> 115             pe        ENDWMS*REG       2       4x10
#> 116            peb     ENDW*ACTS*REG       3    5x10x10
#> 117    pebfactreal        ENDWMS*REG       2       4x10
#> 118     pefactreal        ENDWMS*REG       2       4x10
#> 119            pes     ENDW*ACTS*REG       3    5x10x10
#> 120            pfa     COMM*ACTS*REG       3   10x10x10
#> 121        pfactor               REG       1         10
#> 122      pfactreal     ENDW*ACTS*REG       3    5x10x10
#> 123            pfd     COMM*ACTS*REG       3   10x10x10
#> 124            pfe     ENDW*ACTS*REG       3    5x10x10
#> 125            pfm     COMM*ACTS*REG       3   10x10x10
#> 126           pfob      COMM*REG*REG       3   10x10x10
#> 127            pga          COMM*REG       2      10x10
#> 128            pgd          COMM*REG       2      10x10
#> 129           pgdp               REG       1         10
#> 130            pgm          COMM*REG       2      10x10
#> 131           pgov               REG       1         10
#> 132            pia          COMM*REG       2      10x10
#> 133            pid          COMM*REG       2      10x10
#> 134            pim          COMM*REG       2      10x10
#> 135           pint          ACTS*REG       2      10x10
#> 136           pinv               REG       1         10
#> 137          pm_cr          COMM*REG       2      10x10
#> 138           pmds      COMM*REG*REG       3   10x10x10
#> 139            pms          COMM*REG       2      10x10
#> 140            pmw          COMM*REG       2      10x10
#> 141         pmwcom              COMM       1         10
#> 142         pmwreg               REG       1         10
#> 143             po          ACTS*REG       2      10x10
#> 144            pop               REG       1         10
#> 145            ppa          COMM*REG       2      10x10
#> 146            ppd          COMM*REG       2      10x10
#> 147            ppm          COMM*REG       2      10x10
#> 148          ppriv               REG       1         10
#> 149             pr          COMM*REG       2      10x10
#> 150    profitslack          ACTS*REG       2      10x10
#> 151             ps     COMM*ACTS*REG       3   10x10x10
#> 152          psave               REG       1         10
#> 153     psaveslack               REG       1         10
#> 154            psw               REG       1         10
#> 155             pt              MARG       1          1
#> 156         ptrans      COMM*REG*REG       3   10x10x10
#> 157            pva          ACTS*REG       2      10x10
#> 158             pw              COMM       1         10
#> 159            pwu              COMM       1         10
#> 160            pxw          COMM*REG       2      10x10
#> 161         pxwcom              COMM       1         10
#> 162         pxwreg               REG       1         10
#> 163             qc          COMM*REG       2      10x10
#> 164            qca     COMM*ACTS*REG       3   10x10x10
#> 165            qds          COMM*REG       2      10x10
#> 166             qe        ENDWMS*REG       2       4x10
#> 167            qes     ENDW*ACTS*REG       3    5x10x10
#> 168           qesf    ENDWF*ACTS*REG       3    1x10x10
#> 169            qfa     COMM*ACTS*REG       3   10x10x10
#> 170            qfd     COMM*ACTS*REG       3   10x10x10
#> 171            qfe     ENDW*ACTS*REG       3    5x10x10
#> 172            qfm     COMM*ACTS*REG       3   10x10x10
#> 173            qga          COMM*REG       2      10x10
#> 174            qgd          COMM*REG       2      10x10
#> 175           qgdp               REG       1         10
#> 176            qgm          COMM*REG       2      10x10
#> 177            qia          COMM*REG       2      10x10
#> 178            qid          COMM*REG       2      10x10
#> 179            qim          COMM*REG       2      10x10
#> 180           qint          ACTS*REG       2      10x10
#> 181           qinv               REG       1         10
#> 182            qms          COMM*REG       2      10x10
#> 183            qmw          COMM*REG       2      10x10
#> 184         qmwcom              COMM       1         10
#> 185         qmwreg               REG       1         10
#> 186             qo          ACTS*REG       2      10x10
#> 187            qow              COMM       1         10
#> 188           qowu              COMM       1         10
#> 189            qpa          COMM*REG       2      10x10
#> 190            qpd          COMM*REG       2      10x10
#> 191           qpev          COMM*REG       2      10x10
#> 192            qpm          COMM*REG       2      10x10
#> 193          qpriv               REG       1         10
#> 194          qsave               REG       1         10
#> 195        qsaveev               REG       1         10
#> 196            qst          MARG*REG       2       1x10
#> 197            qtm              MARG       1          1
#> 198         qtmfsd MARG*COMM*REG*REG       4 1x10x10x10
#> 199            qva          ACTS*REG       2      10x10
#> 200            qxs      COMM*REG*REG       3   10x10x10
#> 201            qxw          COMM*REG       2      10x10
#> 202         qxwcom              COMM       1         10
#> 203         qxwreg               REG       1         10
#> 204         rental               REG       1         10
#> 205           rorc               REG       1         10
#> 206           rore               REG       1         10
#> 207            tfd     COMM*ACTS*REG       3   10x10x10
#> 208            tfe     ENDW*ACTS*REG       3    5x10x10
#> 209            tfm     COMM*ACTS*REG       3   10x10x10
#> 210            tgd          COMM*REG       2      10x10
#> 211            tgm          COMM*REG       2      10x10
#> 212            tid          COMM*REG       2      10x10
#> 213            tim          COMM*REG       2      10x10
#> 214           tinc     ENDW*ACTS*REG       3    5x10x10
#> 215             tm          COMM*REG       2      10x10
#> 216            tms      COMM*REG*REG       3   10x10x10
#> 217             to     COMM*ACTS*REG       3   10x10x10
#> 218            tot               REG       1         10
#> 219           tot2               REG       1         10
#> 220            tpd          COMM*REG       2      10x10
#> 221         tpdall          COMM*REG       2      10x10
#> 222            tpm          COMM*REG       2      10x10
#> 223         tpmall          COMM*REG       2      10x10
#> 224          tpreg               REG       1         10
#> 225      tradslack          COMM*REG       2      10x10
#> 226             tx          COMM*REG       2      10x10
#> 227            txs      COMM*REG*REG       3   10x10x10
#> 228              u               REG       1         10
#> 229          uelas               REG       1         10
#> 230        uelasev               REG       1         10
#> 231         uepriv               REG       1         10
#> 232       ueprivev               REG       1         10
#> 233             ug               REG       1         10
#> 234           ugev               REG       1         10
#> 235             up               REG       1         10
#> 236           upev               REG       1         10
#> 237         valuew              COMM       1         10
#> 238        valuewu              COMM       1         10
#> 239           vgdp               REG       1         10
#> 240         vmwcif          COMM*REG       2      10x10
#> 241         vmwcom              COMM       1         10
#> 242         vmwreg               REG       1         10
#> 243         vxwcom              COMM       1         10
#> 244         vxwfob          COMM*REG       2      10x10
#> 245         vxwreg               REG       1         10
#> 246              y               REG       1         10
#> 247            yev               REG       1         10
#> 248             yg               REG       1         10
#> 249           ygev               REG       1         10
#> 250             yp               REG       1         10
#> 251           ypev               REG       1         10
#> 252        ysaveev               REG       1         10
#> 

# Include column and observation counts across multiple datasets
get_var_structure("ALL", sl4_data, sl4_data1, include_col_size = TRUE)
#> $sl4_data
#>           Variable        Dimensions DimSize  DataShape No.Col No.Obs
#> 1      CNTalleffcr          COMM*REG       2      10x10     10     10
#> 2       CNTalleffr          REG*CTAX       2       10x9      9     10
#> 3          CNTdpar               REG       1         10      1     10
#> 4          CNTendw          ENDW*REG       2       5x10     10      5
#> 5         CNTendwr               REG       1         10      1     10
#> 6            CNTkb               REG       1         10      1     10
#> 7          CNTpinv               REG       1         10      1     10
#> 8           CNTpop               REG       1         10      1     10
#> 9           CNTqca     COMM*ACTS*REG       3   10x10x10    100     10
#> 10           CNTqe          ENDW*REG       2       5x10     10      5
#> 11          CNTqea     ENDW*ACTS*REG       3    5x10x10    100      5
#> 12          CNTqfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 13          CNTqfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 14        CNTqfeer          ENDW*REG       2       5x10     10      5
#> 15         CNTqfer               REG       1         10      1     10
#> 16          CNTqfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 17          CNTqfr           REG*DIR       2       10x2      2     10
#> 18          CNTqgd          COMM*REG       2      10x10     10     10
#> 19          CNTqgm          COMM*REG       2      10x10     10     10
#> 20          CNTqgr           REG*DIR       2       10x2      2     10
#> 21          CNTqia           REG*DIR       2       10x2      2     10
#> 22          CNTqid          COMM*REG       2      10x10     10     10
#> 23          CNTqim          COMM*REG       2      10x10     10     10
#> 24          CNTqms      COMM*REG*REG       3   10x10x10    100     10
#> 25         CNTqmsr               REG       1         10      1     10
#> 26           CNTqo          ACTS*REG       2      10x10     10     10
#> 27          CNTqor               REG       1         10      1     10
#> 28          CNTqpd          COMM*REG       2      10x10     10     10
#> 29          CNTqpm          COMM*REG       2      10x10     10     10
#> 30          CNTqpr           REG*DIR       2       10x2      2     10
#> 31          CNTqxs      COMM*REG*REG       3   10x10x10    100     10
#> 32         CNTqxsr               REG       1         10      1     10
#> 33      CNTtech_af     COMM*ACTS*REG       3   10x10x10    100     10
#> 34     CNTtech_afe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 35    CNTtech_aint          ACTS*REG       2      10x10     10     10
#> 36     CNTtech_ams      COMM*REG*REG       3   10x10x10    100     10
#> 37      CNTtech_ao          ACTS*REG       2      10x10     10     10
#> 38  CNTtech_atmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 39     CNTtech_ava          ACTS*REG       2      10x10     10     10
#> 40        CNTtechr      TECHTYPE*REG       2       7x10     10      7
#> 41         CNTtotr               REG       1         10      1     10
#> 42              EV               REG       1         10      1     10
#> 43          EV_ALT               REG       1         10      1     10
#> 44             afa     COMM*ACTS*REG       3   10x10x10    100     10
#> 45           afall     COMM*ACTS*REG       3   10x10x10    100     10
#> 46           afcom              COMM       1         10      1     10
#> 47             afe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 48          afeall     ENDW*ACTS*REG       3    5x10x10    100      5
#> 49          afecom              ENDW       1          5      1      5
#> 50          afereg               REG       1         10      1     10
#> 51          afesec              ACTS       1         10      1     10
#> 52           afreg               REG       1         10      1     10
#> 53           afsec              ACTS       1         10      1     10
#> 54            aint          ACTS*REG       2      10x10     10     10
#> 55         aintall          ACTS*REG       2      10x10     10     10
#> 56         aintreg               REG       1         10      1     10
#> 57         aintsec              ACTS       1         10      1     10
#> 58             ams      COMM*REG*REG       3   10x10x10    100     10
#> 59              ao          ACTS*REG       2      10x10     10     10
#> 60           aoall          ACTS*REG       2      10x10     10     10
#> 61           aoreg               REG       1         10      1     10
#> 62           aosec              ACTS       1         10      1     10
#> 63           atall MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 64             atd               REG       1         10      1     10
#> 65             atf              COMM       1         10      1     10
#> 66             atm              COMM       1         10      1     10
#> 67          atmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 68             ats               REG       1         10      1     10
#> 69              au               REG       1         10      1     10
#> 70             ava          ACTS*REG       2      10x10     10     10
#> 71          avaall          ACTS*REG       2      10x10     10     10
#> 72          avareg               REG       1         10      1     10
#> 73          avasec              ACTS       1         10      1     10
#> 74           c1_cr          COMM*REG       2      10x10     10     10
#> 75            c1_r               REG       1         10      1     10
#> 76           c2_cr          COMM*REG       2      10x10     10     10
#> 77            c2_r               REG       1         10      1     10
#> 78           c3_cr          COMM*REG       2      10x10     10     10
#> 79            c3_r               REG       1         10      1     10
#> 80        cgdslack               REG       1         10      1     10
#> 81       compvalad          ACTS*REG       2      10x10     10     10
#> 82      contgdpexp         REG*GDPEX       2       10x6      6     10
#> 83     del_indtaxr               REG       1         10      1     10
#> 84     del_taxrexp               REG       1         10      1     10
#> 85      del_taxrfu               REG       1         10      1     10
#> 86      del_taxrgc               REG       1         10      1     10
#> 87      del_taxric               REG       1         10      1     10
#> 88     del_taxrimp               REG       1         10      1     10
#> 89     del_taxrinc               REG       1         10      1     10
#> 90      del_taxriu               REG       1         10      1     10
#> 91     del_taxrout               REG       1         10      1     10
#> 92      del_taxrpc               REG       1         10      1     10
#> 93        del_tbal               REG       1         10      1     10
#> 94       del_tbalc          COMM*REG       2      10x10     10     10
#> 95      del_tbalry               REG       1         10      1     10
#> 96       del_ttaxr               REG       1         10      1     10
#> 97            dpav               REG       1         10      1     10
#> 98          dpavev               REG       1         10      1     10
#> 99           dpgov               REG       1         10      1     10
#> 100         dppriv               REG       1         10      1     10
#> 101         dpsave               REG       1         10      1     10
#> 102          dpsum               REG       1         10      1     10
#> 103      endwslack          ENDW*REG       2       5x10     10      5
#> 104         expand         ENDWC*REG       2       1x10     10      1
#> 105        fincome               REG       1         10      1     10
#> 106    incomeslack               REG       1         10      1     10
#> 107             kb               REG       1         10      1     10
#> 108             ke               REG       1         10      1     10
#> 109              p               REG       1         10      1     10
#> 110             pb          ACTS*REG       2      10x10     10     10
#> 111            pca     COMM*ACTS*REG       3   10x10x10    100     10
#> 112           pcif      COMM*REG*REG       3   10x10x10    100     10
#> 113            pds          COMM*REG       2      10x10     10     10
#> 114            pdw               REG       1         10      1     10
#> 115             pe        ENDWMS*REG       2       4x10     10      4
#> 116            peb     ENDW*ACTS*REG       3    5x10x10    100      5
#> 117    pebfactreal        ENDWMS*REG       2       4x10     10      4
#> 118     pefactreal        ENDWMS*REG       2       4x10     10      4
#> 119            pes     ENDW*ACTS*REG       3    5x10x10    100      5
#> 120            pfa     COMM*ACTS*REG       3   10x10x10    100     10
#> 121        pfactor               REG       1         10      1     10
#> 122      pfactreal     ENDW*ACTS*REG       3    5x10x10    100      5
#> 123            pfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 124            pfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 125            pfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 126           pfob      COMM*REG*REG       3   10x10x10    100     10
#> 127            pga          COMM*REG       2      10x10     10     10
#> 128            pgd          COMM*REG       2      10x10     10     10
#> 129           pgdp               REG       1         10      1     10
#> 130            pgm          COMM*REG       2      10x10     10     10
#> 131           pgov               REG       1         10      1     10
#> 132            pia          COMM*REG       2      10x10     10     10
#> 133            pid          COMM*REG       2      10x10     10     10
#> 134            pim          COMM*REG       2      10x10     10     10
#> 135           pint          ACTS*REG       2      10x10     10     10
#> 136           pinv               REG       1         10      1     10
#> 137          pm_cr          COMM*REG       2      10x10     10     10
#> 138           pmds      COMM*REG*REG       3   10x10x10    100     10
#> 139            pms          COMM*REG       2      10x10     10     10
#> 140            pmw          COMM*REG       2      10x10     10     10
#> 141         pmwcom              COMM       1         10      1     10
#> 142         pmwreg               REG       1         10      1     10
#> 143             po          ACTS*REG       2      10x10     10     10
#> 144            pop               REG       1         10      1     10
#> 145            ppa          COMM*REG       2      10x10     10     10
#> 146            ppd          COMM*REG       2      10x10     10     10
#> 147            ppm          COMM*REG       2      10x10     10     10
#> 148          ppriv               REG       1         10      1     10
#> 149             pr          COMM*REG       2      10x10     10     10
#> 150    profitslack          ACTS*REG       2      10x10     10     10
#> 151             ps     COMM*ACTS*REG       3   10x10x10    100     10
#> 152          psave               REG       1         10      1     10
#> 153     psaveslack               REG       1         10      1     10
#> 154            psw               REG       1         10      1     10
#> 155             pt              MARG       1          1      1      1
#> 156         ptrans      COMM*REG*REG       3   10x10x10    100     10
#> 157            pva          ACTS*REG       2      10x10     10     10
#> 158             pw              COMM       1         10      1     10
#> 159            pwu              COMM       1         10      1     10
#> 160            pxw          COMM*REG       2      10x10     10     10
#> 161         pxwcom              COMM       1         10      1     10
#> 162         pxwreg               REG       1         10      1     10
#> 163             qc          COMM*REG       2      10x10     10     10
#> 164            qca     COMM*ACTS*REG       3   10x10x10    100     10
#> 165            qds          COMM*REG       2      10x10     10     10
#> 166             qe        ENDWMS*REG       2       4x10     10      4
#> 167            qes     ENDW*ACTS*REG       3    5x10x10    100      5
#> 168           qesf    ENDWF*ACTS*REG       3    1x10x10    100      1
#> 169            qfa     COMM*ACTS*REG       3   10x10x10    100     10
#> 170            qfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 171            qfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 172            qfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 173            qga          COMM*REG       2      10x10     10     10
#> 174            qgd          COMM*REG       2      10x10     10     10
#> 175           qgdp               REG       1         10      1     10
#> 176            qgm          COMM*REG       2      10x10     10     10
#> 177            qia          COMM*REG       2      10x10     10     10
#> 178            qid          COMM*REG       2      10x10     10     10
#> 179            qim          COMM*REG       2      10x10     10     10
#> 180           qint          ACTS*REG       2      10x10     10     10
#> 181           qinv               REG       1         10      1     10
#> 182            qms          COMM*REG       2      10x10     10     10
#> 183            qmw          COMM*REG       2      10x10     10     10
#> 184         qmwcom              COMM       1         10      1     10
#> 185         qmwreg               REG       1         10      1     10
#> 186             qo          ACTS*REG       2      10x10     10     10
#> 187            qow              COMM       1         10      1     10
#> 188           qowu              COMM       1         10      1     10
#> 189            qpa          COMM*REG       2      10x10     10     10
#> 190            qpd          COMM*REG       2      10x10     10     10
#> 191           qpev          COMM*REG       2      10x10     10     10
#> 192            qpm          COMM*REG       2      10x10     10     10
#> 193          qpriv               REG       1         10      1     10
#> 194          qsave               REG       1         10      1     10
#> 195        qsaveev               REG       1         10      1     10
#> 196            qst          MARG*REG       2       1x10     10      1
#> 197            qtm              MARG       1          1      1      1
#> 198         qtmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 199            qva          ACTS*REG       2      10x10     10     10
#> 200            qxs      COMM*REG*REG       3   10x10x10    100     10
#> 201            qxw          COMM*REG       2      10x10     10     10
#> 202         qxwcom              COMM       1         10      1     10
#> 203         qxwreg               REG       1         10      1     10
#> 204         rental               REG       1         10      1     10
#> 205           rorc               REG       1         10      1     10
#> 206           rore               REG       1         10      1     10
#> 207            tfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 208            tfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 209            tfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 210            tgd          COMM*REG       2      10x10     10     10
#> 211            tgm          COMM*REG       2      10x10     10     10
#> 212            tid          COMM*REG       2      10x10     10     10
#> 213            tim          COMM*REG       2      10x10     10     10
#> 214           tinc     ENDW*ACTS*REG       3    5x10x10    100      5
#> 215             tm          COMM*REG       2      10x10     10     10
#> 216            tms      COMM*REG*REG       3   10x10x10    100     10
#> 217             to     COMM*ACTS*REG       3   10x10x10    100     10
#> 218            tot               REG       1         10      1     10
#> 219           tot2               REG       1         10      1     10
#> 220            tpd          COMM*REG       2      10x10     10     10
#> 221         tpdall          COMM*REG       2      10x10     10     10
#> 222            tpm          COMM*REG       2      10x10     10     10
#> 223         tpmall          COMM*REG       2      10x10     10     10
#> 224          tpreg               REG       1         10      1     10
#> 225      tradslack          COMM*REG       2      10x10     10     10
#> 226             tx          COMM*REG       2      10x10     10     10
#> 227            txs      COMM*REG*REG       3   10x10x10    100     10
#> 228              u               REG       1         10      1     10
#> 229          uelas               REG       1         10      1     10
#> 230        uelasev               REG       1         10      1     10
#> 231         uepriv               REG       1         10      1     10
#> 232       ueprivev               REG       1         10      1     10
#> 233             ug               REG       1         10      1     10
#> 234           ugev               REG       1         10      1     10
#> 235             up               REG       1         10      1     10
#> 236           upev               REG       1         10      1     10
#> 237         valuew              COMM       1         10      1     10
#> 238        valuewu              COMM       1         10      1     10
#> 239           vgdp               REG       1         10      1     10
#> 240         vmwcif          COMM*REG       2      10x10     10     10
#> 241         vmwcom              COMM       1         10      1     10
#> 242         vmwreg               REG       1         10      1     10
#> 243         vxwcom              COMM       1         10      1     10
#> 244         vxwfob          COMM*REG       2      10x10     10     10
#> 245         vxwreg               REG       1         10      1     10
#> 246              y               REG       1         10      1     10
#> 247            yev               REG       1         10      1     10
#> 248             yg               REG       1         10      1     10
#> 249           ygev               REG       1         10      1     10
#> 250             yp               REG       1         10      1     10
#> 251           ypev               REG       1         10      1     10
#> 252        ysaveev               REG       1         10      1     10
#> 
#> $sl4_data1
#>           Variable        Dimensions DimSize  DataShape No.Col No.Obs
#> 1      CNTalleffcr          COMM*REG       2      10x10     10     10
#> 2       CNTalleffr          REG*CTAX       2       10x9      9     10
#> 3          CNTdpar               REG       1         10      1     10
#> 4          CNTendw          ENDW*REG       2       5x10     10      5
#> 5         CNTendwr               REG       1         10      1     10
#> 6            CNTkb               REG       1         10      1     10
#> 7          CNTpinv               REG       1         10      1     10
#> 8           CNTpop               REG       1         10      1     10
#> 9           CNTqca     COMM*ACTS*REG       3   10x10x10    100     10
#> 10           CNTqe          ENDW*REG       2       5x10     10      5
#> 11          CNTqea     ENDW*ACTS*REG       3    5x10x10    100      5
#> 12          CNTqfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 13          CNTqfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 14        CNTqfeer          ENDW*REG       2       5x10     10      5
#> 15         CNTqfer               REG       1         10      1     10
#> 16          CNTqfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 17          CNTqfr           REG*DIR       2       10x2      2     10
#> 18          CNTqgd          COMM*REG       2      10x10     10     10
#> 19          CNTqgm          COMM*REG       2      10x10     10     10
#> 20          CNTqgr           REG*DIR       2       10x2      2     10
#> 21          CNTqia           REG*DIR       2       10x2      2     10
#> 22          CNTqid          COMM*REG       2      10x10     10     10
#> 23          CNTqim          COMM*REG       2      10x10     10     10
#> 24          CNTqms      COMM*REG*REG       3   10x10x10    100     10
#> 25         CNTqmsr               REG       1         10      1     10
#> 26           CNTqo          ACTS*REG       2      10x10     10     10
#> 27          CNTqor               REG       1         10      1     10
#> 28          CNTqpd          COMM*REG       2      10x10     10     10
#> 29          CNTqpm          COMM*REG       2      10x10     10     10
#> 30          CNTqpr           REG*DIR       2       10x2      2     10
#> 31          CNTqxs      COMM*REG*REG       3   10x10x10    100     10
#> 32         CNTqxsr               REG       1         10      1     10
#> 33      CNTtech_af     COMM*ACTS*REG       3   10x10x10    100     10
#> 34     CNTtech_afe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 35    CNTtech_aint          ACTS*REG       2      10x10     10     10
#> 36     CNTtech_ams      COMM*REG*REG       3   10x10x10    100     10
#> 37      CNTtech_ao          ACTS*REG       2      10x10     10     10
#> 38  CNTtech_atmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 39     CNTtech_ava          ACTS*REG       2      10x10     10     10
#> 40        CNTtechr      TECHTYPE*REG       2       7x10     10      7
#> 41         CNTtotr               REG       1         10      1     10
#> 42              EV               REG       1         10      1     10
#> 43          EV_ALT               REG       1         10      1     10
#> 44             afa     COMM*ACTS*REG       3   10x10x10    100     10
#> 45           afall     COMM*ACTS*REG       3   10x10x10    100     10
#> 46           afcom              COMM       1         10      1     10
#> 47             afe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 48          afeall     ENDW*ACTS*REG       3    5x10x10    100      5
#> 49          afecom              ENDW       1          5      1      5
#> 50          afereg               REG       1         10      1     10
#> 51          afesec              ACTS       1         10      1     10
#> 52           afreg               REG       1         10      1     10
#> 53           afsec              ACTS       1         10      1     10
#> 54            aint          ACTS*REG       2      10x10     10     10
#> 55         aintall          ACTS*REG       2      10x10     10     10
#> 56         aintreg               REG       1         10      1     10
#> 57         aintsec              ACTS       1         10      1     10
#> 58             ams      COMM*REG*REG       3   10x10x10    100     10
#> 59              ao          ACTS*REG       2      10x10     10     10
#> 60           aoall          ACTS*REG       2      10x10     10     10
#> 61           aoreg               REG       1         10      1     10
#> 62           aosec              ACTS       1         10      1     10
#> 63           atall MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 64             atd               REG       1         10      1     10
#> 65             atf              COMM       1         10      1     10
#> 66             atm              COMM       1         10      1     10
#> 67          atmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 68             ats               REG       1         10      1     10
#> 69              au               REG       1         10      1     10
#> 70             ava          ACTS*REG       2      10x10     10     10
#> 71          avaall          ACTS*REG       2      10x10     10     10
#> 72          avareg               REG       1         10      1     10
#> 73          avasec              ACTS       1         10      1     10
#> 74           c1_cr          COMM*REG       2      10x10     10     10
#> 75            c1_r               REG       1         10      1     10
#> 76           c2_cr          COMM*REG       2      10x10     10     10
#> 77            c2_r               REG       1         10      1     10
#> 78           c3_cr          COMM*REG       2      10x10     10     10
#> 79            c3_r               REG       1         10      1     10
#> 80        cgdslack               REG       1         10      1     10
#> 81       compvalad          ACTS*REG       2      10x10     10     10
#> 82      contgdpexp         REG*GDPEX       2       10x6      6     10
#> 83     del_indtaxr               REG       1         10      1     10
#> 84     del_taxrexp               REG       1         10      1     10
#> 85      del_taxrfu               REG       1         10      1     10
#> 86      del_taxrgc               REG       1         10      1     10
#> 87      del_taxric               REG       1         10      1     10
#> 88     del_taxrimp               REG       1         10      1     10
#> 89     del_taxrinc               REG       1         10      1     10
#> 90      del_taxriu               REG       1         10      1     10
#> 91     del_taxrout               REG       1         10      1     10
#> 92      del_taxrpc               REG       1         10      1     10
#> 93        del_tbal               REG       1         10      1     10
#> 94       del_tbalc          COMM*REG       2      10x10     10     10
#> 95      del_tbalry               REG       1         10      1     10
#> 96       del_ttaxr               REG       1         10      1     10
#> 97            dpav               REG       1         10      1     10
#> 98          dpavev               REG       1         10      1     10
#> 99           dpgov               REG       1         10      1     10
#> 100         dppriv               REG       1         10      1     10
#> 101         dpsave               REG       1         10      1     10
#> 102          dpsum               REG       1         10      1     10
#> 103      endwslack          ENDW*REG       2       5x10     10      5
#> 104         expand         ENDWC*REG       2       1x10     10      1
#> 105        fincome               REG       1         10      1     10
#> 106    incomeslack               REG       1         10      1     10
#> 107             kb               REG       1         10      1     10
#> 108             ke               REG       1         10      1     10
#> 109              p               REG       1         10      1     10
#> 110             pb          ACTS*REG       2      10x10     10     10
#> 111            pca     COMM*ACTS*REG       3   10x10x10    100     10
#> 112           pcif      COMM*REG*REG       3   10x10x10    100     10
#> 113            pds          COMM*REG       2      10x10     10     10
#> 114            pdw               REG       1         10      1     10
#> 115             pe        ENDWMS*REG       2       4x10     10      4
#> 116            peb     ENDW*ACTS*REG       3    5x10x10    100      5
#> 117    pebfactreal        ENDWMS*REG       2       4x10     10      4
#> 118     pefactreal        ENDWMS*REG       2       4x10     10      4
#> 119            pes     ENDW*ACTS*REG       3    5x10x10    100      5
#> 120            pfa     COMM*ACTS*REG       3   10x10x10    100     10
#> 121        pfactor               REG       1         10      1     10
#> 122      pfactreal     ENDW*ACTS*REG       3    5x10x10    100      5
#> 123            pfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 124            pfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 125            pfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 126           pfob      COMM*REG*REG       3   10x10x10    100     10
#> 127            pga          COMM*REG       2      10x10     10     10
#> 128            pgd          COMM*REG       2      10x10     10     10
#> 129           pgdp               REG       1         10      1     10
#> 130            pgm          COMM*REG       2      10x10     10     10
#> 131           pgov               REG       1         10      1     10
#> 132            pia          COMM*REG       2      10x10     10     10
#> 133            pid          COMM*REG       2      10x10     10     10
#> 134            pim          COMM*REG       2      10x10     10     10
#> 135           pint          ACTS*REG       2      10x10     10     10
#> 136           pinv               REG       1         10      1     10
#> 137          pm_cr          COMM*REG       2      10x10     10     10
#> 138           pmds      COMM*REG*REG       3   10x10x10    100     10
#> 139            pms          COMM*REG       2      10x10     10     10
#> 140            pmw          COMM*REG       2      10x10     10     10
#> 141         pmwcom              COMM       1         10      1     10
#> 142         pmwreg               REG       1         10      1     10
#> 143             po          ACTS*REG       2      10x10     10     10
#> 144            pop               REG       1         10      1     10
#> 145            ppa          COMM*REG       2      10x10     10     10
#> 146            ppd          COMM*REG       2      10x10     10     10
#> 147            ppm          COMM*REG       2      10x10     10     10
#> 148          ppriv               REG       1         10      1     10
#> 149             pr          COMM*REG       2      10x10     10     10
#> 150    profitslack          ACTS*REG       2      10x10     10     10
#> 151             ps     COMM*ACTS*REG       3   10x10x10    100     10
#> 152          psave               REG       1         10      1     10
#> 153     psaveslack               REG       1         10      1     10
#> 154            psw               REG       1         10      1     10
#> 155             pt              MARG       1          1      1      1
#> 156         ptrans      COMM*REG*REG       3   10x10x10    100     10
#> 157            pva          ACTS*REG       2      10x10     10     10
#> 158             pw              COMM       1         10      1     10
#> 159            pwu              COMM       1         10      1     10
#> 160            pxw          COMM*REG       2      10x10     10     10
#> 161         pxwcom              COMM       1         10      1     10
#> 162         pxwreg               REG       1         10      1     10
#> 163             qc          COMM*REG       2      10x10     10     10
#> 164            qca     COMM*ACTS*REG       3   10x10x10    100     10
#> 165            qds          COMM*REG       2      10x10     10     10
#> 166             qe        ENDWMS*REG       2       4x10     10      4
#> 167            qes     ENDW*ACTS*REG       3    5x10x10    100      5
#> 168           qesf    ENDWF*ACTS*REG       3    1x10x10    100      1
#> 169            qfa     COMM*ACTS*REG       3   10x10x10    100     10
#> 170            qfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 171            qfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 172            qfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 173            qga          COMM*REG       2      10x10     10     10
#> 174            qgd          COMM*REG       2      10x10     10     10
#> 175           qgdp               REG       1         10      1     10
#> 176            qgm          COMM*REG       2      10x10     10     10
#> 177            qia          COMM*REG       2      10x10     10     10
#> 178            qid          COMM*REG       2      10x10     10     10
#> 179            qim          COMM*REG       2      10x10     10     10
#> 180           qint          ACTS*REG       2      10x10     10     10
#> 181           qinv               REG       1         10      1     10
#> 182            qms          COMM*REG       2      10x10     10     10
#> 183            qmw          COMM*REG       2      10x10     10     10
#> 184         qmwcom              COMM       1         10      1     10
#> 185         qmwreg               REG       1         10      1     10
#> 186             qo          ACTS*REG       2      10x10     10     10
#> 187            qow              COMM       1         10      1     10
#> 188           qowu              COMM       1         10      1     10
#> 189            qpa          COMM*REG       2      10x10     10     10
#> 190            qpd          COMM*REG       2      10x10     10     10
#> 191           qpev          COMM*REG       2      10x10     10     10
#> 192            qpm          COMM*REG       2      10x10     10     10
#> 193          qpriv               REG       1         10      1     10
#> 194          qsave               REG       1         10      1     10
#> 195        qsaveev               REG       1         10      1     10
#> 196            qst          MARG*REG       2       1x10     10      1
#> 197            qtm              MARG       1          1      1      1
#> 198         qtmfsd MARG*COMM*REG*REG       4 1x10x10x10   1000      1
#> 199            qva          ACTS*REG       2      10x10     10     10
#> 200            qxs      COMM*REG*REG       3   10x10x10    100     10
#> 201            qxw          COMM*REG       2      10x10     10     10
#> 202         qxwcom              COMM       1         10      1     10
#> 203         qxwreg               REG       1         10      1     10
#> 204         rental               REG       1         10      1     10
#> 205           rorc               REG       1         10      1     10
#> 206           rore               REG       1         10      1     10
#> 207            tfd     COMM*ACTS*REG       3   10x10x10    100     10
#> 208            tfe     ENDW*ACTS*REG       3    5x10x10    100      5
#> 209            tfm     COMM*ACTS*REG       3   10x10x10    100     10
#> 210            tgd          COMM*REG       2      10x10     10     10
#> 211            tgm          COMM*REG       2      10x10     10     10
#> 212            tid          COMM*REG       2      10x10     10     10
#> 213            tim          COMM*REG       2      10x10     10     10
#> 214           tinc     ENDW*ACTS*REG       3    5x10x10    100      5
#> 215             tm          COMM*REG       2      10x10     10     10
#> 216            tms      COMM*REG*REG       3   10x10x10    100     10
#> 217             to     COMM*ACTS*REG       3   10x10x10    100     10
#> 218            tot               REG       1         10      1     10
#> 219           tot2               REG       1         10      1     10
#> 220            tpd          COMM*REG       2      10x10     10     10
#> 221         tpdall          COMM*REG       2      10x10     10     10
#> 222            tpm          COMM*REG       2      10x10     10     10
#> 223         tpmall          COMM*REG       2      10x10     10     10
#> 224          tpreg               REG       1         10      1     10
#> 225      tradslack          COMM*REG       2      10x10     10     10
#> 226             tx          COMM*REG       2      10x10     10     10
#> 227            txs      COMM*REG*REG       3   10x10x10    100     10
#> 228              u               REG       1         10      1     10
#> 229          uelas               REG       1         10      1     10
#> 230        uelasev               REG       1         10      1     10
#> 231         uepriv               REG       1         10      1     10
#> 232       ueprivev               REG       1         10      1     10
#> 233             ug               REG       1         10      1     10
#> 234           ugev               REG       1         10      1     10
#> 235             up               REG       1         10      1     10
#> 236           upev               REG       1         10      1     10
#> 237         valuew              COMM       1         10      1     10
#> 238        valuewu              COMM       1         10      1     10
#> 239           vgdp               REG       1         10      1     10
#> 240         vmwcif          COMM*REG       2      10x10     10     10
#> 241         vmwcom              COMM       1         10      1     10
#> 242         vmwreg               REG       1         10      1     10
#> 243         vxwcom              COMM       1         10      1     10
#> 244         vxwfob          COMM*REG       2      10x10     10     10
#> 245         vxwreg               REG       1         10      1     10
#> 246              y               REG       1         10      1     10
#> 247            yev               REG       1         10      1     10
#> 248             yg               REG       1         10      1     10
#> 249           ygev               REG       1         10      1     10
#> 250             yp               REG       1         10      1     10
#> 251           ypev               REG       1         10      1     10
#> 252        ysaveev               REG       1         10      1     10
#> 
```
