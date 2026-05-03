# Get Dimension Patterns from SL4 and HAR Objects

Extracts and lists unique dimension patterns (e.g., `REG*COMM`,
`REG*REG*ACTS`) from one or more datasets.

## Usage

``` r
get_dim_patterns(..., keep_unique = FALSE)
```

## Arguments

- ...:

  One or more structured SL4 or HAR objects containing dimension
  information.

- keep_unique:

  Logical. If `TRUE`, returns only unique dimension patterns. Default is
  `FALSE`.

## Value

A data frame containing:

- `DimPattern`: The unique dimension patterns.

## Details

- Extracts dimension structure details from the dataset.

- If multiple datasets are provided, combines their dimension
  information.

- If `keep_unique = TRUE`, returns only distinct dimension patterns.

## See also

[`get_dim_elements`](https://bodysbobb.github.io/HARplus/reference/get_dim_elements.md),
[`get_var_structure`](https://bodysbobb.github.io/HARplus/reference/get_var_structure.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Extract dimension patterns
get_dim_patterns(sl4_data)
#>            DimPattern
#> 1            COMM*REG
#> 2       COMM*ACTS*REG
#> 3            COMM*REG
#> 4       COMM*ACTS*REG
#> 5       COMM*ACTS*REG
#> 6       COMM*ACTS*REG
#> 7       ENDW*ACTS*REG
#> 8       ENDW*ACTS*REG
#> 9       ENDW*ACTS*REG
#> 10      ENDW*ACTS*REG
#> 11      ENDW*ACTS*REG
#> 12      COMM*ACTS*REG
#> 13      COMM*ACTS*REG
#> 14      COMM*ACTS*REG
#> 15           COMM*REG
#> 16           COMM*REG
#> 17           COMM*REG
#> 18           COMM*REG
#> 19           COMM*REG
#> 20           COMM*REG
#> 21           COMM*REG
#> 22           COMM*REG
#> 23           COMM*REG
#> 24           COMM*REG
#> 25           COMM*REG
#> 26           COMM*REG
#> 27                REG
#> 28                REG
#> 29                REG
#> 30                REG
#> 31       COMM*REG*REG
#> 32       COMM*REG*REG
#> 33       COMM*REG*REG
#> 34       COMM*REG*REG
#> 35           MARG*REG
#> 36                REG
#> 37                REG
#> 38                REG
#> 39                REG
#> 40                REG
#> 41                REG
#> 42                REG
#> 43                REG
#> 44                REG
#> 45      COMM*ACTS*REG
#> 46      ENDW*ACTS*REG
#> 47           ENDW*REG
#> 48           COMM*REG
#> 49           ACTS*REG
#> 50           ACTS*REG
#> 51           ACTS*REG
#> 52           ACTS*REG
#> 53           ACTS*REG
#> 54           ACTS*REG
#> 55           ACTS*REG
#> 56           ACTS*REG
#> 57           ACTS*REG
#> 58      COMM*ACTS*REG
#> 59      COMM*ACTS*REG
#> 60      COMM*ACTS*REG
#> 61      ENDW*ACTS*REG
#> 62           ACTS*REG
#> 63               ACTS
#> 64                REG
#> 65           ACTS*REG
#> 66               COMM
#> 67               ACTS
#> 68                REG
#> 69      COMM*ACTS*REG
#> 70               ACTS
#> 71                REG
#> 72           ACTS*REG
#> 73               ENDW
#> 74               ACTS
#> 75                REG
#> 76      ENDW*ACTS*REG
#> 77               ACTS
#> 78                REG
#> 79           ACTS*REG
#> 80           ACTS*REG
#> 81           COMM*REG
#> 82                REG
#> 83                REG
#> 84                REG
#> 85                REG
#> 86                REG
#> 87                REG
#> 88                REG
#> 89                REG
#> 90                REG
#> 91                REG
#> 92                REG
#> 93                REG
#> 94           COMM*REG
#> 95           COMM*REG
#> 96           COMM*REG
#> 97           COMM*REG
#> 98           COMM*REG
#> 99           COMM*REG
#> 100               REG
#> 101      COMM*REG*REG
#> 102      COMM*REG*REG
#> 103 MARG*COMM*REG*REG
#> 104              MARG
#> 105              MARG
#> 106 MARG*COMM*REG*REG
#> 107              COMM
#> 108              COMM
#> 109               REG
#> 110               REG
#> 111 MARG*COMM*REG*REG
#> 112          COMM*REG
#> 113      COMM*REG*REG
#> 114          COMM*REG
#> 115      COMM*REG*REG
#> 116          COMM*REG
#> 117          COMM*REG
#> 118          COMM*REG
#> 119     COMM*ACTS*REG
#> 120     COMM*ACTS*REG
#> 121          COMM*REG
#> 122          COMM*REG
#> 123          COMM*REG
#> 124          COMM*REG
#> 125          COMM*REG
#> 126          COMM*REG
#> 127          COMM*REG
#> 128          COMM*REG
#> 129               REG
#> 130        ENDWMS*REG
#> 131        ENDWMS*REG
#> 132    ENDWF*ACTS*REG
#> 133     ENDW*ACTS*REG
#> 134               REG
#> 135               REG
#> 136               REG
#> 137               REG
#> 138               REG
#> 139         ENDWC*REG
#> 140               REG
#> 141               REG
#> 142               REG
#> 143               REG
#> 144               REG
#> 145               REG
#> 146               REG
#> 147               REG
#> 148               REG
#> 149               REG
#> 150               REG
#> 151               REG
#> 152          COMM*REG
#> 153          COMM*REG
#> 154               REG
#> 155               REG
#> 156     ENDW*ACTS*REG
#> 157        ENDWMS*REG
#> 158        ENDWMS*REG
#> 159               REG
#> 160               REG
#> 161               REG
#> 162               REG
#> 163               REG
#> 164               REG
#> 165         REG*GDPEX
#> 166          ACTS*REG
#> 167          COMM*REG
#> 168              COMM
#> 169              COMM
#> 170              COMM
#> 171              COMM
#> 172               REG
#> 173              COMM
#> 174          COMM*REG
#> 175               REG
#> 176              COMM
#> 177              COMM
#> 178              COMM
#> 179          COMM*REG
#> 180               REG
#> 181              COMM
#> 182          COMM*REG
#> 183               REG
#> 184              COMM
#> 185              COMM
#> 186              COMM
#> 187          COMM*REG
#> 188               REG
#> 189               REG
#> 190          COMM*REG
#> 191          COMM*REG
#> 192          COMM*REG
#> 193          COMM*REG
#> 194               REG
#> 195               REG
#> 196               REG
#> 197               REG
#> 198               REG
#> 199               REG
#> 200               REG
#> 201               REG
#> 202               REG
#> 203               REG
#> 204               REG
#> 205               REG
#> 206          COMM*REG
#> 207               REG
#> 208               REG
#> 209               REG
#> 210               REG
#> 211               REG
#> 212               REG
#> 213               REG
#> 214          ACTS*REG
#> 215     COMM*ACTS*REG
#> 216          ENDW*REG
#> 217     ENDW*ACTS*REG
#> 218               REG
#> 219          ENDW*REG
#> 220     ENDW*ACTS*REG
#> 221           REG*DIR
#> 222     COMM*ACTS*REG
#> 223     COMM*ACTS*REG
#> 224          COMM*REG
#> 225          COMM*REG
#> 226           REG*DIR
#> 227           REG*DIR
#> 228          COMM*REG
#> 229          COMM*REG
#> 230           REG*DIR
#> 231          COMM*REG
#> 232          COMM*REG
#> 233               REG
#> 234      COMM*REG*REG
#> 235               REG
#> 236      COMM*REG*REG
#> 237          REG*CTAX
#> 238          COMM*REG
#> 239               REG
#> 240               REG
#> 241               REG
#> 242          ENDW*REG
#> 243      TECHTYPE*REG
#> 244          ACTS*REG
#> 245     ENDW*ACTS*REG
#> 246          ACTS*REG
#> 247          ACTS*REG
#> 248     COMM*ACTS*REG
#> 249 MARG*COMM*REG*REG
#> 250      COMM*REG*REG
#> 251               REG
#> 252               REG

# Extract only unique dimension patterns across datasets
get_dim_patterns(sl4_data, sl4_data2, keep_unique = TRUE)
#>           DimPattern
#> 1           COMM*REG
#> 2      COMM*ACTS*REG
#> 3      ENDW*ACTS*REG
#> 4                REG
#> 5       COMM*REG*REG
#> 6           MARG*REG
#> 7           ENDW*REG
#> 8           ACTS*REG
#> 9               ACTS
#> 10              COMM
#> 11              ENDW
#> 12 MARG*COMM*REG*REG
#> 13              MARG
#> 14        ENDWMS*REG
#> 15    ENDWF*ACTS*REG
#> 16         ENDWC*REG
#> 17         REG*GDPEX
#> 18           REG*DIR
#> 19          REG*CTAX
#> 20      TECHTYPE*REG
```
