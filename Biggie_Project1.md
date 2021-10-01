Biggie\_Project1
================
Autumn Biggie
10/1/2021

Load API packages.

``` r
library(httr)
library(jsonlite)
```

``` r
library(tidyverse)
```

Accessing the COVID19API.

``` r
cov.summary <- GET("https://api.covid19api.com/summary")
```

Look at structure of object.

``` r
str(cov.summary, max.level = 1)
```

    ## List of 10
    ##  $ url        : chr "https://api.covid19api.com/summary"
    ##  $ status_code: int 200
    ##  $ headers    :List of 20
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 1
    ##  $ cookies    :'data.frame': 0 obs. of  7 variables:
    ##  $ content    : raw [1:50568] 7b 22 49 44 ...
    ##  $ date       : POSIXct[1:1], format: "2021-10-01 22:42:15"
    ##  $ times      : Named num [1:6] 0 0.716 0.732 2.404 3.158 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

Converting the raw data from `content` into character data.

``` r
ready <- cov.summary$content %>% rawToChar() %>% fromJSON()
ready
```

    ## $ID
    ## [1] "5d211794-9fd6-4082-ba52-7df00347c320"
    ## 
    ## $Message
    ## [1] ""
    ## 
    ## $Global
    ## $Global$NewConfirmed
    ## [1] 289050
    ## 
    ## $Global$TotalConfirmed
    ## [1] 233403939
    ## 
    ## $Global$NewDeaths
    ## [1] 5901
    ## 
    ## $Global$TotalDeaths
    ## [1] 4779803
    ## 
    ## $Global$NewRecovered
    ## [1] 0
    ## 
    ## $Global$TotalRecovered
    ## [1] 0
    ## 
    ## $Global$Date
    ## [1] "2021-10-01T22:38:55.23Z"
    ## 
    ## 
    ## $Countries
    ##                                       ID                         Country
    ## 1   adbbd387-55a7-445d-83cf-09679dcf4164                     Afghanistan
    ## 2   40c011ba-4ddd-4e55-b83d-725babea13cb                         Albania
    ## 3   459c5d13-ae81-4b88-94be-f222d3c1f7f6                         Algeria
    ## 4   03b321ad-92fe-406e-8843-42a89211d32d                         Andorra
    ## 5   36a4bd44-1aba-460e-8289-ea2af6901265                          Angola
    ## 6   722e567f-7755-4e07-9141-008f5baa9026             Antigua and Barbuda
    ## 7   bb055f0c-ec34-472c-8ca4-50fc05b8c857                       Argentina
    ## 8   bf7db616-5bb2-4a68-87fc-9d3ea61fd86c                         Armenia
    ## 9   cf91f6c8-1d54-408a-b432-d2c800937ce5                       Australia
    ## 10  b800f4b6-c444-48ca-8694-2b7b9e643f9f                         Austria
    ## 11  b2088b02-217e-472b-be98-d08d227976a7                      Azerbaijan
    ## 12  890ab081-d8b4-4c4e-8950-bd5201bc123f                         Bahamas
    ## 13  58c9116e-40b9-4fbc-aa2c-4e29f02178d6                         Bahrain
    ## 14  d02332a2-c963-4b0b-a461-cab09716db31                      Bangladesh
    ## 15  62cb93c8-d63a-4158-95be-abbc525c4877                        Barbados
    ## 16  38484545-8fe8-41e9-a6c1-facf7628382d                         Belarus
    ## 17  697b29a6-9ff3-4587-9d11-1e4e2148e173                         Belgium
    ## 18  3571dea1-9568-4630-84af-1bf81108e10f                          Belize
    ## 19  b8657497-95bc-4e46-a518-02fac4515ace                           Benin
    ## 20  e4b8c08b-cc63-49b0-9e84-dbdf088f6fa7                          Bhutan
    ## 21  edad7674-b939-4caa-be67-955e0956bebe                         Bolivia
    ## 22  a5ff8fb2-4af6-43b1-9bf1-3e621e0b4216          Bosnia and Herzegovina
    ## 23  9212b344-4374-4884-909f-76846317572e                        Botswana
    ## 24  83d202ce-f2cd-40fc-aae9-9d9caa27866e                          Brazil
    ## 25  5801231b-02a8-4a95-b2c4-807b29b48dc0               Brunei Darussalam
    ## 26  113d73b1-8c41-49aa-bf8e-9f8cf5039a03                        Bulgaria
    ## 27  dda1e8a8-1b77-4412-8da4-a587638f6e4c                    Burkina Faso
    ## 28  2d380fe2-2611-4250-a9ef-34507fa2e723                         Burundi
    ## 29  552bfbc6-a559-42ca-9e53-83e52ce94992                        Cambodia
    ## 30  96bb865e-2dad-4d62-962d-fa2cc6498357                        Cameroon
    ## 31  564290b3-d069-4484-8da9-6aa2e8553b3b                          Canada
    ## 32  8270c223-a8f0-414d-9dc6-9d15afb5f2ba                      Cape Verde
    ## 33  7a34898e-174c-4cf0-a507-2ee560d8a278        Central African Republic
    ## 34  c8ef48c1-066c-4fc1-a50a-92fc9e316ff3                            Chad
    ## 35  e7497078-21c5-4f99-8175-e44fdfe4de9e                           Chile
    ## 36  4d6048ab-b205-40b1-be9f-78acafca4bda                           China
    ## 37  45b3ef3d-8bb9-4264-b8cc-6b13545885dd                        Colombia
    ## 38  503ff2b5-d755-40e3-abb4-730941bf1f84                         Comoros
    ## 39  c4e1231c-c4ad-4603-888e-a57a6f70dbbc             Congo (Brazzaville)
    ## 40  7ee691f3-854d-4eb9-932f-260c3086a196                Congo (Kinshasa)
    ## 41  b31015b3-17b9-4a77-bc18-ae7fc3fa3d26                      Costa Rica
    ## 42  7b653133-a0c5-41f8-89da-aff93944dd6f                         Croatia
    ## 43  c2e666b1-5217-47a1-b3b5-a10bc23af800                            Cuba
    ## 44  ddfd4cae-61f4-4cc9-957b-ee8746500550                          Cyprus
    ## 45  e0a97458-9327-4094-b440-5027bf43ea73                  Czech Republic
    ## 46  eb8d5967-7dc8-4979-ba06-52df390bf87d                  CÃ´te d'Ivoire
    ## 47  c738016e-3a5c-4ded-9503-377f0db5784d                         Denmark
    ## 48  15f6c522-4e0a-4ac0-b2cb-b510679d3ab3                        Djibouti
    ## 49  a38e2d6e-f1b6-4a74-8798-c52e14fb62d9                        Dominica
    ## 50  252624a6-998b-44fb-886e-353be82b1aab              Dominican Republic
    ## 51  6dee77d2-6f4d-4b7a-9124-0624228be8ad                         Ecuador
    ## 52  477d7a30-8094-45d8-bc95-25ecf7996249                           Egypt
    ## 53  da4c4349-91d9-4585-8cc2-920cb63c7f3f                     El Salvador
    ## 54  d3be994a-04f7-4d78-a49b-2acdc55b142e               Equatorial Guinea
    ## 55  ab5ce6d5-ee81-49f2-bd4a-f38930b2a0d3                         Eritrea
    ## 56  b4f158c3-ec79-404d-9357-fbb19674562d                         Estonia
    ## 57  2218e25d-be48-4e9c-8275-268c3f917956                        Ethiopia
    ## 58  2af8773e-57b6-4a8c-a21c-021ca50c8f87                            Fiji
    ## 59  42ea3934-9d58-4710-8119-61d33e368d70                         Finland
    ## 60  1639d434-d856-43bc-9493-003dda7e911e                          France
    ## 61  110ee87a-5674-40cc-b8bf-2d7734928ff7                           Gabon
    ## 62  d79ad2ba-6fd0-49e7-a38e-936970c50b99                          Gambia
    ## 63  55740822-e19c-492e-aa59-b0932c449724                         Georgia
    ## 64  27907489-fbe2-4c22-8e59-c77fbb976c58                         Germany
    ## 65  50361f2a-a592-4d8f-a2fb-b41fea6543e6                           Ghana
    ## 66  fb3227e0-b41e-4f9f-b30e-e08a61471414                          Greece
    ## 67  3f30fddd-c24b-41ec-8eba-05191001b657                         Grenada
    ## 68  7617f365-a499-46e5-8f60-626957b34206                       Guatemala
    ## 69  c5ea0af9-9c86-4a1b-bff1-0248d7c37810                          Guinea
    ## 70  c4cbda6c-d1fd-45f3-92c2-05f7f41c9716                   Guinea-Bissau
    ## 71  e76fa34d-0118-4c6b-992e-b36d31d0446e                          Guyana
    ## 72  5654d312-ff48-4757-8358-92e9f9019fdf                           Haiti
    ## 73  867b4fa6-e5f0-4657-9eb3-7f2e372f9077   Holy See (Vatican City State)
    ## 74  2561d882-bcae-473f-8b94-4157a2819aae                        Honduras
    ## 75  4e56563b-8cf4-4591-abf6-719feb7cb8e5                         Hungary
    ## 76  2ab48720-a07c-4ab4-88d8-e2a17e74fd86                         Iceland
    ## 77  19312b23-a052-4a7a-928c-269d920fa908                           India
    ## 78  f9bea84f-5bdd-4298-9810-c756cd7443a3                       Indonesia
    ## 79  893d552d-974c-416a-a015-1379cfced1c7       Iran, Islamic Republic of
    ## 80  118c0641-de6e-431c-9b3a-fc61611e5c77                            Iraq
    ## 81  f696623b-60b9-478c-88f7-2646ec8439a0                         Ireland
    ## 82  52674678-3c01-493c-8592-e3a2ce1688a8                          Israel
    ## 83  3d3eb7eb-d5ab-472b-8af5-f7e8d643e131                           Italy
    ## 84  aa6a2cdc-114f-4e6d-aa43-0db16c026d23                         Jamaica
    ## 85  4e31c369-e153-4fd9-9eb7-bd0be2fc89da                           Japan
    ## 86  b1ecd777-39d5-474b-834e-771caa9a50e7                          Jordan
    ## 87  4ee242c5-23b5-433c-a777-53f5242c4da7                      Kazakhstan
    ## 88  cd59f81b-93fe-4d42-9b6d-12340efad782                           Kenya
    ## 89  6fe351d3-322b-48c8-bf5d-0cf37ecab2a1                        Kiribati
    ## 90  d25eace9-b5cf-43f7-8efe-9e36f8c3f88c                   Korea (South)
    ## 91  df15611d-e3af-4cdb-b351-3f6b669631ec                          Kuwait
    ## 92  e503996a-f9c9-4dde-8752-4cbf354e68e7                      Kyrgyzstan
    ## 93  bfe4a938-013c-4a8f-97fc-870dcd0bf890                         Lao PDR
    ## 94  ac583998-eefe-4bee-bf60-faf705a1fb27                          Latvia
    ## 95  b5adf874-34e4-409f-a068-65735d04abb7                         Lebanon
    ## 96  d6d95067-cf74-44aa-b625-b3bd3d3cef76                         Lesotho
    ## 97  fbd32292-f683-4ca7-9bde-a023611ff980                         Liberia
    ## 98  da4fed2c-b5ce-4c21-8cd1-7d6d6681c5dc                           Libya
    ## 99  034bb1fe-d951-450a-9504-800c76a7ec5b                   Liechtenstein
    ## 100 b6fce085-33f6-4b52-bed3-5c442d6b8d6e                       Lithuania
    ## 101 f0813f0c-e797-4cec-9974-8d09e2c5d17e                      Luxembourg
    ## 102 40e929d3-3188-4b07-aae7-76dc4a796fa9          Macedonia, Republic of
    ## 103 ce39e2e6-9e16-456e-983c-b4b0275c75fc                      Madagascar
    ## 104 43fe9588-75fc-49f7-b034-041dcd5a9358                          Malawi
    ## 105 f5649332-c954-4910-b013-322f2ea54828                        Malaysia
    ## 106 28e0377c-d307-4a49-b87f-d714c38390b9                        Maldives
    ## 107 de70d486-e3b7-4a0b-88b6-66c99e16e9d4                            Mali
    ## 108 c87a21cd-ae5a-4bc8-af98-f6ae56f1e9dc                           Malta
    ## 109 288e713f-7375-446e-a344-17e6df7b7121                Marshall Islands
    ## 110 808c006a-1832-4b29-8861-17029fa72606                      Mauritania
    ## 111 3a00f200-eaac-43ae-82ff-f97f6d5a1a01                       Mauritius
    ## 112 afc44838-eed6-4811-8375-c9f627a70c39                          Mexico
    ## 113 ed9a6830-b37b-4e76-9852-67f61e5d920a Micronesia, Federated States of
    ## 114 83c23a2e-ee37-4b6d-8001-68f8466b00c5                         Moldova
    ## 115 e8082c70-7c80-4945-a42a-b5a35622fa23                          Monaco
    ## 116 777e3a95-dae3-421c-b987-5baf6c7e4116                        Mongolia
    ## 117 1f5e5ab4-5db0-4528-9f14-09a9d7b9a5ca                      Montenegro
    ## 118 fcc462ca-7f58-4afa-a269-71741a222a16                         Morocco
    ## 119 f0361e78-944c-43bd-8490-abbb3ccae4c6                      Mozambique
    ## 120 5b64ebc1-20f3-440b-976e-99e1dc18dcd0                         Myanmar
    ## 121 1084e38f-8644-417f-8e14-a119b3456bcb                         Namibia
    ## 122 183384b9-fda3-4d9e-9840-0b4d16b53bca                           Nepal
    ## 123 2868b83f-bccb-4d97-b2fb-35d61a1a8c17                     Netherlands
    ## 124 2e32dbb9-946b-4bc3-b5a7-25f53b3132b0                     New Zealand
    ## 125 d99ddb8e-3f35-4dba-8286-a66c2bb416b3                       Nicaragua
    ## 126 ec8aa508-1328-41c0-b354-34a8d084775e                           Niger
    ## 127 27eb8720-5a58-4d91-860c-89859a628db4                         Nigeria
    ## 128 710f570b-e353-43cc-827e-8c8b2bbe48e0                          Norway
    ## 129 3c9a5bd5-ee0d-4bb4-9f88-2885a8e90b2b                            Oman
    ## 130 4aab6fff-a0e6-4770-b441-e0b506d74d04                        Pakistan
    ## 131 d354ab78-a745-4e67-8112-28e7e043399c                           Palau
    ## 132 6a31ff41-69c2-46e2-bf12-83c2d57c59e0           Palestinian Territory
    ## 133 4a253f9d-6fe2-47d5-bda0-96349b58f947                          Panama
    ## 134 dd729b2c-9150-4730-907d-069a828a8fcd                Papua New Guinea
    ## 135 cc5e8a49-2f3f-4f28-86f5-c02c2cd7a759                        Paraguay
    ## 136 ddb21830-f1e1-4523-8884-006927ba2252                            Peru
    ## 137 1ca07d6b-21ea-42c3-be3b-a2300f0f42b5                     Philippines
    ## 138 e097778b-1270-4f99-b0c1-02fe64c4255f                          Poland
    ## 139 6902b444-f944-48a2-b046-0aa96080cf8e                        Portugal
    ## 140 6292f3a8-0832-4861-8671-5f60d58665c3                           Qatar
    ## 141 8913fa94-803d-4cd1-9ea0-91b12c3cb79e              Republic of Kosovo
    ## 142 8c81b7cf-293c-40d6-bb56-0dd1e95a8c95                         Romania
    ## 143 7fec5742-791e-4e8e-abf8-f65bf3fdabb8              Russian Federation
    ## 144 9f3e157d-c52d-4560-97c3-c1f5ea854db1                          Rwanda
    ## 145 567a196d-8459-4dcc-a38d-eec43b42d1ab           Saint Kitts and Nevis
    ## 146 deba8cfd-4bbb-4e2e-b30a-c31748920437                     Saint Lucia
    ## 147 90390051-6a87-46f7-8f85-9ac854b810a9    Saint Vincent and Grenadines
    ## 148 acf53fbf-aec0-4e39-b009-9cd7d2f7a257                           Samoa
    ## 149 bac2dc77-ce18-453a-8a6d-cc4a8f124af6                      San Marino
    ## 150 8f3a6028-a559-44ec-8edb-3c1d38f87ed7           Sao Tome and Principe
    ## 151 bac68ab0-97e9-4774-9a86-efc0868c202a                    Saudi Arabia
    ## 152 29bf1e65-0f2c-4602-8607-ea3329ed2412                         Senegal
    ## 153 289d26f7-374a-4546-bf1e-876cb2617335                          Serbia
    ## 154 a45dd9fb-c31a-4c20-8e61-fbee819c8f5a                      Seychelles
    ## 155 7435fca8-ad6b-4e8d-bc1b-961173ea2785                    Sierra Leone
    ## 156 ea7a8feb-6254-4791-9b2f-6085088693cb                       Singapore
    ## 157 55a48a0d-dd96-452c-9b0e-f8b0422c3c75                        Slovakia
    ## 158 3fd11eb5-2c3b-44a7-8c56-f36492c206c4                        Slovenia
    ## 159 c5d60d44-5799-4791-9023-a2f4f4ecf4b8                 Solomon Islands
    ## 160 b5a12712-4bd8-4669-8793-3ea3cf4f1ba9                         Somalia
    ## 161 dfd6f83d-7288-4309-9878-d0758f806055                    South Africa
    ## 162 520f8041-e0b7-4b2f-b6c2-1b4e72798524                     South Sudan
    ## 163 08fba318-3d75-47a9-bb56-e94b6ede1ea7                           Spain
    ## 164 42073fbe-8c4c-4dda-860f-ea5405715f9a                       Sri Lanka
    ## 165 ddb45149-5fba-4064-9a7c-d1e260fdd108                           Sudan
    ## 166 5fe4a7e2-fa1d-44fa-8d0e-aefec02d7864                        Suriname
    ## 167 e44f8bcb-2c69-4fbb-9c91-639435385cd1                       Swaziland
    ## 168 5795cb44-1a0d-4aac-b326-e1fb7d6cd349                          Sweden
    ## 169 2cf841df-94db-40f6-8262-56dad1b0e104                     Switzerland
    ## 170 207c1987-9903-43b0-91a4-b533bad5dc35    Syrian Arab Republic (Syria)
    ## 171 5ea90f3d-8d90-4831-afca-30bd564c61f2       Taiwan, Republic of China
    ## 172 a282668b-3a1c-4f49-94ad-e2e84d5c99e6                      Tajikistan
    ## 173 d9336a9f-aa06-424d-9bed-3f47bd92fb4f    Tanzania, United Republic of
    ## 174 60d98ab8-17c6-48a5-9dce-27a2886f397a                        Thailand
    ## 175 cd7143af-638e-474e-8f49-3a3b365e8248                     Timor-Leste
    ## 176 472dc2d9-e5b0-441d-b450-f437bdef61ad                            Togo
    ## 177 fcf327f3-d715-4231-be6a-f386e52b5fcd             Trinidad and Tobago
    ## 178 89913429-8aaf-4a39-9cb3-42203c7b41f3                         Tunisia
    ## 179 15d4f6c3-9df4-4585-adb6-58b0ea072015                          Turkey
    ## 180 aff3f827-501b-4d13-810b-1207996b9e92                          Uganda
    ## 181 511494d0-d22d-45ff-b429-a777eacb8c94                         Ukraine
    ## 182 998b580c-7b22-48a0-9cdf-35d8cefeb546            United Arab Emirates
    ## 183 2532fc36-6dc6-4899-83d6-afcd82bf6aa6                  United Kingdom
    ## 184 f7f1bb7f-abef-4652-8a9a-6663dafe5140        United States of America
    ## 185 f453621b-53f0-4dcd-9d4a-b411b56d1c5d                         Uruguay
    ## 186 a8cde022-8908-4555-89e2-1c9d5a6f1d06                      Uzbekistan
    ## 187 66511633-a785-48de-aded-cd6491e2d2cd                         Vanuatu
    ## 188 02109aad-70d6-4c61-9bf3-e7993f30dfe3 Venezuela (Bolivarian Republic)
    ## 189 6cfa23a0-4648-490b-b299-7b71166101d7                        Viet Nam
    ## 190 1dd124ad-4c19-4925-8fa4-726dd722eac6                           Yemen
    ## 191 85c0fe5a-aa41-4fdc-b730-4a57a64a84c0                          Zambia
    ## 192 f9d2d4fd-0577-497b-9da8-83478cde7256                        Zimbabwe
    ##     CountryCode                             Slug NewConfirmed TotalConfirmed
    ## 1            AF                      afghanistan            0         155174
    ## 2            AL                          albania            0         170131
    ## 3            DZ                          algeria            0         203359
    ## 4            AD                          andorra            0          15222
    ## 5            AO                           angola            0          56583
    ## 6            AG              antigua-and-barbuda            0           3231
    ## 7            AR                        argentina            0        5256902
    ## 8            AM                          armenia            0         261697
    ## 9            AU                        australia         2058         107181
    ## 10           AT                          austria            0         743095
    ## 11           AZ                       azerbaijan            0         483902
    ## 12           BS                          bahamas            0          20984
    ## 13           BH                          bahrain            0         275057
    ## 14           BD                       bangladesh            0        1555911
    ## 15           BB                         barbados            0           8381
    ## 16           BY                          belarus            0         538086
    ## 17           BE                          belgium         2133        1244954
    ## 18           BZ                           belize            0          20801
    ## 19           BJ                            benin            0          23890
    ## 20           BT                           bhutan            0           2601
    ## 21           BO                          bolivia            0         500445
    ## 22           BA           bosnia-and-herzegovina            0         234775
    ## 23           BW                         botswana            0         179220
    ## 24           BR                           brazil        27527       21427073
    ## 25           BN                           brunei            0           7116
    ## 26           BG                         bulgaria            0         502162
    ## 27           BF                     burkina-faso            0          14243
    ## 28           BI                          burundi            0          17728
    ## 29           KH                         cambodia            0         112651
    ## 30           CM                         cameroon            0          92303
    ## 31           CA                           canada         1612        1333449
    ## 32           CV                       cape-verde            0          37576
    ## 33           CF         central-african-republic            0          11371
    ## 34           TD                             chad            0           5038
    ## 35           CL                            chile          858        1654264
    ## 36           CN                            china           37         108450
    ## 37           CO                         colombia         1429        4957277
    ## 38           KM                          comoros            0           4141
    ## 39           CG                congo-brazzaville            0          14244
    ## 40           CD                   congo-kinshasa            0          56937
    ## 41           CR                       costa-rica            0         532185
    ## 42           HR                          croatia            0         404790
    ## 43           CU                             cuba            0         877428
    ## 44           CY                           cyprus            0         120143
    ## 45           CZ                   czech-republic            0        1691489
    ## 46           CI                     cote-divoire            0          60253
    ## 47           DK                          denmark            0         360550
    ## 48           DJ                         djibouti            0          12811
    ## 49           DM                         dominica            0           3481
    ## 50           DO               dominican-republic            0         359047
    ## 51           EC                          ecuador            0         509238
    ## 52           EG                            egypt            0         304524
    ## 53           SV                      el-salvador            0         104348
    ## 54           GQ                equatorial-guinea            0          12362
    ## 55           ER                          eritrea            0           6722
    ## 56           EE                          estonia            0         156257
    ## 57           ET                         ethiopia            0         345674
    ## 58           FJ                             fiji            0          51023
    ## 59           FI                          finland            0         141552
    ## 60           FR                           france            0        7106028
    ## 61           GA                            gabon            0          30155
    ## 62           GM                           gambia            0           9934
    ## 63           GE                          georgia            0         613012
    ## 64           DE                          germany        10999        4239773
    ## 65           GH                            ghana            0         127482
    ## 66           GR                           greece            0         655767
    ## 67           GD                          grenada            0           5195
    ## 68           GT                        guatemala            0         560315
    ## 69           GN                           guinea            0          30411
    ## 70           GW                    guinea-bissau            0           6107
    ## 71           GY                           guyana            0          31827
    ## 72           HT                            haiti            0          21647
    ## 73           VA      holy-see-vatican-city-state            0             27
    ## 74           HN                         honduras            0         365994
    ## 75           HU                          hungary            0         822705
    ## 76           IS                          iceland            0          11801
    ## 77           IN                            india        26727       33766707
    ## 78           ID                        indonesia            0        4215104
    ## 79           IR                             iran            0        5587040
    ## 80           IQ                             iraq            0        2003303
    ## 81           IE                          ireland            0         389932
    ## 82           IL                           israel            0        1282218
    ## 83           IT                            italy         4094        4672355
    ## 84           JM                          jamaica            0          83737
    ## 85           JP                            japan         1568        1702255
    ## 86           JO                           jordan            0         823919
    ## 87           KZ                       kazakhstan            0         962842
    ## 88           KE                            kenya            0         249434
    ## 89           KI                         kiribati            0              2
    ## 90           KR                      korea-south            0         313773
    ## 91           KW                           kuwait            0         411655
    ## 92           KG                       kyrgyzstan            0         178532
    ## 93           LA                          lao-pdr            0          23846
    ## 94           LV                           latvia            0         158291
    ## 95           LB                          lebanon            0         624230
    ## 96           LS                          lesotho            0          14395
    ## 97           LR                          liberia            0           5799
    ## 98           LY                            libya            0         340084
    ## 99           LI                    liechtenstein            0           3447
    ## 100          LT                        lithuania            0         331683
    ## 101          LU                       luxembourg            0          78219
    ## 102          MK                        macedonia            0         191408
    ## 103          MG                       madagascar            0          42898
    ## 104          MW                           malawi            0          61580
    ## 105          MY                         malaysia        12735        2245695
    ## 106          MV                         maldives            0          84809
    ## 107          ML                             mali            0          15219
    ## 108          MT                            malta            0          37149
    ## 109          MH                 marshall-islands            0              4
    ## 110          MR                       mauritania            0          36030
    ## 111          MU                        mauritius            0          15695
    ## 112          MX                           mexico         8828        3664223
    ## 113          FM                       micronesia            0              1
    ## 114          MD                          moldova            0         294392
    ## 115          MC                           monaco            0           3314
    ## 116          MN                         mongolia            0         304084
    ## 117          ME                       montenegro            0         131499
    ## 118          MA                          morocco            0         933071
    ## 119          MZ                       mozambique            0         150723
    ## 120          MM                          myanmar            0         464076
    ## 121          NA                          namibia            0         127589
    ## 122          NP                            nepal            0         795061
    ## 123          NL                      netherlands         1703        2003050
    ## 124          NZ                      new-zealand            0           4292
    ## 125          NI                        nicaragua            0          14448
    ## 126          NE                            niger            0           6008
    ## 127          NG                          nigeria            0         205779
    ## 128          NO                           norway            0         189431
    ## 129          OM                             oman            0         303769
    ## 130          PK                         pakistan         1411        1246538
    ## 131          PW                            palau            0              5
    ## 132          PS                        palestine            0         403716
    ## 133          PA                           panama            0         467113
    ## 134          PG                 papua-new-guinea            0          20221
    ## 135          PY                         paraguay            0         459967
    ## 136          PE                             peru            0        2175305
    ## 137          PH                      philippines            0        2549966
    ## 138          PL                           poland            0        2907071
    ## 139          PT                         portugal            0        1069279
    ## 140          QA                            qatar            0         236643
    ## 141          XK                           kosovo            0         160072
    ## 142          RO                          romania            0        1233668
    ## 143          RU                           russia        23330        7401104
    ## 144          RW                           rwanda            0          97517
    ## 145          KN            saint-kitts-and-nevis            0           1918
    ## 146          LC                      saint-lucia            0          11459
    ## 147          VC saint-vincent-and-the-grenadines            0           3508
    ## 148          WS                            samoa            0              3
    ## 149          SM                       san-marino            0           5440
    ## 150          ST            sao-tome-and-principe            0           3459
    ## 151          SA                     saudi-arabia            0         547134
    ## 152          SN                          senegal            0          73775
    ## 153          RS                           serbia            0         941989
    ## 154          SC                       seychelles            0          21473
    ## 155          SL                     sierra-leone            0           6394
    ## 156          SG                        singapore            0          96521
    ## 157          SK                         slovakia            0         412507
    ## 158          SI                         slovenia            0         293364
    ## 159          SB                  solomon-islands            0             20
    ## 160          SO                          somalia            0          19980
    ## 161          ZA                     south-africa            0        2902672
    ## 162          SS                      south-sudan            0          12010
    ## 163          ES                            spain         2400        4959091
    ## 164          LK                        sri-lanka            0         517377
    ## 165          SD                            sudan            0          38283
    ## 166          SR                         suriname            0          41631
    ## 167          SZ                        swaziland            0          45924
    ## 168          SE                           sweden          859        1152886
    ## 169          CH                      switzerland            0         840363
    ## 170          SY                            syria            0          34205
    ## 171          TW                           taiwan            0          16223
    ## 172          TJ                       tajikistan            0          17484
    ## 173          TZ                         tanzania            0           1367
    ## 174          TH                         thailand            0        1603475
    ## 175          TL                      timor-leste            0          19498
    ## 176          TG                             togo            0          25429
    ## 177          TT              trinidad-and-tobago            0          50709
    ## 178          TN                          tunisia            0         707190
    ## 179          TR                           turkey            0        7124654
    ## 180          UG                           uganda            0         123572
    ## 181          UA                          ukraine        12315        2533301
    ## 182          AE             united-arab-emirates            0         735992
    ## 183          GB                   united-kingdom        35833        7843887
    ## 184          US                    united-states       110594       43460343
    ## 185          UY                          uruguay            0         388928
    ## 186          UZ                       uzbekistan            0         173895
    ## 187          VU                          vanuatu            0              4
    ## 188          VE                        venezuela            0         368968
    ## 189          VN                          vietnam            0         790755
    ## 190          YE                            yemen            0           9067
    ## 191          ZM                           zambia            0         209046
    ## 192          ZW                         zimbabwe            0         130820
    ##     NewDeaths TotalDeaths NewRecovered TotalRecovered                    Date
    ## 1           0        7204            0              0 2021-10-01T22:38:55.23Z
    ## 2           0        2698            0              0 2021-10-01T22:38:55.23Z
    ## 3           0        5812            0              0 2021-10-01T22:38:55.23Z
    ## 4           0         130            0              0 2021-10-01T22:38:55.23Z
    ## 5           0        1537            0              0 2021-10-01T22:38:55.23Z
    ## 6           0          79            0              0 2021-10-01T22:38:55.23Z
    ## 7           0      115179            0              0 2021-10-01T22:38:55.23Z
    ## 8           0        5319            0              0 2021-10-01T22:38:55.23Z
    ## 9          20        1311            0              0 2021-10-01T22:38:55.23Z
    ## 10          0       11009            0              0 2021-10-01T22:38:55.23Z
    ## 11          0        6525            0              0 2021-10-01T22:38:55.23Z
    ## 12          0         531            0              0 2021-10-01T22:38:55.23Z
    ## 13          0        1389            0              0 2021-10-01T22:38:55.23Z
    ## 14          0       27510            0              0 2021-10-01T22:38:55.23Z
    ## 15          0          74            0              0 2021-10-01T22:38:55.23Z
    ## 16          0        4143            0              0 2021-10-01T22:38:55.23Z
    ## 17          7       25602            0              0 2021-10-01T22:38:55.23Z
    ## 18          0         414            0              0 2021-10-01T22:38:55.23Z
    ## 19          0         159            0              0 2021-10-01T22:38:55.23Z
    ## 20          0           3            0              0 2021-10-01T22:38:55.23Z
    ## 21          0       18735            0              0 2021-10-01T22:38:55.23Z
    ## 22          0       10606            0              0 2021-10-01T22:38:55.23Z
    ## 23          0        2368            0              0 2021-10-01T22:38:55.23Z
    ## 24        627      596749            0              0 2021-10-01T22:38:55.23Z
    ## 25          0          42            0              0 2021-10-01T22:38:55.23Z
    ## 26          0       20882            0              0 2021-10-01T22:38:55.23Z
    ## 27          0         184            0              0 2021-10-01T22:38:55.23Z
    ## 28          0          38            0              0 2021-10-01T22:38:55.23Z
    ## 29          0        2319            0              0 2021-10-01T22:38:55.23Z
    ## 30          0        1459            0              0 2021-10-01T22:38:55.23Z
    ## 31         19       25196            0              0 2021-10-01T22:38:55.23Z
    ## 32          0         339            0              0 2021-10-01T22:38:55.23Z
    ## 33          0         100            0              0 2021-10-01T22:38:55.23Z
    ## 34          0         174            0              0 2021-10-01T22:38:55.23Z
    ## 35         13       37468            0              0 2021-10-01T22:38:55.23Z
    ## 36          0        4849            0              0 2021-10-01T22:38:55.23Z
    ## 37         38      126299            0              0 2021-10-01T22:38:55.23Z
    ## 38          0         147            0              0 2021-10-01T22:38:55.23Z
    ## 39          0         193            0              0 2021-10-01T22:38:55.23Z
    ## 40          0        1084            0              0 2021-10-01T22:38:55.23Z
    ## 41          0        6386            0              0 2021-10-01T22:38:55.23Z
    ## 42          0        8640            0              0 2021-10-01T22:38:55.23Z
    ## 43          0        7436            0              0 2021-10-01T22:38:55.23Z
    ## 44          0         552            0              0 2021-10-01T22:38:55.23Z
    ## 45          0       30459            0              0 2021-10-01T22:38:55.23Z
    ## 46          0         624            0              0 2021-10-01T22:38:55.23Z
    ## 47          0        2658            0              0 2021-10-01T22:38:55.23Z
    ## 48          0         167            0              0 2021-10-01T22:38:55.23Z
    ## 49          0          20            0              0 2021-10-01T22:38:55.23Z
    ## 50          0        4046            0              0 2021-10-01T22:38:55.23Z
    ## 51          0       32762            0              0 2021-10-01T22:38:55.23Z
    ## 52          0       17331            0              0 2021-10-01T22:38:55.23Z
    ## 53          0        3234            0              0 2021-10-01T22:38:55.23Z
    ## 54          0         147            0              0 2021-10-01T22:38:55.23Z
    ## 55          0          42            0              0 2021-10-01T22:38:55.23Z
    ## 56          0        1357            0              0 2021-10-01T22:38:55.23Z
    ## 57          0        5582            0              0 2021-10-01T22:38:55.23Z
    ## 58          0         624            0              0 2021-10-01T22:38:55.23Z
    ## 59          0        1079            0              0 2021-10-01T22:38:55.23Z
    ## 60          0      117474            0              0 2021-10-01T22:38:55.23Z
    ## 61          0         186            0              0 2021-10-01T22:38:55.23Z
    ## 62          0         338            0              0 2021-10-01T22:38:55.23Z
    ## 63          0        8946            0              0 2021-10-01T22:38:55.23Z
    ## 64         72       93715            0              0 2021-10-01T22:38:55.23Z
    ## 65          0        1156            0              0 2021-10-01T22:38:55.23Z
    ## 66          0       14828            0              0 2021-10-01T22:38:55.23Z
    ## 67          0         141            0              0 2021-10-01T22:38:55.23Z
    ## 68          0       13564            0              0 2021-10-01T22:38:55.23Z
    ## 69          0         379            0              0 2021-10-01T22:38:55.23Z
    ## 70          0         135            0              0 2021-10-01T22:38:55.23Z
    ## 71          0         786            0              0 2021-10-01T22:38:55.23Z
    ## 72          0         610            0              0 2021-10-01T22:38:55.23Z
    ## 73          0           0            0              0 2021-10-01T22:38:55.23Z
    ## 74          0        9777            0              0 2021-10-01T22:38:55.23Z
    ## 75          0       30190            0              0 2021-10-01T22:38:55.23Z
    ## 76          0          33            0              0 2021-10-01T22:38:55.23Z
    ## 77        277      448339            0              0 2021-10-01T22:38:55.23Z
    ## 78          0      141939            0              0 2021-10-01T22:38:55.23Z
    ## 79          0      120428            0              0 2021-10-01T22:38:55.23Z
    ## 80          0       22260            0              0 2021-10-01T22:38:55.23Z
    ## 81          0        5249            0              0 2021-10-01T22:38:55.23Z
    ## 82          0        7761            0              0 2021-10-01T22:38:55.23Z
    ## 83         51      130921            0              0 2021-10-01T22:38:55.23Z
    ## 84          0        1869            0              0 2021-10-01T22:38:55.23Z
    ## 85         42       17664            0              0 2021-10-01T22:38:55.23Z
    ## 86          0       10718            0              0 2021-10-01T22:38:55.23Z
    ## 87          0       15907            0              0 2021-10-01T22:38:55.23Z
    ## 88          0        5123            0              0 2021-10-01T22:38:55.23Z
    ## 89          0           0            0              0 2021-10-01T22:38:55.23Z
    ## 90          0        2497            0              0 2021-10-01T22:38:55.23Z
    ## 91          0        2449            0              0 2021-10-01T22:38:55.23Z
    ## 92          0        2607            0              0 2021-10-01T22:38:55.23Z
    ## 93          0          18            0              0 2021-10-01T22:38:55.23Z
    ## 94          0        2717            0              0 2021-10-01T22:38:55.23Z
    ## 95          0        8325            0              0 2021-10-01T22:38:55.23Z
    ## 96          0         403            0              0 2021-10-01T22:38:55.23Z
    ## 97          0         286            0              0 2021-10-01T22:38:55.23Z
    ## 98          0        4651            0              0 2021-10-01T22:38:55.23Z
    ## 99          0          60            0              0 2021-10-01T22:38:55.23Z
    ## 100         0        4993            0              0 2021-10-01T22:38:55.23Z
    ## 101         0         835            0              0 2021-10-01T22:38:55.23Z
    ## 102         0        6668            0              0 2021-10-01T22:38:55.23Z
    ## 103         0         958            0              0 2021-10-01T22:38:55.23Z
    ## 104         0        2282            0              0 2021-10-01T22:38:55.23Z
    ## 105       192       26335            0              0 2021-10-01T22:38:55.23Z
    ## 106         0         231            0              0 2021-10-01T22:38:55.23Z
    ## 107         0         548            0              0 2021-10-01T22:38:55.23Z
    ## 108         0         457            0              0 2021-10-01T22:38:55.23Z
    ## 109         0           0            0              0 2021-10-01T22:38:55.23Z
    ## 110         0         775            0              0 2021-10-01T22:38:55.23Z
    ## 111         0          84            0              0 2021-10-01T22:38:55.23Z
    ## 112       534      277507            0              0 2021-10-01T22:38:55.23Z
    ## 113         0           0            0              0 2021-10-01T22:38:55.23Z
    ## 114         0        6777            0              0 2021-10-01T22:38:55.23Z
    ## 115         0          33            0              0 2021-10-01T22:38:55.23Z
    ## 116         0        1277            0              0 2021-10-01T22:38:55.23Z
    ## 117         0        1923            0              0 2021-10-01T22:38:55.23Z
    ## 118         0       14267            0              0 2021-10-01T22:38:55.23Z
    ## 119         0        1917            0              0 2021-10-01T22:38:55.23Z
    ## 120         0       17735            0              0 2021-10-01T22:38:55.23Z
    ## 121         0        3511            0              0 2021-10-01T22:38:55.23Z
    ## 122         0       11135            0              0 2021-10-01T22:38:55.23Z
    ## 123         2       18170            0              0 2021-10-01T22:38:55.23Z
    ## 124         0          27            0              0 2021-10-01T22:38:55.23Z
    ## 125         0         204            0              0 2021-10-01T22:38:55.23Z
    ## 126         0         201            0              0 2021-10-01T22:38:55.23Z
    ## 127         0        2721            0              0 2021-10-01T22:38:55.23Z
    ## 128         0         861            0              0 2021-10-01T22:38:55.23Z
    ## 129         0        4096            0              0 2021-10-01T22:38:55.23Z
    ## 130        56       27785            0              0 2021-10-01T22:38:55.23Z
    ## 131         0           0            0              0 2021-10-01T22:38:55.23Z
    ## 132         0        4098            0              0 2021-10-01T22:38:55.23Z
    ## 133         0        7228            0              0 2021-10-01T22:38:55.23Z
    ## 134         0         229            0              0 2021-10-01T22:38:55.23Z
    ## 135         0       16198            0              0 2021-10-01T22:38:55.23Z
    ## 136         0      199367            0              0 2021-10-01T22:38:55.23Z
    ## 137         0       38294            0              0 2021-10-01T22:38:55.23Z
    ## 138         0       75650            0              0 2021-10-01T22:38:55.23Z
    ## 139         0       17975            0              0 2021-10-01T22:38:55.23Z
    ## 140         0         606            0              0 2021-10-01T22:38:55.23Z
    ## 141         0        2953            0              0 2021-10-01T22:38:55.23Z
    ## 142         0       37041            0              0 2021-10-01T22:38:55.23Z
    ## 143       849      203549            0              0 2021-10-01T22:38:55.23Z
    ## 144         0        1273            0              0 2021-10-01T22:38:55.23Z
    ## 145         0          13            0              0 2021-10-01T22:38:55.23Z
    ## 146         0         201            0              0 2021-10-01T22:38:55.23Z
    ## 147         0          21            0              0 2021-10-01T22:38:55.23Z
    ## 148         0           0            0              0 2021-10-01T22:38:55.23Z
    ## 149         0          91            0              0 2021-10-01T22:38:55.23Z
    ## 150         0          50            0              0 2021-10-01T22:38:55.23Z
    ## 151         0        8716            0              0 2021-10-01T22:38:55.23Z
    ## 152         0        1858            0              0 2021-10-01T22:38:55.23Z
    ## 153         0        8234            0              0 2021-10-01T22:38:55.23Z
    ## 154         0         112            0              0 2021-10-01T22:38:55.23Z
    ## 155         0         121            0              0 2021-10-01T22:38:55.23Z
    ## 156         0          95            0              0 2021-10-01T22:38:55.23Z
    ## 157         0       12637            0              0 2021-10-01T22:38:55.23Z
    ## 158         0        4561            0              0 2021-10-01T22:38:55.23Z
    ## 159         0           0            0              0 2021-10-01T22:38:55.23Z
    ## 160         0        1111            0              0 2021-10-01T22:38:55.23Z
    ## 161         0       87626            0              0 2021-10-01T22:38:55.23Z
    ## 162         0         130            0              0 2021-10-01T22:38:55.23Z
    ## 163        18       86415            0              0 2021-10-01T22:38:55.23Z
    ## 164         0       12906            0              0 2021-10-01T22:38:55.23Z
    ## 165         0        2902            0              0 2021-10-01T22:38:55.23Z
    ## 166         0         884            0              0 2021-10-01T22:38:55.23Z
    ## 167         0        1220            0              0 2021-10-01T22:38:55.23Z
    ## 168         8       14864            0              0 2021-10-01T22:38:55.23Z
    ## 169         0       11087            0              0 2021-10-01T22:38:55.23Z
    ## 170         0        2247            0              0 2021-10-01T22:38:55.23Z
    ## 171         0         842            0              0 2021-10-01T22:38:55.23Z
    ## 172         0         125            0              0 2021-10-01T22:38:55.23Z
    ## 173         0          50            0              0 2021-10-01T22:38:55.23Z
    ## 174         0       16727            0              0 2021-10-01T22:38:55.23Z
    ## 175         0         117            0              0 2021-10-01T22:38:55.23Z
    ## 176         0         229            0              0 2021-10-01T22:38:55.23Z
    ## 177         0        1482            0              0 2021-10-01T22:38:55.23Z
    ## 178         0       24890            0              0 2021-10-01T22:38:55.23Z
    ## 179         0       63827            0              0 2021-10-01T22:38:55.23Z
    ## 180         0        3156            0              0 2021-10-01T22:38:55.23Z
    ## 181       212       59980            0              0 2021-10-01T22:38:55.23Z
    ## 182         0        2097            0              0 2021-10-01T22:38:55.23Z
    ## 183       137      137043            0              0 2021-10-01T22:38:55.23Z
    ## 184      2727      697851            0              0 2021-10-01T22:38:55.23Z
    ## 185         0        6055            0              0 2021-10-01T22:38:55.23Z
    ## 186         0        1239            0              0 2021-10-01T22:38:55.23Z
    ## 187         0           1            0              0 2021-10-01T22:38:55.23Z
    ## 188         0        4469            0              0 2021-10-01T22:38:55.23Z
    ## 189         0       19301            0              0 2021-10-01T22:38:55.23Z
    ## 190         0        1721            0              0 2021-10-01T22:38:55.23Z
    ## 191         0        3648            0              0 2021-10-01T22:38:55.23Z
    ## 192         0        4623            0              0 2021-10-01T22:38:55.23Z
    ## 
    ## $Date
    ## [1] "2021-10-01T22:38:55.23Z"

User-friendly function to access data from covid19 API.

``` r
get.covid <- function(country, status = NULL) {
  base <- "https://api.covid19api.com"
  summary <- "summary"
  
  #if the country argument contains spaces, replace them with dashes. Either way, assign value to new_country
  ifelse(grepl(" ", country, fixed = TRUE) == TRUE, new_country <- sub(" ", "-", country, fixed = TRUE), new_country <- country)
  
  #import summary data for all countries if country = all, otherwise arrange the next piece of the URL using the specified country
  ifelse(new_country == "all", piece1 <- summary, piece1 <- paste("country", new_country, sep = "/"))
  
  #if status is specified, include it in the next part of the URL
  ifelse(!is.null(status), piece2 <- paste("status", status, sep = "/"), piece2 <- "nullstatus")
  
  #paste the appropriate url into an object called url
ifelse(piece2 == "nullstatus", url <- paste(base, piece1, sep = "/"), url <- paste(base, piece1, piece2, sep = "/"))

return(list(new_country, status, piece1, piece2, url))
}
```

``` r
get.covid("new zealand", "confirmed")
```

    ## [[1]]
    ## [1] "new-zealand"
    ## 
    ## [[2]]
    ## [1] "confirmed"
    ## 
    ## [[3]]
    ## [1] "country/new-zealand"
    ## 
    ## [[4]]
    ## [1] "status/confirmed"
    ## 
    ## [[5]]
    ## [1] "https://api.covid19api.com/country/new-zealand/status/confirmed"

``` r
new_count <- NULL
is.null(new_count)
```

    ## [1] TRUE
