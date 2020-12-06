package lele;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class JavaTest4 {

    public static void main(String[] args) {
        String input = "iyr:2015 cid:189 ecl:oth byr:1947 hcl:#6c4ab1 eyr:2026\n" +
                "hgt:174cm\n" +
                "pid:526744288\n" +
                "\n" +
                "pid:688706448 iyr:2017 hgt:162cm cid:174 ecl:grn byr:1943 hcl:#808e9e eyr:2025\n" +
                "\n" +
                "ecl:oth hcl:#733820 cid:124 pid:111220591\n" +
                "iyr:2019 eyr:2001\n" +
                "byr:1933 hgt:159in\n" +
                "\n" +
                "pid:812929897 hgt:159cm hcl:#fffffd byr:1942 iyr:2026 cid:291\n" +
                "ecl:oth\n" +
                "eyr:2024\n" +
                "\n" +
                "cid:83 pid:524032739 iyr:2013 ecl:amb byr:1974\n" +
                "hgt:191cm hcl:#ceb3a1 eyr:2028\n" +
                "\n" +
                "ecl:gry hcl:eefed5 pid:88405792 hgt:183cm cid:221 byr:1963 eyr:2029\n" +
                "\n" +
                "pid:777881168 ecl:grn\n" +
                "hgt:181cm byr:1923 eyr:2021 iyr:2018 hcl:#18171d\n" +
                "\n" +
                "byr:1941 eyr:2027 ecl:gry iyr:2016 pid:062495008 hcl:#a5e1b5 hgt:178cm\n" +
                "\n" +
                "cid:56\n" +
                "byr:1971\n" +
                "hcl:#efcc98 pid:649868696 iyr:2011 eyr:2025 hgt:164cm\n" +
                "\n" +
                "ecl:blu\n" +
                "pid:117915262 eyr:2023 byr:1925 iyr:2020 hcl:#888785\n" +
                "hgt:188cm\n" +
                "\n" +
                "iyr:2012\n" +
                "cid:174\n" +
                "eyr:2024\n" +
                "pid:143293382 ecl:brn byr:1946 hgt:193cm\n" +
                "\n" +
                "eyr:2021 iyr:2011\n" +
                "hgt:192cm pid:251564680\n" +
                "byr:1976\n" +
                "ecl:blu hcl:#602927\n" +
                "\n" +
                "byr:1973 ecl:blu hgt:164cm\n" +
                "eyr:2022 pid:695538656 iyr:2010 cid:244 hcl:#b6652a\n" +
                "\n" +
                "iyr:2014\n" +
                "eyr:2027 pid:358398181 ecl:hzl hgt:74in byr:1949 cid:329\n" +
                "hcl:#ceb3a1\n" +
                "\n" +
                "cid:211\n" +
                "byr:1954 eyr:2023 hgt:172cm ecl:blu iyr:2019 hcl:#623a2f pid:657051725\n" +
                "\n" +
                "pid:562699115 eyr:2026 byr:2000\n" +
                "hgt:162cm hcl:#602927 ecl:amb iyr:2018\n" +
                "\n" +
                "ecl:brn\n" +
                "iyr:2013\n" +
                "pid:835184859 byr:1981 hgt:157cm eyr:2027 hcl:#b6652a\n" +
                "\n" +
                "pid:763432667 byr:1981 hcl:#cfa07d ecl:brn\n" +
                "iyr:2010 hgt:63in cid:107\n" +
                "eyr:2027\n" +
                "\n" +
                "byr:2009\n" +
                "hgt:177cm cid:314\n" +
                "hcl:f55bf8 eyr:2025\n" +
                "pid:632519974\n" +
                "iyr:2015 ecl:amb\n" +
                "\n" +
                "eyr:2024 pid:614239656 hgt:169cm iyr:2014 ecl:hzl byr:1992\n" +
                "hcl:#602927\n" +
                "\n" +
                "ecl:blu\n" +
                "eyr:2026\n" +
                "hcl:#efcc98\n" +
                "byr:1980 iyr:2013\n" +
                "hgt:161cm\n" +
                "pid:065413599\n" +
                "\n" +
                "hgt:182cm\n" +
                "eyr:2025 iyr:2013 pid:939088351 hcl:#b6652a byr:1994 ecl:amb\n" +
                "\n" +
                "hgt:65in cid:220 ecl:amb hcl:#ceb3a1\n" +
                "iyr:2013 eyr:2025 pid:167894964 byr:1976\n" +
                "\n" +
                "hgt:185cm cid:88 ecl:blu iyr:2020\n" +
                "eyr:2020\n" +
                "hcl:#888785 pid:582683387\n" +
                "byr:1981\n" +
                "\n" +
                "hcl:#866857 eyr:2020 byr:1948\n" +
                "pid:358943355\n" +
                "ecl:amb hgt:164cm iyr:2019\n" +
                "\n" +
                "pid:127467714\n" +
                "hcl:#ceb3a1 byr:1991 hgt:163cm eyr:2020 iyr:2017 ecl:blu cid:229\n" +
                "\n" +
                "cid:156 byr:1942 eyr:2024 hcl:#cfa07d\n" +
                "ecl:blu pid:843747591\n" +
                "iyr:2014 hgt:173cm\n" +
                "\n" +
                "hcl:#a97842 hgt:165cm\n" +
                "iyr:2013 ecl:#781088 byr:1952\n" +
                "pid:516882944\n" +
                "eyr:2026\n" +
                "\n" +
                "hgt:179cm\n" +
                "byr:1969 pid:408297435 iyr:2020 ecl:oth hcl:#cfa07d eyr:2020\n" +
                "\n" +
                "ecl:amb iyr:2013 hcl:#b6652a eyr:2023 cid:88\n" +
                "pid:324081998 hgt:66in byr:1945\n" +
                "\n" +
                "iyr:2012\n" +
                "eyr:2024\n" +
                "hcl:#18171d\n" +
                "pid:756726480 byr:1947 ecl:oth\n" +
                "hgt:164cm\n" +
                "\n" +
                "ecl:blu\n" +
                "hcl:#fffffd byr:1951 iyr:2019 pid:544645775\n" +
                "hgt:153cm eyr:2027\n" +
                "\n" +
                "pid:655906238 ecl:brn eyr:2028 byr:1959 hgt:63in cid:338\n" +
                "iyr:2020\n" +
                "\n" +
                "eyr:2020\n" +
                "hcl:#602927 hgt:72in iyr:2014\n" +
                "pid:305025767\n" +
                "cid:297 byr:1957 ecl:gry\n" +
                "\n" +
                "hgt:155cm byr:1942 hcl:#a97842\n" +
                "iyr:2014 ecl:gry pid:593995708\n" +
                "eyr:2022\n" +
                "\n" +
                "pid:219206471 byr:1955 eyr:2030\n" +
                "hcl:#a97842 ecl:oth iyr:2015 cid:134 hgt:170cm\n" +
                "\n" +
                "iyr:2013 cid:268\n" +
                "eyr:2020\n" +
                "hcl:#a97842 ecl:grn pid:235279200 hgt:178cm\n" +
                "byr:1952\n" +
                "\n" +
                "iyr:2013 pid:016384352 eyr:2027\n" +
                "hcl:#866857 ecl:grn hgt:161cm byr:1943\n" +
                "\n" +
                "ecl:amb hgt:169cm pid:149540593\n" +
                "iyr:2012\n" +
                "eyr:2040 hcl:#a97842 byr:1954\n" +
                "\n" +
                "byr:1938\n" +
                "ecl:brn hcl:#b6652a eyr:2026 hgt:184cm iyr:2018 pid:832531235\n" +
                "\n" +
                "byr:1945 iyr:2015 hgt:171cm eyr:2028 pid:998746896 ecl:hzl hcl:#866857\n" +
                "\n" +
                "hgt:73in ecl:hzl eyr:2023 cid:343 pid:458004221 iyr:2017 byr:1962 hcl:#efcc98\n" +
                "\n" +
                "byr:1970 hgt:159cm pid:925022199 iyr:2013\n" +
                "eyr:2028 hcl:#888785\n" +
                "ecl:hzl\n" +
                "\n" +
                "eyr:2027 iyr:2016 ecl:gry\n" +
                "hcl:#cfa07d\n" +
                "pid:006246552 byr:1939 cid:124 hgt:177cm\n" +
                "\n" +
                "byr:1982\n" +
                "iyr:2016 hgt:159cm\n" +
                "cid:102 hcl:#fffffd\n" +
                "eyr:2029\n" +
                "ecl:grn pid:619798285\n" +
                "\n" +
                "iyr:2018\n" +
                "hgt:189cm hcl:#efcc98\n" +
                "byr:1937 eyr:2023 pid:727551553 ecl:oth\n" +
                "\n" +
                "iyr:2014 byr:1976\n" +
                "eyr:2020 hcl:#7d3b0c pid:125102070 ecl:amb\n" +
                "hgt:186cm\n" +
                "\n" +
                "hgt:187cm byr:1949\n" +
                "pid:027653233 eyr:2021 hcl:#341e13 ecl:hzl\n" +
                "iyr:2020\n" +
                "\n" +
                "iyr:2016\n" +
                "byr:1954 pid:545631256\n" +
                "hcl:#602927 eyr:2023\n" +
                "hgt:191cm ecl:amb\n" +
                "\n" +
                "pid:509762954\n" +
                "hgt:190cm ecl:hzl byr:1991\n" +
                "eyr:2022 iyr:2019\n" +
                "cid:187\n" +
                "\n" +
                "hcl:#c0946f eyr:2024 hgt:152cm cid:277 iyr:2015 pid:872373191 byr:1988\n" +
                "\n" +
                "pid:544267207 cid:113\n" +
                "iyr:2015\n" +
                "hgt:181cm\n" +
                "hcl:#6b5442\n" +
                "ecl:gry\n" +
                "byr:1971\n" +
                "\n" +
                "ecl:gry\n" +
                "hgt:161cm iyr:2012 byr:1965\n" +
                "pid:574527322 hcl:#fffffd\n" +
                "\n" +
                "iyr:2018 byr:1976 hcl:#b6652a\n" +
                "pid:024582079 hgt:169cm ecl:oth eyr:2021\n" +
                "\n" +
                "pid:020478204\n" +
                "byr:1945 hcl:#7d3b0c\n" +
                "cid:239 eyr:2025 hgt:188cm\n" +
                "ecl:grn\n" +
                "iyr:2012\n" +
                "\n" +
                "eyr:2026 pid:202653345\n" +
                "byr:1988\n" +
                "hcl:#2cdc09\n" +
                "hgt:185cm iyr:2010\n" +
                "ecl:hzl\n" +
                "\n" +
                "hgt:183cm iyr:2017\n" +
                "hcl:#18171d byr:1977 eyr:2029 pid:804559436 ecl:grn\n" +
                "\n" +
                "hcl:#602927 pid:812072269 hgt:170cm eyr:2026 byr:1955 iyr:2020 ecl:gry\n" +
                "\n" +
                "eyr:2023 iyr:2010\n" +
                "hcl:#cfa07d pid:592419048 byr:1943\n" +
                "ecl:brn\n" +
                "hgt:172cm\n" +
                "\n" +
                "ecl:brn iyr:2013 pid:558179058\n" +
                "hcl:#fffffd eyr:2022\n" +
                "byr:1922\n" +
                "cid:331 hgt:64in\n" +
                "\n" +
                "ecl:xry\n" +
                "hcl:ade850 eyr:1995 pid:976028541\n" +
                "iyr:2030 hgt:179cm\n" +
                "byr:2030\n" +
                "\n" +
                "ecl:#2872b1 pid:158cm eyr:1927 hcl:ee8e92\n" +
                "iyr:2014 hgt:190cm\n" +
                "byr:2025\n" +
                "\n" +
                "hgt:155cm cid:283 eyr:2020 ecl:blu pid:755165290 byr:1936 hcl:#733820 iyr:2012\n" +
                "\n" +
                "eyr:2030\n" +
                "byr:1943\n" +
                "cid:323 pid:906418061 hgt:157cm ecl:amb iyr:2010\n" +
                "hcl:#7d3b0c\n" +
                "\n" +
                "hcl:#fffffd\n" +
                "pid:873200829 hgt:192cm eyr:2022 ecl:blu iyr:2016 byr:1920 cid:200\n" +
                "\n" +
                "eyr:2021\n" +
                "byr:1963\n" +
                "hcl:#a97842 pid:585551405\n" +
                "iyr:2019 cid:91\n" +
                "ecl:brn hgt:60cm\n" +
                "\n" +
                "byr:1946\n" +
                "pid:520273609 hcl:#341e13 cid:66\n" +
                "iyr:2020 hgt:154cm eyr:2024\n" +
                "ecl:brn\n" +
                "\n" +
                "ecl:brn hcl:#d64d7b eyr:2020\n" +
                "byr:1957 hgt:181cm iyr:2019 pid:378496967 cid:135\n" +
                "\n" +
                "pid:002446580\n" +
                "eyr:2027 byr:1939 hcl:#888785\n" +
                "iyr:2011 cid:168\n" +
                "ecl:oth hgt:160cm\n" +
                "\n" +
                "iyr:2019 hgt:70in hcl:#7d3b0c byr:1983\n" +
                "eyr:2024 pid:369493064 cid:54 ecl:oth\n" +
                "\n" +
                "iyr:1979 pid:170cm\n" +
                "hgt:65cm eyr:1933 hcl:z\n" +
                "\n" +
                "ecl:zzz pid:193cm hcl:z eyr:2020 byr:2013 iyr:2016 hgt:177in\n" +
                "\n" +
                "iyr:2010 hgt:187cm\n" +
                "byr:1932\n" +
                "hcl:z ecl:oth pid:665967850 eyr:2030\n" +
                "\n" +
                "eyr:2029\n" +
                "iyr:2013 hcl:#b6652a ecl:amb\n" +
                "byr:1936 pid:516025566\n" +
                "hgt:181cm\n" +
                "\n" +
                "hcl:#c0946f pid:238825672 byr:2000\n" +
                "iyr:2013 eyr:2028 ecl:amb hgt:183cm\n" +
                "\n" +
                "eyr:2021 hcl:#866857\n" +
                "cid:77 iyr:2017 hgt:156cm pid:271118829 ecl:amb\n" +
                "\n" +
                "iyr:2014\n" +
                "hcl:#fffffd\n" +
                "cid:321 hgt:159cm ecl:gry\n" +
                "pid:691381062 eyr:2022 byr:1991\n" +
                "\n" +
                "pid:111506492 hcl:#c1d296 iyr:2011\n" +
                "byr:1934 hgt:176cm cid:263 eyr:2028 ecl:amb\n" +
                "\n" +
                "iyr:2014 hgt:64in eyr:2024 cid:193 hcl:#b6652a byr:1967\n" +
                "ecl:oth pid:138677174\n" +
                "\n" +
                "hgt:168cm iyr:2020 eyr:2030\n" +
                "hcl:#6b5442 ecl:brn pid:975843892 byr:1927\n" +
                "\n" +
                "byr:1957 ecl:amb iyr:2012 pid:177266671 eyr:2026\n" +
                "hcl:#866857 hgt:162cm\n" +
                "\n" +
                "eyr:2029\n" +
                "hcl:#341e13\n" +
                "hgt:175cm pid:465809700 ecl:amb byr:1974\n" +
                "iyr:2010\n" +
                "\n" +
                "hcl:#a97842 iyr:2010\n" +
                "hgt:176cm eyr:2029 byr:1931 ecl:grt pid:161604244\n" +
                "\n" +
                "eyr:2024 iyr:2018 hgt:170in byr:1959 ecl:gmt hcl:#888785\n" +
                "pid:94163132\n" +
                "\n" +
                "iyr:2011\n" +
                "hgt:186cm pid:998471478 byr:1956 ecl:amb\n" +
                "eyr:2029\n" +
                "hcl:#efcc98\n" +
                "cid:76\n" +
                "\n" +
                "ecl:brn\n" +
                "byr:2001 pid:378527883 iyr:2013 hcl:#83bdc5 eyr:2020 hgt:181cm\n" +
                "\n" +
                "iyr:2017 ecl:grn hgt:172cm hcl:#888785 cid:100\n" +
                "eyr:2022 byr:2030\n" +
                "pid:311562177\n" +
                "\n" +
                "pid:097558436\n" +
                "cid:141 hgt:152cm iyr:2019\n" +
                "ecl:brn eyr:2023\n" +
                "byr:1940\n" +
                "hcl:#6b5442\n" +
                "\n" +
                "iyr:2016 eyr:2023 byr:1992\n" +
                "hgt:174cm ecl:amb\n" +
                "pid:691291640 cid:190 hcl:#fffffd\n" +
                "\n" +
                "hcl:#623a2f ecl:brn\n" +
                "eyr:2028 cid:227 iyr:2012 hgt:74in pid:964273950 byr:1965\n" +
                "\n" +
                "hcl:#ceb3a1 eyr:2028\n" +
                "iyr:2013 pid:175294029 hgt:150cm ecl:grn\n" +
                "byr:1936\n" +
                "cid:143\n" +
                "\n" +
                "byr:1935 hcl:#a97842 ecl:oth hgt:180cm iyr:2019\n" +
                "pid:857891916\n" +
                "eyr:2026\n" +
                "\n" +
                "pid:084518249 ecl:hzl eyr:2027 hcl:#c0946f hgt:192cm cid:315 byr:1961\n" +
                "iyr:2010\n" +
                "\n" +
                "hgt:67cm pid:37925169 eyr:2022\n" +
                "hcl:z iyr:2012 cid:315 byr:2028 ecl:dne\n" +
                "\n" +
                "hcl:#c0946f byr:1924\n" +
                "hgt:176cm cid:87 pid:682212551 iyr:2011\n" +
                "eyr:2026\n" +
                "ecl:gry\n" +
                "\n" +
                "hgt:181cm byr:1935\n" +
                "iyr:2018 pid:644964785\n" +
                "eyr:2026 ecl:amb\n" +
                "\n" +
                "pid:789810179\n" +
                "ecl:gry eyr:2021\n" +
                "cid:159 hgt:185cm iyr:2020 hcl:#602927\n" +
                "byr:1965\n" +
                "\n" +
                "pid:672386364\n" +
                "iyr:2013 eyr:2021 byr:1951 hcl:#341e13\n" +
                "ecl:gry hgt:173cm\n" +
                "\n" +
                "hcl:#18171d eyr:2030 pid:957722245 iyr:2012 byr:1955\n" +
                "ecl:grn\n" +
                "hgt:154cm\n" +
                "\n" +
                "byr:1955 ecl:oth\n" +
                "hcl:#cfa07d\n" +
                "eyr:2030\n" +
                "iyr:2013 pid:361945273 hgt:154cm\n" +
                "\n" +
                "iyr:2012 eyr:2027 ecl:grn hcl:#16d373\n" +
                "hgt:192cm\n" +
                "\n" +
                "pid:275525273\n" +
                "byr:1986\n" +
                "iyr:2017\n" +
                "eyr:2022\n" +
                "ecl:grn\n" +
                "hgt:75in\n" +
                "hcl:#919cc0\n" +
                "\n" +
                "eyr:2029\n" +
                "cid:84 hcl:#cfa07d iyr:2013 hgt:78\n" +
                "ecl:brn\n" +
                "byr:1925 pid:281331549\n" +
                "\n" +
                "eyr:2027\n" +
                "cid:219 iyr:2016 byr:1971 hcl:#7d3b0c hgt:179cm ecl:grn\n" +
                "pid:301296222\n" +
                "\n" +
                "eyr:2030 iyr:2010 pid:995982765\n" +
                "byr:1926 ecl:amb hcl:#888785 hgt:186cm\n" +
                "\n" +
                "byr:1955 iyr:2015 hgt:165cm cid:101\n" +
                "eyr:2027 ecl:amb hcl:#602927\n" +
                "pid:168654790\n" +
                "\n" +
                "hcl:#7d3b0c byr:1956 eyr:2029 hgt:155cm\n" +
                "ecl:grn pid:816685992\n" +
                "iyr:2016\n" +
                "\n" +
                "ecl:grn hcl:#cfa07d cid:71\n" +
                "pid:914724136 iyr:2012 eyr:2024\n" +
                "hgt:184cm byr:1938\n" +
                "\n" +
                "ecl:gry\n" +
                "eyr:2029 hcl:#602927 pid:255062643 iyr:2015 hgt:175cm\n" +
                "\n" +
                "hcl:#341e13 iyr:2017 eyr:2028\n" +
                "pid:459704815 byr:1922\n" +
                "cid:312\n" +
                "ecl:brn hgt:152cm\n" +
                "\n" +
                "ecl:dne eyr:1981\n" +
                "pid:8356519470 hgt:176 iyr:1941 byr:2006 hcl:z\n" +
                "\n" +
                "ecl:amb pid:753377589 hcl:#a97842 eyr:2022 hgt:187cm\n" +
                "cid:130 iyr:2013 byr:1961\n" +
                "\n" +
                "pid:952444443\n" +
                "hcl:#bde835 byr:1963 iyr:2020 eyr:2025\n" +
                "ecl:amb hgt:162cm\n" +
                "\n" +
                "eyr:2027 iyr:2018 hcl:#ceb3a1 hgt:152cm pid:882429463 ecl:blu byr:1969\n" +
                "\n" +
                "cid:134 eyr:2021 hcl:#a97842 hgt:63in\n" +
                "ecl:grn byr:1975 iyr:2019 pid:154078695\n" +
                "\n" +
                "byr:1956 eyr:2027\n" +
                "pid:396230480 hcl:#b6652a\n" +
                "hgt:175cm iyr:2020 ecl:oth\n" +
                "\n" +
                "ecl:grn\n" +
                "cid:263 hcl:#506937 byr:1924\n" +
                "eyr:2030 pid:705511368 hgt:159cm\n" +
                "iyr:2011\n" +
                "\n" +
                "eyr:2020 hgt:178cm ecl:grn\n" +
                "byr:1947 hcl:#888785\n" +
                "pid:177476829 iyr:2019\n" +
                "\n" +
                "ecl:hzl cid:211 iyr:2016 hgt:176cm pid:405182470\n" +
                "byr:1952\n" +
                "hcl:#866857 eyr:2028\n" +
                "\n" +
                "eyr:2032 cid:152 ecl:gmt hgt:150in\n" +
                "pid:75969209\n" +
                "byr:2019 hcl:z iyr:1940\n" +
                "\n" +
                "hcl:#fffffd hgt:193cm pid:607407479 cid:300 byr:1944 iyr:2017\n" +
                "ecl:oth\n" +
                "eyr:2026\n" +
                "\n" +
                "hcl:z\n" +
                "cid:125 eyr:2040 ecl:dne byr:2015 pid:733096171 hgt:63cm\n" +
                "iyr:1922\n" +
                "\n" +
                "pid:575721428 hgt:152cm cid:275\n" +
                "hcl:#cfa07d eyr:2028\n" +
                "byr:1935 ecl:hzl iyr:2016\n" +
                "\n" +
                "iyr:2012\n" +
                "ecl:grn eyr:2027 hcl:#623a2f pid:029106453 byr:1984 hgt:168cm\n" +
                "\n" +
                "ecl:blu cid:140 eyr:2028 iyr:2018 hcl:#c0946f\n" +
                "hgt:163cm byr:1944\n" +
                "pid:709288293\n" +
                "\n" +
                "byr:1936\n" +
                "hgt:172cm eyr:1997 hcl:#8b8c88 cid:50\n" +
                "iyr:2016 pid:205477922 ecl:grn\n" +
                "\n" +
                "hgt:170cm pid:872750582 eyr:2027 byr:1985 iyr:2017 hcl:#d6976a ecl:blu\n" +
                "\n" +
                "hgt:163cm\n" +
                "pid:189634089 cid:116 byr:1975 eyr:2030\n" +
                "hcl:#efcc98 ecl:brn iyr:2020\n" +
                "\n" +
                "ecl:amb byr:1953 hcl:#6b5442 pid:418787965\n" +
                "iyr:2018 hgt:193cm\n" +
                "eyr:2026\n" +
                "\n" +
                "ecl:#3ec898 cid:339 hcl:#866857 eyr:2025 hgt:179cm pid:591430028 iyr:1936 byr:1995\n" +
                "\n" +
                "pid:285371937 hgt:159cm\n" +
                "byr:1922\n" +
                "iyr:2013 eyr:2023 hcl:#6b5442 ecl:amb\n" +
                "\n" +
                "pid:545260883 ecl:oth\n" +
                "hgt:163cm\n" +
                "iyr:2015 eyr:2021 byr:1975 hcl:#866857\n" +
                "\n" +
                "ecl:hzl hgt:182cm pid:053762098 eyr:2023 cid:174 hcl:#6daac4 iyr:2017 byr:1937\n" +
                "\n" +
                "hgt:178cm iyr:2015 byr:1956 pid:815359103\n" +
                "ecl:blu hcl:#cfa07d eyr:2030\n" +
                "\n" +
                "hcl:#7d3b0c\n" +
                "pid:438108851 hgt:162cm byr:1930 iyr:2014 eyr:2024 ecl:amb\n" +
                "\n" +
                "eyr:2027 iyr:2019 hcl:#90eb1c hgt:178cm\n" +
                "pid:314810594 cid:278 ecl:amb\n" +
                "byr:2001\n" +
                "\n" +
                "byr:1949 iyr:1942 hcl:#888785 ecl:hzl hgt:184cm eyr:2027 pid:899137640\n" +
                "\n" +
                "hgt:153cm\n" +
                "eyr:2022 iyr:2011 byr:1975\n" +
                "hcl:#602927\n" +
                "ecl:amb pid:178cm\n" +
                "\n" +
                "hcl:#6b5442\n" +
                "ecl:amb iyr:2018 eyr:2025 pid:418735327 byr:1922 hgt:74in\n" +
                "\n" +
                "ecl:gmt hcl:z iyr:2024\n" +
                "eyr:1988 hgt:75cm cid:125 pid:690872200 byr:1928\n" +
                "\n" +
                "eyr:2024 hgt:184cm\n" +
                "pid:4634589837 ecl:zzz iyr:2022 byr:2000 hcl:89c187\n" +
                "\n" +
                "iyr:2017 byr:1966 hcl:#efcc98 ecl:brn pid:473085232 eyr:2021 hgt:174cm\n" +
                "\n" +
                "hgt:67in eyr:2030 iyr:2014 byr:1943 hcl:#602927 cid:344\n" +
                "ecl:oth\n" +
                "pid:210476779\n" +
                "\n" +
                "byr:1955\n" +
                "ecl:oth\n" +
                "hgt:193cm iyr:2012 hcl:#623a2f pid:818289829 eyr:2021\n" +
                "\n" +
                "byr:2018 ecl:#872a51 iyr:2024 hcl:97783d\n" +
                "pid:155cm hgt:174cm\n" +
                "eyr:1964\n" +
                "\n" +
                "hcl:#6b5442 hgt:157cm byr:1932 ecl:brn pid:4275535874\n" +
                "eyr:2024 iyr:2015\n" +
                "\n" +
                "pid:959861097\n" +
                "hgt:151cm cid:140 byr:1935\n" +
                "eyr:2029\n" +
                "iyr:2018 ecl:hzl\n" +
                "hcl:#623a2f\n" +
                "\n" +
                "hgt:181cm pid:911791767 eyr:2027\n" +
                "iyr:2016 byr:1962\n" +
                "ecl:grn hcl:#866857\n" +
                "\n" +
                "eyr:2021\n" +
                "byr:1994\n" +
                "hgt:162cm hcl:#866857 ecl:oth iyr:2014\n" +
                "pid:712345689\n" +
                "\n" +
                "hcl:#7d3b0c\n" +
                "hgt:170cm pid:600132416 eyr:2025\n" +
                "iyr:2016 byr:1978 ecl:brn\n" +
                "\n" +
                "hcl:#0a9307\n" +
                "cid:287 byr:1940 pid:786271493\n" +
                "eyr:2028 hgt:186cm\n" +
                "iyr:2019 ecl:oth\n" +
                "\n" +
                "eyr:2025 hgt:190cm ecl:hzl cid:228 iyr:2019\n" +
                "byr:1932\n" +
                "hcl:#623a2f pid:648307551\n" +
                "\n" +
                "pid:304587325 iyr:2019 byr:1923 hcl:#7d3b0c\n" +
                "hgt:190cm\n" +
                "ecl:gry eyr:2030\n" +
                "\n" +
                "hgt:188cm eyr:2027 byr:1958 pid:572934921\n" +
                "hcl:#888785 ecl:hzl iyr:2010\n" +
                "\n" +
                "iyr:2019\n" +
                "hgt:178cm ecl:grn hcl:#7d3b0c pid:007601227\n" +
                "byr:1975 eyr:2023\n" +
                "\n" +
                "pid:808872803 byr:1929\n" +
                "ecl:grn\n" +
                "eyr:2022 iyr:2019 hgt:74in hcl:#602927\n" +
                "\n" +
                "iyr:2019\n" +
                "cid:67 hcl:#602927 pid:292601338 ecl:hzl\n" +
                "byr:2001 eyr:2023 hgt:171cm\n" +
                "\n" +
                "byr:1962 eyr:2022 hcl:#b6652a hgt:193cm\n" +
                "ecl:oth\n" +
                "iyr:2010\n" +
                "\n" +
                "hgt:70in iyr:2014 hcl:#a97842\n" +
                "cid:169 eyr:2020 ecl:amb\n" +
                "pid:329751670 byr:1959\n" +
                "\n" +
                "byr:1920\n" +
                "ecl:oth hgt:172cm cid:57 pid:515139276\n" +
                "eyr:2030\n" +
                "hcl:#18171d\n" +
                "iyr:2013\n" +
                "\n" +
                "iyr:2012\n" +
                "hcl:#a97842 pid:946040810 hgt:65in\n" +
                "byr:1936 ecl:amb eyr:2020\n" +
                "\n" +
                "byr:1948 hcl:#18171d\n" +
                "iyr:2019\n" +
                "ecl:hzl cid:185\n" +
                "eyr:2023\n" +
                "pid:583625200 hgt:191cm\n" +
                "\n" +
                "hgt:154cm eyr:2022\n" +
                "pid:460137392 iyr:2010\n" +
                "ecl:grn\n" +
                "hcl:#ceb3a1\n" +
                "\n" +
                "eyr:2024\n" +
                "iyr:2016 pid:890698391 hgt:172cm hcl:#a97842 cid:271 ecl:oth byr:1926\n" +
                "\n" +
                "hgt:162cm pid:340904964 hcl:#b6652a\n" +
                "byr:1966\n" +
                "iyr:2010\n" +
                "cid:260 eyr:2028\n" +
                "ecl:amb\n" +
                "\n" +
                "byr:1933 eyr:2029 pid:642043350\n" +
                "iyr:2016 hcl:#b6652a ecl:grn\n" +
                "\n" +
                "pid:602218620 eyr:2023 ecl:blu\n" +
                "hcl:#623a2f\n" +
                "byr:1950 hgt:168cm iyr:2015\n" +
                "\n" +
                "ecl:gry pid:490792384\n" +
                "byr:1974\n" +
                "hcl:#a97842 iyr:2016 hgt:170cm\n" +
                "\n" +
                "iyr:2020 ecl:gry byr:2002\n" +
                "eyr:2029 hcl:#9f45c4\n" +
                "hgt:155cm pid:604239618\n" +
                "\n" +
                "hgt:190cm pid:560653271 iyr:2020 cid:349\n" +
                "eyr:2024 ecl:blu hcl:#efcc98 byr:1936\n" +
                "\n" +
                "eyr:2021 byr:1964 hcl:#efcc98 ecl:grn iyr:2018\n" +
                "hgt:165cm pid:218376636\n" +
                "\n" +
                "pid:186217101\n" +
                "iyr:2019 hgt:155cm\n" +
                "byr:2017 eyr:2022 ecl:grn cid:349 hcl:ece72e\n" +
                "\n" +
                "iyr:2015\n" +
                "eyr:2026 pid:802832833\n" +
                "hcl:#888785 hgt:190cm ecl:brn\n" +
                "byr:1952\n" +
                "cid:202\n" +
                "\n" +
                "cid:151 iyr:2017 hgt:152cm hcl:#a97842 eyr:2020 ecl:hzl\n" +
                "pid:554959609 byr:1941\n" +
                "\n" +
                "cid:116\n" +
                "iyr:2019 hgt:159cm byr:1992 pid:662111811\n" +
                "hcl:#18171d ecl:oth eyr:2024\n" +
                "\n" +
                "ecl:grn byr:1966\n" +
                "iyr:1950 pid:585351486\n" +
                "eyr:2038 hgt:178in hcl:a27d2b\n" +
                "\n" +
                "iyr:2014 cid:238 hgt:187cm pid:523401750 ecl:amb hcl:#18171d eyr:2023 byr:1984\n" +
                "\n" +
                "eyr:2021 byr:1957\n" +
                "pid:340752324\n" +
                "iyr:2015 hgt:157cm\n" +
                "hcl:#602927 cid:70\n" +
                "ecl:oth\n" +
                "\n" +
                "pid:458479816 ecl:hzl\n" +
                "eyr:2022 hcl:z\n" +
                "hgt:60cm\n" +
                "byr:2012 iyr:2005\n" +
                "\n" +
                "cid:57\n" +
                "hgt:154cm pid:446142864\n" +
                "hcl:#341e13 byr:1968 eyr:2030\n" +
                "iyr:2019\n" +
                "ecl:brn\n" +
                "\n" +
                "eyr:2028\n" +
                "pid:243811429 byr:1977\n" +
                "iyr:2011 hcl:#18171d hgt:185cm ecl:oth\n" +
                "\n" +
                "cid:205 byr:1976 eyr:2029 pid:649877471 hcl:#cfa07d hgt:152cm\n" +
                "ecl:blu\n" +
                "iyr:2013\n" +
                "\n" +
                "iyr:2009 pid:559014976 ecl:oth hgt:189cm byr:1936 eyr:2037\n" +
                "hcl:#efcc98\n" +
                "\n" +
                "pid:134378987 byr:1983 iyr:2013 hgt:173cm\n" +
                "ecl:oth hcl:#ceb3a1\n" +
                "cid:80\n" +
                "eyr:2020\n" +
                "\n" +
                "hgt:151cm byr:1964 ecl:grn iyr:2010 hcl:#b6652a pid:939492531\n" +
                "eyr:2028\n" +
                "\n" +
                "byr:1961 iyr:2014 hcl:#733820 hgt:179cm\n" +
                "eyr:2026 ecl:gry pid:732892920\n" +
                "\n" +
                "iyr:2018 byr:1996\n" +
                "pid:944007809 ecl:hzl\n" +
                "hcl:#866857 eyr:2021\n" +
                "hgt:155cm\n" +
                "\n" +
                "pid:374875696 hcl:#7d3b0c\n" +
                "ecl:oth\n" +
                "hgt:193cm byr:1948 cid:238\n" +
                "iyr:2020\n" +
                "\n" +
                "pid:305782299 hcl:#b6652a\n" +
                "ecl:brn\n" +
                "hgt:172cm\n" +
                "iyr:2018 byr:1927\n" +
                "\n" +
                "pid:945869114 cid:95 byr:1989 hgt:173cm eyr:2025 hcl:#b6652a iyr:2012 ecl:amb\n" +
                "\n" +
                "pid:55484149\n" +
                "eyr:1958\n" +
                "iyr:1956 ecl:grn\n" +
                "cid:95 byr:2028\n" +
                "hcl:c2af7e\n" +
                "\n" +
                "hgt:176cm ecl:amb\n" +
                "hcl:#a97842 eyr:2029 pid:937928270\n" +
                "cid:251\n" +
                "byr:1978\n" +
                "iyr:2018\n" +
                "\n" +
                "hgt:154cm\n" +
                "cid:213 pid:767329807 ecl:hzl\n" +
                "iyr:2013\n" +
                "hcl:#888785\n" +
                "eyr:2026 byr:1998\n" +
                "\n" +
                "cid:158 hcl:#b6652a hgt:155cm iyr:2010 eyr:2025\n" +
                "byr:1980 pid:338567803 ecl:amb\n" +
                "\n" +
                "hcl:#efcc98 byr:1940 hgt:62in ecl:oth pid:537307591\n" +
                "eyr:2030\n" +
                "iyr:2017\n" +
                "cid:179\n" +
                "\n" +
                "byr:1965 eyr:2027 pid:691913618 hgt:75in\n" +
                "hcl:#6b5442 ecl:gry iyr:2012\n" +
                "\n" +
                "hgt:163cm byr:1964 eyr:2025\n" +
                "iyr:2010 hcl:#ceb3a1 ecl:oth\n" +
                "pid:936536544\n" +
                "\n" +
                "pid:712946803\n" +
                "cid:343\n" +
                "hgt:187cm ecl:oth iyr:2020 byr:1983 eyr:2030\n" +
                "hcl:#7873b3\n" +
                "\n" +
                "ecl:blu\n" +
                "iyr:2010\n" +
                "hcl:#fffffd\n" +
                "eyr:2030\n" +
                "hgt:175cm pid:047567505 byr:1963\n" +
                "\n" +
                "ecl:gry byr:1946 eyr:2026 hcl:#602927\n" +
                "hgt:164cm\n" +
                "iyr:2010\n" +
                "\n" +
                "pid:223378458\n" +
                "iyr:2014 cid:151 ecl:hzl hgt:171cm\n" +
                "eyr:2020\n" +
                "hcl:#341e13 byr:1964\n" +
                "\n" +
                "ecl:brn byr:1948\n" +
                "hcl:#866857\n" +
                "hgt:193cm eyr:2024\n" +
                "iyr:2013 cid:277\n" +
                "\n" +
                "hcl:#623a2f byr:1943 iyr:2011 ecl:oth\n" +
                "hgt:184cm\n" +
                "pid:371604584 eyr:2024 cid:176\n" +
                "\n" +
                "hcl:#efcc98\n" +
                "eyr:2025 pid:241834382\n" +
                "hgt:178cm\n" +
                "byr:1985\n" +
                "iyr:2017\n" +
                "\n" +
                "hcl:#c0946f\n" +
                "byr:1996 pid:701366586 eyr:2026 hgt:163cm iyr:2015 ecl:oth\n" +
                "\n" +
                "hgt:65cm hcl:#18171d\n" +
                "eyr:2024 ecl:brn pid:172cm\n" +
                "iyr:2010\n" +
                "byr:1990\n" +
                "\n" +
                "hcl:#fffffd pid:68659204 hgt:161cm iyr:2025\n" +
                "ecl:#94b8aa byr:2021 eyr:2032\n" +
                "\n" +
                "ecl:blu iyr:2018 byr:1993 cid:184\n" +
                "hgt:177cm pid:289871693 hcl:#733820 eyr:2026\n" +
                "\n" +
                "cid:138\n" +
                "ecl:gry hgt:174cm eyr:2024 byr:1988 iyr:2014 hcl:#341e13 pid:864852584\n" +
                "\n" +
                "cid:321 eyr:2028 pid:93285596 hgt:173cm\n" +
                "iyr:2013 ecl:gry hcl:#623a2f\n" +
                "byr:1927\n" +
                "\n" +
                "pid:431242259 eyr:2022 ecl:hzl\n" +
                "byr:1960 hgt:151cm hcl:#efcc98 iyr:2020\n" +
                "\n" +
                "hcl:#866857 eyr:2029 iyr:2016 ecl:grn pid:526060780 byr:1929\n" +
                "cid:310 hgt:162cm\n" +
                "\n" +
                "ecl:blu hgt:183cm cid:168\n" +
                "iyr:2015\n" +
                "eyr:2021 byr:1951 hcl:#6b5442\n" +
                "pid:594960553\n" +
                "\n" +
                "hcl:#ceb3a1\n" +
                "iyr:2020 byr:1951 hgt:186cm eyr:2022 ecl:amb pid:317661479\n" +
                "\n" +
                "iyr:2016\n" +
                "hgt:163in hcl:#accfa0\n" +
                "ecl:brn\n" +
                "pid:307377995 byr:2000 eyr:2028\n" +
                "\n" +
                "pid:933380459\n" +
                "byr:1938\n" +
                "cid:291 hcl:#c0946f\n" +
                "ecl:oth iyr:2018\n" +
                "eyr:2026 hgt:170cm\n" +
                "\n" +
                "byr:1974\n" +
                "pid:262927116 eyr:2027 ecl:gry\n" +
                "hcl:#341e13 iyr:2014 cid:232 hgt:161cm\n" +
                "\n" +
                "hcl:#602927\n" +
                "byr:2001 iyr:2011\n" +
                "hgt:177cm eyr:2028 pid:165733929 ecl:amb\n" +
                "\n" +
                "byr:1922 cid:144 pid:333716867 hgt:183cm iyr:2015\n" +
                "hcl:#c25ea9 eyr:2022 ecl:blu\n" +
                "\n" +
                "eyr:2021 cid:147 byr:1978\n" +
                "iyr:2020 pid:938828535\n" +
                "hcl:#7d3b0c ecl:amb hgt:159cm\n" +
                "\n" +
                "hgt:153cm ecl:hzl\n" +
                "cid:232 byr:1953 hcl:#a97842 iyr:2016 pid:356632792 eyr:2029\n" +
                "\n" +
                "pid:745727684 ecl:gry iyr:2020\n" +
                "hcl:#a97842\n" +
                "eyr:2025 cid:275\n" +
                "hgt:65in\n" +
                "byr:1957\n" +
                "\n" +
                "hcl:#733820\n" +
                "ecl:grn iyr:2019 byr:1943 eyr:2024 hgt:70in\n" +
                "pid:953607814\n" +
                "\n" +
                "ecl:gry eyr:2028 hcl:#cfa07d\n" +
                "hgt:163cm\n" +
                "byr:1942 iyr:2019 pid:310104177\n" +
                "\n" +
                "hgt:190cm\n" +
                "eyr:2027 iyr:2010 byr:1978\n" +
                "ecl:gry\n" +
                "hcl:#964ba7\n" +
                "\n" +
                "cid:320\n" +
                "eyr:2022 hgt:169cm\n" +
                "ecl:blu hcl:#a97842 iyr:2015 pid:669007078 byr:1986\n" +
                "\n" +
                "iyr:2019 pid:901370677 hcl:7f2398 cid:305\n" +
                "ecl:amb eyr:2011 hgt:190cm byr:1991\n" +
                "\n" +
                "ecl:brn\n" +
                "cid:256 byr:1987 iyr:2017 eyr:2026 hcl:#623a2f pid:875646528\n" +
                "hgt:160cm\n" +
                "\n" +
                "byr:1955 pid:120131971 hcl:#18171d\n" +
                "hgt:156cm\n" +
                "ecl:blu\n" +
                "iyr:2011 eyr:2028\n" +
                "\n" +
                "iyr:2020 ecl:brn cid:188\n" +
                "hgt:157cm\n" +
                "eyr:2026\n" +
                "pid:504067323 hcl:#733820 byr:1982\n" +
                "\n" +
                "cid:102 hgt:177cm\n" +
                "hcl:#733820 ecl:hzl byr:1984 pid:542750146 eyr:2028 iyr:2020\n" +
                "\n" +
                "pid:419639528 iyr:2013 hgt:175cm ecl:blu\n" +
                "eyr:2026 byr:1999 hcl:#733820\n" +
                "\n" +
                "byr:1963 eyr:2020\n" +
                "pid:683641152 ecl:gry cid:207 hgt:180cm\n" +
                "hcl:#cfa07d\n" +
                "iyr:2020\n" +
                "\n" +
                "hgt:192cm pid:156436859 iyr:2020 hcl:#cfa07d\n" +
                "ecl:blu byr:1963 eyr:2025 cid:147\n" +
                "\n" +
                "eyr:2002\n" +
                "hcl:z iyr:2011\n" +
                "pid:6830168962\n" +
                "hgt:156in cid:288 byr:2029\n" +
                "\n" +
                "eyr:2021\n" +
                "pid:277739802 byr:1992 ecl:hzl iyr:2020\n" +
                "hcl:#7c5fe8 hgt:184cm\n" +
                "\n" +
                "byr:1989 pid:066973099\n" +
                "iyr:2017\n" +
                "eyr:2022 ecl:hzl hcl:#888785 hgt:76in\n" +
                "\n" +
                "hcl:#866857\n" +
                "iyr:2016 cid:306\n" +
                "ecl:hzl\n" +
                "pid:453816800 byr:1971 hgt:71in eyr:2030\n" +
                "\n" +
                "pid:248573931 hcl:#cfa07d\n" +
                "iyr:2014 eyr:2024 hgt:186cm byr:1970 cid:128 ecl:blu\n" +
                "\n" +
                "pid:172567579 ecl:brn iyr:2014 byr:1948 cid:309\n" +
                "hgt:151cm hcl:#888785 eyr:2024\n" +
                "\n" +
                "hgt:153cm eyr:2026 byr:1929 ecl:hzl pid:684760742\n" +
                "hcl:#c45e93 iyr:2018\n" +
                "\n" +
                "pid:#d50a43\n" +
                "iyr:1940\n" +
                "ecl:#7880a9 byr:2018 hcl:dc2fa7 hgt:185in eyr:1978\n" +
                "\n" +
                "hcl:#602927 cid:71 eyr:2020\n" +
                "pid:620634584 hgt:157cm byr:1991\n" +
                "iyr:2020 ecl:amb\n" +
                "\n" +
                "eyr:2023\n" +
                "byr:1959 iyr:1947 hgt:152cm ecl:#503286 pid:63978523 hcl:57dd0d\n" +
                "\n" +
                "hgt:190cm\n" +
                "byr:1955 ecl:blu\n" +
                "pid:507892696\n" +
                "hcl:#9bd1f0 eyr:2029\n" +
                "iyr:2010\n" +
                "\n" +
                "pid:365539813\n" +
                "eyr:2022 hcl:#623a2f iyr:2020 hgt:184cm\n" +
                "ecl:oth byr:1920 cid:213\n" +
                "\n" +
                "cid:50 ecl:oth pid:774859218 hgt:193cm\n" +
                "iyr:2017 byr:1925 hcl:#866857\n" +
                "eyr:2021\n" +
                "\n" +
                "hgt:189cm\n" +
                "iyr:2019 byr:1937\n" +
                "hcl:#a97842\n" +
                "eyr:2025 ecl:oth\n" +
                "pid:787390180\n" +
                "\n" +
                "iyr:2019 eyr:2027 hgt:183cm\n" +
                "ecl:hzl pid:549757712\n" +
                "byr:1956\n" +
                "hcl:#866857\n" +
                "\n" +
                "pid:755580715\n" +
                "hcl:#602927 hgt:187cm iyr:2017 byr:1925 eyr:2020 ecl:blu\n" +
                "\n" +
                "iyr:2019 hgt:69in\n" +
                "ecl:amb\n" +
                "hcl:#602927 eyr:2026\n" +
                "pid:951019647 byr:1974\n" +
                "\n" +
                "byr:1943 eyr:2034 hgt:150 pid:#36aedf ecl:oth\n" +
                "hcl:z\n" +
                "\n" +
                "eyr:2024\n" +
                "ecl:hzl pid:824745692 iyr:2012 hcl:06ab6e\n" +
                "byr:1944\n" +
                "hgt:159cm\n" +
                "cid:183\n" +
                "\n" +
                "hgt:169cm ecl:blu\n" +
                "eyr:2030 iyr:2013 byr:1945 pid:791359040 hcl:#7d3b0c\n" +
                "\n" +
                "iyr:2018\n" +
                "ecl:hzl hgt:152cm\n" +
                "hcl:#18171d eyr:2026 byr:1924 pid:534667048\n" +
                "\n" +
                "eyr:2029 pid:933295825\n" +
                "iyr:2011\n" +
                "hcl:#cfa07d byr:1981\n" +
                "hgt:164cm ecl:grn\n" +
                "\n" +
                "ecl:amb byr:1964 iyr:2018\n" +
                "pid:014457573\n" +
                "cid:152\n" +
                "eyr:2028 hgt:171cm hcl:#866857\n" +
                "\n" +
                "hgt:167cm\n" +
                "byr:1974 iyr:2012 ecl:amb pid:512315114\n" +
                "cid:278\n" +
                "eyr:2028 hcl:#623a2f\n" +
                "\n" +
                "hgt:153cm ecl:oth iyr:2012\n" +
                "eyr:2027 hcl:#888785 byr:1999 pid:416990697\n" +
                "\n" +
                "eyr:2025 ecl:blu byr:1991 hcl:#866857\n" +
                "hgt:189cm pid:546461828\n" +
                "\n" +
                "iyr:2016\n" +
                "byr:1988\n" +
                "hgt:160cm eyr:2025 ecl:amb hcl:#602927\n" +
                "pid:562766105\n" +
                "\n" +
                "ecl:oth byr:1942\n" +
                "hcl:#341e13 pid:564975864 cid:158\n" +
                "hgt:159cm eyr:2028\n" +
                "iyr:2018\n" +
                "\n" +
                "pid:406209763 hgt:170cm cid:331\n" +
                "iyr:2018 eyr:2026 byr:1981\n" +
                "hcl:#733820 ecl:gry\n" +
                "\n" +
                "pid:279164109 ecl:oth\n" +
                "cid:197 hcl:#7d3b0c\n" +
                "eyr:2024\n" +
                "hgt:185cm iyr:2020 byr:1925\n" +
                "\n" +
                "hcl:#efcc98 ecl:hzl\n" +
                "cid:92 hgt:190cm pid:724466265 iyr:2020\n" +
                "eyr:2025 byr:1996\n" +
                "\n" +
                "byr:1996\n" +
                "cid:55 pid:906572505 ecl:grn eyr:2022 hcl:#602927 hgt:160cm iyr:2014\n" +
                "\n" +
                "eyr:2028 hcl:#b6652a ecl:hzl hgt:186cm iyr:2016 pid:132872161 byr:1932\n" +
                "\n" +
                "hcl:#fffffd iyr:2019 eyr:2020 hgt:188cm\n" +
                "byr:1951 ecl:brn\n" +
                "pid:842126902\n" +
                "\n" +
                "hcl:#602927\n" +
                "hgt:158cm\n" +
                "eyr:2023 iyr:2010\n" +
                "pid:681061896 byr:1977 ecl:gry\n" +
                "\n" +
                "iyr:2018 hgt:192cm byr:1970 cid:200 ecl:grn eyr:2027\n" +
                "pid:164408694 hcl:#888785\n" +
                "\n" +
                "eyr:2029\n" +
                "pid:447061655 iyr:2010 hcl:#341e13 ecl:oth\n" +
                "cid:187 hgt:185cm byr:1943\n" +
                "\n" +
                "byr:1925 iyr:2012 eyr:2025\n" +
                "hgt:190cm hcl:#18171d pid:017534154 ecl:brn\n" +
                "\n" +
                "hgt:172cm byr:1923\n" +
                "eyr:2026 iyr:2015\n" +
                "pid:580812884 hcl:#c0946f ecl:hzl\n" +
                "\n" +
                "hcl:#888785 eyr:2028\n" +
                "byr:1952 ecl:brn pid:818889983\n" +
                "iyr:2010 hgt:180cm\n" +
                "\n" +
                "eyr:2026 ecl:gry byr:1982 hgt:188cm hcl:#c0946f pid:610689703 iyr:2011\n" +
                "\n" +
                "eyr:2028\n" +
                "iyr:2018\n" +
                "pid:921660781 ecl:amb\n" +
                "hcl:#cfa07d hgt:178cm byr:1975\n" +
                "\n" +
                "byr:1977 pid:667631009 iyr:2010\n" +
                "cid:86 eyr:2022 hgt:189cm hcl:#7d3b0c ecl:oth\n" +
                "\n" +
                "pid:214679440 hgt:190cm ecl:blu iyr:2017\n" +
                "eyr:2025 cid:292\n" +
                "\n" +
                "ecl:amb\n" +
                "iyr:2017 hcl:531ad3\n" +
                "hgt:163 pid:689027667 byr:2006 eyr:2033\n" +
                "\n" +
                "hgt:68in byr:1928 iyr:2010 cid:227 eyr:2023\n" +
                "ecl:hzl pid:#87bab9 hcl:#fffffd\n" +
                "\n" +
                "ecl:grn byr:1940 cid:294 hgt:152cm pid:310277488\n" +
                "iyr:2015 hcl:#18171d eyr:2030\n" +
                "\n" +
                "byr:1965 pid:240720987\n" +
                "eyr:2030 ecl:oth hgt:192cm hcl:#733820\n" +
                "iyr:2016\n" +
                "\n" +
                "pid:830487275\n" +
                "ecl:blu byr:1930\n" +
                "hcl:#b6652a iyr:2013 hgt:188cm eyr:2025\n" +
                "\n" +
                "hgt:177cm byr:1955 eyr:2030 ecl:amb pid:476675886 iyr:2016 hcl:#c0946f\n" +
                "\n" +
                "pid:152702068 iyr:2016 hcl:#b6652a\n" +
                "cid:82 ecl:blu eyr:2029 byr:1975 hgt:161cm\n" +
                "\n" +
                "pid:136852264\n" +
                "eyr:2024 cid:339 ecl:oth byr:1949 iyr:2011\n" +
                "\n" +
                "iyr:2020 pid:772739059\n" +
                "eyr:2025 hgt:157cm\n" +
                "byr:1945 ecl:brn\n" +
                "hcl:#6b5442\n" +
                "\n" +
                "hcl:#18171d eyr:2022\n" +
                "iyr:2018 ecl:grn byr:1933 pid:053763751\n" +
                "\n" +
                "pid:214212776 hcl:#18171d\n" +
                "eyr:2030\n" +
                "iyr:2020 byr:1988\n" +
                "cid:122\n" +
                "hgt:170cm ecl:oth\n" +
                "\n" +
                "pid:883116919 iyr:2018 ecl:brn byr:1938 hgt:187cm eyr:2020\n" +
                "\n" +
                "iyr:2020 hcl:#a97842\n" +
                "cid:329 eyr:2025 byr:1946 pid:636649774\n" +
                "ecl:grn hgt:158cm\n" +
                "\n" +
                "eyr:2023\n" +
                "ecl:blu hgt:161cm\n" +
                "hcl:#341e13 byr:1951\n" +
                "iyr:2020 pid:461889565 cid:97\n" +
                "\n" +
                "hgt:168cm pid:492241189\n" +
                "eyr:2029\n" +
                "iyr:2013\n" +
                "cid:150\n" +
                "byr:1980 hcl:#cfa07d ecl:hzl\n" +
                "\n" +
                "byr:1998 ecl:gry hgt:150cm eyr:2024 pid:401735295 cid:153 hcl:#733820 iyr:2016\n" +
                "\n" +
                "ecl:hzl hgt:184cm iyr:2018\n" +
                "byr:2001\n" +
                "pid:453480077 eyr:2025 hcl:#a97842";

        String input2 = "eyr:1972 cid:100\n" +
                "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n" +
                "\n" +
                "iyr:2019\n" +
                "hcl:#602927 eyr:1967 hgt:170cm\n" +
                "ecl:grn pid:012533040 byr:1946\n" +
                "\n" +
                "hcl:dab227 iyr:2012\n" +
                "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n" +
                "\n" +
                "hgt:59cm ecl:zzz\n" +
                "eyr:2038 hcl:74454a iyr:2023\n" +
                "pid:3556412378 byr:2007";

        String input3="pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n" +
                "hcl:#623a2f\n" +
                "\n" +
                "eyr:2029 ecl:blu cid:129 byr:1989\n" +
                "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n" +
                "\n" +
                "hcl:#888785\n" +
                "hgt:164cm byr:2001 iyr:2015 cid:88\n" +
                "pid:545766238 ecl:hzl\n" +
                "eyr:2022\n" +
                "\n" +
                "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719";

        String[] passports = input.split("\n\n");
        Pattern pattern = Pattern.compile("(([a-z]*)):");

        String fields = "byr\n" +
                "iyr\n" +
                "eyr\n" +
                "hgt\n" +
                "hcl\n" +
                "ecl\n" +
                "pid";
        String cid = "cid";

        Set<String> mandatoryFields = Arrays.stream(fields.split("\n")).collect(Collectors.toSet());

        int validPassport = 0;
//        for (String passport : passports) {
//            Matcher matcher = pattern.matcher(passport);
//            Set<String> currentFields = new HashSet<>();
//            matcher.results().forEach(matchResult -> {
//                System.out.println(matchResult.group(1));
//                currentFields.add(matcher.group(1));
//            });
//
////            for(int i=1; i< matcher.groupCount(); i++){
////                currentFields.add(matcher.group(i));
////            }
//            boolean passCheck = currentFields.containsAll(mandatoryFields) ;
//            if(passCheck) validPassport++;
//        }
//
//        System.out.println("valid passports: " + validPassport);

        for (String passport : passports) {
            Set<String> currentFields = new HashSet<>();
            String[] pairs = passport.trim().replace("\n"," ").split(" ");
            for (String pair : pairs) {
                String key = pair.split(":")[0].trim();
                String value = pair.split(":")[1].trim();
//
//                hgt (Height) - a number followed by either cm or in:
//                If cm, the number must be at least 150 and at most 193.
//                If in, the number must be at least 59 and at most 76.
//                hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
//                        ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
//                        pid (Passport ID) - a nine-digit number, including leading zeroes.
//                cid (Country ID) - ignored, missing or not.

                if(key.equals("byr")) {
                    if(Integer.valueOf(value)<1920 || Integer.valueOf(value)>2002) {
                        System.out.println("wrong value:" + pair);
                        break;
                    }
                }
                if(key.equals("iyr")) {
                    if(Integer.valueOf(value)<2010 || Integer.valueOf(value)>2020) {
                        System.out.println("wrong value:" + pair);
                        break;
                    }
                }
                if(key.equals("eyr")) {
                    if(Integer.valueOf(value)<2020 || Integer.valueOf(value)>2030) {
                        System.out.println("wrong value:" + pair);
                        break;
                    }
                }
                if(key.equals("hgt")) {
                    if(value.contains("in")) {
                        if(Integer.valueOf(value.replace("in",""))<59 || Integer.valueOf(value.replace("in",""))>76) {
                            System.out.println("wrong value:" + pair);
                            break;
                        }
                    }
                    else if(value.contains("cm")) {
                        if(Integer.valueOf(value.replace("cm",""))<150 || Integer.valueOf(value.replace("cm",""))>193) {
                            System.out.println("wrong value:" + pair);
                            break;
                        }
                    } else {
                        System.out.println("wrong value:" + pair);
                        break;
                    }
                }
                if(key.equals("hcl")) {
                    Pattern hair = Pattern.compile("^#[0-9a-f]{6}$");
                    Matcher matcher = hair.matcher(value);
                    if(!matcher.find()) {
                        System.out.println("wrong value:" + pair);
                        break;
                    }
                }
                if(key.equals("pid")) {
                    Pattern pid = Pattern.compile("^([0-9]{9})$");
                    Matcher matcher = pid.matcher(value);
                    if(!matcher.find()) {
                        System.out.println("wrong value:" + pair);
                        break;
                    }
                }
                if(key.equals("ecl")) {
                    if(!"amb blu brn gry grn hzl oth".contains(value)) {
                        System.out.println("wrong value:" + pair);
                        break;
                    }
                }

                currentFields.add(key);
            }
            boolean passCheck = currentFields.containsAll(mandatoryFields) ;
            if(passCheck) {
                validPassport++;
            }
        }

        System.out.println("valid passports: " + validPassport);
    }
}
