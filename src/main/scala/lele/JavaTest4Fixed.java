package lele;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class JavaTest4Fixed {

    private static String loadInput(String filename) throws IOException {
        ClassLoader classLoader = JavaTest4Fixed.class.getClassLoader();
        var resource = classLoader.getResource(filename);
        if (resource == null) {
            throw new IllegalArgumentException("file not found! " + filename);
        } else {
            var file = new File(resource.getFile());
            return Files.readString(file.toPath());

        }
    }

    public static void main(String[] args) throws IOException {

        var input = loadInput("./lele_input_day_4.txt");

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
