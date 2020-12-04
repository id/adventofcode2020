import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;

public class Day04 {

  static String[] testData = {
      "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
      "byr:1937 iyr:2017 cid:147 hgt:183cm",
      "",
      "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
      "hcl:#cfa07d byr:1929",
      "",
      "hcl:#ae17e1 iyr:2013",
      "eyr:2024",
      "ecl:brn pid:760753108 byr:1931",
      "hgt:179cm",
      "",
      "hcl:#cfa07d eyr:2025 pid:166559648",
      "iyr:2011 ecl:brn hgt:59in"
  };

  final static String[] eyeColors = {
      "amb", "blu", "brn", "gry", "grn", "hzl", "oth"
  };

  final static String[] requiredFields = {
    "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"
  };

  final static List<String> requiredFieldsList = Arrays.asList(requiredFields);

  public static void main(String[] args) {
    solve(testData);

    Scanner scanner = null;
    try {
      scanner = new Scanner(new File("input/day04.txt"));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }

    ArrayList<String> lines = new ArrayList<>();
    int emptyLines = 0;
    while (scanner.hasNextLine()) {
      String line = scanner.nextLine();
      if (line.isEmpty())
        emptyLines++;
      lines.add(line);
    }
    System.out.println("empty lines: " + emptyLines);
    String[] passports = lines.toArray(new String[0]);
    solve(passports);
  }

  static void solve(String[] data) {
    int validPassports = 0;
    int validPassports2 = 0;
    int invalidPassports = 0;
    int totalPassports = 0;
    HashMap<String, String> fieldsMap = new HashMap<>();
    for (int i = 0; i < data.length; i++) {
      if (data[i].isEmpty()) {
        totalPassports++;
        if (fieldsMap.keySet().containsAll(requiredFieldsList)) {
          validPassports++;
          if (validPassport(fieldsMap))
            validPassports2++;
        } else {
          invalidPassports++;
        }

        fieldsMap.clear();
        continue;
      }
      String[] tokens = data[i].split(" ");
      for (String token : tokens) {
        String[] kv = token.split(":");
        if (!kv[0].equals("cid")) {
          fieldsMap.put(kv[0], kv[1]);
        }
      }
    }
    System.out.println("valid passports: " + validPassports);
    System.out.println("valid passports 2: " + validPassports2);
    System.out.println("invalid passports: " + invalidPassports);
    System.out.println("total passports: " + totalPassports);
  }

  static boolean validPassport(HashMap<String, String> fields) {
    try {
      int byr = Integer.parseInt(fields.get("byr"));
      if (byr < 1920 || byr > 2002)
        return false;

      int iyr = Integer.parseInt(fields.get("iyr"));
      if (iyr < 2010 || iyr > 2020)
        return false;

      int eyr = Integer.parseInt(fields.get("eyr"));
      if (eyr < 2020 || eyr > 2030)
        return false;

      String hgt = fields.get("hgt");
      if (hgt.length() < 3)
        return false;
      String unit = hgt.substring(hgt.length()-2);
      if (unit.equals("cm")) {
        int units = Integer.parseInt(hgt.substring(0, hgt.length()-2));
        if (units < 150 || units > 193)
          return false;
      }
      else if (unit.equals("in")) {
        int units = Integer.parseInt(hgt.substring(0, hgt.length()-2));
        if (units < 59 || units > 76)
          return false;
      }
    } catch (NumberFormatException e) {
      return false;
    }

    String hcl = fields.get("hcl");
    if (!hcl.matches("#[a-f0-9]{6}"))
      return false;

    String ecl = fields.get("ecl");
    if (Arrays.stream(eyeColors).noneMatch(s -> s.equals(ecl)))
      return false;

    String pid = fields.get("pid");
    if (!pid.matches("[0-9]{9}"))
      return false;

    return true;
  }
}
