import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Day02 {
  public static void main(String[] args) {
    Scanner scanner = null;
    try {
      scanner = new Scanner(new File("input/day02.txt"));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }

    int validPasswords1 = 0;
    int validPasswords2 = 0;
    while (scanner.hasNextLine()) {
      String line = scanner.nextLine();
      if (line.isEmpty())
        break;
      String[] policyPassword = line.split(":");
      String password = policyPassword[1].strip();
      String[] policyParts = policyPassword[0].split(" ");
      char mustContain = policyParts[1].charAt(0);
      String[] minMax = policyParts[0].split("-");
      int min = Integer.parseInt(minMax[0]);
      int max = Integer.parseInt(minMax[1]);
      int found = 0;
      for (char c : password.toCharArray()) {
        if (c == mustContain) {
          found++;
        }
      }
      if (found >= min && found <= max) {
        validPasswords1++;
      }
      if ((password.charAt(min-1) == mustContain && password.charAt(max-1) != mustContain) ||
          (password.charAt(min-1) != mustContain && password.charAt(max-1) == mustContain)) {
        validPasswords2++;
      }
    }
    System.out.println(validPasswords1);
    System.out.println(validPasswords2);
  }
}
