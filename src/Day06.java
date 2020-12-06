import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Scanner;

public class Day06 {
  public static void main(String[] args) {
    Scanner scanner = null;
    try {
      scanner = new Scanner(new File("input/day06.txt"));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }

    ArrayList<String> lines = new ArrayList<>();
    while (scanner.hasNextLine()) {
      lines.add(scanner.nextLine());
    }
    solvePart1(lines);
    solvePart2(lines);
  }

  static void solvePart1(ArrayList<String> input) {
    HashSet<Character> answers = new HashSet<>();
    int total = 0;
    for (String line : input) {
      if (line.isEmpty()) {
        total += answers.size();
        answers.clear();
        continue;
      }
      for (char c : line.toCharArray()) {
        answers.add(c);
      }
    }
    total += answers.size();
    System.out.println(total);
  }

  static void solvePart2(ArrayList<String> input) {
    HashSet<Character> groupAnswers = new HashSet<>();
    int total = 0;
    boolean newGroup = true;
    for (String line : input) {
      if (line.isEmpty()) {
        total += groupAnswers.size();
        groupAnswers.clear();
        newGroup = true;
        continue;
      }
      HashSet<Character> answers = new HashSet<>();
      for (char c : line.toCharArray()) {
        answers.add(c);
      }
      if (newGroup) {
        groupAnswers = new HashSet<>(answers);
        newGroup = false;
      }
      else
        groupAnswers.retainAll(answers);
    }
    total += groupAnswers.size();
    System.out.println(total);
  }
}
