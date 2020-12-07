import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Day07 {

  public static void main(String[] args) {
    Scanner scanner = null;
    try {
      scanner = new Scanner(new File("input/day07.txt"));
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
    int total = 0;
    Pattern pattern = Pattern.compile("(.*)s contain .*shiny gold bag.*");
    HashSet<String> bagColorsSet = new HashSet<>();
    bagColorsSet.add("shiny gold bag");
    while (true) {
      Set<String> bags = input.stream()
          .map(pattern::matcher)
          .filter(Matcher::find)
          .map(m -> m.group(1))
          .collect(Collectors.toSet());
      bags.removeAll(bagColorsSet);

      if (bags.isEmpty()) {
        break;
      }
      total += bags.size();
      String allBags = String.join("|", bags);
      pattern = Pattern.compile("(.*)s contain .*(" + allBags + ").*");
      bagColorsSet.addAll(bags);
    }
    System.out.println(total);
  }

  static void solvePart2(ArrayList<String> input) {
    Pattern pattern = Pattern.compile("^(.+) bags contain (.+)\\.$");
    Pattern bagPattern = Pattern.compile("(\\d) (.+) bag[s]?");
    Map<String, Map<String, Integer>> rulesMap = input.stream()
        .map(pattern::matcher)
        .filter(Matcher::find)
        .collect(Collectors.toMap(
            m -> m.group(1),
            m -> Arrays
                .stream(m.group(2).split(", "))
                .map(bagPattern::matcher)
                .filter(Matcher::find)
                .collect(Collectors.toMap(
                    mm -> mm.group(2),
                    mm -> Integer.parseInt(mm.group(1))
                ))
        ));

    System.out.println(countInnerBags(rulesMap, "shiny gold"));
  }

  static int countInnerBags(Map<String, Map<String, Integer>> rulesMap, String color) {
    return rulesMap.get(color).entrySet().stream()
        .map(rule -> countBag(rule, rulesMap))
        .reduce(Integer::sum).orElse(0);
  }

  static int countBag(Map.Entry<String, Integer> rule, Map<String, Map<String, Integer>> rulesMap) {
    int sumInnerBags = countInnerBags(rulesMap, rule.getKey());
    return rule.getValue() + rule.getValue() * sumInnerBags;
  }
}
