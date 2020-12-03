import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class Day03 {
  static String[] testMap = {
      "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
  };

  public static void main(String[] args) {
    Scanner scanner = null;
    try {
      scanner = new Scanner(new File("input/day03.txt"));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }

    ArrayList<String> map = new ArrayList<>();
    while (scanner.hasNextLine()) {
      map.add(scanner.nextLine());
    }

    solve(testMap, 1, 1);
    solve(testMap, 3, 1);
    solve(testMap, 5, 1);
    solve(testMap, 7, 1);
    solve(testMap, 1, 2);
    solve(map.toArray(new String[map.size()]), 1, 1);
    solve(map.toArray(new String[map.size()]), 3, 1);
    solve(map.toArray(new String[map.size()]), 5, 1);
    solve(map.toArray(new String[map.size()]), 7, 1);
    solve(map.toArray(new String[map.size()]), 1, 2);
  }

  private static void solve(String[] map, int right, int down) {
    int rowLength = map[0].length();
    int treesFound = 0;
    int currentPos = right;
    for (int i = down; i < map.length; i += down) {
      if (map[i].charAt(currentPos) == '#') {
        treesFound++;
      }
      if (currentPos + right >= rowLength) {
        currentPos = right - (rowLength - currentPos);
      } else {
        currentPos += right;
      }
    }
    System.out.println(treesFound);
  }
}
