import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

enum Seat {FLOOR, EMPTY, OCCUPIED};

public class Day11 {
  public static void main(String[] args) throws IOException {
    List<String> lines = Files.readAllLines(Paths.get("input/day11.txt"));
    Seat[][] map = new Seat[lines.size()][lines.get(0).length()];
    for (int i = 0; i < lines.size(); i++) {
      for (int j = 0; j < lines.get(0).length(); j++) {
        switch (lines.get(i).charAt(j)) {
          case '.': map[i][j] = Seat.FLOOR; break;
          case 'L': map[i][j] = Seat.EMPTY; break;
          case '#': map[i][j] = Seat.OCCUPIED; break;
        }
      }
    }
//    System.out.println(solveDay1(map));
    System.out.println(solveDay2(map));
  }

  static int solveDay1(Seat[][] map) {
    int changes = 0;
    do {
      Seat[][] newMap = copyMap(map);
      changes = 0;
      for (int i = 0; i < map.length; i++) {
        for (int j = 0; j < map[0].length; j++) {
          int adjacentOccupied = countAdjacentOccupied(i, j, map);
          if (map[i][j] == Seat.EMPTY && adjacentOccupied == 0) {
            newMap[i][j] = Seat.OCCUPIED;
            changes++;
          } else if (map[i][j] == Seat.OCCUPIED && adjacentOccupied >= 4) {
            newMap[i][j] = Seat.EMPTY;
            changes++;
          }
        }
      }
      map = newMap;
    } while (changes > 0);
    int occupiedCount = 0;
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[0].length; j++) {
        if (map[i][j] == Seat.OCCUPIED)
          occupiedCount++;
      }
    }
    return occupiedCount;
  }

  static int countAdjacentOccupied(int i, int j, Seat[][] map) {
    int count = 0;
    if (i > 0 && j > 0 && map[i-1][j-1] == Seat.OCCUPIED) count++;
    if (i > 0 && map[i-1][j] == Seat.OCCUPIED) count++;
    if (i > 0 && j < map[0].length - 1 && map[i-1][j+1] == Seat.OCCUPIED) count++;
    if (j < map[0].length - 1 && map[i][j+1] == Seat.OCCUPIED) count++;
    if (i < map.length - 1 && j < map[0].length - 1 && map[i+1][j+1] == Seat.OCCUPIED) count++;
    if (i < map.length - 1 && map[i+1][j] == Seat.OCCUPIED) count++;
    if (i < map.length - 1 && j > 0 && map[i+1][j-1] == Seat.OCCUPIED) count++;
    if (j > 0 && map[i][j-1] == Seat.OCCUPIED) count++;
    return count;
  }

  static int solveDay2(Seat[][] map) {
    int changes = 0;
    do {
      Seat[][] newMap = copyMap(map);
      changes = 0;
      for (int i = 0; i < map.length; i++) {
        for (int j = 0; j < map[0].length; j++) {
          int adjacentOccupied = countAdjacentOccupied2(i, j, map);
          if (map[i][j] == Seat.EMPTY && adjacentOccupied == 0) {
            newMap[i][j] = Seat.OCCUPIED;
            changes++;
          } else if (map[i][j] == Seat.OCCUPIED && adjacentOccupied >= 5) {
            newMap[i][j] = Seat.EMPTY;
            changes++;
          }
        }
      }
      map = newMap;
    } while (changes > 0);
    int occupiedCount = 0;
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[0].length; j++) {
        if (map[i][j] == Seat.OCCUPIED)
          occupiedCount++;
      }
    }
    return occupiedCount;
  }

  static int countAdjacentOccupied2(final int i, final int j, Seat[][] map) {
    int count = 0;
    int di = 1, dj = 1;
    while (i-di >= 0 && j-dj >= 0 && map[i-di][j-dj] == Seat.FLOOR) { di++; dj++; }
    if (i-di >= 0 && j-dj >= 0 && map[i-di][j-dj] == Seat.OCCUPIED) count++;
    di = 1; dj = 1;
    while (i-di > 0 && map[i-di][j] == Seat.FLOOR) { di++; }
    if (i-di >= 0 && map[i-di][j] == Seat.OCCUPIED) count++;
    di = 1; dj = 1;
    while (i-di >= 0 && j+dj <= map[0].length - 1 && map[i-di][j+dj] == Seat.FLOOR) { di++; dj++; }
    if (i-di >= 0 && j+dj <= map[0].length - 1 && map[i-di][j+dj] == Seat.OCCUPIED) count++;
    di = 1; dj = 1;
    while (j+dj <= map[0].length - 1 && map[i][j+dj] == Seat.FLOOR) { dj++; }
    if (j+dj <= map[0].length - 1 && map[i][j+dj] == Seat.OCCUPIED) count++;
    di = 1; dj = 1;
    while (i+di <= map.length - 1 && j+dj <= map[0].length - 1 && map[i+di][j+dj] == Seat.FLOOR) { di++; dj++; }
    if (i+di <= map.length - 1 && j+dj <= map[0].length - 1 && map[i+di][j+dj] == Seat.OCCUPIED) count++;
    di = 1; dj = 1;
    while (i+di <= map.length - 1 && map[i+di][j] == Seat.FLOOR) { di++; }
    if (i+di <= map.length - 1 && map[i+di][j] == Seat.OCCUPIED) count++;
    di = 1; dj = 1;
    while (i+di <= map.length - 1 && j-dj >= 0 && map[i+di][j-dj] == Seat.FLOOR) { di++; dj++; }
    if (i+di <= map.length - 1 && j-dj >= 0 && map[i+di][j-dj] == Seat.OCCUPIED) count++;
    di = 1; dj = 1;
    while (j-dj >= 0 && map[i][j-dj] == Seat.FLOOR) { dj++; }
    if (j-dj >= 0 && map[i][j-dj] == Seat.OCCUPIED) count++;
    return count;
  }

  static void printMap(Seat[][] map) {
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[0].length; j++) {
        switch (map[i][j]) {
          case FLOOR: System.out.print('.'); break;
          case EMPTY: System.out.print('L'); break;
          case OCCUPIED: System.out.print('#'); break;
        }
      }
      System.out.println();
    }
    System.out.println();
  }

  static Seat[][] copyMap(Seat[][] map) {
    Seat[][] newMap = new Seat[map.length][map[0].length];
    for (int i = 0; i < map.length; i++) {
      newMap[i] = Arrays.copyOf(map[i], map[i].length);
    }
    return newMap;
  }
}
