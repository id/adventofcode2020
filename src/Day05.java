import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.PriorityQueue;
import java.util.Scanner;

public class Day05 {
  public static void main(String[] args) {
    Scanner scanner = null;
    try {
      scanner = new Scanner(new File("input/day05.txt"));
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }

    ArrayList<Integer> seats = new ArrayList<>();
    PriorityQueue<Integer> pq = new PriorityQueue<>(Collections.reverseOrder());
    while (scanner.hasNextLine()) {
      int seatId = parseBoardingPass(scanner.nextLine());
      pq.add(seatId);
      seats.add(seatId);
    }
    System.out.println(pq.peek());

    seats.sort(null);
    int currentSeatId = seats.get(0);
    for (int i = 1; i < seats.size(); i++) {
      if (seats.get(i) != currentSeatId+1)
        System.out.println(currentSeatId+1);
      currentSeatId = seats.get(i);
    }
  }

  static final int ROWS_HIGH = 128;
  static final int COLUMNS_HIGH = 8;

  static int parseBoardingPass(String pass) {
    int low = 0, high = ROWS_HIGH;
    for (int i = 0; i < 7; i++) {
      if (pass.charAt(i) == 'F')
        high = (low + high) / 2;
      else if (pass.charAt(i) == 'B')
        low = (low + high) / 2;
    }
    int row = pass.charAt(6) == 'F' ? low : (high-1);

    low = 0;
    high = COLUMNS_HIGH;
    for (int i = 7; i < 10; i++) {
      if (pass.charAt(i) == 'L')
        high = (low + high) / 2;
      else if (pass.charAt(i) == 'R')
        low = (low + high) / 2;
    }
    int column = pass.charAt(9) == 'L' ? low : (high-1);
    return row*8 + column;
  }
}
