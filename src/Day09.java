import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.LongSummaryStatistics;

public class Day09 {
  static final int PREAMBLE = 25;

  public static void main(String[] args) throws Exception {
    List<String> lines = Files.readAllLines(Paths.get("input/day09.txt"));
    long[] input = lines.stream().mapToLong(Long::parseLong).toArray();
    long target = solve1(input);
    solve2(input,target);
  }

  static long solve1(long[] input) {
    for (int i = PREAMBLE; i < input.length; i++) {
      if (!valid(input[i], i, input)) {
        System.out.println(input[i]);
        return input[i];
      }
    }
    return -1;
  }

  static void solve2(long[] input, final long target) {
    for (int i = 0; i < input.length; i++) {
      long slidingSum = input[i];
      for (int j = i+1; j < input.length; j++) {
        if (target == slidingSum + input[j]) {
          long[] range = Arrays.copyOfRange(input, i, j);
          LongSummaryStatistics stats = Arrays.stream(range).summaryStatistics();
          System.out.println(stats.getMin() + stats.getMax());
          return;
        }
        slidingSum += input[j];
      }
    }
  }

  static boolean valid(long n, int k, long[] input) {
    for (int i = k-PREAMBLE; i < k; i++) {
      for (int j = i+1; j < k; j++) {
        if (input[i] + input[j] == n)
          return true;
      }
    }
    return false;
  }

  static void test1_basic() {
    long[] test1 = new long[26];
    for (int i = 0; i < 25; i++) {
      test1[i] = i+1;
    }
    test1[25] = 26;
    solve1(test1);
    test1[25] = 49;
    solve1(test1);
    test1[25] = 100;
    solve1(test1);
    test1[25] = 50;
    solve1(test1);
    long[] test2 = new long[27];
    for (int i = 0; i < 26; i++) {
      test2[i] = i+1;
    }
    test2[0] = 20;
    test2[19] = 0;
    test2[25] = 45;
    test2[26] = 26;
    solve1(test2);
    test2[26] = 65;
    solve1(test2);
    test2[26] = 64;
    solve1(test2);
    test2[26] = 66;
    solve1(test2);
  }
}
