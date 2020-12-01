import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class Day01 {

    public static void main(String[] args) {
        Scanner scanner = null;
        try {
            scanner = new Scanner(new File("input/day01.txt"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        ArrayList<Integer> numbers = new ArrayList<Integer>();
        while (scanner.hasNextInt()) {
            numbers.add(scanner.nextInt());
        }
        boolean keepGoing = true;
        for (int i = 0; keepGoing && i < numbers.size(); i++) {
            for (int j = 0; keepGoing && j < numbers.size(); j++) {
                if (numbers.get(i) + numbers.get(j) == 2020) {
                    System.out.println(numbers.get(i) * numbers.get(j));
                    keepGoing = false;
                    break;
                }
            }
        }
        keepGoing = true;
        for (int i = 0; keepGoing && i < numbers.size(); i++) {
            for (int j = 0; keepGoing && j < numbers.size(); j++) {
                for (int k = 0; keepGoing && k < numbers.size(); k++) {
                    if (numbers.get(i) + numbers.get(j) + numbers.get(k) == 2020) {
                        System.out.println(numbers.get(i) * numbers.get(j) * numbers.get(k));
                        keepGoing = false;
                        break;
                    }
                }
            }
        }
    }
}
