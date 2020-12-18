import java.io.File;
import java.io.FileNotFoundException; 
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.HashMap;

public class Puzzle {
    public static void part1(List<Integer> spoken_in) {
        int MAX_TURNS = 2020;
        List<Integer> spoken = new ArrayList<Integer>(spoken_in);
        int curr_turn = spoken.size();
        while (curr_turn < MAX_TURNS) {
            int last_spoken = spoken.get(curr_turn - 1);
            int turn_before_last = spoken.subList(0, curr_turn - 1).lastIndexOf(last_spoken);
            if (turn_before_last == -1) {
                spoken.add(0);
            } else {
                spoken.add(curr_turn - 1 - turn_before_last);
            }
            curr_turn += 1;
        }
        System.out.println(spoken.get(spoken.size() - 1));
    }

    /*
    Number => Turn where it was mentioned before the last turn (0 means new)
    0 => 0
    3 => 0
    6 => 0
    */

    // Arg, why am I sucking at this.
    public static void part2(List<Integer> spoken) {
        /* Like Part 1, but smarter to handle a huge request */
        // For each number, keep track of when it was spoken last,
        // rather than a list and having to search for indices.
        int MAX_TURNS = 30000000;
        HashMap<Integer, Integer> spoken_map = new HashMap<Integer, Integer>();
        for (int i = 0; i < spoken.size(); i++) {
            spoken_map.put(spoken.get(i), i);
        }
        int curr_turn = spoken.size();
        int spoken_prev = spoken.get(curr_turn - 1);
        int turn_spoken_prev = -1;

        while (curr_turn < MAX_TURNS) {
            if (turn_spoken_prev == -1) {
                // Last turn was the first time saying that number
                spoken_prev = 0;
            } else {
                // Seen it previously, so calculuate age
                spoken_prev = (curr_turn - 1) - turn_spoken_prev;
            }
            if (spoken_map.containsKey(spoken_prev)) {
                turn_spoken_prev = spoken_map.get(spoken_prev);
            } else {
                // This is the first time saying 'spoken_prev', so remember it for next turn.
                turn_spoken_prev = -1;
            }
            spoken_map.put(spoken_prev, curr_turn);
            curr_turn += 1;
        }
        System.out.println(spoken_prev);
    }

    public static void main(String[] args) {
        try {
            Scanner reader = new Scanner(new File("input"));
            List<Integer> starting = Arrays.asList(reader.nextLine().split(",")).
                                        stream().map(Integer::parseInt).collect(Collectors.toList());
            part1(starting);
            part2(starting);
            reader.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}