<?php

define("OCCUPIED", "#");
define("SEAT", "L");
define("FLOOR", ".");

function print_ferry($ferry) {
  foreach ($ferry as $row) {
    foreach ($row as $c) {
      echo $c;
    }
    echo "\n";
  }
}

function is_occupied($s) {
  return $s == OCCUPIED;
}

function is_seat($s) {
  return $s == SEAT;
}

function is_floor($s) {
  return $s == FLOOR;
}

function adjacent_seats($ferry, $i, $j) {
  $nrows = count($ferry);
  $ncols = count($ferry[0]);
  $adj_seats = array();
  if ($j > 0) { // Left
    array_push($adj_seats, $ferry[$i][$j-1]);
  }
  if ($j > 0 && $i > 0) { // Upper-left
    array_push($adj_seats, $ferry[$i-1][$j-1]);
  }
  if ($i > 0) { // Up
    array_push($adj_seats, $ferry[$i-1][$j]);
  }
  if ($i > 0 && $j < ($ncols - 1)) { // Upper-right
    array_push($adj_seats, $ferry[$i-1][$j+1]);
  }
  if ($j < ($ncols - 1)) { // Right
    array_push($adj_seats, $ferry[$i][$j+1]);
  }
  if ($i < ($nrows - 1) && $j < ($ncols - 1)) { // Lower-right
    array_push($adj_seats, $ferry[$i+1][$j+1]);
  }
  if ($i < ($nrows - 1)) { // Down
    array_push($adj_seats, $ferry[$i+1][$j]);
  }
  if ($j > 0 && $i < ($nrows - 1)) { // Lower-left
    array_push($adj_seats, $ferry[$i+1][$j-1]);
  }
  return $adj_seats;
}

function n_adj_occupied($ferry, $i, $j) {
  $adj = adjacent_seats($ferry, $i, $j);
  $occ = array_filter($adj, "is_occupied");
  return count($occ);
}

function push_if_nonnull(&$array, $el) {
  // Important to pass array by reference (via '&'), so we can modify it
  if ($el != null) {
    array_push($array, $el);
  }
}

function find_occupied($ferry, $i, $j, $di, $dj) {
  $nrows = count($ferry);
  $ncols = count($ferry[0]);

  $i += $di;
  $j += $dj;
  while ($i >= 0 && $i < $nrows && $j >= 0 && $j < $ncols) {
    $seat = $ferry[$i][$j];
    if (is_seat($seat)) {
      return null; // seat blocking view, so saw no occupied seats in this direction
    } elseif (is_occupied($seat)) {
      return $seat;
    }
    $i += $di;
    $j += $dj;
  }
  return null;
}

function n_visible_occupied($ferry, $i, $j) {
  $visible_occupied = array();
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, -1, 0)); // Left
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, -1, -1)); // Upper-left
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, 0, -1)); // Up
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, -1, 1)); // Upper-right
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, 0, 1)); // Right
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, 1, 1)); // Lower-right
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, 1, 0)); // Down
  push_if_nonnull($visible_occupied, find_occupied($ferry, $i, $j, 1, -1)); // Lower-left
  return count($visible_occupied);
}

function fill_initial_seats($ferry) {
  $nrows = count($ferry);
  $ncols = count($ferry[0]);
  $filled = array();

  for ($i = 0; $i < $nrows; $i++) {
    $row_new = array();
    for ($j = 0; $j < $ncols; $j++) {
      if (is_seat($ferry[$i][$j])) {
         array_push($row_new, OCCUPIED);
      } else {
         array_push($row_new, $ferry[$i][$j]);
      }
    }
    array_push($filled, $row_new);
  }
  return $filled;
}

function part1($ferry) {
  $nseats = find_fixpoint($ferry,
        function($ferry, $i, $j) {
          return n_adj_occupied($ferry, $i, $j) == 0;
        },
        function ($ferry, $i, $j) {
          return n_adj_occupied($ferry, $i, $j) >= 4;
        });
  echo $nseats, "\n";
}

function part2($ferry) {
  $nseats = find_fixpoint($ferry,
        function($ferry, $i, $j) {
          return n_visible_occupied($ferry, $i, $j) == 0;
        },
        function ($ferry, $i, $j) {
          return n_visible_occupied($ferry, $i, $j) >= 5;
        });
  echo $nseats, "\n";
}

function find_fixpoint($ferry, $occupy_f, $leave_f) {
  $nrows = count($ferry);
  $ncols = count($ferry[0]);

  // Fill all seats
  $next = fill_initial_seats($ferry);

  // Update until a fixpoint
  while ($ferry != $next) {
    $ferry = $next;
    $next = array();
    for ($i = 0; $i < $nrows; $i++) {
      $row_new = array();
      for ($j = 0; $j < $ncols; $j++) {
        $seat = $ferry[$i][$j];
        if (is_seat($seat)) {
          if ($occupy_f($ferry, $i, $j)) {
            array_push($row_new, OCCUPIED);
          } else {
            array_push($row_new, SEAT);
          }
        } elseif (is_occupied($seat)) {
          if ($leave_f($ferry, $i, $j)) {
            array_push($row_new, SEAT);
          } else {
            array_push($row_new, OCCUPIED);
          }
        } else {
          array_push($row_new, FLOOR);
        }
      }
      array_push($next, $row_new);
    }
  }

  // Count number of occupied seats
  $n_occupied = 0;
  foreach ($ferry as $row) {
    foreach ($row as $c) {
      if (is_occupied($c)) {
        $n_occupied += 1;
      }
    }
  }
  return $n_occupied;
}

function main() {
  $handle = fopen("input", "r");
  $ferry = array();
  while (($line = fgets($handle)) !== false) {
    $row = str_split(trim($line));
    array_push($ferry, $row); 
  }
  fclose($handle);

  part1($ferry);
  part2($ferry);

  exit(0);
}

main();
?>
