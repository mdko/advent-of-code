from enum import IntEnum
import unittest
import pyrtl

# The order of these is actually important,
# because adding 1 to each is turning to the right.
# We rely on this when turning right (adding, i.e. going clockwise)
# or turning left (subtracting), and wrapping around if needed.
class Direction(IntEnum):
    NORTH = 0b00
    EAST = 0b01
    SOUTH = 0b10
    WEST = 0b11

    def __str__(self):
        return self.name[0]

class Command(IntEnum):
    NORTH = 0b000
    EAST = 0b001
    SOUTH = 0b010
    WEST = 0b011
    LEFT = 0b100
    RIGHT = 0b101  # Turn right
    FORWARD = 0b110  # Turn left
    HALT = 0b111

def bitwidth(n):
    return pyrtl.as_wires(n).bitwidth

LARGEST_VALUE = 10000  # NOTE: making sure this is large enough was important during debugging (was getting wrong answer because not enough room)
MAX_INSTRUCTIONS = 1000

CMD_BITWIDTH = bitwidth(len(Command) - 1)
DIR_BITWIDTH = bitwidth(len(Direction) - 1)
VAL_BITWIDTH = bitwidth(LARGEST_VALUE)
INSTR_BITWIDTH = bitwidth(MAX_INSTRUCTIONS)

def turns(n):
    # I would replace with a floordivision if I could.
    # I would also just use a conditional_assignment with block,
    # but currently PyRTL doesn't support nested conditionals.
    w = pyrtl.WireVector(bitwidth(len(Direction)))
    w <<= pyrtl.select(n == 90, 1, pyrtl.select(n == 180, 2, pyrtl.select(n == 270, 3, 0)))
    return w

def str_to_inst(s):
    op, value = s[0], int(s[1:])
    op_map = {
        "N": Command.NORTH,
        "E": Command.EAST,
        "S": Command.SOUTH,
        "W": Command.WEST,
        "L": Command.LEFT,
        "R": Command.RIGHT,
        "F": Command.FORWARD,
        "H": Command.HALT,
    }
    return (op_map[op] << VAL_BITWIDTH) + value

def rotate(longitude, latitude, degrees, left):
    """
        The idea behind this as follows.
        Given a latitude of 'a' and a longitue of 'b', when you rotate left by 90 degrees,
        this is how your latitude and longitude change:

        Going left:
            Start #1   #2   #3   #4 (original)
        Long: b   -a   -b    a    b
        Lat:  a    b   -a   -b    a

        Going right:
            Start #1   #2   #3   #4 (original)
        Long: b    a   -b   -a    b
        Lat:  a   -b   -a    b    a
    """
    new_long = pyrtl.WireVector(len(longitude))
    new_lat = pyrtl.WireVector(len(latitude))
    t = turns(degrees)
    with pyrtl.conditional_assignment:
        with left:
            with t == 0:
                new_long |= longitude
                new_lat |= latitude
            with t == 1:
                new_long |= 0 - latitude
                new_lat |= longitude
            with t == 2:
                new_long |= 0 - longitude
                new_lat |= 0 - latitude
            with t == 3:
                new_long |= latitude
                new_lat |= 0 - longitude
        with pyrtl.otherwise:
            with t == 0:
                new_long |= longitude
                new_lat |= latitude
            with t == 1:
                new_long |= latitude
                new_lat |= 0 - longitude
            with t == 2:
                new_long |= 0 - longitude
                new_lat |= 0 - latitude
            with t == 3:
                new_long |= 0 - latitude
                new_lat |= longitude
    return new_long, new_lat

def lat_to_str(l):
    v = pyrtl.val_to_signed_integer(l, VAL_BITWIDTH)
    if v < 0:
        return f"{abs(v)}{Direction.SOUTH}"
    else:
        return f"{v}{Direction.NORTH}"

def long_to_str(l):
    v = pyrtl.val_to_signed_integer(l, VAL_BITWIDTH)
    if v < 0:
        return f"{abs(v)}{Direction.WEST}"
    else:
        return f"{v}{Direction.EAST}"

def manhattan_distance(lat, long):
    return abs(pyrtl.val_to_signed_integer(lat, VAL_BITWIDTH)) + abs(pyrtl.val_to_signed_integer(long, VAL_BITWIDTH))

def import_instructions(filename):
    with open(filename, "r") as f:
        arr = f.readlines()
        m = {i: str_to_inst(s) for i, s in enumerate(arr)} 
        assert m[len(arr) - 1] == str_to_inst("H0")
        return m

def part1(ins_map):
    # State
    im = pyrtl.MemBlock(bitwidth=CMD_BITWIDTH + VAL_BITWIDTH, addrwidth=INSTR_BITWIDTH, name='im')
    pc = pyrtl.Register(INSTR_BITWIDTH, 'pc')
    facing = pyrtl.Register(DIR_BITWIDTH, 'facing')
    longitude = pyrtl.Register(VAL_BITWIDTH, 'longitude')
    latitude = pyrtl.Register(VAL_BITWIDTH, 'latitude')

    # Combinational logic
    instruction = im[pc]
    cmd, value = pyrtl.chop(instruction, CMD_BITWIDTH, VAL_BITWIDTH)

    # Sequential logic:
    with pyrtl.conditional_assignment:
        with (cmd == Command.NORTH) | ((cmd == Command.FORWARD) & (facing == Direction.NORTH)):
            latitude.next |= latitude + value
        with (cmd == Command.SOUTH) | ((cmd == Command.FORWARD) & (facing == Direction.SOUTH)):
            latitude.next |= latitude - value
        with (cmd == Command.EAST) | ((cmd == Command.FORWARD) & (facing == Direction.EAST)):
            longitude.next |= longitude + value
        with (cmd == Command.WEST) | ((cmd == Command.FORWARD) & (facing == Direction.WEST)):
            longitude.next |= longitude - value
        with cmd == Command.RIGHT:
            facing.next |= facing + turns(value)
        with cmd == Command.LEFT:
            facing.next |= facing - turns(value)

    pc.next <<= pyrtl.select(cmd == Command.HALT, pc, pc + 1)

    # Simulate it!
    sim = pyrtl.Simulation(
        register_value_map={
            facing: Direction.EAST,
        },
        memory_value_map={
          im: ins_map,
        }
    )
    while sim.inspect(cmd) != Command.HALT:
        sim.step({})

    # fv = sim.inspect(facing)
    lat = sim.inspect(latitude)
    long = sim.inspect(longitude)
    # print(f"Facing: {Direction(fv)}")
    # print(f"Latitude: {lat_to_str(lat)}")
    # print(f"Longitude: {long_to_str(long)}")
    # sim.tracer.render_trace()

    print(manhattan_distance(lat, long))

def part2_machine():
    # State
    im = pyrtl.MemBlock(bitwidth=CMD_BITWIDTH + VAL_BITWIDTH, addrwidth=INSTR_BITWIDTH, name='im')
    pc = pyrtl.Register(INSTR_BITWIDTH, 'pc')
    ship_longitude = pyrtl.Register(VAL_BITWIDTH, 'ship_longitude')
    ship_latitude = pyrtl.Register(VAL_BITWIDTH, 'ship_latitude')
    waypoint_rel_longitude = pyrtl.Register(VAL_BITWIDTH, 'waypoint_relative_longitude')
    waypoint_rel_latitude = pyrtl.Register(VAL_BITWIDTH, 'waypoint_relative_latitude')

    # Combinational logic
    instruction = im[pc]
    cmd, value = pyrtl.chop(instruction, CMD_BITWIDTH, VAL_BITWIDTH)
    rotate_res = rotate(waypoint_rel_longitude, waypoint_rel_latitude, value, cmd == Command.LEFT)

    # Sequential logic:
    with pyrtl.conditional_assignment:
        with (cmd == Command.NORTH):
            waypoint_rel_latitude.next |= waypoint_rel_latitude + value
        with (cmd == Command.SOUTH):
            waypoint_rel_latitude.next |= waypoint_rel_latitude - value
        with (cmd == Command.EAST):
            waypoint_rel_longitude.next |= waypoint_rel_longitude + value
        with (cmd == Command.WEST):
            waypoint_rel_longitude.next |= waypoint_rel_longitude - value
        with cmd == Command.RIGHT:
            waypoint_rel_longitude.next |= rotate_res[0]
            waypoint_rel_latitude.next |= rotate_res[1]
        with cmd == Command.LEFT:
            waypoint_rel_longitude.next |= rotate_res[0]
            waypoint_rel_latitude.next |= rotate_res[1]
        with cmd == Command.FORWARD:
            ship_longitude.next |= ship_longitude + (waypoint_rel_longitude * value)
            ship_latitude.next |= ship_latitude + (waypoint_rel_latitude * value)

    pc.next <<= pyrtl.select(cmd == Command.HALT, pc, pc + 1)

    return (cmd == Command.HALT)

def part2(ins_map):
    done = part2_machine()
    block = pyrtl.working_block()
    waypoint_rel_longitude = block.get_wirevector_by_name('waypoint_relative_longitude')
    waypoint_rel_latitude = block.get_wirevector_by_name('waypoint_relative_latitude')
    ship_latitude = block.get_wirevector_by_name('ship_latitude')
    ship_longitude = block.get_wirevector_by_name('ship_longitude')
    im = block.get_memblock_by_name('im')

    # Simulate it!
    sim = pyrtl.Simulation(
        register_value_map={
            waypoint_rel_longitude: 10,
            waypoint_rel_latitude: 1,
        },
        memory_value_map={
          im: ins_map,
        }
    )
    while not sim.inspect(done):
        sim.step({})

    lat = sim.inspect(ship_latitude)
    long = sim.inspect(ship_longitude)
    # print(f"Facing: {Direction(fv)}")
    # print(f"Latitude: {lat_to_str(lat)}")
    # print(f"Longitude: {long_to_str(long)}")
    # sim.tracer.render_trace()

    print(manhattan_distance(lat, long))

class TestRotate(unittest.TestCase):
    def setUp(self):
        pyrtl.reset_working_block()
        block = pyrtl.working_block()
        self.done = part2_machine()
        self.waypoint_rel_longitude = block.get_wirevector_by_name('waypoint_relative_longitude')
        self.waypoint_rel_latitude = block.get_wirevector_by_name('waypoint_relative_latitude')
        self.ship_latitude = block.get_wirevector_by_name('ship_latitude')
        self.ship_longitude = block.get_wirevector_by_name('ship_longitude')
        self.im = block.get_memblock_by_name('im')
    
    def neg_val(self, sim, wire):
        return pyrtl.val_to_signed_integer(sim.inspect(wire), VAL_BITWIDTH)
    
    def _run(self, *ins):
        self.sim = pyrtl.Simulation(
            register_value_map={
                self.waypoint_rel_longitude: 10,
                self.waypoint_rel_latitude: 1,
            },
            memory_value_map={
                self.im: {i: str_to_inst(s) for i, s in enumerate(ins)} 
            }
        )
        while not self.sim.inspect(self.done):
            self.sim.step({})

    def test_rotate_left_0(self):
        self._run(
            "L0",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), 10)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), 1)
    
    def test_rotate_left_90(self):
        self._run(
            "L90",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), -1)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), 10)

    def test_rotate_left_180(self):
        self._run(
            "L180",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), -10)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), -1)

    def test_rotate_left_270(self):
        self._run(
            "L270",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), 1)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), -10)

    def test_rotate_right_0(self):
        self._run(
            "R0",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), 10)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), 1)
    
    def test_rotate_right_90(self):
        self._run(
            "R90",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), 1)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), -10)

    def test_rotate_right_180(self):
        self._run(
            "R180",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), -10)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), -1)

    def test_rotate_right_270(self):
        self._run(
            "R270",
            "H0",
        )
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_longitude), -1)
        self.assertEqual(self.neg_val(self.sim, self.waypoint_rel_latitude), 10)
    
    # TODO insert tests for the other instructions

if __name__ == "__main__":
    ins_map = import_instructions("example-input")
    part1(ins_map)
    pyrtl.reset_working_block()
    part2(ins_map)