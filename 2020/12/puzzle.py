from enum import IntEnum
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

def turns(n):
    # I would replace with a floordivision if I could.
    # I would also just use a conditional_assignment with block,
    # but currently PyRTL doesn't support nested conditionals.
    w = pyrtl.WireVector(bitwidth(len(Direction)))
    w <<= pyrtl.select(n == 90, 1, pyrtl.select(n == 180, 2, pyrtl.select(n == 270, 3, 0)))
    return w


LARGEST_VALUE = 10000  # NOTE: making sure this is large enough was important during debugging (was getting wrong answer because not enough room)
MAX_INSTRUCTIONS = 1000

CMD_BITWIDTH = bitwidth(len(Command) - 1)
DIR_BITWIDTH = bitwidth(len(Direction) - 1)
VAL_BITWIDTH = bitwidth(LARGEST_VALUE)
INSTR_BITWIDTH = bitwidth(MAX_INSTRUCTIONS)

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

def as_twos_compl(v, width):
    if format(v, f"0{width}b")[0] == '1':
        return v - (1 << width)
    else:
        return v

def lat_to_str(l):
    v = as_twos_compl(l, VAL_BITWIDTH)
    if v < 0:
        return f"{abs(v)}{Direction.SOUTH}"
    else:
        return f"{v}{Direction.NORTH}"

def long_to_str(l):
    v = as_twos_compl(l, VAL_BITWIDTH)
    if v < 0:
        return f"{abs(v)}{Direction.WEST}"
    else:
        return f"{v}{Direction.EAST}"

def manhattan_distance(lat, long):
    return abs(as_twos_compl(lat, VAL_BITWIDTH)) + abs(as_twos_compl(long, VAL_BITWIDTH))

def import_instructions(filename):
    with open(filename, "r") as f:
        arr = f.readlines()
        return {i: str_to_inst(s) for i, s in enumerate(arr)} 

if __name__ == "__main__":
    sim = pyrtl.Simulation(
        register_value_map={
            facing: Direction.EAST
        },
        memory_value_map={
          im: import_instructions("input")
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

    # Part 1
    print(manhattan_distance(lat, long))