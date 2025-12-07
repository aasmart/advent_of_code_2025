Pos = Struct.new(:row, :col)

def read_input(filename)
  File.readlines(filename)
end

lines = read_input(ARGV[0])

def part1(lines)
  beam_cols = Set.new

  total_split = 0

  beam_cols << lines[0].index('S')

  for beam_row in 0..(lines.length - 1) do
    split_at = Set.new
    next_beam_cols = Set.new

    for beam_col in beam_cols do
      if lines[beam_row][beam_col] != '^'
        next_beam_cols << beam_col
        next
      end

      split_at << beam_col

      next_beam_cols << beam_col - 1 if beam_col - 1 >= 0
      next_beam_cols << beam_col + 1 if beam_col + 1 < lines[0].length
    end

    beam_cols = next_beam_cols
    total_split += split_at.length
  end

  total_split
end

puts part1(lines)
