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

def part2(lines)
  beams_in_cols = Hash.new(0)

  beams_in_cols[lines[0].index('S')] = 1

  for beam_row in 1..(lines.length - 1) do
    next_beams_in_cols = Hash.new(0)
    beams_in_cols.each do |beam_col, num_beams|
      if lines[beam_row][beam_col] != '^'
        next_beams_in_cols[beam_col] += num_beams
        next
      end

      next_beams_in_cols[beam_col - 1] += num_beams if beam_col - 1 >= 0
      next_beams_in_cols[beam_col + 1] += num_beams if beam_col + 1 < lines[0].length
    end

    beams_in_cols = next_beams_in_cols
  end
  beams_in_cols.values.reduce(0) { |sum, hash| sum + hash }
end

puts part1(lines)
puts part2(lines)
