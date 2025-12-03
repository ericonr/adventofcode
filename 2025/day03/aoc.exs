defmodule AoC do
  def max_char(line, one_before, pos, current_max) do
    if pos + one_before == String.length(line) do
      current_max
    else
      {_, current_max_value} = current_max
      value = String.to_integer(String.at(line, pos))

      new_max =
        if value > current_max_value do
          {pos, value}
        else
          current_max
        end

      max_char(line, one_before, pos + 1, new_max)
    end
  end

  def max_joltage(line) do
    {first_pos, first_val} = max_char(line, 1, 0, {0, 0})
    {_, second_val} = max_char(line, 0, first_pos + 1, {0, 0})
    10 * first_val + second_val
  end
end

[file_name] = System.argv()
body = String.trim(File.read!(file_name))
lines = String.split(body)

total_max_joltage = Enum.reduce(lines, 0, fn l, s -> s + AoC.max_joltage(l) end)
IO.puts(total_max_joltage)
