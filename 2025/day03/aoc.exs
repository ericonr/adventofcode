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

  def max_joltage(line, sum, starting_pos, n_batteries) when n_batteries > 0 do
    {new_pos, new_value} = max_char(line, n_batteries - 1, starting_pos, {0, 0})

    max_joltage(
      line,
      sum + new_value * 10 ** (n_batteries - 1),
      new_pos + 1,
      n_batteries - 1
    )
  end

  def max_joltage(_line, sum, _starting_pos, 0) do
    sum
  end

  def total_joltage(lines, n_batteries) do
    Enum.reduce(lines, 0, fn l, s -> s + AoC.max_joltage(l, 0, 0, n_batteries) end)
  end
end

[file_name] = System.argv()
body = String.trim(File.read!(file_name))
lines = String.split(body)

IO.puts(AoC.total_joltage(lines, 2))
IO.puts(AoC.total_joltage(lines, 12))
