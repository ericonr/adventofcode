defmodule AoC do
  def parse_math([head], values) do
    mult = fn a, b -> a * b end
    sum = fn a, b -> a + b end

    ops =
      Enum.map(String.split(head, " ", trim: true), fn c ->
        case c do
          "*" -> mult
          "+" -> sum
        end
      end)

    {values, ops}
  end

  def parse_math([head | tail], values) do
    values = [Enum.map(String.split(head, " ", trim: true), &String.to_integer/1) | values]
    parse_math(tail, values)
  end

  def apply_ops(values, ops) do
    Enum.zip_with(Enum.zip(values), ops, fn vs, op -> Enum.reduce(Tuple.to_list(vs), op) end)
  end

  def part_1_result(results) do
    Enum.sum(results)
  end
end

[file_name] = System.argv()
body = String.trim(File.read!(file_name))
lines = String.split(body, "\n")

{values, ops} = AoC.parse_math(lines, [])
IO.inspect(AoC.part_1_result(AoC.apply_ops(values, ops)))
