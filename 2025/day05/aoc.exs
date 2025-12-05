defmodule AoC do
  def count_nums_in_range(nums, ranges) do
    Enum.count(nums, fn n -> Enum.any?(ranges, fn r -> n in r end) end)
  end
end

[file_name] = System.argv()
body = String.trim(File.read!(file_name))
parts = String.split(body, "\n\n")

fresh_ranges =
  Enum.map(String.split(hd(parts)), fn l ->
    Enum.map(String.split(l, "-"), &String.to_integer/1)
  end)

fresh_ranges = for [b, e] <- fresh_ranges, do: b..e
ingredients = for n <- String.split(hd(tl(parts))), do: String.to_integer(n)

IO.inspect(AoC.count_nums_in_range(ingredients, fresh_ranges))
