defmodule AoC do
  def count_nums_in_range(nums, ranges) do
    Enum.count(nums, fn n -> Enum.any?(ranges, fn r -> n in r end) end)
  end

  defp join_ranges(r1, r2) do
    get_start_end = fn %Range{first: s, last: e} -> {s, e} end
    {r1s, r1e} = get_start_end.(r1)
    {r2s, r2e} = get_start_end.(r2)
    min(r1s, r2s)..max(r1e, r2e)
  end

  defp scan_range(element, ranges) do
    disjoint? = fn r -> Range.disjoint?(element, r) end
    # joints will receive a range with element in it, which simplifies the logic for callers
    {disjoints, joints} = Enum.split_with(ranges, disjoint?)

    if Enum.empty?(joints) do
      [element | disjoints]
    else
      scan_range(Enum.reduce(joints, element, &join_ranges/2), disjoints)
    end
  end

  defp accumulate_ranges(ranges, idx) do
    ranges = scan_range(Enum.at(ranges, idx), ranges)

    if idx + 1 >= Enum.count(ranges) do
      ranges
    else
      accumulate_ranges(ranges, idx + 1)
    end
  end

  def total_elements_ranges(ranges) do
    ranges = accumulate_ranges(ranges, 0)
    Enum.sum(Enum.map(ranges, &Enum.count/1))
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
IO.inspect(AoC.total_elements_ranges(fresh_ranges))
