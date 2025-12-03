require Integer

defmodule AoC do
  def check_id_1?(id_n) do
    id = "#{id_n}"
    len = String.length(id)

    Integer.is_even(len) and
      String.slice(id, 0, div(len, 2)) == String.slice(id, div(len, 2), len)
  end

  def check_string?(remaining, len) do
    if String.length(remaining) == len do
      true
    else
      particle = String.slice(remaining, 0, len)
      new_remaining = String.slice(remaining, len..-1//1)
      particle == String.slice(new_remaining, 0, len) and check_string?(new_remaining, len)
    end
  end

  def check_id_2?(id_n) do
    id = "#{id_n}"
    len = String.length(id)

    divisors = for n <- 1..div(len, 2)//1, rem(len, n) == 0, do: n
    Enum.any?(Enum.map(divisors, fn d -> check_string?(id, d) end))
  end

  def sum_id(id, sum, checkfn) do
    if checkfn.(id) do
      sum + id
    else
      sum
    end
  end

  def sum_id_1(id, sum) do
    sum_id(id, sum, &check_id_1?/1)
  end

  def sum_id_2(id, sum) do
    sum_id(id, sum, &check_id_2?/1)
  end

  def scan_range(sum, [head | tail], sumfn) do
    ids = Enum.map(String.split(head, "-"), &String.to_integer/1)
    range = hd(ids)..hd(tl(ids))
    new_sum = Enum.reduce(range, 0, sumfn) + sum
    scan_range(new_sum, tail, sumfn)
  end

  def scan_range(sum, [], _sumfn) do
    sum
  end
end

[file_name] = System.argv()
body = String.trim(File.read!(file_name))
ranges = String.split(body, ",")

IO.puts(AoC.scan_range(0, ranges, &AoC.sum_id_1/2))
IO.puts(AoC.scan_range(0, ranges, &AoC.sum_id_2/2))
