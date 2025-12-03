require Integer

defmodule AoC do
  def check_id(id_n) do
    id = "#{id_n}"
    len = String.length(id)

    Integer.is_even(len) and
      String.slice(id, 0, div(len, 2)) == String.slice(id, div(len, 2), len)
  end

  def sum_id(id, sum) do
    if check_id(id) do
      sum + id
    else
      sum
    end
  end

  def scan_range(sum, [head | tail]) do
    ids = Enum.map(String.split(head, "-"), &String.to_integer/1)
    range = hd(ids)..hd(tl(ids))
    new_sum = Enum.reduce(range, 0, &sum_id/2) + sum
    scan_range(new_sum, tail)
  end

  def scan_range(sum, []) do
    sum
  end
end

[file_name] = System.argv()
body = String.trim(File.read!(file_name))
ranges = String.split(body, ",")

sum = AoC.scan_range(0, ranges)
IO.puts(sum)
