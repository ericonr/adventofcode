defmodule AoC do
  def visit_column(map, nrow, ncolumn, [head | tail]) do
    visit_column(Map.put(map, {nrow, ncolumn}, head), nrow, ncolumn + 1, tail)
  end

  def visit_column(map, _nrow, _ncolumn, []) do
    map
  end

  def visit_row(map, nrow, [head | tail]) do
    visit_row(visit_column(map, nrow, 0, head), nrow + 1, tail)
  end

  def visit_row(map, _nrow, []) do
    map
  end

  def create_2d(rows) do
    {visit_row(%{}, 0, rows), length(rows), length(hd(rows))}
  end

  def points_to_check({i, j}) do
    for di <- -1..1, dj <- -1..1, not (di == 0 and dj == 0), do: {i + di, j + dj}
  end

  def adjacent(map, n_adjacent, point) do
    # we define our own sentinel for out of bounds as ","
    check_char = fn c ->
      case c do
        "@" -> 1
        "." -> 0
        "," -> 0
      end
    end

    n =
      Enum.reduce(points_to_check(point), 0, fn p, s -> s + check_char.(Map.get(map, p, ",")) end)

    n < n_adjacent
  end

  def get_paper_rolls(map, nrows, ncolumns) do
    for i <- 0..(nrows - 1), j <- 0..(ncolumns - 1), Map.get(map, {i, j}) == "@", do: {i, j}
  end

  def forklift({map, nrows, ncolumns}) do
    points = get_paper_rolls(map, nrows, ncolumns)
    Enum.count(Enum.filter(points, fn p -> adjacent(map, 4, p) end))
  end

  def forklift_with_removal({map, nrows, ncolumns}, total_removed \\ 0) do
    points = get_paper_rolls(map, nrows, ncolumns)
    removals = Enum.filter(points, fn p -> adjacent(map, 4, p) end)
    nremovals = Enum.count(removals)

    if nremovals > 0 do
      new_map = Enum.reduce(removals, map, fn p, m -> Map.put(m, p, ".") end)
      forklift_with_removal({new_map, nrows, ncolumns}, total_removed + nremovals)
    else
      total_removed
    end
  end
end

[file_name] = System.argv()
body = String.trim(File.read!(file_name))
rows = Enum.map(String.split(body), &String.codepoints/1)

map_props = AoC.create_2d(rows)
IO.puts(AoC.forklift(map_props))
IO.puts(AoC.forklift_with_removal(map_props))
