defmodule AoC do
  def perform_moves(zcount, position, [head | tail]) do
    IO.puts("zeroes #{zcount} position #{position} move #{head}")

    new_position_sign = rem(position + head, 100)

    new_position =
      cond do
        new_position_sign < 0 -> new_position_sign + 100
        new_position_sign >= 0 -> new_position_sign
      end

    change_zcount =
      case new_position do
        0 -> 1
        _ -> 0
      end

    perform_moves(zcount + change_zcount, new_position, tail)
  end

  def perform_moves(zcount, position, []) do
    IO.puts("zeroes #{zcount} position #{position} nomoves")
    {zcount, position}
  end
end

[file_name] = System.argv()
body = File.read!(file_name)
lines = String.split(body)

moves =
  Enum.map(lines, fn s ->
    String.to_integer(String.slice(s, 1..-1//1)) *
      case String.at(s, 0) do
        "R" ->
          1

        "L" ->
          -1
      end
  end)

start = 50

{zcount, _} = AoC.perform_moves(0, start, moves)
IO.puts(zcount)
