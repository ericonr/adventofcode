defmodule AoC do
  def position_is_zero(position) do
    case position do
      0 -> 1
      _ -> 0
    end
  end

  def move_1b1(zcount_method, position, move, sign) when move > 0 do
    new_position_sign = rem(position + sign, 100)

    new_position =
      cond do
        new_position_sign < 0 -> new_position_sign + 100
        new_position_sign >= 0 -> new_position_sign
      end

    move_1b1(zcount_method + position_is_zero(new_position), new_position, move - 1, sign)
  end

  def move_1b1(zcount_method, position, 0, _sign) do
    {zcount_method, position}
  end

  def perform_moves(zcount, zcount_method, position, [head | tail]) do
    IO.puts("zeroes #{zcount} position #{position} move #{head}")

    {new_zcount_method, new_position} =
      move_1b1(
        zcount_method,
        position,
        abs(head),
        if head >= 0 do
          1
        else
          -1
        end
      )

    perform_moves(zcount + position_is_zero(new_position), new_zcount_method, new_position, tail)
  end

  def perform_moves(zcount, zcount_method, position, []) do
    IO.puts("zeroes #{zcount} zeroes_method #{zcount_method} position #{position} nomoves")
    {zcount, zcount_method, position}
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

{zcount, zcount_method, _} = AoC.perform_moves(0, 0, start, moves)
IO.puts(zcount)
IO.puts(zcount_method)
