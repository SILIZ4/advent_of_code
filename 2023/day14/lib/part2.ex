defmodule Part2 do
  def resolve(file_name, n) do
    file_name
    |> Part1.parse_file()
    |> cycle(n)
    |> Part1.transpose()
    |> Enum.map(&(count_weight(&1)))
    |> Enum.sum()
  end

  defp count_weight(pattern) do
    len = pattern |> Enum.count()
    pattern
    |> Enum.with_index()
    |> Enum.reduce(0, fn {x, i}, acc ->
      if x == ?O, do: acc+len-i, else: acc
    end)
  end

  defp cycle(pattern ,n) do
    case 0..n
    |> Enum.reduce_while({pattern, MapSet.new(), nil}, fn i, {x, y, z}->
      case do_cycle(x, y, z) do
        {:cycle, cycle} -> {:halt, Enum.at(cycle |> Enum.reverse(), rem(n-i-1, Enum.count(cycle)))}
        res -> {:cont, res}
      end
    end) do
      {res, _, _} -> res
      res -> res
    end
  end

  defp do_cycle(pattern, seen_patterns, cycle_start) when cycle_start==nil do
    new_pattern = pattern
    |> move_north()
    |> move_west()
    |> move_south()
    |> move_east()

    str = List.to_string(new_pattern)
    if MapSet.member?(seen_patterns, str) do
      {new_pattern, [new_pattern], new_pattern}
    else
      {new_pattern, MapSet.put(seen_patterns, str), nil}
    end
  end

  defp do_cycle(pattern, seen_patterns, cycle_start) do
    new_pattern = pattern
    |> move_north()
    |> move_west()
    |> move_south()
    |> move_east()

    if new_pattern == cycle_start do
      {:cycle, seen_patterns}
    else
      {new_pattern, [new_pattern|seen_patterns], cycle_start}
    end
  end

  defp move_rocks_to_anchor(line) do
    {res, rock, i} = line
    |> Enum.reduce({"", 0, 0}, fn y, {line, rock, i}=_acc ->
      case y do
        ?. -> {line, rock, i+1}
        ?O -> {line, rock+1, i+1}
        ?# -> {line <> get_moved_rocks(rock, i) <> "#" , 0, 0}
      end
    end)
    (if i > 0, do: res <> get_moved_rocks(rock, i), else: res)
    |> String.to_charlist()
  end

  defp get_moved_rocks(rock, len) do
    String.duplicate("O", rock) <> String.duplicate(".", len-rock)
  end

  defp flip_lines(lines) do
    Enum.map(lines, &(Enum.reverse(&1)))
  end

  defp move_north(pattern) do
    pattern
    |> Part1.transpose()
    |> Enum.map(&(move_rocks_to_anchor(&1)))
    |> Part1.transpose()
  end

  defp move_south(pattern) do
    pattern
    |> Part1.transpose() |> flip_lines()
    |> Enum.map(&(move_rocks_to_anchor(&1)))
    |> flip_lines() |> Part1.transpose()
  end

  defp move_east(pattern) do
    pattern
    |> flip_lines()
    |> Enum.map(&(move_rocks_to_anchor(&1)))
    |> flip_lines()
  end

  defp move_west(pattern) do
    pattern |> Enum.map(&(move_rocks_to_anchor(&1)))
  end
end
