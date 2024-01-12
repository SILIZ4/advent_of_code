defmodule Part1 do
  def resolve(file_name) do
    content = File.read!(file_name) |> String.split("\n", trim: true)

    content
    |> Enum.map(fn x ->
      [characters, pattern] = String.split(x, " ")
      count_solutions(
        String.to_charlist(characters),
        String.split(pattern, ",") |> Enum.map(&(String.to_integer(&1))),
        0)
    end)
    |> Enum.sum()
  end

  def count_solutions(characters, pattern, group_size) when characters==[] and pattern == [] do
    if group_size==0, do: 1, else: 0
  end

  def count_solutions(characters, [last_group | tail]=_pattern, group_size) when characters==[] do
    if tail != [] do
      0
    else
      if last_group == group_size, do: 1, else: 0
    end
  end

  def count_solutions(characters, pattern, group_size) do
    [c| tail] = characters
    case c do
      ?. -> if group_size == 0 do
          count_solutions(tail, pattern, 0)
        else
          if List.first(pattern) == group_size && pattern != [] do
            count_solutions(tail, tl(pattern), 0)
          else
            0
          end
        end

      ?# -> if group_size+1 <= List.first(pattern) do
          count_solutions(tail, pattern, group_size+1)
        else
          0
        end

      ?? -> count_solutions([?.|tail], pattern, group_size) + count_solutions([?#|tail], pattern, group_size)
    end
  end
end
