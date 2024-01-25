defmodule Part1 do
  def resolve(file_name) do
    file_name
    |> parse_file()
    |> transpose()
    |> Enum.map(&(count_weight(&1)))
    |> Enum.sum()
  end

  def parse_file(file_name) do
    File.read!(file_name)
    |> String.split("\n", trim: true)
    |> Enum.map(&(String.to_charlist(&1)))
  end

  def transpose(grid) do
    grid |> Enum.zip() |> Enum.map(&(&1 |> Tuple.to_list()))
  end

  def count_weight(col) do
    cols = Enum.count(col)
    {weight, anchor, rocks} = col
    |> Enum.with_index()
    |> Enum.reduce({0, 0, 0}, fn {x, i}=_, {weight, anchor, rocks}=acc ->
      case x do
        ?O -> {weight, anchor, rocks+1}
        ?. -> acc
        ?# -> {weight + succesive_rock_weight(rocks, anchor, cols), i+1, 0}
      end
    end)
    if rocks != 0 do
      weight + succesive_rock_weight(rocks, anchor, cols)
    else
      weight
    end
  end

  defp succesive_rock_weight(rocks, anchor, cols) do
    trunc((rocks*(rocks-1))/2 + (cols-anchor-rocks+1)*rocks)
  end
end
