defmodule Part1 do
  def resolve(file_name) do
    File.read!(file_name)
    |> String.split(",")
    |> Enum.map(&(&1 |> String.trim_trailing() |> String.to_charlist() |> hash()))
    |> Enum.sum()
  end
  def hash(chars) do
    Enum.reduce(chars, 0, fn x, acc ->
      rem((acc + x) * 17, 256)
    end)
  end
end
