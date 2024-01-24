defmodule Part1 do
  def resolve(file_name) do
    file_name
    |> parse_patterns()
    |> Enum.map(&(&1 |> find_symmetry() |> compute_score()))
    |> Enum.sum()
  end

  def compute_score(symmetry) do
      case symmetry do
        {:row, res} -> (res+1)*100
        {:column, res} -> res+1
      end
  end

  def find_symmetry(pattern) do
    column_symmetries = find_pattern_row_symmetries(pattern)
    if MapSet.size(column_symmetries) == 0 do
      {:row, pattern |> transpose_pattern() |> find_pattern_row_symmetries() |> Enum.sum()}
    else
      {:column, column_symmetries |> Enum.sum()}
    end
  end

  def transpose_pattern(pattern) do
    pattern
    |> Enum.zip()
    |> Enum.map(&(&1 |> Tuple.to_list() |> Enum.reverse()))
  end

  def parse_patterns(file_name) do
    {patterns, last_pattern} = File.read!(file_name)
    |> String.split("\n")
    |> Enum.reduce({[], []}, fn x, {patterns, current_pattern}=_acc ->
      if x == "" do
        if current_pattern == [], do: {patterns, []}, else: {[current_pattern|patterns], []}
      else
        {patterns, [String.to_charlist(x)|current_pattern]}
      end
    end)

    if last_pattern == [] do
      patterns
    else
      [last_pattern | patterns]
    end
  end

  def find_pattern_row_symmetries(rows) do
    line_length = Enum.count(hd(rows))
    rows
    |> Enum.map(&(find_row_symmetries(&1)))
    |> Enum.reduce(MapSet.new(Enum.to_list(0..line_length)), fn x, acc ->
      MapSet.intersection(acc, x)
    end)
  end

  def find_row_symmetries(characters) do
    do_find_row_symmetries([hd(characters)], tl(characters), 0, MapSet.new([]))
  end

  defp do_find_row_symmetries(_left, right, _i, res) when right==[], do: res
  defp do_find_row_symmetries(left, right, i, res) do
    new_res = if same_beginning(left, right), do: MapSet.put(res, i), else: res
    do_find_row_symmetries([hd(right)|left], tl(right), i+1, new_res)
  end

  defp same_beginning(left, right) do
    Enum.zip(left, right)
    |> Enum.map(fn {x, y}=_ -> x==y end)
    |> Enum.all?()
  end
end
