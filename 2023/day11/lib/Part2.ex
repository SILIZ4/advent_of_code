defmodule Part2 do
  def resolve(file_name, duplicate_factor) do
    content = File.read!(file_name) |> String.split("\n", trim: true)

    line_length = String.length(List.first(content))
    {empty_rows, empty_cols, _} = Part1.identify_empty_rows_cols(content, line_length)

    content
    |> find_galaxy_positions(empty_rows, empty_cols, duplicate_factor)
    |> Part1.sum_pairwise_distances()
  end

  def find_galaxy_positions(content, empty_rows, empty_cols, duplicate_factor) do
    {res, _, _} = content
    |> Enum.reduce({[], 0, 0}, fn line, {lines, row_shift, i}=_acc ->
      new_row_shift = if i in empty_rows, do: row_shift+duplicate_factor-1, else: row_shift
      {in_row, _, _} = line
      |> String.to_charlist()
      |> Enum.reduce({[], 0, 0}, fn char, {positions, col_shift, j}=_acc ->
        new_col_shift = if j in empty_cols, do: col_shift+duplicate_factor-1, else: col_shift
        if char == ?# do
          {[{i+new_row_shift, j+new_col_shift}| positions], new_col_shift, j+1}
        else
          {positions, new_col_shift, j+1}
        end
      end)
      {in_row ++ lines, new_row_shift, i+1}
    end)
    res
  end
end
