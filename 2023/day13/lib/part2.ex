defmodule Part2 do
  def resolve(file_name) do
    file_name
    |> Part1.parse_patterns()
    |> Enum.map(&(&1 |> Enum.reverse() |> find_smudge() |> compute_score()) )
    |> Enum.sum()
  end

  def find_smudge(pattern) do
    transposed_pattern = transpose_pattern(pattern)
    rows = Enum.count(pattern)
    cols = Enum.count(transposed_pattern)
    original_symmetries = Map.merge(find_candidates(pattern, :row), find_candidates(transposed_pattern, :column))
    {original_col_symmetry, original_row_symmetry} = find_symmetry(original_symmetries, rows, cols)

    pattern
    |> Enum.with_index()
    |> Enum.reduce_while({-1, -1}, fn {line, i}=_, acc ->
        res = line
        |> Enum.with_index()
        |> Enum.reduce_while({-1, -1}, fn {char, j}=_, acc ->
          replacement_char = if char==?#, do: ?., else: ?#

          new_row_symmetries = line |> List.replace_at(j, replacement_char) |> find_candidates_line(i, :row)
          new_col_symmetries = transposed_pattern |> Enum.at(j) |> List.replace_at(i, replacement_char)
                                                  |> find_candidates_line(j, :column)

          new_symmetries = original_symmetries |> Map.merge(new_row_symmetries) |> Map.merge(new_col_symmetries)
          {new_col_symmetry, new_row_symmetry} = find_symmetry(new_symmetries, rows, cols)
          result_col = MapSet.difference(new_col_symmetry, original_col_symmetry)
          result_row = MapSet.difference(new_row_symmetry, original_row_symmetry)

          if MapSet.size(result_col) > 0 || MapSet.size(result_row) > 0 do
            if MapSet.size(result_col) > 0 do
              {:halt, {:column, Enum.sum(result_col)}}
            else
              {:halt, {:row, Enum.sum(result_row)}}
            end
          else
            {:cont, acc}
          end
        end)
        if res == {-1, -1} do
          {:cont, acc}
        else
          {:halt, res}
        end
      end)
  end

  defp compute_score(symmetry) do
      case symmetry do
        {:row, res} -> res+1
        {:column, res} -> (res+1)*100
      end
  end

  defp transpose_pattern(pattern) do
    pattern
    |> Enum.zip()
    |> Enum.map(&(&1 |> Tuple.to_list()))
  end

  defp find_symmetry(candidates, rows, cols) do
    candidates
    |> Enum.reduce({MapSet.new(Enum.to_list(0..rows)), MapSet.new(Enum.to_list(0..cols))},
      fn x, {col_symmetry, row_symmetry}=_acc ->
        case x do
          {{:column, _i}, value} -> {MapSet.intersection(col_symmetry, value), row_symmetry}
          {{:row, _i}, value} -> {col_symmetry, MapSet.intersection(row_symmetry, value)}
        end
    end)
  end

  defp find_candidates_line(line, i, alignment) do
    %{{alignment, i} => Part1.find_row_symmetries(line)}
  end

  defp find_candidates(lines, alignment) do
    lines
    |> Enum.map(&(Part1.find_row_symmetries(&1)))
    |> Enum.with_index()
    |> Enum.reduce(Map.new(), fn {x, i}=_, acc -> Map.put(acc, {alignment, i}, x) end)
  end
end
