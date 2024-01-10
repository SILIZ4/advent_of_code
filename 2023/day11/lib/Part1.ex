defmodule Part1 do
  def resolve(file_name) do
    content = File.read!(file_name) |> String.split("\n", trim: true)

    line_length = String.length(List.first(content))
    {empty_rows, empty_cols, _} = identify_empty_rows_cols(content, line_length)

    content
    |> duplicate_empty_lines(empty_rows)
    |> duplicate_empty_cols(empty_cols)
    |> find_galaxy_positions()
    |> sum_pairwise_distances()
  end

  def sum_pairwise_distances(positions) do
    res = (for {i, j} <- positions, {r, s} <- positions, do: abs(i-r) + abs(j-s)) |> Enum.sum()
    trunc(res/2)
  end

  def find_galaxy_positions(content) do
    {res, _} = content
    |> Enum.reduce({[], 0}, fn line, {lines, i}=_acc ->
      in_row = line
      |> String.to_charlist()
      |> find_galaxies_in_line()
      |> Enum.map(&({i, &1}))
      {in_row ++ lines, i+1}
    end)
    res
  end

  def identify_empty_rows_cols(content, line_length) do
    content
      |> Enum.reduce({[], MapSet.new(0..line_length-1), 0},
        fn line, {empty_rows, empty_cols, i}=_acc ->
          galaxies = find_galaxies_in_line(String.to_charlist(line))
          if galaxies == [] do
            {[i | empty_rows], empty_cols, i+1}
          else
            {empty_rows, Enum.reduce(galaxies, empty_cols, fn x, acc2 -> MapSet.delete(acc2, x) end), i+1}
        end
      end)
  end

  def find_galaxies_in_line(line) do
    {res, _} = Enum.reduce(line, {[], 0}, fn char, {positions, i}=_acc ->
      if char == ?# do
        {[i | positions], i+1}
      else
        {positions, i+1}
      end
    end)
    res
  end

  def duplicate_empty_lines(content, empty_lines) do
    {res, _} = Enum.reduce(content, {[], 0}, fn x, {lines, i}=_acc ->
      if i in empty_lines do
        {[x | [x | lines]], i+1}
      else
        {[x | lines], i+1}
      end
    end)
    res
  end

  def duplicate_empty_cols(content, empty_cols) do
    content
    |> Enum.map(fn x ->
      {res, _} = x
      |> String.to_charlist()
      |> Enum.reduce({[], 0}, fn x, {chars, i} = _acc ->
        if i in empty_cols do
          {[x | [x | chars]], i+1}
        else
          {[x | chars], i+1}
        end
      end)
      res
      |> List.to_string()
    end)
  end
end
