defmodule Part2 do
  def resolve(file_name, duplicates) do
    content = File.read!(file_name) |> String.split("\n", trim: true)

    {res, _} = content
    |> Enum.reduce({0, %{}}, fn x, {sum, cache} ->
      [string, pattern] = String.split(x, " ")
      characters = string |> String.replace(~r/\.+/, ".") |> String.to_charlist() |> duplicate(??, duplicates)
      group_sizes = pattern |> String.split(",") |> Enum.map(&(String.to_integer(&1))) |> List.duplicate(duplicates) |> List.flatten()

      {res, new_cache} = count_solutions({characters, group_sizes}, cache)
      {sum+res, new_cache}
    end)
    res
  end

  def duplicate(list, _between, count) when count==1, do: list
  def duplicate(list, between, count) do
    duplicate(list, between, count-1) ++ [between | list]
  end

  defp cached_return(cache, args, func) do
    case Map.fetch(cache, args) do
      {:ok, value} -> {value, cache}
      _ -> {value, new_cache} = func.(args, cache)
        {value, Map.put(new_cache, args, value)}
    end
  end

  def count_solutions({characters, groups}, cache) when groups == [] do
    if Enum.member?(characters, ?#), do: {0, cache}, else: {1, cache}
  end

  def count_solutions(args, input_cache) do
    cached_return(input_cache, args, fn {characters, groups}, cache=_args ->
      seq_len = length(characters)
      group_len = hd(groups)
      if seq_len - Enum.sum(groups) - length(groups) + 1 < 0 do
        {0, cache}
      else
        has_holes = characters |> Enum.take(group_len) |> Enum.member?(?.)
        if seq_len == group_len do
          if has_holes, do: {0, cache}, else: {1, cache}
        else
          can_use = not has_holes && Enum.at(characters, group_len) != ?#
          if List.first(characters) == ?# do
            if can_use do
              count_remove_trailing_dot(characters |> Enum.drop(group_len+1), tl(groups), cache)
            else
              {0, cache}
            end
          else
            {skip, new_cache} = count_remove_trailing_dot(characters |> Enum.drop(1), groups, cache)
            if not can_use do
              {skip, new_cache}
            else
              {count, newer_cache} = count_remove_trailing_dot(characters |> Enum.drop(group_len+1), tl(groups), new_cache)
              {count+skip, newer_cache}
            end
          end
        end
      end
      end)
    end
    defp count_remove_trailing_dot(characters, groups, cache) do
        if List.first(characters) == ?. do
          count_solutions({characters |> Enum.drop(1), groups}, cache)
        else
          count_solutions({characters, groups}, cache)
        end
    end
end
