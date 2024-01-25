defmodule Part2 do
  def resolve(file_name) do
    File.read!(file_name)
    |> String.split(",")
    |> Enum.map(&(&1 |> String.trim_trailing() |> String.to_charlist() ))
    |> Enum.reduce(%{}, fn code, boxes -> apply_transform(code, boxes) end)
    |> Enum.map(fn {box_id, box_content} ->
      box_content
      |> Enum.with_index()
      |> Enum.reduce(0, fn {{_, strength}, i}, acc -> acc + (box_id+1)*strength*(i+1) end)
      end)
    |> Enum.sum()
  end

  def apply_transform(code, boxes) do
    Enum.reduce(code, {[], boxes, nil}, fn x, {label, boxes, add}->
      if add != nil do
        add_lens(label |> Enum.reverse(), x-?0, boxes)
      else
        case x do
          ?- -> remove_lens(label |> Enum.reverse(), boxes)
          ?= -> {label, boxes, true}
          _ -> {[x | label], boxes, add}
        end
      end
    end)
  end

  def remove_lens(label, boxes) do
    pos = Part1.hash(label)
    box = Map.get(boxes, pos)
    if box == nil do
      boxes
    else
      Map.put(boxes, pos,
        Enum.reduce(box, [], fn {blabel, _}=el, acc ->
          if blabel == label do
            acc
          else
            [el | acc]
          end
        end)
        |> Enum.reverse()
      )
    end
  end

  def add_lens(label, strength, boxes) do
    pos = Part1.hash(label)
    box = Map.get(boxes, pos)

    new_lens = {label, strength}
    if box == nil do
      Map.put(boxes, pos, [new_lens])
    else
      {new_lenses, found} = Enum.reduce(box, {[], false}, fn {blabel, _}=el, {lenses, found} ->
          if blabel == label do
            {[new_lens | lenses], true}
          else
            {[el | lenses], found}
          end
        end)
      Map.put(boxes, pos, Enum.reverse(if found, do: new_lenses, else: [new_lens | new_lenses]))
    end
  end
end
