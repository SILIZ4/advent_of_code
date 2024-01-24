args = System.argv()
if length(args) != 1 do
  IO.inspect(args)
  IO.puts(:stderr, "Incorrect arguments")
  exit(-1)
end
IO.puts("The sum is #{Part2.resolve(List.first(args))}.")
