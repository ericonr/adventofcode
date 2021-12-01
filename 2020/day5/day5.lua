max_id_possible = 127 * 7 + 7
--[[ table to hold seats accounted for --]]
occupied = {}
for i = 0,max_id_possible
do
	occupied[i] = false
end

--[[ max id in data --]]
m = 0
for l in io.lines("data")
do
	row1 = 0
	row2 = 127
	for i = 1,7,1
	do
		c = l:sub(i,i)
		median = (row1 + row2) / 2
		if (c == 'F')
		then
			row2 = math.floor(median)
		else
			row1 = math.ceil(median)
		end
	end
	col1 = 0
	col2 = 7
	for i = 8,10,1
	do
		c = l:sub(i,i)
		median = (col1 + col2) / 2
		if (c == 'L')
		then
			col2 = math.floor(median)
		else
			col1 = math.ceil(median)
		end
	end

	id = row1 * 8 + col1
	if (id > m)
	then
		m = id
	end

	occupied[id] = true
end

print("max id: ", m)

for i = 0,max_id_possible
do
	if (occupied[i] == false)
	then
		if(occupied[i-1] and occupied[i+1])
		then
			print("your id: ", i)
		end
	end
end
