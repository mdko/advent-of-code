file = File.open("input")
file_data = file.readlines.map(&:chomp)

# Part 1
memory = {}
mask_set_indices = nil
mask_clear_indices = nil
for line in file_data
    arr = line.split(" = ")
    if arr[0] == "mask" then
        mask = arr[1]
        mask_set_indices = (0 ... mask.length).find_all{ |i| mask[i,1] == '1'}
        mask_clear_indices = (0 ... mask.length).find_all{ |i| mask[i,1] == '0'} 
    else
        raise "Mask should be set by now" unless (mask_set_indices != nil && mask_clear_indices != nil)
        loc = arr[0].match(/(\d+)/)
        val = arr[1].to_i.to_s(2)
        val = ("0" * (36 - val.length)) + val
        for i in mask_set_indices
            val[i] = '1'
        end
        for i in mask_clear_indices
            val[i] = '0'
        end
        memory[loc] = Integer("0b" + val)
    end
end

sum = 0
for v in memory.values
    sum += v
end
puts sum

# Part 2
memory = {}
mask_set_indices = nil
mask_floating_indices = nil
for line in file_data
    arr = line.split(" = ")
    if arr[0] == "mask" then
        mask = arr[1]
        mask_set_indices = (0 ... mask.length).find_all{ |i| mask[i,1] == '1'}
        mask_floating_indices = (0 ... mask.length).find_all{ |i| mask[i,1] == 'X'} 
    else
        raise "Mask should be set by now" unless (mask_set_indices != nil && mask_floating_indices != nil)
        val = arr[1].to_i
        loc = arr[0].match(/(\d+)/)[0].to_i.to_s(2)
        loc = ("0" * (36 - loc.length)) + loc
        for i in mask_set_indices
            loc[i] = '1'
        end
        # Find the *set* of memory locations, based on floating indices from mask
        # Ugly, but I really don't know Ruby.
        ncombs = 2 ** (mask_floating_indices.length)
        for n in (0 ... ncombs)
            n = ("0" * (mask_floating_indices.length - n.to_s(2).length)) + n.to_s(2)
            loc_n = loc.clone
            mask_floating_indices.each_with_index do |float_ix, i|
                loc_n[float_ix] = n[i]
            end
            memory[loc_n] = val
        end
    end
end

sum = 0
for v in memory.values
    sum += v
end
puts sum