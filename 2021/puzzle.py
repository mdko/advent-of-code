with open("input") as f:
    nums = [int(l.strip()) for l in f.readlines()]

    def count_larger_than_previous(l):
        last = None
        count = 0
        for n in l:
            if last is not None and n > last:
                count += 1
            last = n
        print(count)

    # question 1
    count_larger_than_previous(nums)

    # question 2
    windows = [sum(t) for t in zip(nums, nums[1:], nums[2:])]
    count_larger_than_previous(windows)
