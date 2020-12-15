module read_inputs
! Allows compiler to check that all variables are declared properly (otherwise treats them based on first letter of var)
implicit none

contains
    subroutine inputs (earliest, buses, i)
        integer, intent(out) :: i
        integer :: j
        integer :: f
        integer, intent(out) :: earliest
        integer, dimension(100), intent(out) :: buses
        character (len=1000) :: line

        open(UNIT=f, FILE="input", STATUS="old")

        read(f, *) earliest
        ! print '(I7)', earliest

        read(f, '(a)') line
        i = 0
        read_loop: do
            if (line .eq. '') then
                exit read_loop
            end if

            do while (line(1:1) .eq. ',')
                line = line(2:)
            end do

            if (line .eq. '') then
                exit read_loop
            end if

            j = 1
            do while (line(j:j) .ne. ',')
                j = j + 1
            end do

            i = i + 1
            if (line(1:1) .eq. 'x') then
                buses(i) = -1  ! store out-of-service bus as '-1'
            else
                read(line(1:j-1), *) buses(i)
            end if

            line = line(j:)
        end do read_loop
    end subroutine inputs
end module read_inputs

module part1
implicit none

contains
    subroutine part1_f (earliest, buses, len)
        integer, intent(in) :: earliest
        integer, dimension(100), intent(in) :: buses
        integer, intent(in) :: len
        integer :: i
        integer :: bus
        integer :: nearby
        integer :: curr

        nearby = CEILING(REAL(earliest) / buses(1)) * buses(1)
        bus = 1
        do i = 2, len
            if (buses(i) .ne. -1) then
                curr = CEILING(REAL(earliest) / buses(i)) * buses(i)
                if (curr .lt. nearby) then
                    nearby = curr
                    bus = buses(i)
                end if
            end if
        end do
        print '(I10)', (nearby - earliest) * bus  ! difference between chosen bus and earliest time we can depart, times bus number
    end subroutine part1_f
end module part1

module part2
contains
    subroutine part2_f (buses, len)
        integer, dimension(100), intent(in) :: buses
        integer, intent(in) :: len
        logical :: done
        integer :: i
        integer :: t

        t = 0
        toplevel: do
            ! print '(I20)', t
            done = .true.
            inner: do i = 2, len
                if (buses(i) .gt. t) then
                    done = .false.
                    exit inner  ! for efficiency
                endif
                if (buses(i) .ne. -1) then
                    if (MOD((t + i - 1), buses(i)) .ne. 0) then
                        done = .false.
                        exit inner
                    endif
                end if
            end do inner
            if (done) then
                exit toplevel
            endif
            t = t + buses(1)
        end do toplevel

        print '(I20)', t
    end subroutine part2_f
end module part2

program puzzle
use read_inputs
use part1
use part2
implicit none

integer :: len
integer :: earliest
integer, dimension(100) :: buses
call inputs (earliest, buses, len)
call part1_f (earliest, buses, len)
call part2_f (buses, len)
end program puzzle