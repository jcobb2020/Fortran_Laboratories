program main
    use naive
    use better
    use dot
    implicit none
    integer :: i, j

    open(1, file = "res/naive_4.dat", status = "replace")
    open(2, file = "res/better_4.dat", status = "replace")
    open(3, file = "res/dot_4.dat", status = "replace")
    open(4, file = "res/matmull_4.dat", status = "replace")

    open(8, file = "res/naive_8.dat", status = "replace")
    open(9, file = "res/better_8.dat", status = "replace")
    open(10, file = "res/dot_8.dat", status = "replace")
    open(11, file = "res/matmull_8.dat", status = "replace")

    open(16, file = "res/naive_16.dat", status = "replace")
    open(17, file = "res/better_16.dat", status = "replace")
    open(18, file = "res/dot_16.dat", status = "replace")
    open(19, file = "res/matmull_16.dat", status = "replace")

    do i = 1, 8
        j = 2**i
        call count_4(j, 1, 2, 3, 4)
        call count_8(j, 8, 9, 10, 11)
        call count_16(j, 16, 17, 18, 19)

    end do

contains

    subroutine count_4(ar_size, fp1, fp2, fp3, fp4)
        integer(kind = 4), intent(in) :: ar_size, fp1, fp2, fp3, fp4
        real(kind = 4) :: A(ar_size, ar_size), B(ar_size, ar_size), C(ar_size, ar_size)
        real(kind = 16) :: start, naive, better, dot, mat

        call RANDOM_NUMBER(A)
        call RANDOM_NUMBER(B)
        call CPU_TIME(start)
        C = naivmull(A, B)
        call CPU_TIME(naive)
        naive = naive - start
        C = 0
        call CPU_TIME(start)
        C = bettermull(A, B)
        call CPU_TIME(better)
        better = better - start
        C = 0
        call CPU_TIME(start)
        C = dotmull(A, B)
        call CPU_TIME(dot)
        dot = dot - start
        C = 0
        call CPU_TIME(start)
        C = matmul(A, B)
        call CPU_TIME(mat)
        mat = mat - start

        write(fp1, *)ar_size, ' ', naive
        write(fp2, *)ar_size, ' ', better
        write(fp3, *)ar_size, ' ', dot
        write(fp4, *)ar_size, ' ', mat

    end subroutine count_4

    subroutine count_8(ar_size, fp1, fp2, fp3, fp4)
        integer(kind = 4), intent(in) :: ar_size, fp1, fp2, fp3, fp4
        real(kind = 8) :: A(ar_size, ar_size), B(ar_size, ar_size), C(ar_size, ar_size)
        real(kind = 16) :: start, naive, better, dot, mat

        call RANDOM_NUMBER(A)
        call RANDOM_NUMBER(B)
        call CPU_TIME(start)
        C = naivmull(A, B)
        call CPU_TIME(naive)
        naive = naive - start
        C = 0
        call CPU_TIME(start)
        C = bettermull(A, B)
        call CPU_TIME(better)
        better = better - start
        C = 0
        call CPU_TIME(start)
        C = dotmull(A, B)
        call CPU_TIME(dot)
        dot = dot - start
        C = 0
        call CPU_TIME(start)
        C = matmul(A, B)
        call CPU_TIME(mat)
        mat = mat - start

        write(fp1, *)ar_size, ' ', naive
        write(fp2, *)ar_size, ' ', better
        write(fp3, *)ar_size, ' ', dot
        write(fp4, *)ar_size, ' ', mat

    end subroutine count_8

    subroutine count_16(ar_size, fp1, fp2, fp3, fp4)
        integer(kind = 4), intent(in) :: ar_size, fp1, fp2, fp3, fp4
        real(kind = 16) :: A(ar_size, ar_size), B(ar_size, ar_size), C(ar_size, ar_size)
        real(kind = 16) :: start, naive, better, dot, mat

        call RANDOM_NUMBER(A)
        call RANDOM_NUMBER(B)
        call CPU_TIME(start)
        C = naivmull(A, B)
        call CPU_TIME(naive)
        naive = naive - start
        C = 0
        call CPU_TIME(start)
        C = bettermull(A, B)
        call CPU_TIME(better)
        better = better - start
        C = 0
        call CPU_TIME(start)
        C = dotmull(A, B)
        call CPU_TIME(dot)
        dot = dot - start
        C = 0
        call CPU_TIME(start)
        C = matmul(A, B)
        call CPU_TIME(mat)
        mat = mat - start

        write(fp1, *)ar_size, ' ', naive
        write(fp2, *)ar_size, ' ', better
        write(fp3, *)ar_size, ' ', dot
        write(fp4, *)ar_size, ' ', mat

    end subroutine count_16


end program main