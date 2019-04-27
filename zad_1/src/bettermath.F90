module better

    implicit none

    public:: bettermull
    private:: better_multiplication_4, better_multiplication_8, better_multiplication_16
    interface bettermull
        procedure better_multiplication_4, better_multiplication_8, better_multiplication_16
    end interface bettermull

contains

    function better_multiplication_4(A, B) result(C)
        integer :: size1, size2, size3, i, j, k
        real(kind = 4), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        C = 0
        do j = 1, size2
            do k = 1, size3
                do i = 1, size1
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end function better_multiplication_4

    function better_multiplication_8(A, B) result(C)
        integer :: size1, size2, size3, i, j, k
        real(kind = 8), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        C = 0
        do j = 1, size2
            do k = 1, size3
                do i = 1, size1
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end function better_multiplication_8

    function better_multiplication_16(A, B) result(C)
        integer :: size1, size2, size3, i, j, k
        real(kind = 16), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        C = 0
        do j = 1, size2
            do k = 1, size3
                do i = 1, size1
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end function better_multiplication_16

end module better