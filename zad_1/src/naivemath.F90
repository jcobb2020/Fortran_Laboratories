module naive

    implicit none
    public :: naivmull
    private :: naive_multiplication_4, naive_multiplication_8, naive_multiplication_16
    interface naivmull
        procedure naive_multiplication_4, naive_multiplication_8, naive_multiplication_16
    end interface naivmull

contains

    function naive_multiplication_4(A, B) result(C)
        integer :: size1, size2, size3, i, j, k
        real(kind = 4), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        c = 0
        do i = 1, size1
            do j = 1, size2
                do k = 1, size3
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end function naive_multiplication_4

    function naive_multiplication_8(A, B) result(C)
        integer :: size1, size2, size3, i, j, k
        real(kind = 8), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        c = 0
        do i = 1, size1
            do j = 1, size2
                do k = 1, size3
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end function naive_multiplication_8

    function naive_multiplication_16(A, B) result(C)
        integer :: size1, size2, size3, i, j, k
        real(kind = 16), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        c = 0
        do i = 1, size1
            do j = 1, size2
                do k = 1, size3
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end function naive_multiplication_16

end module naive
