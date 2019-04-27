module dot
    implicit none

    public :: dotmull
    private :: dot_multiplication_4,dot_multiplication_8, dot_multiplication_16

    interface dotmull
        procedure dot_multiplication_4, dot_multiplication_8, dot_multiplication_16
    end interface dotmull

contains

    function dot_multiplication_4(A, B) result(C)
        integer :: size1, size2, size3, i, j
        real(kind = 4), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        c = 0
        do i = 1, size3
            do j = 1, size2
                C(i, j) = dot_product(A(i, :), B(:, j))
            end do
        end do
    end function dot_multiplication_4

    function dot_multiplication_8(A, B) result(C)
        integer :: size1, size2, size3, i, j
        real(kind = 8), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        c = 0
        do i = 1, size3
            do j = 1, size2
                C(i, j) = dot_product(A(i, :), B(:, j))
            end do
        end do
    end function dot_multiplication_8

    function dot_multiplication_16(A, B) result(C)
        integer :: size1, size2, size3, i, j
        real(kind = 16), intent(in) :: A(:, :), B(:, :)
        real :: C(size(A, 1), size(B, 2))
        size1 = size(A, 1)
        size2 = size(B, 2)
        size3 = size(A, 2)
        c = 0
        do i = 1, size3
            do j = 1, size2
                C(i, j) = dot_product(A(i, :), B(:, j))
            end do
        end do
    end function dot_multiplication_16

end module dot