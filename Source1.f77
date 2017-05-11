        PROGRAM searchProgram
        iMPLICIT INTEGER (A-Z)
        
C       allocates array with size of 1000
        DIMENSION numbers(1000)
        PRINT*, 'enter your array size: (limit 1000)'
        
C       set length of array
        READ*, len
        PRINT*, 'len:'
        PRINT*, len
        
C       input values into array
        PRINT*, 'enter each of your numbers one by one:'
        DO i = 1, len
        READ*, y
        numbers(i) = y
        END DO
        
C       sort array
        CALL sort(numbers,len)
        PRINT*, ' '
        PRINT*, 'sorted array:'
        CALL printArray(numbers, len)
        
C       ask user to keep searching array
   50   IF(.TRUE.) THEN
            PRINT*, ''
            PRINT*, 'what number would you like to search for?'
            READ*, toSearch
            CALL search(numbers,toSearch,len)
        GO TO 50
        END IF

        END PROGRAM searchProgram

C       subroutine that prints contents of array in list form
        SUBROUTINE printArray(arr, len)
        IMPLICIT INTEGER (A-Z)
        DIMENSION arr(len)
        DO i = 1, len
        PRINT*, arr(i)
        END DO
        END

C       bubble sort algorithm
        SUBROUTINE sort(arr,len)
        IMPLICIT INTEGER (A-Z)
        DIMENSION arr(len)

        adjust = len - 3
        DO 20, I = 1,len
           DO 10, k = len,len - adjust -1, -1
               IF ( arr(k) .LT. arr(k - 1) ) THEN
C                 SWAP if A(k) < A(k-1)
                  temp = arr(k)
                  arr(k) = arr(k-1)
                  arr(k-1) = temp
               END IF
   10     CONTINUE

   20  CONTINUE
       END

C      binary search
       SUBROUTINE search(arr,number,len)
       IMPLICIT INTEGER (A-Z)
       DIMENSION arr(len)
       found = 0
       PRINT*, ' '
       PRINT*, 'searching...'
       i = 1
       j = len
       IF (number .LT. arr(1)) THEN
          PRINT*, 'not found'
          RETURN
       ELSE IF ( number .GT. arr(len)) THEN
          PRINT*, 'not found'
          RETURN
       END IF
   30  IF (i .LT. j) THEN
          middle = (i+j)/2
          IF (number .GT. arr(middle)) THEN
              i = middle + 1
          ELSE
              j = middle
          END IF
          IF (arr(i) .EQ. number) THEN
                found = 1
          END IF
       GO TO 30
       END IF
            IF(found .EQ. 1) THEN
                PRINT*, 'found at:'
                PRINT*, i
            ELSE
                PRINT*, 'not found'
            END IF
       END


