# Tests for cachematrix.R

library("testthat")

context("makeCacheMatrix")

test_that("default makeCacheMatrix()$get() is an empty matrix", {
        expect_equal(makeCacheMatrix()$get(), matrix())
})

test_that("makeCacheMatrix(x)$get() is x", {
        x <- matrix(1:4,ncol=2)
        expect_equal(makeCacheMatrix(x)$get(), x)
})

test_that("using $set(x) on a special 'matrix' object updates it's value", {
        x <- matrix(1:9,ncol=3)
        cacheMatrix <- makeCacheMatrix()
        cacheMatrix$set(x)
        expect_equal(cacheMatrix$get(), x)
})

context("cacheSolve")

test_that("cacheSolve returns the same result as solve", {
        x <- matrix(4:1,ncol=2)
        expect_equal(cacheSolve(makeCacheMatrix(x)), solve(x))
})

test_that("using cacheSolve more than once on the same object retrieves the cached inverse", {
        x <- matrix(2:5,ncol=2)
        matrixObj <- makeCacheMatrix(x)
        expect_equal(cacheSolve(matrixObj), solve(x))
        expect_message(cacheSolve(matrixObj), "getting cached data")
})

test_that("changing the object matrix refreshes the cached inverse", {
        x <- matrix(3:6,ncol=2)
        matrixObj <- makeCacheMatrix(x)
        inverse <- cacheSolve(matrixObj)
        expect_equal(inverse, solve(x))
        matrixObj$set(inverse)
        expect_equal(cacheSolve(matrixObj), x)
})
