## The MIT License (MIT)
##
## Copyright (c) 2014 Charlie Barto
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

## This file implements a dynamic matrix class
import matrixOptions
type DynamicMatrix*[T; O: Options] = object
  data*: seq[T]
  rows: int
  cols: int
type
  MatX*[T] = DynamicMatrix[T, ColMajor]
  MatXf* = MatX[float32]
  MatXd* = MatX[float]
  MatXi* = MatX[int]
proc matX*[T](rows, cols: int): MatX[T] =
  result.rows = rows
  result.cols = cols
  result.data = newSeq(rows * cols)

#subscript indexing
proc `[]`*(self: DynamicMatrix, i,j: int): self.T =
  when self.O is RowMajor:
    var idx = (self.cols * (i-1)) + (j-1)
    result = self.data[idx]
  when Matrix.O is ColMajor:
    var idx = (self.rows * (j-1)) + (i-1)
    result = self.data[idx]

proc `[]=`*(self: DynamicMatrix; i,j: int; val: DynamicMatrix.T) =
  when self.O is RowMajor:
    var idx = (self.cols * (i-1)) + (j-1)
    self.data[idx] = val
  when Matrix.O is ColMajor:
    var idx = (self.rows * (j-1)) + (i-1)
    self.data[idx] = val
