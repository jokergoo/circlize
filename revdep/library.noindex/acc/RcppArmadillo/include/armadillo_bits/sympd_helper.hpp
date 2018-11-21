// Copyright 2008-2016 Conrad Sanderson (http://conradsanderson.id.au)
// Copyright 2008-2016 National ICT Australia (NICTA)
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ------------------------------------------------------------------------


//! \addtogroup sympd_helper
//! @{


namespace sympd_helper
{



template<typename eT>
inline
typename enable_if2<is_cx<eT>::no, bool>::result
guess_sympd(const Mat<eT>& A)
  {
  arma_extra_debug_sigprint();
  
  // computationally inexpensive algorithm to guess whether a real matrix is positive definite:
  // (1) ensure the the matrix is symmetric
  // (2) ensure the diagonal entries are greater than zero
  // (3) ensure that the value with largest modulus is on the diagonal
  // the above conditions are necessary, but not sufficient;
  // doing it properly would be too computationally expensive for our purposes
  // more info: http://mathworld.wolfram.com/PositiveDefiniteMatrix.html
  
  if((A.n_rows != A.n_cols) || (A.n_elem == 0))  { return false; }
  
  const eT threshold = eT(10) * std::numeric_limits<eT>::epsilon();  // allow some leeway
  
  const uword N = A.n_rows;
  
  const eT* A_col = A.memptr();
  
  eT max_diag = eT(0);
  
  for(uword j=0; j < N; ++j)
    {
    const eT A_jj = A_col[j];
    
    if(A_jj <= eT(0))  { return false; }
    
    max_diag = (A_jj > max_diag) ? A_jj : max_diag;
    
    A_col += N;
    }
  
  A_col = A.memptr();
  
  const uword Nm1 = N-1;
  
  for(uword j=0; j < Nm1; ++j)
    {
    const uword jp1   = j+1;
    const eT*   A_row = &(A.at(j,jp1));
    
    for(uword i=jp1; i < N; ++i)
      {
      const eT A_ij = A_col[i];
      
      if( (std::abs(A_ij - (*A_row)) > threshold) || (std::abs(A_ij) > max_diag) )  { return false; }
      
      A_row += N;
      }
    
    A_col += N;
    }
  
  return true;
  }



template<typename eT>
inline
typename enable_if2<is_cx<eT>::yes, bool>::result
guess_sympd(const Mat<eT>& A)
  {
  arma_extra_debug_sigprint();
  
  // computationally inexpensive algorithm to guess whether a complex matrix is positive definite:
  // (1) ensure the the matrix is hermitian
  // (2) ensure the diagonal entries are real and greater than zero
  // (3) ensure that the value with largest modulus is on the diagonal
  // the above conditions are necessary, but not sufficient;
  // doing it properly would be too computationally expensive for our purposes
  // more info: http://mathworld.wolfram.com/PositiveDefiniteMatrix.html
  // NOTE: (3) is done approximately for complex numbers,
  // NOTE  as std::abs() on each complex element is too expensive
  
  typedef typename get_pod_type<eT>::result T;
  
  if((A.n_rows != A.n_cols) || (A.n_elem == 0))  { return false; }
  
  const T threshold = T(10) * std::numeric_limits<T>::epsilon();  // allow some leeway
  
  const uword N = A.n_rows;
  
  const eT* A_col = A.memptr();
  
  T max_diag = T(0);
  
  for(uword j=0; j < N; ++j)
    {
    const eT& A_jj      = A_col[j];
    const  T  A_jj_real = std::real(A_jj);
    const  T  A_jj_imag = std::imag(A_jj);
        
    if( (A_jj_real <= T(0)) || (std::abs(A_jj_imag) > threshold) )  { return false; }
    
    max_diag = (A_jj_real > max_diag) ? A_jj_real : max_diag;
    
    A_col += N;
    }
  
  A_col = A.memptr();
  
  const uword Nm1 = N-1;
  
  for(uword j=0; j < Nm1; ++j)
    {
    const uword jp1   = j+1;
    const eT*   A_row = &(A.at(j,jp1));
    
    for(uword i=jp1; i < N; ++i)
      {
      const eT& A_ij      = A_col[i];
      const  T  A_ij_real = std::real(A_ij);
      const  T  A_ij_imag = std::imag(A_ij);
      
      const eT& A_ji      = (*A_row);
      const  T  A_ji_real = std::real(A_ji);
      const  T  A_ji_imag = std::imag(A_ji);
      
      if( (std::abs(A_ij_real - A_ji_real) > threshold) || (std::abs(A_ij_imag + A_ji_imag) > threshold) || (std::abs(A_ij_real) > max_diag) || (std::abs(A_ij_imag) > max_diag) )  { return false; }
      
      A_row += N;
      }
    
    A_col += N;
    }
  
  return true;
  }



}  // end of namespace sympd_helper


//! @}
