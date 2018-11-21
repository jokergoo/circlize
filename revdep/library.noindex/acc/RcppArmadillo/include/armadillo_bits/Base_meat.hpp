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


//! \addtogroup Base
//! @{



template<typename elem_type, typename derived>
arma_inline
const derived&
Base<elem_type,derived>::get_ref() const
  {
  return static_cast<const derived&>(*this);
  }



template<typename elem_type, typename derived>
arma_cold
inline
void
Base<elem_type,derived>::print(const std::string extra_text) const
  {
  if(is_op_strans<derived>::value || is_op_htrans<derived>::value)
    {
    const Proxy<derived> P( (*this).get_ref() );
    
    const quasi_unwrap< typename Proxy<derived>::stored_type > tmp(P.Q);
    
    tmp.M.impl_print(extra_text);
    }
  else
    {
    const quasi_unwrap<derived> tmp( (*this).get_ref() );
    
    tmp.M.impl_print(extra_text);
    }
  }



template<typename elem_type, typename derived>
arma_cold
inline
void
Base<elem_type,derived>::print(std::ostream& user_stream, const std::string extra_text) const
  {
  if(is_op_strans<derived>::value || is_op_htrans<derived>::value)
    {
    const Proxy<derived> P( (*this).get_ref() );
    
    const quasi_unwrap< typename Proxy<derived>::stored_type > tmp(P.Q);
    
    tmp.M.impl_print(user_stream, extra_text);
    }
  else
    {
    const quasi_unwrap<derived> tmp( (*this).get_ref() );
    
    tmp.M.impl_print(user_stream, extra_text);
    }
  }
  


template<typename elem_type, typename derived>
arma_cold
inline
void
Base<elem_type,derived>::raw_print(const std::string extra_text) const
  {
  if(is_op_strans<derived>::value || is_op_htrans<derived>::value)
    {
    const Proxy<derived> P( (*this).get_ref() );
    
    const quasi_unwrap< typename Proxy<derived>::stored_type > tmp(P.Q);
    
    tmp.M.impl_raw_print(extra_text);
    }
  else
    {
    const quasi_unwrap<derived> tmp( (*this).get_ref() );
    
    tmp.M.impl_raw_print(extra_text);
    }
  }



template<typename elem_type, typename derived>
arma_cold
inline
void
Base<elem_type,derived>::raw_print(std::ostream& user_stream, const std::string extra_text) const
  {
  if(is_op_strans<derived>::value || is_op_htrans<derived>::value)
    {
    const Proxy<derived> P( (*this).get_ref() );
    
    const quasi_unwrap< typename Proxy<derived>::stored_type > tmp(P.Q);
    
    tmp.M.impl_raw_print(user_stream, extra_text);
    }
  else
    {
    const quasi_unwrap<derived> tmp( (*this).get_ref() );
    
    tmp.M.impl_raw_print(user_stream, extra_text);
    }
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
elem_type
Base<elem_type,derived>::min() const
  {
  return op_min::min( (*this).get_ref() );
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
elem_type
Base<elem_type,derived>::max() const
  {
  return op_max::max( (*this).get_ref() );
  }



template<typename elem_type, typename derived>
inline
elem_type
Base<elem_type,derived>::min(uword& index_of_min_val) const
  {
  const Proxy<derived> P( (*this).get_ref() );
  
  return op_min::min_with_index(P, index_of_min_val);
  }



template<typename elem_type, typename derived>
inline
elem_type
Base<elem_type,derived>::max(uword& index_of_max_val) const
  {
  const Proxy<derived> P( (*this).get_ref() );
  
  return op_max::max_with_index(P, index_of_max_val);
  }



template<typename elem_type, typename derived>
inline
elem_type
Base<elem_type,derived>::min(uword& row_of_min_val, uword& col_of_min_val) const
  {
  const Proxy<derived> P( (*this).get_ref() );
  
  uword index = 0;
  
  const elem_type val = op_min::min_with_index(P, index);
  
  const uword local_n_rows = P.get_n_rows();
  
  row_of_min_val = index % local_n_rows;
  col_of_min_val = index / local_n_rows;
  
  return val;
  }



template<typename elem_type, typename derived>
inline
elem_type
Base<elem_type,derived>::max(uword& row_of_max_val, uword& col_of_max_val) const
  {
  const Proxy<derived> P( (*this).get_ref() );
  
  uword index = 0;
  
  const elem_type val = op_max::max_with_index(P, index);
  
  const uword local_n_rows = P.get_n_rows();
  
  row_of_max_val = index % local_n_rows;
  col_of_max_val = index / local_n_rows;
  
  return val;
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
uword
Base<elem_type,derived>::index_min() const
  {
  const Proxy<derived> P( (*this).get_ref() );
  
  uword index = 0;
  
  if(P.get_n_elem() == 0)
    {
    arma_debug_check(true, "index_min(): object has no elements");
    }
  else
    {
    op_min::min_with_index(P, index);
    }
  
  return index;
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
uword
Base<elem_type,derived>::index_max() const
  {
  const Proxy<derived> P( (*this).get_ref() );
  
  uword index = 0;
  
  if(P.get_n_elem() == 0)
    {
    arma_debug_check(true, "index_max(): object has no elements");
    }
  else
    {
    op_max::max_with_index(P, index);
    }
  
  return index;
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_symmetric() const
  {
  arma_extra_debug_sigprint();
  
  const quasi_unwrap<derived> U( (*this).get_ref() );
  
  const Mat<elem_type>& A = U.M;
  
  if(A.n_rows != A.n_cols)  { return false; }
  if(A.n_elem <= 1       )  { return true;  }
  
  const uword N   = A.n_rows;
  const uword Nm1 = N-1;
  
  const elem_type* A_col = A.memptr();
  
  for(uword j=0; j < Nm1; ++j)
    {
    const uword jp1 = j+1;
    
    const elem_type* A_row = &(A.at(j,jp1));
    
    for(uword i=jp1; i < N; ++i)
      {
      if(A_col[i] != (*A_row))  { return false; }
      
      A_row += N;
      }
    
    A_col += N;
    }
  
  return true;
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_symmetric(const typename get_pod_type<elem_type>::result tol) const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<elem_type>::result T;
  
  if(tol == T(0))  { return (*this).is_symmetric(); }
  
  arma_debug_check( (tol < T(0)), "is_symmetric(): parameter 'tol' must be >= 0" );
  
  const quasi_unwrap<derived> U( (*this).get_ref() );
  
  const Mat<elem_type>& A = U.M;
  
  if(A.n_rows != A.n_cols)  { return false; }
  if(A.n_elem <= 1       )  { return true;  }
  
  const T norm_A = as_scalar( arma::max(sum(abs(A), 1), 0) );
  
  if(norm_A == T(0))  { return true; }
  
  const T norm_A_Ast = as_scalar( arma::max(sum(abs(A - A.st()), 1), 0) );
  
  return ( (norm_A_Ast / norm_A) <= tol );
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_hermitian() const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<elem_type>::result T;
  
  const quasi_unwrap<derived> U( (*this).get_ref() );
  
  const Mat<elem_type>& A = U.M;
  
  if(A.n_rows != A.n_cols)  { return false; }
  if(A.n_elem == 0       )  { return true;  }
  
  const uword N = A.n_rows;
  
  const elem_type* A_col = A.memptr();
  
  for(uword j=0; j < N; ++j)
    {
    if( access::tmp_imag(A_col[j]) != T(0) )  { return false; }
    
    A_col += N;
    }
  
  A_col = A.memptr();
  
  const uword Nm1 = N-1;
  
  for(uword j=0; j < Nm1; ++j)
    {
    const uword jp1 = j+1;
    
    const elem_type* A_row = &(A.at(j,jp1));
    
    for(uword i=jp1; i < N; ++i)
      {
      if(A_col[i] != access::alt_conj(*A_row))  { return false; }
      
      A_row += N;
      }
    
    A_col += N;
    }
  
  return true;
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_hermitian(const typename get_pod_type<elem_type>::result tol) const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<elem_type>::result T;
  
  if(tol == T(0))  { return (*this).is_hermitian(); }
  
  arma_debug_check( (tol < T(0)), "is_hermitian(): parameter 'tol' must be >= 0" );
  
  const quasi_unwrap<derived> U( (*this).get_ref() );
  
  const Mat<elem_type>& A = U.M;
  
  if(A.n_rows != A.n_cols)  { return false; }
  if(A.n_elem == 0       )  { return true;  }
  
  const T norm_A = as_scalar( arma::max(sum(abs(A), 1), 0) );
  
  if(norm_A == T(0))  { return true; }
  
  const T norm_A_At = as_scalar( arma::max(sum(abs(A - A.t()), 1), 0) );
  
  return ( (norm_A_At / norm_A) <= tol );
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_empty() const
  {
  arma_extra_debug_sigprint();
  
  const Proxy<derived> P( (*this).get_ref() );
  
  return (P.get_n_elem() == uword(0));
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_square() const
  {
  arma_extra_debug_sigprint();
  
  const Proxy<derived> P( (*this).get_ref() );
  
  return (P.get_n_rows() == P.get_n_cols());
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_vec() const
  {
  arma_extra_debug_sigprint();
  
  if( (Proxy<derived>::is_row) || (Proxy<derived>::is_col) )  { return true; }
  
  const Proxy<derived> P( (*this).get_ref() );
  
  return ( (P.get_n_rows() == uword(1)) || (P.get_n_cols() == uword(1)) );
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_colvec() const
  {
  arma_extra_debug_sigprint();
  
  if(Proxy<derived>::is_col)  { return true; }
  
  const Proxy<derived> P( (*this).get_ref() );
  
  return (P.get_n_cols() == uword(1));
  }



template<typename elem_type, typename derived>
inline
arma_warn_unused
bool
Base<elem_type,derived>::is_rowvec() const
  {
  arma_extra_debug_sigprint();
  
  if(Proxy<derived>::is_row)  { return true; }
  
  const Proxy<derived> P( (*this).get_ref() );
  
  return (P.get_n_rows() == uword(1));
  }



//
// extra functions defined in Base_inv_yes

template<typename derived>
arma_inline
const Op<derived,op_inv>
Base_inv_yes<derived>::i() const
  {
  return Op<derived,op_inv>(static_cast<const derived&>(*this));
  }



template<typename derived>
arma_deprecated
inline
const Op<derived,op_inv>
Base_inv_yes<derived>::i(const bool) const   // argument kept only for compatibility with old user code
  {
  // arma_debug_warn(".i(bool) is deprecated and will be removed; change to .i()");
  
  return Op<derived,op_inv>(static_cast<const derived&>(*this));
  }



template<typename derived>
arma_deprecated
inline
const Op<derived,op_inv>
Base_inv_yes<derived>::i(const char*) const   // argument kept only for compatibility with old user code
  {
  // arma_debug_warn(".i(char*) is deprecated and will be removed; change to .i()");
  
  return Op<derived,op_inv>(static_cast<const derived&>(*this));
  }



//
// extra functions defined in Base_eval_Mat

template<typename elem_type, typename derived>
arma_inline
const derived&
Base_eval_Mat<elem_type, derived>::eval() const
  {
  arma_extra_debug_sigprint();
  
  return static_cast<const derived&>(*this);
  }



//
// extra functions defined in Base_eval_expr

template<typename elem_type, typename derived>
arma_inline
Mat<elem_type>
Base_eval_expr<elem_type, derived>::eval() const
  {
  arma_extra_debug_sigprint();
  
  return Mat<elem_type>( static_cast<const derived&>(*this) );
  }



//
// extra functions defined in Base_trans_cx

template<typename derived>
arma_inline
const Op<derived,op_htrans>
Base_trans_cx<derived>::t() const
  {
  return Op<derived,op_htrans>( static_cast<const derived&>(*this) );
  }



template<typename derived>
arma_inline
const Op<derived,op_htrans>
Base_trans_cx<derived>::ht() const
  {
  return Op<derived,op_htrans>( static_cast<const derived&>(*this) );
  }



template<typename derived>
arma_inline
const Op<derived,op_strans>
Base_trans_cx<derived>::st() const
  {
  return Op<derived,op_strans>( static_cast<const derived&>(*this) );
  }



//
// extra functions defined in Base_trans_default

template<typename derived>
arma_inline
const Op<derived,op_htrans>
Base_trans_default<derived>::t() const
  {
  return Op<derived,op_htrans>( static_cast<const derived&>(*this) );
  }



template<typename derived>
arma_inline
const Op<derived,op_htrans>
Base_trans_default<derived>::ht() const
  {
  return Op<derived,op_htrans>( static_cast<const derived&>(*this) );
  }



template<typename derived>
arma_inline
const Op<derived,op_htrans>
Base_trans_default<derived>::st() const
  {
  return Op<derived,op_htrans>( static_cast<const derived&>(*this) );
  }



//! @}
