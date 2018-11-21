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


//! \addtogroup spglue_times
//! @{



class spglue_times
  {
  public:
  
  template<typename T1, typename T2>
  inline static void apply(SpMat<typename T1::elem_type>& out, const SpGlue<T1,T2,spglue_times>& X);
  
  template<typename T1, typename T2>
  inline static void apply(SpMat<typename T1::elem_type>& out, const SpGlue<SpOp<T1,spop_scalar_times>,T2,spglue_times>& X);
  
  template<typename eT>
  arma_hot inline static void apply_noalias(SpMat<eT>& c, const SpMat<eT>& x, const SpMat<eT>& y);
  };



class spglue_times_misc
  {
  public:
  
  template<typename T1, typename T2>
  inline static void sparse_times_dense(Mat<typename T1::elem_type>& out, const T1& x, const T2& y);
  
  template<typename T1, typename T2>
  inline static void dense_times_sparse(Mat<typename T1::elem_type>& out, const T1& x, const T2& y);
  };



class spglue_times_mixed
  {
  public:
  
  template<typename T1, typename T2>
  inline static void sparse_times_sparse(SpMat< typename promote_type<typename T1::elem_type, typename T2::elem_type>::result >& out, const T1& X, const T2& Y);

  template<typename T1, typename T2>
  inline static void sparse_times_dense(Mat< typename promote_type<typename T1::elem_type, typename T2::elem_type>::result >& out, const T1& X, const T2& Y);

  template<typename T1, typename T2>
  inline static void dense_times_sparse(Mat< typename promote_type<typename T1::elem_type, typename T2::elem_type>::result >& out, const T1& X, const T2& Y);
  };



//! @}
