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


//! \addtogroup spglue_elem_helper
//! @{



class spglue_elem_helper
  {
  public:
  
  template<typename T1, typename T2>
  arma_hot inline static uword max_n_nonzero_plus(const SpProxy<T1>& pa, const SpProxy<T2>& pb);
  
  template<typename T1, typename T2>
  arma_hot inline static uword max_n_nonzero_schur(const SpProxy<T1>& pa, const SpProxy<T2>& pb);
  };



//! @}

