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



template<typename T1, typename T2>
arma_hot
inline
uword
spglue_elem_helper::max_n_nonzero_plus(const SpProxy<T1>& pa, const SpProxy<T2>& pb)
  {
  arma_extra_debug_sigprint();
  
  // assuming that pa and pb have the same size, ie.
  // pa.get_n_rows() == pb.get_n_rows()
  // pa.get_n_cols() == pb.get_n_cols()
  
  typename SpProxy<T1>::const_iterator_type x_it  = pa.begin();
  typename SpProxy<T1>::const_iterator_type x_end = pa.end();
  
  typename SpProxy<T2>::const_iterator_type y_it  = pb.begin();
  typename SpProxy<T2>::const_iterator_type y_end = pb.end();
  
  uword count = 0;
  
  while( (x_it != x_end) || (y_it != y_end) )
    {
    if(x_it == y_it)
      {
      ++x_it;
      ++y_it;
      }
    else
      {
      const uword x_it_col = x_it.col();
      const uword x_it_row = x_it.row();
      
      const uword y_it_col = y_it.col();
      const uword y_it_row = y_it.row();
      
      if((x_it_col < y_it_col) || ((x_it_col == y_it_col) && (x_it_row < y_it_row))) // if y is closer to the end
        {
        ++x_it;
        }
      else
        {
        ++y_it;
        }
      }
    
    ++count;
    }
  
  return count;
  }
  


template<typename T1, typename T2>
arma_hot
inline
uword
spglue_elem_helper::max_n_nonzero_schur(const SpProxy<T1>& pa, const SpProxy<T2>& pb)
  {
  arma_extra_debug_sigprint();
  
  // assuming that pa and pb have the same size, ie.
  // pa.get_n_rows() == pb.get_n_rows()
  // pa.get_n_cols() == pb.get_n_cols()
  
  typename SpProxy<T1>::const_iterator_type x_it  = pa.begin();
  typename SpProxy<T1>::const_iterator_type x_end = pa.end();
  
  typename SpProxy<T2>::const_iterator_type y_it  = pb.begin();
  typename SpProxy<T2>::const_iterator_type y_end = pb.end();
  
  uword count = 0;
  
  while( (x_it != x_end) || (y_it != y_end) )
    {
    if(x_it == y_it)
      {
      ++x_it;
      ++y_it;
      
      ++count;
      }
    else
      {
      const uword x_it_col = x_it.col();
      const uword x_it_row = x_it.row();
      
      const uword y_it_col = y_it.col();
      const uword y_it_row = y_it.row();
      
      if((x_it_col < y_it_col) || ((x_it_col == y_it_col) && (x_it_row < y_it_row))) // if y is closer to the end
        {
        ++x_it;
        }
      else
        {
        ++y_it;
        }
      }
    }
  
  return count;
  }
  


//! @}
