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


//! \addtogroup MapMat
//! @{



template<typename eT>
inline
MapMat<eT>::~MapMat()
  {
  arma_extra_debug_sigprint_this(this);
  
  if(map_ptr)  { (*map_ptr).clear();  delete map_ptr; }
  
  // try to expose buggy user code that accesses deleted objects
  if(arma_config::debug)  { map_ptr = NULL; }
  
  arma_type_check(( is_supported_elem_type<eT>::value == false ));
  }



template<typename eT>
inline
MapMat<eT>::MapMat()
  : n_rows (0)
  , n_cols (0)
  , n_elem (0)
  , map_ptr(NULL)
  {
  arma_extra_debug_sigprint_this(this);
  
  init_cold();
  }



template<typename eT>
inline
MapMat<eT>::MapMat(const uword in_n_rows, const uword in_n_cols)
  : n_rows (in_n_rows)
  , n_cols (in_n_cols)
  , n_elem (in_n_rows * in_n_cols)
  , map_ptr(NULL)
  {
  arma_extra_debug_sigprint_this(this);
  
  init_cold();
  }



template<typename eT>
inline
MapMat<eT>::MapMat(const SizeMat& s)
  : n_rows (s.n_rows)
  , n_cols (s.n_cols)
  , n_elem (s.n_rows * s.n_cols)
  , map_ptr(NULL)
  {
  arma_extra_debug_sigprint_this(this);
  
  init_cold();
  }



template<typename eT>
inline
MapMat<eT>::MapMat(const MapMat<eT>& x)
  : n_rows (0)
  , n_cols (0)
  , n_elem (0)
  , map_ptr(NULL)
  {
  arma_extra_debug_sigprint_this(this);
  
  init_cold();
  
  (*this).operator=(x);
  }



template<typename eT>
inline
void
MapMat<eT>::operator=(const MapMat<eT>& x)
  {
  arma_extra_debug_sigprint();
  
  if(this == &x)  { return; }
  
  access::rw(n_rows) = x.n_rows;
  access::rw(n_cols) = x.n_cols;
  access::rw(n_elem) = x.n_elem;
  
  (*map_ptr) = *(x.map_ptr);
  }



template<typename eT>
inline
MapMat<eT>::MapMat(const SpMat<eT>& x)
  : n_rows (0)
  , n_cols (0)
  , n_elem (0)
  , map_ptr(NULL)
  {
  arma_extra_debug_sigprint_this(this);
  
  init_cold();
  
  (*this).operator=(x);
  }



template<typename eT>
inline
void
MapMat<eT>::operator=(const SpMat<eT>& x)
  {
  arma_extra_debug_sigprint();
  
  const uword x_n_rows = x.n_rows;
  const uword x_n_cols = x.n_cols;
  
  (*this).zeros(x_n_rows, x_n_cols);
  
  if(x.n_nonzero == 0)  { return; }
  
  const    eT* x_values      = x.values;
  const uword* x_row_indices = x.row_indices;
  const uword* x_col_ptrs    = x.col_ptrs;
  
  map_type& map_ref = (*map_ptr);
  
  for(uword col = 0; col < x_n_cols; ++col)
    {
    const uword start = x_col_ptrs[col    ];
    const uword end   = x_col_ptrs[col + 1];
    
    for(uword i = start; i < end; ++i)
      {
      const uword row = x_row_indices[i];
      const eT    val = x_values[i];
      
      const uword index = (x_n_rows * col) + row;
      
      #if defined(ARMA_USE_CXX11)
        map_ref.emplace_hint(map_ref.cend(), index, val);
      #else
        map_ref.operator[](index) = val;
      #endif
      }
    }
  }



#if defined(ARMA_USE_CXX11)

  template<typename eT>
  inline
  MapMat<eT>::MapMat(MapMat<eT>&& x)
    : n_rows (x.n_rows )
    , n_cols (x.n_cols )
    , n_elem (x.n_elem )
    , map_ptr(x.map_ptr)
    {
    arma_extra_debug_sigprint_this(this);
    
    access::rw(x.n_rows)  = 0;
    access::rw(x.n_cols)  = 0;
    access::rw(x.n_elem)  = 0;
    access::rw(x.map_ptr) = NULL;
    }
  
  
  
  template<typename eT>
  inline
  void
  MapMat<eT>::operator=(MapMat<eT>&& x)
    {
    arma_extra_debug_sigprint();
    
    reset();
    
    if(map_ptr)  { delete map_ptr; }
    
    access::rw(n_rows)  = x.n_rows;
    access::rw(n_cols)  = x.n_cols;
    access::rw(n_elem)  = x.n_elem;
    access::rw(map_ptr) = x.map_ptr;
    
    access::rw(x.n_rows)  = 0;
    access::rw(x.n_cols)  = 0;
    access::rw(x.n_elem)  = 0;
    access::rw(x.map_ptr) = NULL;
    }

#endif



template<typename eT>
inline
void
MapMat<eT>::reset()
  {
  arma_extra_debug_sigprint();
  
  access::rw(n_rows) = 0;
  access::rw(n_cols) = 0;
  access::rw(n_elem) = 0;
  
  if((*map_ptr).empty() == false)  { (*map_ptr).clear(); }
  }



template<typename eT>
inline
void
MapMat<eT>::set_size(const uword in_n_rows)
  {
  arma_extra_debug_sigprint();
  
  init_warm(in_n_rows, 1);
  }



template<typename eT>
inline
void
MapMat<eT>::set_size(const uword in_n_rows, const uword in_n_cols)
  {
  arma_extra_debug_sigprint();
  
  init_warm(in_n_rows, in_n_cols);
  }



template<typename eT>
inline
void
MapMat<eT>::set_size(const SizeMat& s)
  {
  arma_extra_debug_sigprint();
  
  init_warm(s.n_rows, s.n_cols);
  }



template<typename eT>
inline
void
MapMat<eT>::zeros()
  {
  arma_extra_debug_sigprint();
  
  (*map_ptr).clear();
  }



template<typename eT>
inline
void
MapMat<eT>::zeros(const uword in_n_rows)
  {
  arma_extra_debug_sigprint();
  
  init_warm(in_n_rows, 1);
  
  (*map_ptr).clear();
  }



template<typename eT>
inline
void
MapMat<eT>::zeros(const uword in_n_rows, const uword in_n_cols)
  {
  arma_extra_debug_sigprint();
  
  init_warm(in_n_rows, in_n_cols);
  
  (*map_ptr).clear();
  }



template<typename eT>
inline
void
MapMat<eT>::zeros(const SizeMat& s)
  {
  arma_extra_debug_sigprint();
  
  init_warm(s.n_rows, s.n_cols);
  
  (*map_ptr).clear();
  }



template<typename eT>
inline
void
MapMat<eT>::eye()
  {
  arma_extra_debug_sigprint();
  
  (*this).eye(n_rows, n_cols);
  }



template<typename eT>
inline
void
MapMat<eT>::eye(const uword in_n_rows, const uword in_n_cols)
  {
  arma_extra_debug_sigprint();
  
  zeros(in_n_rows, in_n_cols);
  
  const uword N = (std::min)(in_n_rows, in_n_cols);
  
  map_type& map_ref = (*map_ptr);
  
  for(uword i=0; i<N; ++i)
    {
    const uword index = (in_n_rows * i) + i;
    
    #if defined(ARMA_USE_CXX11)
      map_ref.emplace_hint(map_ref.cend(), index, eT(1));
    #else
      map_ref.operator[](index) = eT(1);
    #endif
    }
  }



template<typename eT>
inline
void
MapMat<eT>::eye(const SizeMat& s)
  {
  arma_extra_debug_sigprint();
  
  (*this).eye(s.n_rows, s.n_cols);
  }



template<typename eT>
inline
void
MapMat<eT>::speye()
  {
  arma_extra_debug_sigprint();
  
  (*this).eye();
  }



template<typename eT>
inline
void
MapMat<eT>::speye(const uword in_n_rows, const uword in_n_cols)
  {
  arma_extra_debug_sigprint();
  
  (*this).eye(in_n_rows, in_n_cols);
  }



template<typename eT>
inline
void
MapMat<eT>::speye(const SizeMat& s)
  {
  arma_extra_debug_sigprint();
  
  (*this).eye(s);
  }



template<typename eT>
arma_inline
arma_warn_unused
MapMat_val<eT>
MapMat<eT>::operator[](const uword index)
  {
  return MapMat_val<eT>(*this, index);
  }



template<typename eT>
inline
arma_warn_unused
eT
MapMat<eT>::operator[](const uword index) const
  {
  map_type& map_ref = (*map_ptr);
  
  typename map_type::const_iterator it     = map_ref.find(index);
  typename map_type::const_iterator it_end = map_ref.end();
  
  return (it != it_end) ? eT((*it).second) : eT(0);
  }



template<typename eT>
arma_inline
arma_warn_unused
MapMat_val<eT>
MapMat<eT>::operator()(const uword index)
  {
  arma_debug_check( (index >= n_elem), "MapMat::operator(): index out of bounds" );
  
  return MapMat_val<eT>(*this, index);
  }



template<typename eT>
inline
arma_warn_unused
eT
MapMat<eT>::operator()(const uword index) const
  {
  arma_debug_check( (index >= n_elem), "MapMat::operator(): index out of bounds" );
  
  map_type& map_ref = (*map_ptr);
  
  typename map_type::const_iterator it     = map_ref.find(index);
  typename map_type::const_iterator it_end = map_ref.end();
  
  return (it != it_end) ? eT((*it).second) : eT(0);
  }



template<typename eT>
arma_inline
arma_warn_unused
MapMat_val<eT>
MapMat<eT>::at(const uword in_row, const uword in_col)
  {
  const uword index = (n_rows * in_col) + in_row;
  
  return MapMat_val<eT>(*this, index);
  }



template<typename eT>
inline
arma_warn_unused
eT
MapMat<eT>::at(const uword in_row, const uword in_col) const
  {
  const uword index = (n_rows * in_col) + in_row;
  
  map_type& map_ref = (*map_ptr);
  
  typename map_type::const_iterator it     = map_ref.find(index);
  typename map_type::const_iterator it_end = map_ref.end();
  
  return (it != it_end) ? eT((*it).second) : eT(0);
  }



template<typename eT>
arma_inline
arma_warn_unused
MapMat_val<eT>
MapMat<eT>::operator()(const uword in_row, const uword in_col)
  {
  arma_debug_check( ((in_row >= n_rows) || (in_col >= n_cols)), "MapMat::operator(): index out of bounds" );
  
  const uword index = (n_rows * in_col) + in_row;
  
  return MapMat_val<eT>(*this, index);
  }



template<typename eT>
inline
arma_warn_unused
eT
MapMat<eT>::operator()(const uword in_row, const uword in_col) const
  {
  arma_debug_check( ((in_row >= n_rows) || (in_col >= n_cols)), "MapMat::operator(): index out of bounds" );
  
  const uword index = (n_rows * in_col) + in_row;
  
  map_type& map_ref = (*map_ptr);
  
  typename map_type::const_iterator it     = map_ref.find(index);
  typename map_type::const_iterator it_end = map_ref.end();
  
  return (it != it_end) ? eT((*it).second) : eT(0);
  }



template<typename eT>
inline
arma_warn_unused
bool
MapMat<eT>::is_empty() const
  {
  return (n_elem == 0);
  }



template<typename eT>
inline
arma_warn_unused
bool
MapMat<eT>::is_vec() const
  {
  return ( (n_rows == 1) || (n_cols == 1) );
  }



template<typename eT>
inline
arma_warn_unused
bool
MapMat<eT>::is_rowvec() const
  {
  return (n_rows == 1);
  }



//! returns true if the object can be interpreted as a column vector
template<typename eT>
inline
arma_warn_unused
bool
MapMat<eT>::is_colvec() const
  {
  return (n_cols == 1);
  }



template<typename eT>
inline
arma_warn_unused
bool
MapMat<eT>::is_square() const
  {
  return (n_rows == n_cols);
  }



// this function is for debugging purposes only
template<typename eT>
inline
void
MapMat<eT>::sprandu(const uword in_n_rows, const uword in_n_cols, const double density)
  {
  arma_extra_debug_sigprint();
  
  zeros(in_n_rows, in_n_cols);
  
  const uword N = uword(density * double(n_elem));
  
  const Col<eT>    vals(N, fill::randu);
  const Col<uword> indx = linspace< Col<uword> >(0, ((n_elem > 0) ? uword(n_elem-1) : uword(0)) , N);
  
  const eT*    vals_mem = vals.memptr();
  const uword* indx_mem = indx.memptr();
  
  map_type& map_ref = (*map_ptr);
  
  for(uword i=0; i < N; ++i)
    {
    const uword index = indx_mem[i];
    const eT    val   = vals_mem[i];
    
    #if defined(ARMA_USE_CXX11)
      map_ref.emplace_hint(map_ref.cend(), index, val);
    #else
      map_ref.operator[](index) = val;
    #endif
    }
  }



// this function is for debugging purposes only
template<typename eT>
inline
void
MapMat<eT>::print(const std::string& extra_text) const
  {
  arma_extra_debug_sigprint();
  
  if(extra_text.length() != 0)
    {
    const std::streamsize orig_width = get_cout_stream().width();
    
    get_cout_stream() << extra_text << '\n';
    
    get_cout_stream().width(orig_width);
    }
  
  map_type& map_ref = (*map_ptr);
  
  const uword n_nonzero = uword(map_ref.size());
  
  const double density = (n_elem > 0) ? ((double(n_nonzero) / double(n_elem))*double(100)) : double(0);
  
  get_cout_stream()
    << "[matrix size: " << n_rows << 'x' << n_cols << "; n_nonzero: " << n_nonzero
    << "; density: " << density << "%]\n\n";
  
  if(n_nonzero > 0)
    {
    typename map_type::const_iterator it = map_ref.begin();

    for(uword i=0; i < n_nonzero; ++i)
      {
      const std::pair<uword, eT>& entry = (*it);
      
      const uword index = entry.first;
      const eT    val   = entry.second;
      
      const uword row = index % n_rows;
      const uword col = index / n_rows;
      
      get_cout_stream() << '(' << row << ", " << col << ") ";
      get_cout_stream() << val << '\n';
      
      ++it;
      }
    }
  
  get_cout_stream().flush();
  }



template<typename eT>
inline
uword
MapMat<eT>::get_n_nonzero() const
  {
  arma_extra_debug_sigprint();
  
  return uword((*map_ptr).size());
  }



template<typename eT>
inline
void
MapMat<eT>::get_locval_format(umat& locs, Col<eT>& vals) const
  {
  arma_extra_debug_sigprint();
  
  map_type& map_ref = (*map_ptr);
  
  typename map_type::const_iterator it = map_ref.begin();
  
  const uword N = uword(map_ref.size());
  
  locs.set_size(2,N);
  vals.set_size(N);
  
  eT* vals_mem = vals.memptr();
  
  for(uword i=0; i<N; ++i)
    {
    const std::pair<uword, eT>& entry = (*it);
    
    const uword index = entry.first;
    const eT    val   = entry.second;
    
    const uword row = index % n_rows;
    const uword col = index / n_rows;
    
    uword* locs_colptr = locs.colptr(i);
    
    locs_colptr[0] = row;
    locs_colptr[1] = col;
    
    vals_mem[i] = val;
    
    ++it;
    }
  }



template<typename eT>
inline
void
MapMat<eT>::init_cold()
  {
  arma_extra_debug_sigprint();
  
  // ensure that n_elem can hold the result of (n_rows * n_cols)
  
  #if (defined(ARMA_USE_CXX11) || defined(ARMA_64BIT_WORD))
    const char* error_message = "MapMat(): requested size is too large";
  #else
    const char* error_message = "MapMat(): requested size is too large; suggest to compile in C++11 mode or enable ARMA_64BIT_WORD";
  #endif
  
  arma_debug_check
    (
      (
      ( (n_rows > ARMA_MAX_UHWORD) || (n_cols > ARMA_MAX_UHWORD) )
        ? ( (double(n_rows) * double(n_cols)) > double(ARMA_MAX_UWORD) )
        : false
      ),
    error_message
    );
  
  map_ptr = new (std::nothrow) map_type;
  
  arma_check_bad_alloc( (map_ptr == NULL), "MapMat(): out of memory" );
  }



template<typename eT>
inline
void
MapMat<eT>::init_warm(const uword in_n_rows, const uword in_n_cols)
  {
  arma_extra_debug_sigprint();
  
  if( (n_rows == in_n_rows) && (n_cols == in_n_cols))  { return; }
  
  // ensure that n_elem can hold the result of (n_rows * n_cols)
  
  #if (defined(ARMA_USE_CXX11) || defined(ARMA_64BIT_WORD))
    const char* error_message = "MapMat(): requested size is too large";
  #else
    const char* error_message = "MapMat(): requested size is too large; suggest to compile in C++11 mode or enable ARMA_64BIT_WORD";
  #endif
  
  arma_debug_check
    (
      (
      ( (in_n_rows > ARMA_MAX_UHWORD) || (in_n_cols > ARMA_MAX_UHWORD) )
        ? ( (double(in_n_rows) * double(in_n_cols)) > double(ARMA_MAX_UWORD) )
        : false
      ),
    error_message
    );
  
  const uword new_n_elem = in_n_rows * in_n_cols;
  
  access::rw(n_rows) = in_n_rows;
  access::rw(n_cols) = in_n_cols;
  access::rw(n_elem) = new_n_elem;
  
  if(new_n_elem == 0)  { (*map_ptr).clear(); }
  }



template<typename eT>
arma_inline
void
MapMat<eT>::set_val(const uword index, const eT& in_val)
  {
  arma_extra_debug_sigprint();
  
  if(in_val != eT(0))
    {
    #if defined(ARMA_USE_CXX11)
      {
      map_type& map_ref = (*map_ptr);
      
      if( (map_ref.empty() == false) && (index > uword(map_ref.crbegin()->first)) )
        {
        map_ref.emplace_hint(map_ref.cend(), index, in_val);
        }
      else
        {
        map_ref.operator[](index) = in_val;
        }
      }
    #else
      {
      (*map_ptr).operator[](index) = in_val;
      }
    #endif
    }
  else
    {
    (*this).erase_val(index);
    }
  }



template<typename eT>
inline
void
MapMat<eT>::erase_val(const uword index)
  {
  arma_extra_debug_sigprint();
  
  map_type& map_ref = (*map_ptr);
  
  typename map_type::iterator it     = map_ref.find(index);
  typename map_type::iterator it_end = map_ref.end();
  
  if(it != it_end)  { map_ref.erase(it); }
  }






// MapMat_val



template<typename eT>
arma_inline
MapMat_val<eT>::MapMat_val(MapMat<eT>& in_parent, const uword in_index)
  : parent(in_parent)
  , index (in_index )
  {
  arma_extra_debug_sigprint();
  }



template<typename eT>
arma_inline
MapMat_val<eT>::operator eT() const
  {
  arma_extra_debug_sigprint();
  
  const MapMat<eT>& const_parent = parent;
  
  return const_parent.operator[](index);
  }



template<typename eT>
arma_inline
typename get_pod_type<eT>::result
MapMat_val<eT>::real() const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const MapMat<eT>& const_parent = parent;
  
  return T( access::tmp_real( const_parent.operator[](index) ) );
  }



template<typename eT>
arma_inline
typename get_pod_type<eT>::result
MapMat_val<eT>::imag() const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const MapMat<eT>& const_parent = parent;
  
  return T( access::tmp_imag( const_parent.operator[](index) ) );
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator=(const MapMat_val<eT>& x)
  {
  arma_extra_debug_sigprint();
  
  const eT in_val = eT(x);
  
  parent.set_val(index, in_val);
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  parent.set_val(index, in_val);
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator+=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  typename MapMat<eT>::map_type& map_ref = *(parent.map_ptr);
  
  if(in_val != eT(0))
    {
    eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
    
    val += in_val;
    
    if(val == eT(0))  { map_ref.erase(index); }
    }
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator-=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  typename MapMat<eT>::map_type& map_ref = *(parent.map_ptr);
  
  if(in_val != eT(0))
    {
    eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
    
    val -= in_val;
    
    if(val == eT(0))  { map_ref.erase(index); }
    }
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator*=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  typename MapMat<eT>::map_type& map_ref = *(parent.map_ptr);
  
  typename MapMat<eT>::map_type::iterator it     = map_ref.find(index);
  typename MapMat<eT>::map_type::iterator it_end = map_ref.end();
  
  if(it != it_end)
    {
    if(in_val != eT(0))
      {
      eT& val = (*it).second;
      
      val *= in_val;
      
      if(val == eT(0))  { map_ref.erase(it); }
      }
    else
      {
      map_ref.erase(it);
      }
    }
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator/=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  typename MapMat<eT>::map_type& map_ref = *(parent.map_ptr);
  
  typename MapMat<eT>::map_type::iterator it     = map_ref.find(index);
  typename MapMat<eT>::map_type::iterator it_end = map_ref.end();
  
  if(it != it_end)
    {
    eT& val = (*it).second;
    
    val /= in_val;
    
    if(val == eT(0))  { map_ref.erase(it); }
    }
  else
    {
    // silly operation, but included for completness
    
    const eT val = eT(0) / in_val;
    
    if(val != eT(0))  { parent.set_val(index, val); }
    }
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator++()
  {
  arma_extra_debug_sigprint();
  
  typename MapMat<eT>::map_type& map_ref = *(parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  val += eT(1);  // can't use ++,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator++(int)
  {
  arma_extra_debug_sigprint();
  
  (*this).operator++();
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator--()
  {
  arma_extra_debug_sigprint();
  
  typename MapMat<eT>::map_type& map_ref = *(parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  val -= eT(1);  // can't use --,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  }



template<typename eT>
arma_inline
void
MapMat_val<eT>::operator--(int)
  {
  arma_extra_debug_sigprint();
  
  (*this).operator--();
  }





// SpMat_MapMat_val



template<typename eT>
arma_inline
SpMat_MapMat_val<eT>::SpMat_MapMat_val(SpMat<eT>& in_s_parent, MapMat<eT>& in_m_parent, const uword in_row, const uword in_col)
  : s_parent(in_s_parent)
  , m_parent(in_m_parent)
  , row     (in_row     )
  , col     (in_col     )
  {
  arma_extra_debug_sigprint();
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>::operator eT() const
  {
  arma_extra_debug_sigprint();
  
  const SpMat<eT>& const_s_parent = s_parent;  // declare as const for clarity of intent
  
  return const_s_parent.get_value(row,col);
  }



template<typename eT>
inline
typename get_pod_type<eT>::result
SpMat_MapMat_val<eT>::real() const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const SpMat<eT>& const_s_parent = s_parent;  // declare as const for clarity of intent
  
  return T( access::tmp_real( const_s_parent.get_value(row,col) ) );
  }



template<typename eT>
inline
typename get_pod_type<eT>::result
SpMat_MapMat_val<eT>::imag() const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const SpMat<eT>& const_s_parent = s_parent;  // declare as const for clarity of intent
  
  return T( access::tmp_imag( const_s_parent.get_value(row,col) ) );
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator=(const SpMat_MapMat_val<eT>& x)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const eT    in_val = eT(x);
  const uword index  = (m_parent.n_rows * col) + row;
  
  m_parent.set_val(index, in_val);
  
  s_parent.sync_state = 1;
  access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
  
  return *this;
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  m_parent.set_val(index, in_val);
  
  s_parent.sync_state = 1;
  access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
 
  return *this;
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator+=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  if(in_val != eT(0))
    {
    eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
    
    val += in_val;
    
    if(val == eT(0))  { map_ref.erase(index); }
    
    s_parent.sync_state = 1;
    access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
    }
  
  return *this;
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator-=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  if(in_val != eT(0))
    {
    eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
    
    val -= in_val;
    
    if(val == eT(0))  { map_ref.erase(index); }
    
    s_parent.sync_state = 1;
    access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
    }
  
  return *this;
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator*=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  typename MapMat<eT>::map_type::iterator it     = map_ref.find(index);
  typename MapMat<eT>::map_type::iterator it_end = map_ref.end();
  
  if(it != it_end)
    {
    if(in_val != eT(0))
      {
      eT& val = (*it).second;
      
      val *= in_val;
      
      if(val == eT(0))  { map_ref.erase(it); }
      }
    else
      {
      map_ref.erase(it);
      }
    
    s_parent.sync_state = 1;
    access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
    }
  
  return *this;
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator/=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  typename MapMat<eT>::map_type::iterator it     = map_ref.find(index);
  typename MapMat<eT>::map_type::iterator it_end = map_ref.end();
  
  if(it != it_end)
    {
    eT& val = (*it).second;
    
    val /= in_val;
    
    if(val == eT(0))  { map_ref.erase(it); }
    
    s_parent.sync_state = 1;
    access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
    }
  else
    {
    // silly operation, but included for completness
    
    const eT val = eT(0) / in_val;
    
    if(val != eT(0))
      {
      m_parent.set_val(index, val);
      
      s_parent.sync_state = 1;
      access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
      }
    }
  
  return *this;
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator++()
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  val += eT(1);  // can't use ++,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  s_parent.sync_state = 1;
  access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
  
  return *this;
  }



template<typename eT>
inline
eT
SpMat_MapMat_val<eT>::operator++(int)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  const eT old_val = val;
  
  val += eT(1);  // can't use ++,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  s_parent.sync_state = 1;
  access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
  
  return old_val;
  }



template<typename eT>
inline
SpMat_MapMat_val<eT>&
SpMat_MapMat_val<eT>::operator--()
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  val -= eT(1);  // can't use --,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  s_parent.sync_state = 1;
  access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
  
  return *this;
  }



template<typename eT>
inline
eT
SpMat_MapMat_val<eT>::operator--(int)
  {
  arma_extra_debug_sigprint();
  
  s_parent.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  const eT old_val = val;
  
  val -= eT(1);  // can't use --,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  s_parent.sync_state = 1;
  access::rw(s_parent.n_nonzero) = m_parent.get_n_nonzero();
  
  return old_val;
  }





// SpSubview_MapMat_val



template<typename eT>
arma_inline
SpSubview_MapMat_val<eT>::SpSubview_MapMat_val(SpSubview<eT>& in_v_parent, MapMat<eT>& in_m_parent, const uword in_row, const uword in_col)
  : v_parent(in_v_parent)
  , m_parent(in_m_parent)
  , row     (in_row     )
  , col     (in_col     )
  {
  arma_extra_debug_sigprint();
  }



template<typename eT>
arma_inline
void
SpSubview_MapMat_val<eT>::update_n_nonzeros()
  {
  arma_extra_debug_sigprint();
  
  const uword old_n_nonzero = v_parent.m.n_nonzero;
  
  access::rw(v_parent.m.n_nonzero) = m_parent.get_n_nonzero();
  
       if(v_parent.m.n_nonzero > old_n_nonzero)  { access::rw(v_parent.n_nonzero)++; }
  else if(v_parent.m.n_nonzero < old_n_nonzero)  { access::rw(v_parent.n_nonzero)--; }
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>::operator eT() const
  {
  arma_extra_debug_sigprint();
  
  const SpMat<eT>& const_s_parent = v_parent.m;  // declare as const for clarity of intent
  
  return const_s_parent.get_value(row,col);
  }



template<typename eT>
inline
typename get_pod_type<eT>::result
SpSubview_MapMat_val<eT>::real() const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const SpMat<eT>& const_s_parent = v_parent.m;  // declare as const for clarity of intent
  
  return T( access::tmp_real( const_s_parent.get_value(row,col) ) );
  }



template<typename eT>
inline
typename get_pod_type<eT>::result
SpSubview_MapMat_val<eT>::imag() const
  {
  arma_extra_debug_sigprint();
  
  typedef typename get_pod_type<eT>::result T;
  
  const SpMat<eT>& const_s_parent = v_parent.m;  // declare as const for clarity of intent
  
  return T( access::tmp_imag( const_s_parent.get_value(row,col) ) );
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator=(const SpSubview_MapMat_val<eT>& x)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const eT    in_val = eT(x);
  const uword index  = (m_parent.n_rows * col) + row;
  
  m_parent.set_val(index, in_val);
  
  v_parent.m.sync_state = 1;
  update_n_nonzeros();
  
  return *this;
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  m_parent.set_val(index, in_val);
  
  v_parent.m.sync_state = 1;
  update_n_nonzeros();
  
  return *this;
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator+=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  if(in_val != eT(0))
    {
    eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
    
    val += in_val;
    
    if(val == eT(0))  { map_ref.erase(index); }
    
    v_parent.m.sync_state = 1;
    update_n_nonzeros();
    }
  
  return *this;
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator-=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  if(in_val != eT(0))
    {
    eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
    
    val -= in_val;
    
    if(val == eT(0))  { map_ref.erase(index); }
    
    v_parent.m.sync_state = 1;
    update_n_nonzeros();
    }
  
  return *this;
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator*=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  typename MapMat<eT>::map_type::iterator it     = map_ref.find(index);
  typename MapMat<eT>::map_type::iterator it_end = map_ref.end();
  
  if(it != it_end)
    {
    if(in_val != eT(0))
      {
      eT& val = (*it).second;
      
      val *= in_val;
      
      if(val == eT(0))  { map_ref.erase(it); }
      }
    else
      {
      map_ref.erase(it);
      }
    
    v_parent.m.sync_state = 1;
    update_n_nonzeros();
    }
  
  return *this;
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator/=(const eT in_val)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  typename MapMat<eT>::map_type::iterator it     = map_ref.find(index);
  typename MapMat<eT>::map_type::iterator it_end = map_ref.end();
  
  if(it != it_end)
    {
    eT& val = (*it).second;
    
    val /= in_val;
    
    if(val == eT(0))  { map_ref.erase(it); }
    
    v_parent.m.sync_state = 1;
    update_n_nonzeros();
    }
  else
    {
    // silly operation, but included for completness
    
    const eT val = eT(0) / in_val;
    
    if(val != eT(0))
      {
      m_parent.set_val(index, val);
      
      v_parent.m.sync_state = 1;
      update_n_nonzeros();
      }
    }
  
  return *this;
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator++()
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  val += eT(1);  // can't use ++,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  v_parent.m.sync_state = 1;
  update_n_nonzeros();
  
  return *this;
  }



template<typename eT>
inline
eT
SpSubview_MapMat_val<eT>::operator++(int)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  const eT old_val = val;
  
  val += eT(1);  // can't use ++,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  v_parent.m.sync_state = 1;
  update_n_nonzeros();
  
  return old_val;
  }



template<typename eT>
inline
SpSubview_MapMat_val<eT>&
SpSubview_MapMat_val<eT>::operator--()
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  val -= eT(1);  // can't use --,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  v_parent.m.sync_state = 1;
  update_n_nonzeros();
  
  return *this;
  }



template<typename eT>
inline
eT
SpSubview_MapMat_val<eT>::operator--(int)
  {
  arma_extra_debug_sigprint();
  
  v_parent.m.sync_cache();
  
  const uword index = (m_parent.n_rows * col) + row;
  
  typename MapMat<eT>::map_type& map_ref = *(m_parent.map_ptr);
  
  eT& val = map_ref.operator[](index);  // creates the element if it doesn't exist
  
  const eT old_val = val;
  
  val -= eT(1);  // can't use --,  as eT can be std::complex
  
  if(val == eT(0))  { map_ref.erase(index); }
  
  v_parent.m.sync_state = 1;
  update_n_nonzeros();
  
  return old_val;
  }



//! @}
