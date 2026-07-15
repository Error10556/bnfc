{-# LANGUAGE QuasiQuotes #-}
{-|
  Module      : BNFC.Backend.CPPVar.SWLVariant
  Description : A custom implementation of variants.

  A custom implementation of variants
  (taken from <https://github.com/groundswellaudio/swl-variant>).
-}

module BNFC.Backend.CPPVar.SWLVariant
  (
    createFiles
  ) where

import Data.String.QQ (s)
import Text.PrettyPrint (Doc)

import BNFC.Backend.Base (MkFiles, mkfile)
import BNFC.Backend.CPPVar.CPPUtil (unlinesToText)

------------------------------------------------------------------------
-- * The entrypoint.
------------------------------------------------------------------------

createFiles :: MkFiles ()
createFiles = do
  mkfile "variant.hpp" comm variantHpp
  mkfile "variant_detail.hpp" comm variantDetailHpp
  mkfile "variant_visit.hpp" comm variantVisitHpp
  where
    comm msg = unlines
      [ "/*"
      , "    " ++ msg
      , "    From https://github.com/groundswellaudio/swl-variant"
      , "    Commit 23f9929e7781be3885a62b59045e967eb410c45d"
      , "*/"
      ]

------------------------------------------------------------------------
-- * Code listings.
------------------------------------------------------------------------

-- | The main file that is to be included.
variantHpp :: Doc
variantHpp = unlinesToText [s|
/*
MIT License

Copyright (c) 2021 Jean-Baptiste Vallon Hoarau

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef SWL_CPP_LIBRARY_VARIANT_HPP
#define SWL_CPP_LIBRARY_VARIANT_HPP

#include <exception>
#include <type_traits>
#include <utility> // swap
#include <limits> // used for index_type
#include <initializer_list>

// a user-provided header for tweaks
// possible macros defined :
// SWL_VARIANT_NO_CONSTEXPR_EMPLACE
// SWL_VARIANT_NO_STD_HASH
#if __has_include("swl_variant_knobs.hpp")
	#include "swl_variant_knobs.hpp"
#endif

#ifndef SWL_VARIANT_NO_CONSTEXPR_EMPLACE
	#include <memory>
#else
	#include <new>
#endif

#ifndef SWL_VARIANT_NO_STD_HASH
	#include <functional>
#endif

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#define SWL_FWD(x) static_cast<decltype(x)&&>(x)
#define SWL_MOV(x) static_cast< std::remove_reference_t<decltype(x)>&& >(x)

#ifdef SWL_VARIANT_DEBUG
	#include <iostream>
	#define DebugAssert(X) if (not (X)) std::cout << "Variant : assertion failed : [" << #X << "] at line : " << __LINE__ << "\n";
	#undef SWL_VARIANT_DEBUG
#else
	#define DebugAssert(X)
#endif

namespace swl {

class bad_variant_access  final : std::exception {
	const char* message = ""; // llvm test requires a well formed what() on default init
	public :
	bad_variant_access(const char* str) noexcept : message{str} {}
	bad_variant_access() noexcept = default;
	bad_variant_access(const bad_variant_access&) noexcept = default;
	bad_variant_access& operator= (const bad_variant_access&) noexcept = default;
	const char* what() const noexcept override { return message; }
};

namespace vimpl {
	struct variant_tag{};
	struct emplacer_tag{};
}

template <class T>
struct in_place_type_t : private vimpl::emplacer_tag {};

template <std::size_t Index>
struct in_place_index_t : private vimpl::emplacer_tag {};

template <std::size_t Index>
inline static constexpr in_place_index_t<Index> in_place_index;

template <class T>
inline static constexpr in_place_type_t<T> in_place_type;

namespace vimpl {
	#include "variant_detail.hpp"
	#include "variant_visit.hpp"

	struct variant_npos_t {
		template <class T>
		constexpr bool operator==(T idx) const noexcept { return idx == std::numeric_limits<T>::max(); }
	};
}

template <class T>
inline constexpr bool is_variant_like = std::is_base_of_v<vimpl::variant_tag, std::decay_t<T>>;

template <class Fn, class V>
  requires (is_variant_like<V>)
constexpr decltype(auto) visit_with_index(Fn&& fn, V&& v);

inline static constexpr vimpl::variant_npos_t variant_npos;

template <class... Ts>
class variant;

// ill-formed variant, an empty specialization prevents some really bad errors messages on gcc
template <class... Ts>
	requires (
		(std::is_array_v<Ts> || ...)
		|| (std::is_reference_v<Ts> || ...)
		|| (std::is_void_v<Ts> || ...)
		|| sizeof...(Ts) == 0
	)
class variant<Ts...> {
	static_assert( sizeof...(Ts) > 0, "A variant cannot be empty.");
	static_assert( not (std::is_reference_v<Ts> || ...), "A variant cannot contain references, consider using reference wrappers instead." );
	static_assert( not (std::is_void_v<Ts> || ...), "A variant cannot contains void." );
	static_assert( not (std::is_array_v<Ts> || ...), "A variant cannot contain a raw array type, consider using std::array instead." );
};

template <class... Ts>
class variant : private vimpl::variant_tag {

	using storage_t = vimpl::union_node<false, vimpl::make_tree_union<Ts...>, vimpl::dummy_type>;

	static constexpr bool is_trivial           = std::is_trivial_v<storage_t>;
	static constexpr bool has_copy_ctor        = std::is_copy_constructible_v<storage_t>;
	static constexpr bool trivial_copy_ctor    = is_trivial || std::is_trivially_copy_constructible_v<storage_t>;
	static constexpr bool has_copy_assign      = std::is_copy_constructible_v<storage_t>;
	static constexpr bool trivial_copy_assign  = is_trivial || std::is_trivially_copy_assignable_v<storage_t>;
	static constexpr bool has_move_ctor        = std::is_move_constructible_v<storage_t>;
	static constexpr bool trivial_move_ctor    = is_trivial || std::is_trivially_move_constructible_v<storage_t>;
	static constexpr bool has_move_assign      = std::is_move_assignable_v<storage_t>;
	static constexpr bool trivial_move_assign  = is_trivial || std::is_trivially_move_assignable_v<storage_t>;
	static constexpr bool trivial_dtor         = std::is_trivially_destructible_v<storage_t>;

	public :

	template <std::size_t Idx>
	using alternative = std::remove_reference_t< decltype( std::declval<storage_t&>().template get<Idx>() ) >;

	static constexpr bool can_be_valueless = not is_trivial;

	static constexpr unsigned size = sizeof...(Ts);

	using index_type = vimpl::smallest_suitable_integer_type<sizeof...(Ts) + can_be_valueless, unsigned char, unsigned short, unsigned>;

	static constexpr index_type npos = -1;

	template <class T>
	static constexpr int index_of = vimpl::find_first_true( {std::is_same_v<T, Ts>...} );

	// ============================================= constructors (20.7.3.2)

	// default constructor
	constexpr variant()
		noexcept (std::is_nothrow_default_constructible_v<alternative<0>>)
		requires std::is_default_constructible_v<alternative<0>>
	: storage{ in_place_index<0> }, current{0}
	{}

	// copy constructor (trivial)
	constexpr variant(const variant&)
		requires trivial_copy_ctor
	= default;

	// note : both the copy and move constructor cannot be meaningfully constexpr without std::construct_at
	// copy constructor
	constexpr variant(const variant& o)
		requires (has_copy_ctor and not trivial_copy_ctor)
	: storage{ vimpl::dummy_type{} } {
		construct_from(o);
	}

	// move constructor (trivial)
	constexpr variant(variant&&)
		requires trivial_move_ctor
	= default;

	// move constructor
	constexpr variant(variant&& o)
		noexcept ((std::is_nothrow_move_constructible_v<Ts> && ...))
		requires (has_move_ctor and not trivial_move_ctor)
	: storage{ vimpl::dummy_type{} } {
		construct_from(static_cast<variant&&>(o));
	}

	// generic constructor
	template <class T, class M = vimpl::best_overload_match<T&&, Ts...>, class D = std::decay_t<T>>
	constexpr variant(T&& t)
		noexcept ( std::is_nothrow_constructible_v<M, T&&> )
		requires ( not std::is_same_v<D, variant> and not std::is_base_of_v<vimpl::emplacer_tag, D> )
	: variant{ in_place_index< index_of<M> >, static_cast<T&&>(t) }
	{}

	// construct at index
	template <std::size_t Index, class... Args>
		requires (Index < size && std::is_constructible_v<alternative<Index>, Args&&...>)
	explicit constexpr variant(in_place_index_t<Index> tag, Args&&... args)
	: storage{tag, static_cast<Args&&>(args)...}, current(Index)
	{}

	// construct a given type
	template <class T, class... Args>
		requires (vimpl::appears_exactly_once<T, Ts...> && std::is_constructible_v<T, Args&&...>)
	explicit constexpr variant(in_place_type_t<T>, Args&&... args)
	: variant{ in_place_index< index_of<T> >, static_cast<Args&&>(args)... }
	{}

	// initializer-list constructors
	template <std::size_t Index, class U, class... Args>
		requires (
			(Index < size) and
			std::is_constructible_v< alternative<Index>, std::initializer_list<U>&, Args&&... >
		)
	explicit constexpr variant (in_place_index_t<Index> tag, std::initializer_list<U> list, Args&&... args)
	: storage{ tag, list, SWL_FWD(args)... }, current{Index}
	{}

	template <class T, class U, class... Args>
		requires (
			vimpl::appears_exactly_once<T, Ts...>
			&& std::is_constructible_v< T, std::initializer_list<U>&, Args&&... >
		)
	explicit constexpr variant (in_place_type_t<T>, std::initializer_list<U> list, Args&&... args)
	: storage{ in_place_index<index_of<T>>, list, SWL_FWD(args)... }, current{index_of<T>}
	{}

	// ================================ destructors (20.7.3.3)

	constexpr ~variant() requires trivial_dtor = default;

	constexpr ~variant() requires (not trivial_dtor) {
		reset();
	}

	// ================================ assignment (20.7.3.4)

	// copy assignment (trivial)
	constexpr variant& operator=(const variant& o)
		requires trivial_copy_assign && trivial_copy_ctor
	= default;

	// copy assignment
	constexpr variant& operator=(const variant& rhs)
		requires (has_copy_assign and not(trivial_copy_assign && trivial_copy_ctor))
	{
		assign_from(rhs, [this] (const auto& elem, auto index_cst) {
			if (index() == index_cst)
				unsafe_get<index_cst>() = elem;
			else{
				using type = alternative<index_cst>;
				if constexpr (std::is_nothrow_copy_constructible_v<type>
								or not std::is_nothrow_move_constructible_v<type>)
					this->emplace<index_cst>(elem);
				else{
					alternative<index_cst> tmp = elem;
					this->emplace<index_cst>( SWL_MOV(tmp) );
				}
			}
		});
		return *this;
	}

	// move assignment (trivial)
	constexpr variant& operator=(variant&& o)
		requires (trivial_move_assign and trivial_move_ctor and trivial_dtor)
	= default;

	// move assignment
	constexpr variant& operator=(variant&& o)
		noexcept ((std::is_nothrow_move_constructible_v<Ts> && ...) && (std::is_nothrow_move_assignable_v<Ts> && ...))
		requires (has_move_assign && has_move_ctor and not(trivial_move_assign and trivial_move_ctor and trivial_dtor))
	{
		this->assign_from( SWL_FWD(o), [this] (auto&& elem, auto index_cst)
		{
			if (index() == index_cst)
				this->unsafe_get<index_cst>() = SWL_MOV(elem);
			else
				this->emplace<index_cst>( SWL_MOV(elem) );
		});
		return *this;
	}

	// generic assignment
	template <class T>
		requires vimpl::has_non_ambiguous_match<T, Ts...>
	constexpr variant& operator=(T&& t)
		noexcept( std::is_nothrow_assignable_v<vimpl::best_overload_match<T&&, Ts...>, T&&>
				  && std::is_nothrow_constructible_v<vimpl::best_overload_match<T&&, Ts...>, T&&> )
	{
		using related_type = vimpl::best_overload_match<T&&, Ts...>;
		constexpr auto new_index = index_of<related_type>;

		if (this->current == new_index)
			this->unsafe_get<new_index>() = SWL_FWD(t);
		else {

			constexpr bool do_simple_emplace =
				std::is_nothrow_constructible_v<related_type, T>
				or not std::is_nothrow_move_constructible_v<related_type>;

			if constexpr ( do_simple_emplace )
				this->emplace<new_index>( SWL_FWD(t) );
			else {
				related_type tmp = t;
				this->emplace<new_index>( SWL_MOV(tmp) );
			}
		}

		return *this;
	}

	// ================================== modifiers (20.7.3.5)

	template <class T, class... Args>
		requires (std::is_constructible_v<T, Args&&...> && vimpl::appears_exactly_once<T, Ts...>)
	constexpr T& emplace(Args&&... args){
		return emplace<index_of<T>>(static_cast<Args&&>(args)...);
	}

	template <std::size_t Idx, class... Args>
		requires (Idx < size and std::is_constructible_v<alternative<Idx>, Args&&...>  )
	constexpr auto& emplace(Args&&... args){
		return emplace_impl<Idx>(SWL_FWD(args)...);
	}

	// emplace with initializer-lists
	template <std::size_t Idx, class U, class... Args>
		requires ( Idx < size
			&& std::is_constructible_v<alternative<Idx>, std::initializer_list<U>&, Args&&...> )
	constexpr auto& emplace(std::initializer_list<U> list, Args&&... args){
		return emplace_impl<Idx>(list, SWL_FWD(args)...);
	}

	template <class T, class U, class... Args>
		requires ( std::is_constructible_v<T, std::initializer_list<U>&, Args&&...>
				   && vimpl::appears_exactly_once<T, Ts...> )
	constexpr T& emplace(std::initializer_list<U> list, Args&&... args){
		return emplace_impl<index_of<T>>( list, SWL_FWD(args)... );
	}

	// ==================================== value status (20.7.3.6)

	constexpr bool valueless_by_exception() const noexcept {
		if constexpr ( can_be_valueless )
			return current == npos;
		else return false;
	}

	constexpr index_type index() const noexcept {
		return current;
	}

	// =================================== swap (20.7.3.7)

	constexpr void swap(variant& o)
		noexcept ( (std::is_nothrow_move_constructible_v<Ts> && ...)
				   && (vimpl::swap_trait::template nothrow<Ts> && ...) )
		requires (has_move_ctor && (vimpl::swap_trait::template able<Ts> && ...))
	{

		if constexpr (can_be_valueless){
			// if one is valueless, move the element form the non-empty variant,
			// reset it, and set it to valueless
			constexpr auto impl_one_valueless = [] (auto&& full, auto& empty) {
				swl::visit_with_index( vimpl::emplace_no_dtor_from_elem<variant&>{empty}, SWL_FWD(full) );
				full.reset_no_check();
				full.current = npos;
			};

			switch( static_cast<int>(this->index() == npos) + static_cast<int>(o.index() == npos) * 2 )
			{
				case 0 :
					break;
				case 1 :
					// "this" is valueless
					impl_one_valueless( SWL_MOV(o), *this );
					return;
				case 2 :
					// "other" is valueless
					impl_one_valueless( SWL_MOV(*this), o );
					return;
				case 3 :
					// both are valueless, do nothing
					return;
			}
		}

		DebugAssert( not (valueless_by_exception() && o.valueless_by_exception()) );

		swl::visit_with_index( [&o, this] (auto&& elem, auto index_) {

			if (this->index() == index_){
				using std::swap;
				swap( this->unsafe_get<index_>(), elem );
				return;
			}

			using idx_t = decltype(index_);
			swl::visit_with_index([this, &o, &elem] (auto&& this_elem, auto this_index) {

				auto tmp { SWL_MOV(this_elem) };

				// destruct the element
				vimpl::destruct<alternative<this_index>>(this_elem);

				// ok, we just destroyed the element in this, don't call the dtor again
				this->emplace_no_dtor<idx_t::value>( SWL_MOV(elem) );

				// we could refactor this
				vimpl::destruct<alternative<idx_t::value>>(elem);
				o.template emplace_no_dtor< (unsigned)(this_index) >( SWL_MOV(tmp) );

			}, *this);
		}, o);
	}

	// +================================== methods for internal use
	// these methods performs no errors checking at all

	template <vimpl::union_index_t Idx>
	constexpr auto& unsafe_get() & noexcept	{
		static_assert(Idx < size);
		DebugAssert(current == Idx);
		return storage.template get<Idx>();
	}

	template <vimpl::union_index_t Idx>
	constexpr auto&& unsafe_get() && noexcept {
		static_assert(Idx < size);
		DebugAssert(current == Idx);
		return SWL_MOV( storage.template get<Idx>() );
	}

	template <vimpl::union_index_t Idx>
	constexpr const auto& unsafe_get() const & noexcept {
		static_assert(Idx < size);
		DebugAssert(current == Idx);
		return const_cast<variant&>(*this).unsafe_get<Idx>();
	}

	template <vimpl::union_index_t Idx>
	constexpr const auto&& unsafe_get() const && noexcept {
		static_assert(Idx < size);
		DebugAssert(current == Idx);
		return SWL_MOV(unsafe_get<Idx>());
	}

	private :

	// assign from another variant
	template <class Other, class Fn>
	constexpr void assign_from(Other&& o, Fn&& fn){
		if constexpr (can_be_valueless){
			if (o.index() == npos){
				if (current != npos){
					reset_no_check();
					current = npos;
				}
				return;
			}
		}
		DebugAssert(not o.valueless_by_exception());
		swl::visit_with_index( SWL_FWD(fn), SWL_FWD(o) );
	}

	template <unsigned Idx, class... Args>
	constexpr auto& emplace_impl(Args&&... args)
	{
		reset();
		emplace_no_dtor<Idx>( SWL_FWD(args)... );
		return unsafe_get<Idx>();
	}

	// can be used directly only when the variant is in a known empty state
	template <unsigned Idx, class... Args>
	constexpr void emplace_no_dtor(Args&&... args)
	{
		using T = alternative<Idx>;

		if constexpr ( not std::is_nothrow_constructible_v<T, Args&&...> )
		{
			if constexpr ( std::is_nothrow_move_constructible_v<T> )
				do_emplace_no_dtor<Idx>( T{SWL_FWD(args)...} );
			else if constexpr ( std::is_nothrow_copy_constructible_v<T> )
			{
				T tmp {SWL_FWD(args)...};
				do_emplace_no_dtor<Idx>( tmp );
			}
			else
			{
				static_assert( can_be_valueless,  // EDIT: removed `&& (Idx == Idx)`
					"Internal error : the possibly valueless branch of emplace was taken despite |can_be_valueless| being false");
				current = npos;
				do_emplace_no_dtor<Idx>( SWL_FWD(args)... );
			}
		}
		else
			do_emplace_no_dtor<Idx>( SWL_FWD(args)... );
	}

	template <unsigned Idx, class... Args>
	constexpr void do_emplace_no_dtor(Args&&... args)
	{
		auto* ptr = vimpl::addressof( unsafe_get<Idx>() );

		#ifdef SWL_VARIANT_NO_CONSTEXPR_EMPLACE
			using T = alternative<Idx>;
			new ((void*)(ptr)) T ( SWL_FWD(args)... );
		#else
			std::construct_at( ptr, SWL_FWD(args)... );
		#endif

		current = static_cast<index_type>(Idx);
	}

	// destroy the current elem IFF not valueless
	constexpr void reset() {
		if constexpr (can_be_valueless)
			if (valueless_by_exception()) return;
		reset_no_check();
	}

	// destroy the current element without checking for valueless
	constexpr void reset_no_check(){
		DebugAssert( index() < size );
		if constexpr ( not trivial_dtor ){
			swl::visit_with_index( [] (auto& elem, auto index_) {
				vimpl::destruct<alternative<index_>>(elem);
			}, *this);
		}
	}

	// construct this from another variant, for constructors only
	template <class Other>
	constexpr void construct_from(Other&& o){
		if constexpr (can_be_valueless)
			if (o.valueless_by_exception()){
				current = npos;
				return;
			}

		swl::visit_with_index( vimpl::emplace_no_dtor_from_elem<variant&>{*this}, SWL_FWD(o) );
	}

	template <class T>
	friend struct vimpl::emplace_no_dtor_from_elem;

	storage_t storage;
	index_type current;
};

// ================================= value access (20.7.5)

template <class T, class... Ts>
constexpr bool holds_alternative(const variant<Ts...>& v) noexcept {
	static_assert( (std::is_same_v<T, Ts> || ...), "Requested type is not contained in the variant" );
	constexpr auto Index = variant<Ts...>::template index_of<T>;
	return v.index() == Index;
}

// ========= get by index

template <std::size_t Idx, class... Ts>
constexpr auto& get (variant<Ts...>& v){
	static_assert( Idx < sizeof...(Ts), "Index exceeds the variant size. ");
	if (v.index() != Idx) throw bad_variant_access{"swl::variant : Bad variant access in get."};
	return (v.template unsafe_get<Idx>());
}

template <std::size_t Idx, class... Ts>
constexpr const auto& get (const variant<Ts...>& v){
	return swl::get<Idx>(const_cast<variant<Ts...>&>(v));
}

template <std::size_t Idx, class... Ts>
constexpr auto&& get (variant<Ts...>&& v){
	return SWL_MOV( swl::get<Idx>(v) );
}

template <std::size_t Idx, class... Ts>
constexpr const auto&& get (const variant<Ts...>&& v){
	return SWL_MOV( swl::get<Idx>(v) );
}

// ========= get by type

template <class T, class... Ts>
constexpr T& get (variant<Ts...>& v){
	return swl::get< variant<Ts...>::template index_of<T> >(v);
}

template <class T, class... Ts>
constexpr const T& get (const variant<Ts...>& v){
	return swl::get< variant<Ts...>::template index_of<T> >(v);
}

template <class T, class... Ts>
constexpr T&& get (variant<Ts...>&& v){
	return swl::get< variant<Ts...>::template index_of<T> >( SWL_FWD(v) );
}

template <class T, class... Ts>
constexpr const T&& get (const variant<Ts...>&& v){
	return swl::get< variant<Ts...>::template index_of<T> >( SWL_FWD(v) );
}

// ===== get_if by index

template <std::size_t Idx, class... Ts>
constexpr const auto* get_if(const variant<Ts...>* v) noexcept {
	using rtype = typename variant<Ts...>::template alternative<Idx>*;
	if (v == nullptr || v->index() != Idx)
		return rtype{nullptr};
	else
		return vimpl::addressof( v->template unsafe_get<Idx>() );
}

template <std::size_t Idx, class... Ts>
constexpr auto* get_if(variant<Ts...>* v) noexcept {
	using rtype = typename variant<Ts...>::template alternative<Idx>;
	return const_cast<rtype*>(
		swl::get_if<Idx>( static_cast<const variant<Ts...>*>(v) )
	);
}

// ====== get_if by type

template <class T, class... Ts>
constexpr T* get_if(variant<Ts...>* v) noexcept {
	static_assert( (std::is_same_v<T, Ts> || ...), "Requested type is not contained in the variant" );
	return swl::get_if< variant<Ts...>::template index_of<T> >(v);
}

template <class T, class... Ts>
constexpr const T* get_if(const variant<Ts...>* v) noexcept {
	static_assert( (std::is_same_v<T, Ts> || ...), "Requested type is not contained in the variant" );
	return swl::get_if< variant<Ts...>::template index_of<T> >(v);
}

// =============================== visitation (20.7.7)

template <class Fn, class... Vs>
constexpr decltype(auto) visit(Fn&& fn, Vs&&... vs){
	if constexpr ( (std::decay_t<Vs>::can_be_valueless || ...) )
		if ( (vs.valueless_by_exception() || ...) )
			throw bad_variant_access{"swl::variant : Bad variant access in visit."};

	if constexpr (sizeof...(Vs) == 1)
		return vimpl::visit( SWL_FWD(fn), SWL_FWD(vs)...);
	else
		return vimpl::multi_visit( SWL_FWD(fn), SWL_FWD(vs)...);
}

template <class Fn>
constexpr decltype(auto) visit(Fn&& fn){
  return SWL_FWD(fn)();
}

template <class R, class Fn, class... Vs>
	requires (is_variant_like<Vs> && ...)
constexpr R visit(Fn&& fn, Vs&&... vars){
  return static_cast<R>( swl::visit( SWL_FWD(fn), SWL_FWD(vars)...) );
}

template <class Fn, class V>
  requires (is_variant_like<V>)
constexpr decltype(auto) visit_with_index(Fn&& fn, V&& v) {
  return vimpl::single_visit_w_index_tail<0, vimpl::rtype_index_visit<Fn&&, V&&>>(SWL_FWD(fn), SWL_FWD(v));
}

// ============================== relational operators (20.7.6)

template <class... Ts>
	requires ( vimpl::has_eq_comp<Ts> && ... )
constexpr bool operator==(const variant<Ts...>& v1, const variant<Ts...>& v2){
	if (v1.index() != v2.index())
		return false;
	if constexpr (variant<Ts...>::can_be_valueless)
		if (v1.valueless_by_exception()) return true;
	return swl::visit_with_index( [&v1] (auto& elem, auto index) -> bool {
		return (v1.template unsafe_get<index>() == elem);
	}, v2);
}

template <class... Ts>
constexpr bool operator!=(const variant<Ts...>& v1, const variant<Ts...>& v2)
	requires requires { v1 == v2; }
{
	return not(v1 == v2);
}

template <class... Ts>
	requires ( vimpl::has_lesser_comp<const Ts&> && ... )
constexpr bool operator<(const variant<Ts...>& v1, const variant<Ts...>& v2){
	if constexpr (variant<Ts...>::can_be_valueless){
		if (v2.valueless_by_exception()) return false;
		if (v1.valueless_by_exception()) return true;
	}
	if ( v1.index() == v2.index() ){
		return swl::visit_with_index( [&v2] (auto& elem, auto index) -> bool {
			return (elem < v2.template unsafe_get<index>());
		}, v1);
	}
	else
		return (v1.index() < v2.index());
}

template <class... Ts>
constexpr bool operator>(const variant<Ts...>& v1, const variant<Ts...>& v2)
	requires requires { v2 < v1; }
{
	return v2 < v1;
}

template <class... Ts>
	requires ( vimpl::has_less_or_eq_comp<const Ts&> && ... )
constexpr bool operator<=(const variant<Ts...>& v1, const variant<Ts...>& v2){
	if constexpr (variant<Ts...>::can_be_valueless){
		if (v1.valueless_by_exception()) return true;
		if (v2.valueless_by_exception()) return false;
	}
	if ( v1.index() == v2.index() ){
		return swl::visit_with_index( [&v2] (auto& elem, auto index) -> bool {
			return (elem <= v2.template unsafe_get<index>());
		}, v1);
	}
	else
		return (v1.index() < v2.index());
}

template <class... Ts>
constexpr bool operator>=(const variant<Ts...>& v1, const variant<Ts...>& v2)
	requires requires { v2 <= v1; }
{
	return v2 <= v1;
}

// ===================================== monostate (20.7.8, 20.7.9)

struct monostate{};
constexpr bool operator==(monostate, monostate) noexcept { return true; }
constexpr bool operator> (monostate, monostate) noexcept { return false; }
constexpr bool operator< (monostate, monostate) noexcept { return false; }
constexpr bool operator<=(monostate, monostate) noexcept { return true; }
constexpr bool operator>=(monostate, monostate) noexcept { return true; }

// ===================================== specialized algorithms (20.7.10)

template <class... Ts>
constexpr void swap(variant<Ts...>& a, variant<Ts...>& b)
	noexcept (noexcept(a.swap(b)))
	requires requires { a.swap(b); }
{
	a.swap(b);
}

// ===================================== helper classes (20.7.4)

template <class T>
	requires is_variant_like<T>
inline constexpr std::size_t variant_size_v = std::decay_t<T>::size;

// not sure why anyone would need this, i'm adding it anyway
template <class T>
	requires is_variant_like<T>
struct variant_size : std::integral_constant<std::size_t, variant_size_v<T>> {};

namespace vimpl {
	// ugh, we have to take care of volatile here
	template <bool IsVolatile>
	struct var_alt_impl {
		template <std::size_t Idx, class T>
		using type = std::remove_reference_t<decltype( std::declval<T>().template unsafe_get<Idx>() )>;
	};

	template <>
	struct var_alt_impl<true> {
		template <std::size_t Idx, class T>
		using type = volatile typename var_alt_impl<false>::template type<Idx, std::remove_volatile_t<T>>;
	};
}

template <std::size_t Idx, class T>
	requires (Idx < variant_size_v<T>)
using variant_alternative_t = typename vimpl::var_alt_impl< std::is_volatile_v<T> >::template type<Idx, T>;

template <std::size_t Idx, class T>
	requires is_variant_like<T>
struct variant_alternative {
	using type = variant_alternative_t<Idx, T>;
};

// ===================================== extensions (unsafe_get)

template <std::size_t Idx, class Var>
	requires is_variant_like<Var>
constexpr auto&& unsafe_get(Var&& var) noexcept {
	static_assert( Idx < std::decay_t<Var>::size, "Index exceeds the variant size." );
	return SWL_FWD(var).template unsafe_get<Idx>();
}

template <class T, class Var>
	requires is_variant_like<Var>
constexpr auto&& unsafe_get(Var&& var) noexcept {
	return swl::unsafe_get< std::decay_t<Var>::template index_of<T> >( SWL_FWD(var) );
}

} // SWL

// ====================================== hash support (20.7.12)
#ifndef SWL_VARIANT_NO_STD_HASH

	namespace std {
		template <class... Ts>
			requires (swl::vimpl::has_std_hash<Ts> && ...)
		struct hash<swl::variant<Ts...>> {
			std::size_t operator()(const swl::variant<Ts...>& v) const {
				if constexpr ( swl::variant<Ts...>::can_be_valueless )
					if (v.valueless_by_exception()) return -1;

				return swl::visit_with_index( [] (auto& elem, auto index_) {
					using type = std::remove_cvref_t<decltype(elem)>;
					return std::hash<type>{}(elem) + index_;
				}, v);
			}
		};

		template <>
		struct hash<swl::monostate> {
			constexpr std::size_t operator()(swl::monostate) const noexcept { return -1; }
		};
	}
#else
	#undef SWL_VARIANT_NO_STD_HASH
#endif // std-hash

#undef DebugAssert
#undef SWL_FWD
#undef SWL_MOV

#ifdef SWL_VARIANT_NO_CONSTEXPR_EMPLACE
	#undef SWL_VARIANT_NO_CONSTEXPR_EMPLACE
#endif

#endif // eof
|]

variantDetailHpp :: Doc
variantDetailHpp = unlinesToText [s|
#ifdef SWL_CPP_LIBRARY_VARIANT_HPP

template <int N>
constexpr int find_first_true(bool (&&arr)[N]){
	for (int k = 0; k < N; ++k)
		if (arr[k])
			return k;
	return -1;
}

template <class T, class... Ts>
inline constexpr bool appears_exactly_once = (static_cast<unsigned short>(std::is_same_v<T, Ts>) + ...) == 1;

// ============= type pack element

#if __has_builtin(__type_pack_element)

template <std::size_t K, class... Ts>
using type_pack_element = __type_pack_element<K, Ts...>;

#else

template <unsigned char = 1>
struct find_type_i;

template <>
struct find_type_i<1> {
	template <std::size_t Idx, class T, class... Ts>
	using f = typename find_type_i<(Idx != 1)>::template f<Idx - 1, Ts...>;
};

template <>
struct find_type_i<0> {
	template <std::size_t, class T, class... Ts>
	using f = T;
};

template <std::size_t K, class... Ts>
using type_pack_element = typename find_type_i<(K != 0)>::template f<K, Ts...>;

#endif

// ============= overload match detector. to be used for variant generic assignment

template <class T>
using arr1 = T[1];

template <std::size_t N, class A>
struct overload_frag {
	using type = A;
	template <class T>
		requires requires { arr1<A>{std::declval<T>()}; }
	auto operator()(A, T&&) -> overload_frag<N, A>;
};

template <class Seq, class... Args>
struct make_overload;

template <std::size_t... Idx, class... Args>
struct make_overload<std::integer_sequence<std::size_t, Idx...>, Args...>
	 : overload_frag<Idx, Args>... {
	using overload_frag<Idx, Args>::operator()...;
};

template <class T, class... Ts>
using best_overload_match = typename decltype(
	make_overload<std::make_index_sequence<sizeof...(Ts)>, Ts...>{}
	( std::declval<T>(), std::declval<T>() )
)::type;

template <class T, class... Ts>
concept has_non_ambiguous_match =
	requires { typename best_overload_match<T, Ts...>; };

// ================================== rel ops

template <class From, class To>
concept convertible = std::is_convertible_v<From, To>;

template <class T>
concept has_eq_comp = requires (T a, T b) {
	{ a == b } -> convertible<bool>;
};

template <class T>
concept has_lesser_comp = requires (T a, T b) {
	{ a < b } -> convertible<bool>;
};

template <class T>
concept has_less_or_eq_comp = requires (T a, T b) {
	{ a <= b } -> convertible<bool>;
};

template <class A>
struct emplace_no_dtor_from_elem {
	template <class T>
	constexpr void operator()(T&& elem, auto index_) const {
		a.template emplace_no_dtor<index_>( static_cast<T&&>(elem) );
	}
	A& a;
};

template <class E, class T>
constexpr void destruct(T& obj){
	if constexpr (not std::is_trivially_destructible_v<E>)
		obj.~E();
}

// =============================== variant union types

// =================== base variant storage type
// this type is used to build a N-ary tree of union.

struct dummy_type{ static constexpr int elem_size = 0; }; // used to fill the back of union nodes

using union_index_t = unsigned;

#define TRAIT(trait) ( std::is_##trait##_v<A> && std::is_##trait##_v<B> )

#define SFM(signature, trait) \
	signature = default; \
	signature requires (TRAIT(trait) and not TRAIT(trivially_##trait)) {}

// given the two members of type A and B of an union X
// this create the proper conditionally trivial special members functions
#define INJECT_UNION_SFM(X) \
	SFM(constexpr X (const X &), copy_constructible) \
	SFM(constexpr X (X&&), move_constructible) \
	SFM(constexpr X& operator=(const X&), copy_assignable) \
	SFM(constexpr X& operator=(X&&), move_assignable) \
	SFM(constexpr ~X(), destructible)

template <bool IsLeaf>
struct node_trait;

template <>
struct node_trait<true> {

	template <class A, class B>
	static constexpr auto elem_size = not( std::is_same_v<B, dummy_type> ) ? 2 : 1;

	template <std::size_t, class>
	static constexpr char ctor_branch = 0;
};

template <>
struct node_trait<false> {
	template <class A, class B>
	static constexpr auto elem_size = A::elem_size + B::elem_size;

	template <std::size_t Index, class A>
	static constexpr char ctor_branch = (Index < A::elem_size) ? 1 : 2;
};

template <bool IsLeaf, class A, class B>
struct union_node {

	union {
		A a;
		B b;
	};

	static constexpr auto elem_size = node_trait<IsLeaf>::template elem_size<A, B>;

	constexpr union_node() = default;

	template <std::size_t Index, class... Args>
		requires (node_trait<IsLeaf>::template ctor_branch<Index, A> == 1)
	constexpr union_node(in_place_index_t<Index>, Args&&... args)
	: a{ in_place_index<Index>, static_cast<Args&&>(args)... }
	{}

	template <std::size_t Index, class... Args>
		requires (node_trait<IsLeaf>::template ctor_branch<Index, A> == 2)
	constexpr union_node(in_place_index_t<Index>, Args&&... args)
	: b{ in_place_index<Index - A::elem_size>, static_cast<Args&&>(args)... }
	{}

	template <class... Args>
		requires (IsLeaf)
	constexpr union_node(in_place_index_t<0>, Args&&... args)
	: a{static_cast<Args&&>(args)...}
	{}

	template <class... Args>
		requires (IsLeaf)
	constexpr union_node(in_place_index_t<1>, Args&&... args)
	: b{static_cast<Args&&>(args)...}
	{}

	constexpr union_node(dummy_type)
		requires (std::is_same_v<dummy_type, B>)
	: b{}
	{}

	template <union_index_t Index>
	constexpr auto& get()
	{
		if constexpr (IsLeaf)
		{
			if constexpr ( Index == 0 )
				return a;
			else
				return b;
		}
		else
		{
			if constexpr ( Index < A::elem_size )
				return a.template get<Index>();
			else
				return b.template get<Index - A::elem_size>();
		}
	}

	INJECT_UNION_SFM(union_node)
};

#undef INJECT_UNION_SFM
#undef SFM
#undef TRAIT

// =================== algorithm to build the tree of unions
// take a sequence of types and perform an order preserving fold until only one type is left
// the first parameter is the numbers of types remaining for the current pass

constexpr unsigned char pick_next(unsigned remaining){
	return remaining >= 2 ? 2 : remaining;
}

template <unsigned char Pick, unsigned char GoOn, bool FirstPass>
struct make_tree;

template <bool IsFirstPass>
struct make_tree<2, 1, IsFirstPass> {
	template <unsigned Remaining, class A, class B, class... Ts>
	using f = typename make_tree
	<
	 pick_next(Remaining - 2),
	 sizeof...(Ts) != 0,
	 IsFirstPass
	>
	::template f
	<
	 Remaining - 2,
	 Ts...,
	 union_node<IsFirstPass, A, B>
	>;
};

// only one type left, stop
template <bool F>
struct make_tree<0, 0, F> {
	template <unsigned, class A>
	using f = A;
};

// end of one pass, restart
template <bool IsFirstPass>
struct make_tree<0, 1, IsFirstPass> {
	template <unsigned Remaining, class... Ts>
	using f = typename make_tree
	<
	 pick_next(sizeof...(Ts)),
	 (sizeof...(Ts) != 1),
	 false  // <- both first pass and tail call recurse into a tail call
	>
	::template f<sizeof...(Ts), Ts...>;
};

// one odd type left in the pass, put it at the back to preserve the order
template <>
struct make_tree<1, 1, false> {
	template <unsigned Remaining, class A, class... Ts>
	using f = typename make_tree<0, sizeof...(Ts) != 0, false>
		::template f<0, Ts..., A>;
};

// one odd type left in the first pass, wrap it in an union
template <>
struct make_tree<1, 1, true> {
	template <unsigned, class A, class... Ts>
	using f = typename make_tree<0, sizeof...(Ts) != 0, false>
		::template f<0, Ts..., union_node<true, A, dummy_type>>;
};

template <class... Ts>
using make_tree_union = typename
	make_tree<pick_next(sizeof...(Ts)), 1, true>::template f<sizeof...(Ts), Ts...>;

// ============================================================

// Ts... must be sorted in ascending size
template <std::size_t Num, class... Ts>
using smallest_suitable_integer_type =
	type_pack_element<(static_cast<unsigned char>(Num > std::numeric_limits<Ts>::max()) + ...),
					  Ts...
					  >;

// why do we need this again? i think something to do with GCC?
namespace swap_trait {
	using std::swap;

	template <class A>
	concept able = requires (A a, A b) { swap(a, b); };

	template <class A>
	inline constexpr bool nothrow = noexcept( swap(std::declval<A&>(), std::declval<A&>()) );
}

#ifndef SWL_VARIANT_NO_STD_HASH
	template <class T>
	inline constexpr bool has_std_hash = requires (T t) {
		std::size_t( ::std::hash< std::remove_cvref_t<T> >{}(t) );
	};
#endif

template <class T>
inline constexpr T* addressof( T& obj ) noexcept {
	#if defined(__GNUC__) || defined( __clang__ )
		return __builtin_addressof(obj);
	#elif defined (SWL_VARIANT_NO_CONSTEXPR_EMPLACE)
		// if & is overloaded, use the ugly version
		if constexpr ( requires { obj.operator&(); } )
			return reinterpret_cast<T*>
			(&const_cast<char&>(reinterpret_cast<const volatile char&>(obj)));
		else
			return &obj;
	#else
		return std::address_of(obj);
	#endif
}

#endif // eof
|]

variantVisitHpp :: Doc
variantVisitHpp = unlinesToText [s|
#ifdef SWL_CPP_LIBRARY_VARIANT_HPP

// ========================= visit dispatcher

template <class Fn, class... Vars>
using rtype_visit = decltype( ( std::declval<Fn>()( std::declval<Vars>().template unsafe_get<0>()... ) ) );

template <class Fn, class Var>
using rtype_index_visit = decltype( ( std::declval<Fn>()( std::declval<Var>().template unsafe_get<0>(),
								 	  std::integral_constant<std::size_t, 0>{} ) )
								  );

inline namespace v1 {

#if defined(__GNUC__) || defined( __clang__ ) || defined( __INTEL_COMPILER )
	#define DeclareUnreachable() __builtin_unreachable()
#elif defined (_MSC_VER)
	#define DeclareUnreachable() __assume(false)
#else
	#error "Compiler not supported, please file an issue."
#endif

#define DEC(N) X((N)) X((N) + 1) X((N) + 2) X((N) + 3) X((N) + 4) X((N) + 5) X((N) + 6) X((N) + 7) X((N) + 8) X((N) + 9)

#define SEQ30(N) DEC( (N) + 0 ) DEC( (N) + 10 ) DEC( (N) + 20 )
#define SEQ100(N) SEQ30((N) + 0) SEQ30((N) + 30) SEQ30((N) + 60) DEC((N) + 90)
#define SEQ200(N) SEQ100((N) + 0) SEQ100((N) + 100)
#define SEQ400(N) SEQ200((N) + 0) SEQ200((N) + 200)
#define CAT(M, N) M##N
#define CAT2(M, N) CAT(M, N)
#define INJECTSEQ(N) CAT2(SEQ, N)(0)

// single-visitation

template <unsigned Offset, class Rtype, class Fn, class V>
constexpr Rtype single_visit_tail(Fn&& fn, V&& v){

	constexpr auto RemainingIndex = std::decay_t<V>::size - Offset;

	#define X(N) case (N + Offset) : \
		if constexpr (N < RemainingIndex) { \
			return static_cast<Fn&&>(fn)( static_cast<V&&>(v).template unsafe_get<N+Offset>() ); \
			break; \
		} else DeclareUnreachable();

	#define SEQSIZE 200

	switch( v.index() ){

		INJECTSEQ(SEQSIZE)

		default :
			if constexpr (SEQSIZE < RemainingIndex)
				return vimpl::single_visit_tail<Offset + SEQSIZE, Rtype>(static_cast<Fn&&>(fn), static_cast<V&&>(v));
			else
				DeclareUnreachable();
	}

	#undef X
	#undef SEQSIZE
}

template <unsigned Offset, class Rtype, class Fn, class V>
constexpr Rtype single_visit_w_index_tail(Fn&& fn, V&& v){

	constexpr auto RemainingIndex = std::decay_t<V>::size - Offset;

	#define X(N) case (N + Offset) : \
		if constexpr (N < RemainingIndex) { \
			return static_cast<Fn&&>(fn)( static_cast<V&&>(v).template unsafe_get<N+Offset>(), std::integral_constant<unsigned, N+Offset>{} ); \
			break; \
		} else DeclareUnreachable();

	#define SEQSIZE 200

	switch( v.index() ){

		INJECTSEQ(SEQSIZE)

		default :
			if constexpr (SEQSIZE < RemainingIndex)
				return vimpl::single_visit_w_index_tail<Offset + SEQSIZE, Rtype>(static_cast<Fn&&>(fn), static_cast<V&&>(v));
			else
				DeclareUnreachable();
	}

	#undef X
	#undef SEQSIZE
}

template <class Fn, class V>
constexpr decltype(auto) visit(Fn&& fn, V&& v){
	return vimpl::single_visit_tail<0, rtype_visit<Fn&&, V&&>>(SWL_FWD(fn), SWL_FWD(v));
}

template <class Fn, class Head, class... Tail>
constexpr decltype(auto) multi_visit(Fn&& fn, Head&& head, Tail&&... tail){

	// visit them one by one, starting with the last
	auto vis = [&fn, &head] (auto&&... args) -> decltype(auto) {
		return vimpl::visit( [&fn, &args...] (auto&& elem) -> decltype(auto) {
			return SWL_FWD(fn)( SWL_FWD(elem), SWL_FWD(args)... );
		}, SWL_FWD(head) );
	};

	if constexpr (sizeof...(tail) == 0)
		return SWL_FWD(vis)();
	else if constexpr (sizeof...(tail) == 1)
		return vimpl::visit( SWL_FWD(vis), SWL_FWD(tail)... );
	else
		return vimpl::multi_visit(SWL_FWD(vis), SWL_FWD(tail)...);
}

#undef DEC
#undef SEQ30
#undef SEQ100
#undef SEQ200
#undef SEQ400
#undef DeclareUnreachable
#undef CAT
#undef CAT2
#undef INJECTSEQ

}


#endif
|]
