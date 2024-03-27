// property lists and some other supporting code

package main

import "fmt"

// key on strings
type PlistCode = string

// Plist is a linked list of key-value pairs
// The value slot is an empty interface
// The client must know how to decode possible types of value given ident.
// Some generic methods are provided: Find, Add, Delete.
// To use Plists, embed a Plist struct in your favorite struct; all the methods
// are on *Plist, so you will generally need to take addresses. The mod methods
// assume this convention and mod the passed entity as needed.
type Plist struct {
	ident PlistCode
	value interface{}
	next  *Plist
}

func zeroPlist() Plist { return Plist{"", nil, nil} }

// internal version of find
func (pl *Plist) find(code PlistCode) *Plist {
	for pl != nil {
		if pl.ident == code {
			return pl
		}
		pl = pl.next
	}
	return nil
}

// separate internal version to find for deletion; pl.next known != nil
func (pl *Plist) find4next(code PlistCode) *Plist {
	for pl.next != nil {
		if pl.next.ident == code {
			return pl
		}
		pl = pl.next
	}
	return nil
}

// return the first elt with a matching code, or nil
func (pl *Plist) Find(code PlistCode) interface{} {
	plf := pl.find(code)
	if plf != nil {
		return plf.value
	} else {
		return nil
	}
}

// If code exists, replace the value there, else alloc new.
func (pl *Plist) Add(code PlistCode, val0 interface{}) {
	if pl.value == nil {
		// empty Plist, just add
		pl.ident = code
		pl.value = val0
	} else {
		plc := pl.find(code)
		if plc == nil {
			// add elt on front of LL
			plc = &Plist{pl.ident, pl.value, pl.next}
			pl.ident = code
			pl.value = val0
			pl.next = plc
		} else {
			plc.value = val0
		}
	}
}

// Like Add but create and/or append to a list rather than replacing the value
func (pl *Plist) Accum(code PlistCode, val interface{}) {
	xstg := pl.Find(code)
	if xstg == nil {
		val0 := []interface{}{val}
		pl.Add(code, val0)
	} else {
		xstg0 := xstg.([]interface{})
		pl.Add(code, append(xstg0, val))
	}
}

// Copy and return the linked list.
func (pl *Plist) Copy() *Plist {
	cpy := &Plist{pl.ident, pl.value, nil}
	pl = pl.next
	for plcp := cpy; pl != nil; pl = pl.next {
		plcp.next = &Plist{pl.ident, pl.value, nil}
		plcp = plcp.next
	}
	return cpy
}

// Like copy but add opl into pl.
func (pl *Plist) CopyInto(opl *Plist) {
	if opl.ident == "" {
		return
	}
	for pl.next != nil {
		pl = pl.next
	}
	for ; opl != nil; opl = opl.next {
		nxpl := &Plist{opl.ident, opl.value, nil}
		pl.next = nxpl
		pl = nxpl
	}
}

func (pl *Plist) String() string {
	if pl.ident == "" {
		return ""
	}
	ret := ""
	for ; pl != nil; pl = pl.next {
		ret += fmt.Sprint(pl.ident, "=", pl.value, " ")
	}
	return ret
}

// Delete the first (should be only) occurrence of code; return bool true if something was deleted.
func (pl *Plist) Delete(code PlistCode) bool {
	if pl.ident == code {
		pln := pl.next
		if pln == nil {
			pl.ident = ""
			pl.value = nil
		} else {
			pl.ident = pln.ident
			pl.value = pln.value
			pl.next = pln.next
		}
		return true
	} else {
		pln := pl.find4next(code)
		if pln != nil {
			pln.next = pln.next.next
		}
		return pln != nil
	}
}
/**** END OF PLIST *****/


// Manually instantiated binsearch, adapted to return the index of the greatest elt <= tgt (or -1).
// Similar to code in go compiler front end. What's funny about the comments there is that
// they treat the need to instantiate by hand as a compiler optimization failure whereas I see
// it as more of a language design problem.
// Anyway, this is used to extract line numbers from positions given a linestarts slice.
// As well as other general []int searches.
func binsearchInts(ints []int, tgt int) int {
	if len(ints) == 0 || ints[len(ints)-1] < tgt {
		return -1
	}
	mn, mx := 0, len(ints)-1
	cur := mx/2
	for ; mn <= mx;  {
		v := ints[cur]
		if(v < tgt) {
			mn = cur+1
		} else {
			mx = cur-1
		}
		cur = mn + (mx - mn)/2
	}
	return cur
}

// same deal for strings
func binsearchStrings(strgs []string, tgt string) int {
	if len(strgs) == 0 || strgs[len(strgs)-1] < tgt {
		return -1
	}
	mn, mx := 0, len(strgs)-1
	cur := mx/2
	for ; mn <= mx;  {
		v := strgs[cur]
		if(v < tgt) {
			mn = cur+1
		} else {
			mx = cur-1
		}
		cur = mn + (mx - mn)/2
	}
	return cur
}

// insert tgt into strgs at inx; presumably this is sorted but that's up to the caller
func insertStrings(strgs []string, inx int, tgt string) []string {
	nstrgs := len(strgs)
	strgs = append(strgs, tgt)
	if inx >= 0 {
		copy(strgs[inx+1:nstrgs+1], strgs[inx:nstrgs])
		strgs[inx] = tgt
	}
	return strgs
}

// symbolSlice packages a couple useful methods with []*Symbol
type symbolSlice []*Symbol

// Like the other binsearch ftns, this returns the index of the first elt >= the one
// sought, or -1. Note that -1 can mean (1) syms is empty or (2) syms.last.ident < tgt. In either case,
// the right place to add a symbol with ident==tgt is at the end.
func (syms symbolSlice) binsearch(tgt string) int {
	if len(syms) == 0 || syms[len(syms)-1].ident < tgt {
		return -1
	}
	mn, mx := 0, len(syms)-1
	cur := mx/2
	for ; mn <= mx;  {
		v := syms[cur].ident
		if(v < tgt) {
			mn = cur+1
		} else {
			mx = cur-1
		}
		cur = mn + (mx - mn)/2
	}
	return cur
}

// is osym in syms? Return index if so, else -1
func (syms symbolSlice) findsym(osym *Symbol) int {
	inx := syms.binsearch(osym.ident)
	if inx >= 0 {
		for ; inx < len(syms); inx++ {
			if osym == syms[inx] {
				return inx
			}
			if syms[inx].ident != osym.ident {
				break
			}
		}
	}
	return -1
}

func (syms *symbolSlice) copy() *symbolSlice {
	rslt := make(symbolSlice, len(*syms))
	copy(rslt, *syms)
	return &rslt
}

// inx is what binsearch returns; insert sym there. Note that inx < 0 means at end.
func (syms *symbolSlice) insert(inx int, sym *Symbol) {
	nsyms := len(*syms)
	*syms = append(*syms, sym)
	if inx >= 0 {
		copy((*syms)[inx+1:nsyms+1], (*syms)[inx:nsyms])
		(*syms)[inx] = sym
	}
}

// delete the symbol at inx; inx < 0 means from end
func (syms *symbolSlice) delete(inx int) {
	lenm1 := len(*syms) - 1
	if inx >= 0 && inx < lenm1 {
		copy((*syms)[inx:lenm1], (*syms)[inx+1:lenm1+1])
	}
	*syms = (*syms)[0:lenm1]
}

// Bitset is an abstract interface for a bitset, initially max 64b but easily changeable to handle more
type Bitset uint

func makeBitset(maxbits int) Bitset {
	if maxbits > 64 {
		panic("bitset size not implemented")
	}
	return 0
}

func (bs Bitset) eltof(val int) bool {
	return (bs & (1 << uint(val))) != 0
}

func (bs Bitset) isEmpty() bool {
	return bs == 0
}

func (bs Bitset) add(val int) Bitset {
	return bs | (1 << uint(val))
}

func (bs Bitset) delete(val int) Bitset {
	return bs &^ (1 << uint(val))
}

func (bs Bitset) union(obs Bitset) Bitset {
	return bs | obs
}

func (bs Bitset) intersect(obs Bitset) Bitset {
	return bs & obs
}
