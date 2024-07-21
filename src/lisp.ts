type brand<t, brand extends string> = t & { __brand: brand };
type sym<s extends string> = brand<s, 'sym'>
type nil = brand<[], 'nil'>
type cons<x, y> = [x, y];
type car<l> = l extends [infer h, unknown] ? h : never;
type cdr<l> = l extends [unknown, infer t] ? t : never;
type cadr<l> = car<cdr<l>>;
type cdar<l> = cdr<car<l>>;
type caddr<l> = car<cdr<cdr<l>>>;
type caar<l> = car<car<l>>;
type cadar<l> = car<cdr<car<l>>>;
type caddar<l> = car<cdr<cdr<car<l>>>>;

type list<arr extends unknown[]> =
  arr extends []
    ? nil
  : arr extends [infer first, ...(infer rest)] 
    ? cons<first, list<rest>>
  : never;

type atom<t> = t extends cons<unknown, unknown> ? false : true;

type subst<x, y, z> = 
  atom<z> extends true 
    ? eq<z, y> extends true
      ? x
    : z
  : z extends cons<infer h, infer t>
    ? cons<subst<x, y, h>, subst<x, y, t>>
  : never;

type and<x, y> = x extends true ? y extends true ? true : false : false;
type or<x, y> = x extends true ? true : y extends true ? true : false;
type not<x> = x extends true ? false : true;

type eq<x, y> = x extends y ? y extends x ? true : false : false;

type equal<x, y> = 
  atom<x> extends true 
    ? atom<y> extends true 
      ? eq<x, y>
    : false
  : x extends cons<infer xh, infer xt>
    ? y extends cons<infer yh, infer yt>
      ? and<eq<xh, yh>, equal<xt, yt>>
    : false
  : never;

type nullp<x> = atom<x> extends true ? eq<x, nil> : false;

type append<x, y> =
  nullp<x> extends true 
    ? y 
  : x extends [infer h, infer t] 
    ? cons<h, append<t, y>>
  : never;

type among<x, y> =
  and<
    not<nullp<x>>,
    y extends [infer h, infer t]
      ? or<
          equal<x, h>,
          among<x, t>
        >
    : false
  >

type pair<x, y> =
  and<nullp<x>, nullp<y>> extends true
    ? nil
  : and<not<atom<x>>, not<atom<y>>> extends true
    ? cons<list<[car<x>, car<y>]>, pair<cdr<x>, cdr<y>>>
  : never;

type assoc<x, y> =
  eq<caar<y>, x> extends true
    ? cadar<y>
  : assoc<x, cdr<y>>;

type sub2<x, z> =
  nullp<x> extends true
   ? z
  : eq<caar<x>, z> extends true
    ? cadar<x>
  : sub2<cdr<x>, z>;


type sublis<x, y> =
  atom<y> extends true
    ? sub2<x, y>
  : cons<sublis<x, car<y>>, sublis<x, cdr<y>>>

type printDot = '.';

type print<x, isList = false> =
  atom<x> extends true
    ? x extends sym<infer a>
      ? `${a}`
    : x extends nil
      ? `'()`
    : x extends string | number | bigint | boolean | null | undefined
      ?`${x}`
    : never
  : atom<cdr<x>> extends true
    ? isList extends true
      ? nullp<cdr<x>> extends true
        ? `${print<car<x>>}`
      : `${print<car<x>>} ${printDot} ${print<cdr<x>>}`
    : `(${print<car<x>>} ${printDot} ${print<cdr<x>>})`
  : isList extends true
    ? `${print<car<x>>}, ${print<cdr<x>, true>}`
  : `(${print<car<x>>}, ${print<cdr<x>, true>})`;


type ATOM = sym<'ATOM'>;
type LABEL = sym<'LABEL'>;
type QUOTE = sym<'QUOTE'>;
type COND = sym<'COND'>;
type LAMBDA = sym<'LAMBDA'>;
type CONS = sym<'CONS'>;
type CAR = sym<'CAR'>;
type CDR = sym<'CDR'>;
type EQ = sym<'EQ'>;
type NIL = sym<'NIL'>;
type T = sym<'T'>

type evcon<c, a> =
  eval<caar<c>, a> extends true
    ? eval<cadar<c>, a>
  : evcon<cdr<c>, a>;

type evlis<m, a> =
  nullp<m> extends true
    ? nil
  : cons<
      eval<car<m>, a>,
      evlis<cdr<m>, a>
    >

type eval<e, a> =
  atom<e> extends true
    ? e extends NIL
      ? nil
    : e extends T
      ? true
    : assoc<e, a>
  : atom<car<e>> extends true
    ? eq<car<e>, QUOTE> extends true
      ? cadr<e>
    : eq<car<e>, ATOM> extends true
      ? atom<eval<cadr<e>, a>>
    : eq<car<e>, EQ> extends true
      ? eq<eval<cadr<e>, a>, eval<caddr<e>, a>>
    : eq<car<e>, COND> extends true
      ? evcon<cdr<e>, a>
    : eq<car<e>, CAR> extends true
      ? car<eval<cadr<e>, a>>
    : eq<car<e>, CDR> extends true
      ? cdr<eval<cadr<e>, a>>
    : eq<car<e>, CONS> extends true
      ? cons<
          eval<cadr<e>, a>,
          eval<caddr<e>, a>
        >
    : eval<
        cons<
          assoc<car<e>, a>,
          cdr<e>
        >,
        a
      >
  : eq<caar<e>, LABEL> extends true
    ? eval<
        cons<caddar<e>, cdr<e>>,
        cons<list<[cadar<e>, car<e>]>, a>
      >
  : eq<caar<e>, LAMBDA> extends true
    ? eval<
       caddar<e>,
       append<
         pair<cadar<e>, evlis<cdr<e>, a>>,
         a
       >
      >
  : never;

type appq<m> =
  nullp<m> extends true
    ? nil
  : cons<list<[QUOTE, car<m>]>, appq<cdr<m>>>;

type apply<f, args> = eval<cons<f, appq<args>>, nil>;

// (LABEL HAVE-A 
//   (LAMBDA (X A) 
//     (COND
//       ((EQ A NIL) NIL)
//        ((NOT (EQ (ASSOC X A) NIL)) T))))



// @ts-ignore 
type n = print<
  apply<
    list<[
      LABEL,
      sym<'FF'>,
      list<[
        LAMBDA,
        list<[sym<'X'>]>,
        list<[
          COND,
          list<[
            list<[ATOM, sym<'X'>]>,
            sym<'X'>
          ]>,
          list<[
            sym<'T'>,
            list<[sym<'FF'>, list<[CAR, sym<'X'>]>]>
          ]>
        ]>
      ]>
    ]>,
    list<[
      list<[
        cons<sym<'A'>, sym<'B'>>,
        cons<sym<'C'>, sym<'D'>>
      ]>
    ]>
  >
>

// type symbolchar =
//   | 'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' | 'g' | 'G'
//   | 'h' | 'H' | 'i' | 'I' | 'j' | 'J' | 'k' | 'K' | 'l' | 'L' | 'm' | 'M' | 'n' | 'N'
//   | 'o' | 'O' | 'p' | 'P' | 'q' | 'Q' | 'r' | 'R' | 's' | 'S' | 't' | 'T' | 'u' | 'U'
//   | 'v' | 'V' | 'w' | 'W' | 'x' | 'X' | 'y' | 'Y' | 'z' | '1' | '2' | '3' | '4' | '5'
//   | '6' | '7' | '8' | '9' | '0' | '-' | '!' | '_' | '?' | '*' | '^' | '$' | '@' | '%'
//   | '&' | '|' | '='

// type symbolstartchar = Exclude<
//   symbolchar,
//   '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'
// >


// type readSymbol<s extends string> = '';

// type readCons<s extends string> = '';

// type readAtom<s extends string> = '';

// type read<s extends string> =
//   s extends `(${string}`
//    ? readCons<s> extends infer result
//      ? result extends [true, infer cons]
//          ? cons
//      : result extends [false, infer error]
//          ? err
//      : 
   
//   s extends `'${symbolstartchar}${string}`
//     ? readSymbol<s>
//   : s extends `(${string})${string}`
//     ? readConsBody<s>
//   : s extends `"${string}"`
//     ? readString<s>


