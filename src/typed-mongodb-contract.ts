import type {
  Document,
  Filter,
  CollationOptions,
} from 'mongodb';

type DottedAccessNotation<S extends string> = 
  S extends `${infer Target}.${infer Selector}.${infer S1}`
    ? { target: Target, selector: DottedAccessNotation<`${Selector}.${S1}`>}
  : S extends `${infer Target}.${infer Selector}`
    ? { target: Target, selector: Selector }
  : { target: S, selector: never };


type StringKeysOf<T extends Document> = {
  [K in keyof T]: K extends string ? K : never;
}[keyof T];

type FilteredPositionalOperatorIdentifier<S extends string> = S extends `${string}.${string}` ? never : S;

type ArrayPositionSelectors = [
  `${number}`,  
  "$",  
  "$[]",  
  `$[${FilteredPositionalOperatorIdentifier<string>}]`
];

type ArrayPositionSelectorsOf<T, Prefix extends string = ""> =
 T extends Document
   ? [
       `${Prefix}${ArrayPositionSelectors}`, 
        FlattenedSelectorsOfDocument<T, `${Prefix}.${ArrayPositionSelectors}.`>,
     ]
 : [`${Prefix}${ArrayPositionSelectors}`];

type FlattenedArrayPositionSelectorsOf<T, Prefix extends string = ""> =
  ArrayPositionSelectorsOf<T, Prefix>[number]

type GronRecord<S extends string = string, T extends unknown = unknown> = { selector: S, type: T };

type Lambda = {
  input: unknown,
  output: unknown
}

type Apply<
  L extends Lambda,
  Input extends unknown[],
> = (L & {input: Input})['output'];

interface Map extends Lambda {
  output: this['input'] extends [infer L, infer TArr] ? L extends Lambda ?
    TArr extends [] 
      ? [] 
    : TArr extends [infer First, ...infer Rest] 
      ? [Apply<L, [First]>, ...Apply<Map, [L, Rest]>]
    : never : never : never
}

interface AppendPrefix extends Lambda {
  output:
    this['input'] extends [infer Prefix, infer S]
    ? Prefix extends string ? S extends string
       ? `${Prefix}${S}`
     : never : never
    : never
}

interface AppendPrefixToSelector extends Lambda {
  output:
    this['input'] extends
    [infer Prefix, infer GR] ? Prefix extends string ? GR extends GronRecord<infer Selector, infer RType> ?
    GronRecord<Apply<AppendPrefix, [Prefix, Selector]>, RType>
    : never : never : never
}

interface Curry<
  F extends Lambda,
  FirstArg,
> extends Lambda {
  output: this['input'] extends [...(infer RestArgs)] ?
    Apply<F, [FirstArg, ...RestArgs]>
  : never
}

interface Flip<F extends Lambda> extends Lambda {
  output: this['input'] extends [infer First, ...(infer Rest)]
  ? Apply<F, [...Rest, First]>
  : never
}

interface Id extends Lambda {
  output: this['input'] extends [infer A] ? A : never;
}

interface AppendPrefixToSelectors extends Lambda {
  output:
  this['input'] extends [infer Prefix, infer Records] 
  ? Prefix extends string ? Records extends GronRecord[]
  ? Apply<
      Map,
      [
        Curry<AppendPrefixToSelector, Prefix>,
        Records
      ]
    >
  : never : never : never
}  

type Simplify<T> = {} & { [K in keyof T]: T[K] };

interface AppendPrefixesToSelectors extends Lambda {
  output:
  this['input'] extends [[...(infer Prefixes)], [...(infer Records)]]
  ? Prefixes extends string[] ? Records extends GronRecord[]
  ? Apply<
      Map,
      [
        Curry<Flip<AppendPrefixToSelectors>, Records>,
        Prefixes
      ]
    >
  : never 
: never : never;  
}

interface AppendPrefixToStrings extends Lambda {
  output: this['input'] extends [infer Prefix, infer SArr] ? Prefix extends string ? SArr extends string[]
  ? Apply<Map, [Curry<AppendPrefix, Prefix>, SArr]>
 : never : never : never
}

interface Index extends Lambda {
  output: this['input'] extends [infer T, infer Key] ? 
  T extends Record<string, unknown> 
    ? Key extends keyof T
      ? T[Key]
    : never
  : T extends unknown[]
    ? Key extends number
      ? T[Key]
    : never
  : never : never;
}

interface Cons extends Lambda {
  output: this['input'];
}

interface EntryAt extends Lambda {
  output: this['input'] extends [infer T, infer Key] ? T extends Record<string, unknown> ? Key extends keyof T ?
    Apply<Cons, [Key, T[Key]]>
: never : never : never;
}

interface Entries extends Lambda {
  output: this['input'] extends [infer T] ? T extends Record<string, unknown> ?
    Apply<
      Map,
      [
        Curry<EntryAt, T>,
        UnionToTuple<StringKeysOf<T>>
      ]
    >
  : never : never;
}

type UnionToIntersection<U> = (
  U extends never ? never : (arg: U) => never
) extends (arg: infer I) => void
  ? I
  : never;

type UnionToTuple<T> = UnionToIntersection<
  T extends never ? never : (t: T) => T
> extends (_: never) => infer W
  ? [...UnionToTuple<Exclude<T, W>>, W]
  : [];

interface Call extends Lambda {
  output: this['input'] extends [infer L, infer Args] ? L extends Lambda ? Args extends unknown[]
  ? Apply<L, Args>
  : never : never : never;
}

interface Gron extends Lambda {
  output: this['input'] extends [infer T, infer Prefix] ? Prefix extends string
  ? T extends [...(infer UArr)] 
    ? Apply<
        AppendPrefixesToSelectors, 
        [
          Apply<Map, [Curry<Flip<AppendPrefix>, '.'>, ArrayPositionSelectors]>, 
          Apply<Map, [Curry<Flip<Gron>, Prefix>, UArr]>
        ]
      >
  : T extends Document
    ? Apply<Map, [Curry<Call, Flip<Gron>>, Apply<Entries, [T]>]>
  : GronRecord<Prefix, T>
  : never : never;
}

type BBB = { 
  thing: string, 
  bing: number, 
  joe: 'mama', 
  hurtle: [
    'bing', 
    'bang', 
    // { 
    //   ginormous: symbol, 
    //   woman: { 
    //     good: string, 
    //     god: 'damn'
    //   }
    // }
  ]
};
type B = Apply<AppendPrefixesToSelectors, [ Apply<Map, [Curry<Flip<AppendPrefix>, '.'>, ArrayPositionSelectors]>, [GronRecord<'thing', number>, GronRecord<'bing', string>]]>

type SelectorsOfDocument<T extends Document, Prefix extends string = ""> =
  {
    [K in StringKeysOf<T>]:
      T[K] extends Date
        ? [`${Prefix}${K}`]
      : T[K] extends (infer U)[]
          ? U extends Document
            ? [
                `${Prefix}${K}`, 
                FlattenedArrayPositionSelectorsOf<`${Prefix}${K}`, `${Prefix}${K}.`>, 
                FlattenedSelectorsOfDocument<U, `${Prefix}${K}.`>
            ]
          : [
              `${Prefix}${K}`, 
              FlattenedArrayPositionSelectorsOf<`${Prefix}${K}`, `${Prefix}${K}.`>
          ]
      : T[K] extends Document
        ? [`${Prefix}${K}`, FlattenedSelectorsOfDocument<T[K], `${Prefix}${K}.`>]
      : [`${Prefix}${K}`]
  }[StringKeysOf<T>]


type FlattenedSelectorsOfDocument<T extends Document, Prefix extends string = ""> =
  SelectorsOfDocument<T, Prefix>[number];

type A = FlattenedSelectorsOfDocument<{
  foo: string,
  bar: string,
  spingus: Date,
  quux: {
    bing: number,
    bang: string,
    thing: number[],
    bong: {
      bingo: string,
      bango: { buckle: 'my', shoe: number },
      bongo: string[],
      [Symbol.for('mom')]: 'mom',
    }[]
  }
}>;

type NestedAccessNotation<Schema, O extends Record<string, unknown>> = {
  [K in keyof O]: {
    ...DottedAccessNotation<K>,
    value: 
  }
}

type B = DottedAccessNotation<"hello.name.fellow.$">

type InsertOneModel<TCreate> = { document: TCreate };

type ReplaceOneModel<TRead, TReplace> = {
  filter: Filter<TRead>,
  replacement: TReplace,
  collation?: CollationOptions,
  hint?: string | Document,
  upsert?: boolean;
};

type UpdateOneModel<TRead, TUpdate, ArrayIdentifier extends string> = {
  filter: Filter<TRead>,
  collation?: CollationOptions,
  hint?: string | Document,
  upsert?: boolean,
}

export type TypedAnyBulkWriteOperation<
  TRead extends Document,
  TCreate extends Document,
  TUpdate extends Document,
  TReplace extends Document,
  TDelete extends Document,
>= {
  insertOne: InsertOneModel<TCreate>,
} |
{
  replaceOne: ReplaceOneModel<TRead, TReplace>,
} |
{
  
}
