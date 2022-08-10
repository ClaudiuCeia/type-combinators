type Int<
  T extends string,
  R extends unknown[] = []
> = `${R["length"]}` extends T ? R["length"] : Int<T, [...R, T]>;

export type Split<
  T extends number | string,
  R extends string[] = []
> = `${T}` extends `${infer head}${infer tail}` ? Split<tail, [...R, head]> : R;
type _split = Split<12345>;

type Join<
  T extends (string | number)[],
  Idx extends unknown[] = [],
  R extends string = ""
> = {
  done: R;
  copy: T[Idx["length"]] extends undefined
    ? R
    : Join<T, [...Idx, 1], `${R}${T[Idx["length"]]}`>;
}[Idx["length"] extends T["length"] ? "done" : "copy"];

type Substring<
  S extends string,
  From extends number,
  To extends number,
  Idx extends number = From,
  Acc extends string[] = []
> = {
  done: Join<Acc>;
  copy: S[Idx] extends undefined
    ? never
    : Substring<S, From, To, Inc<Idx>, [...Acc, Split<S>[Idx]]>;
}[Idx extends Split<S>["length"] ? "done" : Idx extends To ? "done" : "copy"];
type _substring = Substring<"abcdefg", 1, 3>;

type Add<N1 extends number, N2 extends number> = [
  ...Repeat<N1>,
  ...Repeat<N2>
]["length"] extends number
  ? [...Repeat<N1>, ...Repeat<N2>]["length"]
  : 0;

/* type Subtract<
  N1 extends number,
  N2 extends number,
  Count extends number = 0,
  Out extends number = N2
> = {
  done: Count;
  increment: Subtract<N1, N2, Inc<Count>, Inc<N2>>;
}[Out extends N1 ? "done" : "increment"];
type _minus = Subtract<10, 5>; */

type Digits = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
type NextDigit<T extends Digits> = {
  "0": "1";
  "1": "2";
  "2": "3";
  "3": "4";
  "4": "5";
  "5": "6";
  "6": "7";
  "7": "8";
  "8": "9";
  "9": "0";
}[T];

type Tail<T> = T extends string
  ? T extends `${infer Head}${infer Rest}`
    ? Rest extends ""
      ? Head
      : Tail<Rest>
    : never
  : T extends number
  ? Int<Tail<`${T}`>>
  : T extends [...infer Head, infer Rest]
  ? Rest
  : never;

type _stail = Tail<"abc123">;
type _ntail2 = Tail<12345>;
type _atail = Tail<[1, 2, 3, 4, 5]>;

type Head<T> = T extends string
  ? T extends `${infer Head}${infer Rest}`
    ? Head
    : never
  : T extends number
  ? Int<Head<`${T}`>>
  : T extends [infer Head, ...infer Rest]
  ? Head
  : never;

type _shead = Head<"abc123">;
type _nhead2 = Head<12345>;
type _anead = Head<[1, 2, 3, 4, 5]>;

type IncTuple<
  T extends string[],
  Idx extends number = Dec<T["length"]>,
  Carry extends boolean = true,
  Acc extends number[] = []
> = {
  done: Carry extends true
    ? T[Idx] extends Digits
      ? NextDigit<T[Idx]> extends "0"
        ? [1, 0, ...Acc]
        : [Int<NextDigit<T[Idx]>>, ...Acc]
      : never
    : [Int<T[Idx]>, ...Acc];
  copy: IncTuple<T, Dec<Idx>, false, [Int<T[Idx]>, ...Acc]>;
  inc: T[Idx] extends Digits
    ? NextDigit<T[Idx]> extends "0"
      ? IncTuple<T, Dec<Idx>, true, [Int<NextDigit<T[Idx]>>, ...Acc]>
      : IncTuple<T, Dec<Idx>, false, [Int<NextDigit<T[Idx]>>, ...Acc]>
    : never;
}[Idx extends 0 ? "done" : Carry extends true ? "inc" : "copy"];

type Inc<T extends number> = IncTuple<Split<T>> extends number[]
  ? Int<Join<IncTuple<Split<T>>>>
  : never;

type _inc2 = Inc<997>;

type Repeat<
  T extends string | number,
  R extends unknown[] = []
> = T extends string
  ? `${R["length"]}` extends T
    ? R
    : Repeat<T, [...R, T]>
  : R["length"] extends T
  ? R
  : Repeat<T, [...R, T]>;

type Dec<T extends number> = Repeat<T> extends [...infer R, infer U]
  ? R["length"]
  : never;

// type AltInc<T extends number> = [...Repeat<T>, 1]["length"];

type Merge<
  T extends Record<string, unknown>,
  U extends Record<string, unknown>
> = {
  [K in keyof T]: K extends keyof U ? U[K] : T[K];
};

type Pop<Arr extends unknown[]> = Arr extends [...infer R, infer U] ? R : never;

// -----------------------------

type Empty = null;

type State<
  Input extends string | string[] = "",
  Idx extends number = 0,
  Tree extends unknown[] = [],
  Errors extends unknown[] = []
> = {
  input: Input extends string ? Split<Input> : Input;
  idx: Idx;
  tree: Tree;
  errors: Errors;
};

/* type DerivedState<S extends State, Derivation extends Partial<State>> = {
  input: Derivation["input"] extends undefined
    ? S["input"]
    : Derivation["input"];
  idx: S["idx"];
  tree: S["tree"];
  errors: S["errors"];
}; */

type NodeKind = "char" | "digit" | "str" | "num" | "op" | "eof";

type Node<
  Kind extends NodeKind,
  Value,
  Start extends number,
  End extends number
> = {
  kind: Kind;
  value: Value;
  start: Start;
  end: End;
};

type Error<Message extends string, Start extends number, End extends number> = {
  message: Message;
  start: Start;
  end: End;
};

type Print<V> = V extends string
  ? `${V}`
  : V extends number
  ? `${V}`
  : V extends [...infer R]
  ? `${R["length"]}`
  : V extends never
  ? "[never]"
  : "[unknown]";

type Read<S, C extends number = 1> = S extends State<
  infer _,
  infer __,
  infer ___
>
  ? S["input"]["length"] extends S["idx"]
    ? [S, undefined]
    : [
        State<Join<S["input"]>, Add<S["idx"], C>, S["tree"], S["errors"]>,
        Substring<Join<S["input"]>, S["idx"], Add<S["idx"], C>>
      ]
  : `Attempted to Read<S> from non-existent state`;

type _read = Read<State<`abc123`, 3>>;
type _read2 = Read<State<`abc123`, 0>, 3>;

type ReplaceLastNode<S1, N> = S1 extends State<
  infer _Input1,
  infer _Idx1,
  infer Tree1,
  infer _Errors1
>
  ? N extends Node<infer _Kind, infer _Value, infer _Start, infer _End>
    ? Merge<S1, { tree: [...Pop<Tree1>, N] }>
    : never
  : never;

type Char = "_Char";
type _Char<What extends string, S> = Read<S> extends infer R
  ? R extends [infer Next, infer Token]
    ? Next extends State<infer _, infer __, infer ___>
      ? Token extends What
        ? Merge<
            Next,
            {
              tree: [
                ...Next["tree"],
                Node<"char", What, Dec<Next["idx"]>, Next["idx"]>
              ];
            }
          >
        : Merge<
            Next,
            {
              errors: [
                ...Next["errors"],
                Error<
                  `Unable to parse Char<'${What}'>, found '${Print<Token>}'`,
                  Dec<Next["idx"]>,
                  Next["idx"]
                >
              ];
            }
          >
      : S
    : never
  : `Unexpected error in Char<S, ${What}>`;

type _cc = _Char<"a", State<"">>;

type Str = "_Str";
type _Str<What extends string, S> = S extends State<
  infer _,
  infer __,
  infer ___
>
  ? Read<S, Split<What>["length"]> extends infer R
    ? R extends [infer Next, infer Token]
      ? Next extends State<infer _, infer __, infer ___>
        ? Token extends What
          ? Merge<
              Next,
              {
                tree: [
                  ...Next["tree"],
                  Node<"str", What, S["idx"], Next["idx"]>
                ];
              }
            >
          : Merge<
              Next,
              {
                errors: [
                  ...Next["errors"],
                  Error<
                    `Unable to parse Str<'${What}'>, found '${Print<Token>}'`,
                    S["idx"],
                    Next["idx"]
                  >
                ];
              }
            >
        : S
      : never
    : `Unexpected error in Char<S, ${What}>`
  : never;

type Digit = "_Digit";
type _Digit<What extends Digits, S> = S extends State<
  infer _s,
  infer __s,
  infer ___s,
  infer ____s
>
  ? P<[Char, What], S> extends infer R
    ? R extends State<infer _s, infer __s, infer ___s, infer ____s>
      ? ReplaceLastNode<R, Node<"digit", What, S["idx"], R["idx"]>>
      : never
    : never
  : never;

type Eof = "_Eof";
type _Eof<S> = S extends State<infer Input, infer Idx, infer _s, infer __s>
  ? Idx extends Input["length"]
    ? Merge<
        S,
        {
          tree: [...S["tree"], Node<"eof", "", Idx, Idx>];
        }
      >
    : Merge<
        S,
        {
          errors: [...S["errors"], Error<"EOF not found", Idx, Idx>];
        }
      >
  : never;

type Parser<What extends string = "", S = never> = {
  _Char: _Char<What, S>;
  _Digit: What extends Digits ? _Digit<What, S> : never;
  _Str: _Str<What, S>;
  _Eof: _Eof<S>;
};

type P<P extends [keyof Parser, string], S> = S extends State<
  infer _s,
  infer __s,
  infer ___s,
  infer ____s
>
  ? Parser<P[1], S>[P[0]]
  : "failed to decode parser";

type _pfromt = P<[Char, "a"], State<`abc123`>>;

type Alt = "_Alt";
type _Alt<
  P1 extends [keyof Parser, string],
  P2 extends [keyof Parser, string],
  S,
  D1 = P<P1, S>,
  D2 = P<P2, S>
> = S extends State<infer _s, infer __s, infer ___s, infer ____s>
  ? D1 extends State<infer _d1, infer __d1, infer ___d1, infer ____d1>
    ? D2 extends State<infer _d2, infer __d2, infer ___d2, infer ____d1>
      ? D1["errors"]["length"] extends 0
        ? D1
        : D2["errors"]["length"] extends 0
        ? D2
        : Merge<
            S,
            {
              errors: [
                Error<
                  `Alt<> failed to match either parser`,
                  S["idx"],
                  S["input"]["length"]
                >,
                ...D1["errors"],
                ...D2["errors"]
              ];
            }
          >
      : `Unexpected error in Alt<S, >`
    : never
  : never;

type ManyTill = "_ManyTill";
type _ManyTill<
  P1 extends [keyof Parser, string],
  P2 extends [keyof Parser, string],
  S,
  D1 = P<P1, S>,
  D2 = P<P2, S>
> = {
  end: D2;
  parserFail: S extends State<infer _s, infer __s, infer ___s, infer ____s>
    ? D1 extends State<infer _d1, infer __d1, infer ___d1, infer ____d1>
      ? D2 extends State<infer _d2, infer __d2, infer ___d2, infer ____d1>
        ? Merge<
            S,
            {
              errors: [
                Error<
                  `ManyTill<> failed to match either parser`,
                  S["idx"],
                  S["input"]["length"]
                >,
                ...D1["errors"],
                ...D2["errors"]
              ];
            }
          >
        : never
      : never
    : never;
  combinatorFail: `Unexpected error in _ManyTill<S, >`;
  next: _ManyTill<P1, P2, D1>;
  control: "control";
}[S extends State<infer _s, infer __s, infer ___s, infer ____s>
  ? D1 extends State<infer _d1, infer __d1, infer ___d1, infer ____d1>
    ? D2 extends State<infer _d2, infer __d2, infer ___d2, infer ____d1>
      ? D2["errors"]["length"] extends 0
        ? "end"
        : D1["errors"]["length"] extends 0
        ? "next"
        : "parserFail"
      : "combinatorFail"
    : never
  : never];

type _eof = P<[Eof, ""], P<[Char, "a"], State<"a">>>;

type _s = State<`aaabccc`>;
type _aaab = _ManyTill<[Char, "a"], [Char, "b"], _s>;
type _m = P<[Char, "c"], _aaab>;

type _m2 = State<`cc`>;
type _x = _ManyTill<[Char, "c"], [Eof, ""], _m2>;

/* type _many = ManyTill<
    P<[Char, "c"], ManyTill<[Char, "a"], [Char, "b"], >>,
    P<[Eof]
>["tree"]; */
/* 
type _state = State<`abc123`, 0>;
type _char1 = P<[Digit, "1"], Alt<[Str, "def"], [Str, "abc"], _state>>;
type _char = P<[Digit, "1"], Alt<[Str, "def"], [Str, "abc"], _state>>;

type _abc = P<[Str, "abc"], _state>;
type _char2 = Alt<[Str, "321"], [Str, "123"], _abc>;
 */
